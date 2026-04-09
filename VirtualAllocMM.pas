unit VirtualAllocMM;

///<Summary>
///  A Custom Debugging Memory Manager, which encapuslates each GetMem memory chunk in its own page, provoking AV exceptions on Access errors.
///</Summary>
///<Remarks>
///  This helper Memory Manager can be used in combination with our patched FASTMM4 by using the useVAMM precompiler defintion or as a drop in replacement.
///  Use it by defining 'useVAMM' to disable FASTMM4
///</Remarks>
///<Author>Hanno Hugenberg</Author>
///<Version>0.2</Version>

interface

{$DEFINE useVAMM}


const
  {.$DEFINE USE_4K_RESERVATIONS}
  // on x68, small allocations are usually consuming all possible VirtualAlloc calls (~64k)
  // - to fix this, we reserve a special memory region for vars < 4k and pick them from there in 4k steps (with 4k uncommited space in between)
  // - so we double the following value for allocation - def 100k = 200k*4k space = 0.76 GB
  // default = 100000
  GReserved4kPages = 100000; //use 100k 4k pages, since we had ~60k max allocation in our software
  GMaxStatLogDepth = 20; //log


  //Alignement
  //16 is the general default (QUAD DWORD Alignement)
  // - using 1 detects all errors which access invalid data with an offset of 1
  // - using n < 16 may introduce problems when dealing with alignment optimized code or SSE/MMX code
  GVAMMDefaultByteAlignement = 1;

type
  NativeInt = Integer;

  TVAMMStats = record
    GetMemCalls:    UInt64;
    FreeMemCalls:   UInt64;
    ReallocCalls:   UInt64;
    AllocatedPages: UInt64;
    AllocatedBytes: UInt64;
    Allocated64ks:  UInt64;
    //allocated pages, index callced by Top(Log2(Top(ASize / dwPageSize)))
    AllocPage2nT: array [0..GMaxStatLogDepth] of Integer; //Total
    AllocPage2nA: array [0..GMaxStatLogDepth] of Integer; //Active
    AllocPage2nM: array [0..GMaxStatLogDepth] of Integer; //Max all time
    Free4KPages: UInt64;
    Reserved4KPages: UInt64;
    Allocated4KBytes: UInt64;
  end;

  function GetVAMMStats: TVAMMStats;

  ///<Summary> Set Memory Manager Byte Alignement. Can be [1 - 'page size'] bytes (e.g. windows 4k = page alignement)
  procedure SetVAMMByteAlignement(const AAlignement: Cardinal);
  function GetVAMMByteAlignement: Cardinal;

procedure RaiseException(dwExceptionCode, dwExceptionFlags, nNumberOfArguments: Cardinal;
  lpArguments: PCardinal); stdcall; external 'kernel32.dll' name 'RaiseException';

implementation

uses
  Windows,
  Math;

//{$POINTERMATH on}

type
  TPageInfo = record
    Ptr: Pointer;
    Size: NativeInt
  end;
  PPageInfo = ^TPageInfo;

var
  GPageMap: PPageInfo;
  GStats:   TVAMMStats;
  GSysInfo: TSystemInfo;
//Allocation Pages Granularity = Number of Pages per Allocation Step - e.g. VirtualAlloc has 64k but 4k Page Size = 16 Page Granularity (PG)
  GAllocPG: Cardinal;
  GOldMM:   TMemoryManagerEx;
  GLock:    TRTLCriticalSection;
  GAlignement: Cardinal = GVAMMDefaultByteAlignement;
  GInitialized: Boolean = False;

{$IFDEF USE_4K_RESERVATIONS}
var
  G4KPagesPtr: Pointer;
  //to optimize reusage of freed 4k pages, we keep a ringbuffer of free 4k page indices
  G4KFreeArr: array [0..GReserved4kPages-1] of Cardinal;
  G4KFreeFront, G4kFreeBack: Cardinal; //front & back index of ringbuffer (fetch from from and increment, store to end an increment)
{$ENDIF}

{$IFDEF CPUX64}
const
  //On x64 systems, since we store page information !per page!, we should not exceed a certain amount of array space for this
  // - in theory, windows supports 128TB of virtual memory
  // - this would result in 128TB / 4KB = 32G indices
  // - since we store a (Ptr+ASize) info which needs 16byte per index, we would need 512GB.
  // - we limit ourself to an internal array of 4GB ( 1/128th) which still allows 1TB of Virtual Memory or 256M Pages
  GMaxMapArrSize = UInt64(1024)*1024*1024*4;
var
  //will be calculated during Initialization
  GMaxPtr: Pointer;
{$ENDIF}

function GetVAMMStats: TVAMMStats;
begin
{$IFDEF USE_4K_RESERVATIONS}
  GStats.Free4KPages := (G4kFreeBack-G4KFreeFront)+1;
{$ENDIF}
  Result := GStats;
end;

procedure SetVAMMByteAlignement(const AAlignement: Cardinal);
begin
  EnterCriticalSection(GLock);
  GAlignement := Math.EnsureRange( AAlignement, 1, GSysInfo.dwPageSize );
  LeaveCriticalSection(GLock);
end;

function GetVAMMByteAlignement: Cardinal;
begin
  Result := GAlignement;
end;

procedure _AssertNotContinuable;
var
  ReturnAddr: Pointer;
begin
  asm
    MOV EAX, [ESP]
    MOV ReturnAddr, EAX
  end;

  RaiseException($C0000025, 1, 1, @ReturnAddr);
end;



// internal functions

{$IFDEF USE_4K_RESERVATIONS}
function _AllocFrom4kReserve(ASize: NativeInt): Pointer;
var
  LIdx : Cardinal;
begin
  Result := nil;
  if (G4KFreeFront >= G4KFreeBack) then Exit();

  //get array index of next free 4k page
  LIdx := (G4KFreeFront mod GReserved4kPages);
  //calc address from 4k-page-index
  Result := Pointer( NativeUInt(G4KPagesPtr) + NativeUInt(G4KFreeArr[LIdx])*NativeUInt(GSysInfo.dwPageSize) );
  G4KFreeArr[LIdx] := Cardinal(-1);
  //set front pointer to next element
  AtomicIncrement(G4KFreeFront,1);
  //now commit this page to make it accessible
  Result := VirtualAlloc(Result, GSysInfo.dwPageSize, MEM_COMMIT, PAGE_READWRITE);
end;

function _FreeTo4kReserve(APointer: Pointer): Boolean;
var
  LIdx: Cardinal;
begin
  Result := False;
  if (G4KFreeBack-G4KFreeFront) >= GReserved4kPages then
  begin
    Assert(False, 'VAMM: Can not free more 4k pages then reserved');
    _AssertNotContinuable;
  end;

  //calc array index of next free pointer block
  LIdx := (G4KFreeBack mod GReserved4kPages);

  G4KFreeArr[LIdx] := Cardinal(((NativeUInt(APointer) - (NativeUInt(APointer) mod GSysInfo.dwPageSize)) - NativeUInt(G4KPagesPtr)) div GSysInfo.dwPageSize);
  Inc(G4KFreeBack);

  Result := not VirtualFree(APointer, GSysInfo.dwPageSize, MEM_DECOMMIT);
end;

function _IsFrom4kReserve(APointer: Pointer): Boolean;
begin
  Result := Math.InRange( NativeUInt(APointer),
                          NativeUInt(G4KPagesPtr),
                          NativeUInt(G4KPagesPtr) + (((GReserved4kPages*2)-1)*GSysInfo.dwPageSize));

end;

{$ENDIF}


function AtomicIncrement(var Target: Integer; Increment: Integer = 1): Integer; overload;
asm
  MOV ECX, EDX        // ECX = Increment
  MOV EDX, EAX        // EDX = @Target
  MOV EAX, ECX        // EAX = Increment
  LOCK XADD [EDX], EAX
  ADD EAX, ECX        // result = old + increment
end;

function AtomicIncrement(var Target: UInt64; Increment: UInt64 = 1): UInt64; overload;
asm
  PUSH ESI
  PUSH EDI
  PUSH EBX
  PUSH EBP

  MOV ESI, EAX        // @Target

  // Save Increment (EDX:ECX)
  MOV EDI, EDX        // inc low
  MOV EBP, ECX        // inc high

  @Retry:
    MOV EAX, [ESI]    // current low
    MOV EDX, [ESI+4]  // current high

    MOV EBX, EAX      // old low
    MOV ECX, EDX      // old high

    ADD EBX, EDI      // new low
    ADC ECX, EBP      // new high

    LOCK CMPXCHG8B [ESI]
    JNZ @Retry

    MOV EAX, EBX
    MOV EDX, ECX

  POP EBP
  POP EBX
  POP EDI
  POP ESI
end;


function AtomicDecrement(var Target: Integer; Decrement: Integer = 1): Integer; overload;
begin
  Result := AtomicIncrement(Target, Decrement*-1);
end;

function AtomicDecrement(var Target: UInt64; Decrement: UInt64 = 1): Integer; overload;
begin
  Result := AtomicIncrement(Target, Decrement*-1);
end;

procedure SetPageMap(AIndex: NativeUInt; APointer: Pointer; ASize: NativeInt);
var
  LPage: PPageInfo;
begin
  LPage := PPageInfo( NativeUInt(GPageMap) + (AIndex * SizeOf(TPageInfo)));
  LPage.Ptr := APointer;
  LPage.Size := ASize;
end;

function GetPageMap(AIndex: NativeUInt): PPageInfo;
begin
  Result := PPageInfo( NativeUInt(GPageMap) + (AIndex * SizeOf(TPageInfo)));
end;

function _GetMem(ASize: NativeInt): Pointer;
var
  LIndex: NativeUInt;
  LOffset: NativeUInt;
  LOverlapNeeded: Boolean;
begin
  AtomicIncrement(GStats.GetMemCalls);
  //if (ASize <= 0) then Exit(nil);

  //we want to provoke Access Violations for RW access in case of Buffer Overflows.
  // - we do this by guaranteeing, that the page after our memory buffer is not used.
  // - Also, we move the requested memory buffer to the end of the allocated page(s), so when writing over the
  //   requested buffer size [...X][Y...] (e.g. write in Y), the OS will throw an Access Violation Exception.
  // - we guarantee the free page at the end by allocating it with this buffer and explicitly dcommiting it afterwards
  // - with this, the adress space is still registered but unusable and can not be reused again

  // - after some tests, we discovered, that pages are allocated in 64k steps (since the virtual adress granularity seems to be 64k, not 4k)
  // - but we get AV errors, if we try to access a 4k page which is not explicitly commited.
  // - so: we only need to allocate an overlapping additional 64k page, if we allocate (ASize mod 64k > 60k)

  // x86 - 32 bit problem
  // - due to the 64k page size, we are limited to (4G div 64k) = 64k single pages, which reduces the total amount of single GetMem calls massively.
  // - some of our modules do use more independent memory pieces, which are also very small.

  // Solution:
  // - Since pages are commited with a granularity of 4k, we can split the 64k in 16 seperate 4k chunks.
  // - if only a small amount of the 64k page is requested, we can use the remaining chunks for an additional variable
  // - the important thing here is, that we still need a 4k chunk in front and at the end to be decommited to throw access violations on buffer overflow
  //
  // Solution2:
  // - after analyzing the allocation sizes by calculating Log2( VarSize / dwPageSize ) and creating a statistic over it,
  //   we figured out that most allocations use only 1-2 pages [~60k 1 page allocations at "out of memory" time]
  // - with this informations, we can save massive space and avoid memory fragmentation if we allocate all data with 1 page size from a continuous block
  // - so we can allocate X pages with only 2X page consumptions and 2X/dwAllocSize TLB pages. We save around 7/8 of space compared to VirtualAlloc per 4k calls.

  Result := nil;

  LOverlapNeeded := (ASize mod (64*1024)) > (60*1024);
  //allocate buffer + additional page


  if LOverlapNeeded then // >60k
    Result := VirtualAlloc(nil, ASize+GSysInfo.dwPageSize, MEM_COMMIT or MEM_RESERVE, PAGE_READWRITE)
  else
  begin
  {$IFDEF USE_4K_RESERVATIONS}
    //for data <= dwPageSize (4k), we allocate data from a specialized array
    if (ASize <= GSysInfo.dwPageSize) then
      Result := _AllocFrom4kReserve(ASize);
  {$ENDIF}
    //if <4k allocation failed
    if not Assigned(Result) then
      Result := VirtualAlloc(nil, ASize, MEM_COMMIT or MEM_RESERVE, PAGE_READWRITE);
  end;

{$IFDEF CPUX64}
  Assert(NativeUInt(Result) < NativeUInt(GMaxPtr),'VirtualAlloc allocated data at > 1TB Adress. Not supported with pmVAMM due to internal optimizations');
{$ENDIF}
  if (Result = nil) then
  begin
	//we are out of memory - it is not even possible to call the default exception handling (it also needs memory)
	// - throw a non-continuable exception
    _AssertNotContinuable;
  end;

  //calculate necessary buffer offset to move the buffer to the end of the page
  LOffset := (GSysInfo.dwPageSize - (ASize mod GSysInfo.dwPageSize)) mod GSysInfo.dwPageSize;

  if LOverlapNeeded then
  begin
    //dcommit the page directly after our buffer
    LOverlapNeeded := VirtualFree(Pointer(NativeUInt(Result)+ASize+LOffset), GSysInfo.dwPageSize, MEM_DECOMMIT);

    //remember freed page for later debugging
    LIndex := NativeUInt((NativeUInt(Result)+ASize+LOffset) div GSysInfo.dwPageSize);
    SetPageMap(LIndex, Pointer(NativeUInt(Result)+ASize+LOffset), -2);
    //GPageMap[LIndex].Ptr := Pointer(NativeUInt(Result)+ASize+LOffset);
    //GPageMap[LIndex].Size := -2;
  end;

  //move requested buffer to the end of the page
  Result := Pointer(NativeUInt(Result)+LOffset);
  //ensure, the data can be (e.g. Quad DWord) aligned.
  // - sadly, this is necessary for a lot of 3d party libs, e.g. JCL and Interbase which assumes data to be aligned
  if (GAlignement > 1) then
    Result := Pointer(NativeUInt(Result) - NativeUInt(Result) mod GAlignement);

  //increment stats
  AtomicIncrement(GStats.AllocatedBytes, ASize);
  AtomicIncrement(GStats.AllocatedPages, Ceil(ASize / GSysInfo.dwPageSize));
  AtomicIncrement(GStats.Allocated64ks,  Ceil(ASize / GSysInfo.dwAllocationGranularity));
  if LOverlapNeeded then
    AtomicIncrement(GStats.Allocated64ks);

  LIndex := Math.EnsureRange(Ceil(Log2(Ceil(ASize / GSysInfo.dwPageSize))), 0, GMaxStatLogDepth);
  AtomicIncrement(GStats.AllocPage2nT[LIndex], 1);
  AtomicIncrement(GStats.AllocPage2nA[LIndex], 1);
  GStats.AllocPage2nM[LIndex] := Max(GStats.AllocPage2nA[LIndex], GStats.AllocPage2nM[LIndex]);

  //remember pointer and size
  LIndex := NativeUInt(NativeUInt(Result) div GSysInfo.dwPageSize);
  SetPageMap(LIndex, Result, ASize);

  FillChar(Result^, ASize, 0);
end;

function _FreeMem(APtr: Pointer): Integer;
var
  LIndex: NativeUInt;
  LStatsIndex: NativeUInt;
  LPageInfo: PPageInfo;
begin
  Result := 0;
  if (APtr = nil) then Exit;

  //increment stats
  AtomicIncrement(GStats.FreeMemCalls);
  LIndex := NativeUInt(NativeUInt(APtr) div GSysInfo.dwPageSize);


  LPageInfo := GetPageMap(LIndex);

  //check, if the page was allocated by us
  if (LPageInfo.Ptr = nil) then
  begin
    Result := GOldMM.FreeMem(APtr);
    Exit;
  end;
    
  if (LPageInfo.Size < 0) then
  begin
    _AssertNotContinuable;
    Assert(False, 'pmVAMM - FreeMem with already freed ptr called!');
  end;
  //remove mem

  {$IFDEF USE_4K_RESERVATIONS}
  if (LPageInfo.Size <= GSysInfo.dwPageSize) and _IsFrom4kReserve(APtr) then
    _FreeTo4kReserve(APtr)
  else
  {$ENDIF}
  //freemem returns 0 when success, VirtualFree returns True when success
  // - attention: we need to free the BASE POINTER, which might differ since we moved the memory block to the end of the page
  begin
    //APtr := Pointer(LIndex * GSysInfo.dwPageSize);
    Result := Integer(not(VirtualFree(APtr, 0, MEM_RELEASE)));
    if Result <> 0 then
    begin
      _AssertNotContinuable;
      Assert(Result=0, 'pmVAMM - VirtualFree Failed!!');
    end;
  end;

  //decrement stats by stored data
  AtomicDecrement(GStats.AllocatedBytes, LPageInfo.Size);
  AtomicDecrement(GStats.AllocatedPages, Ceil(LPageInfo.Size / GSysInfo.dwPageSize));

  //was a overlap necessary?
  if ((LPageInfo.Size mod (64*1024)) > (60*1024)) then
    AtomicDecrement(GStats.Allocated64ks, Ceil(LPageInfo.Size / GSysInfo.dwAllocationGranularity) + 1)
  else
    AtomicDecrement(GStats.Allocated64ks, Ceil(LPageInfo.Size / GSysInfo.dwAllocationGranularity));

  LStatsIndex := Math.EnsureRange(Ceil(Log2(Ceil(LPageInfo.Size / GSysInfo.dwPageSize))), 0, GMaxStatLogDepth);
  AtomicDecrement(GStats.AllocPage2nA[LStatsIndex], 1);

  //reset stored data
  //LPageInfo.Ptr  := nil; -- do not reset ptr to detect "double free" calls
  LPageInfo.Size := -1;
end;

function _ReallocMem(APtr: Pointer; ASize: NativeInt): Pointer;
var
  LIndex: NativeUInt;
  LPageInfo: PPageInfo;  
begin
  //since reallocation is not handled by the virtual memory system directly, we fake it by copying the memory into a new region
  //0. check, if a new allocation is necessary
  //1. get a new memory chunk
  //2. copy old data into new chunk (partly, if datasize has been reduced)

  //Hint: This is a very slow and lazy approach.

  AtomicIncrement(GStats.ReallocCalls);

  //no source pointer: just allocate data
  if (APtr = nil) then
  begin
    Result := _GetMem(ASize);
    Exit;
  end;

  //identify PageInfo Index
  LIndex := NativeUInt(NativeUInt(APtr) div GSysInfo.dwPageSize);

  LPageInfo := GetPageMap(LIndex);

  //same size, nothing to do
  if (LPageInfo.Size = ASize) then
  begin
    Result := APtr;
    Exit;
  end;
  //check, if the page was allocated by us
  if (LPageInfo.Ptr = nil) then
  begin
    //if not - redirect it to the old memory manager
    Result := GOldMM.ReallocMem(APtr, ASize);
    Exit;
  end;

  if (LPageInfo.Size < 0) then
  begin
    _AssertNotContinuable;
    Assert(False, 'pmVAMM - _ReallocMem with already freed ptr called!');
  end;

  //if we need to allcoate data
  if (ASize > 0) then
  begin
    Result := _GetMem(ASize);
    //clone memory
    Move(APtr^, Result^, Min(Integer(ASize), Integer(LPageInfo.Size)));
  end;
  //free old memory
  _FreeMem(APtr);
end;

// external functions
function GetMem(ASize: NativeInt): Pointer;
begin
  EnterCriticalSection(GLock);
  Result := _GetMem(ASize);
  LeaveCriticalSection(GLock);
end;

function FreeMem(APtr: Pointer): Integer;
begin
  EnterCriticalSection(GLock);
  Result := _FreeMem(APtr);
  LeaveCriticalSection(GLock);
end;

function ReallocMem(APtr: Pointer; ASize: NativeInt): Pointer;
begin
  EnterCriticalSection(GLock);
  Result := _ReallocMem(APtr, ASize);
  LeaveCriticalSection(GLock);
end;

function AllocMem(ASize: Cardinal): Pointer;
begin
  EnterCriticalSection(GLock);
  //virtual alloc ensures 0ed memory anyway
  Result := _GetMem(ASize);
  LeaveCriticalSection(GLock);
end;

function RegisterUnregisterExpectedMemoryLeak(P: Pointer): Boolean;
begin
  Result := False;
end;

procedure InitializeInternalStructure;
var
  LPages: NativeUInt;
  LSize:  NativeUInt;
  LIdx: Cardinal;
begin
  //ensure initialization happens only once
  if GInitialized then Exit;
  GInitialized := True;

  GetSystemInfo(GSysInfo);

  //allocation page granularity
  GAllocPG := GSysInfo.dwAllocationGranularity div GSysInfo.dwPageSize;

  //We need a store to remember the used size for allocation calls
  // - this is necessary to simulate realloc
  // - for this, we need to store max (MaxProcessMemoryAdress div PageGranularity) items
  LPages := NativeUInt(UInt64(GSysInfo.lpMaximumApplicationAddress) div GSysInfo.dwPageSize);
{$IFDEF CPUX64}
  LPages  := Min(LPages, GMaxMapArrSize div SizeOf(TPageInfo));
  GMaxPtr := Pointer(LPages * GSysInfo.dwPageSize);
{$ENDIF}

  //allocate enough space to store all necessary Page Informations
  GPageMap := VirtualAlloc(nil, LPages*SizeOf(TPageInfo), MEM_COMMIT or MEM_RESERVE, PAGE_READWRITE);
  GStats.AllocatedBytes := LPages*SizeOf(TPageInfo);
  GStats.AllocatedPages := Ceil(GStats.AllocatedBytes / GSysInfo.dwPageSize);
  GStats.GetMemCalls    := 0;
  GStats.FreeMemCalls   := 0;
  GStats.ReallocCalls   := 0;

  //prepare 4k area
  GStats.Free4KPages      := 0;
  GStats.Reserved4KPages  := 0;
  GStats.Allocated4KBytes := 0;

  {$IFDEF USE_4K_RESERVATIONS}
  //allocate double amount of expected 4k pages, since every second 4k page will stay decommited
  G4KPagesPtr := VirtualAlloc(nil, GReserved4kPages * GSysInfo.dwPageSize * 2, MEM_RESERVE, PAGE_READWRITE);
  Assert(G4KPagesPtr<>nil,'VAMM - not able to allocate enough memory for reserved 4k page block');

  G4KFreeFront := 0;
  for G4kFreeBack := 0 to GReserved4kPages-1 do
    G4KFreeArr[G4KFreeBack] := G4KFreeBack*2;
  //free pointer mod GReserved4kPages need to point to the first element, since that will be the first free element after the first allocation
  G4KFreeBack := GReserved4kPages;

  GStats.Free4KPages      := GReserved4kPages;
  GStats.Reserved4KPages  := GReserved4kPages;
  GStats.Allocated4KBytes := GReserved4kPages * GSysInfo.dwPageSize * 2;
  {$ENDIF}

  InitializeCriticalSection(GLock);
end;

const
  MemoryManager: TMemoryManagerEx = (
    GetMem: GetMem;
    FreeMem: FreeMem;
    ReallocMem: ReallocMem;
    AllocMem: AllocMem;
    RegisterExpectedMemoryLeak: RegisterUnregisterExpectedMemoryLeak;
    UnregisterExpectedMemoryLeak: RegisterUnregisterExpectedMemoryLeak
  );

initialization
{$IFDEF useVAMM}
  InitializeInternalStructure();
  GetMemoryManager(GOldMM);
  SetMemoryManager(MemoryManager);
{$ENDIF}
Finalization

end.
