unit VirtualAllocMM.UsageTracker;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TVAMMUsageTracker = class(TForm)
    mVAMMStats: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    btnUpdate: TButton;
    cbAutoUpdate: TCheckBox;
    timAutoUpdater: TTimer;
    procedure timAutoUpdaterTimer(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure cbAutoUpdateClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    procedure UpdateVAMMStats();
  end;


function ShowVAMMUsageTracker: TVAMMUsageTracker;
function CreateVAMMUsageTracker: TVAMMUsageTracker;

implementation

uses
  VirtualAllocMM,
  Math;

{$R *.dfm}

var
  GSysInfo: TSystemInfo;


function ShowVAMMUsageTracker: TVAMMUsageTracker;
begin
  Application.CreateForm(TVAMMUsageTracker, Result);
  if Assigned(Result) then
  begin
    Result.UpdateVAMMStats;
    Result.Show;
  end;
end;

function CreateVAMMUsageTracker: TVAMMUsageTracker;
begin
  Application.CreateForm(TVAMMUsageTracker, Result);
  if Assigned(Result) then
  begin
    Result.BorderIcons := [];
    Result.BorderStyle := bsNone;
    Result.UpdateVAMMStats;
  end;
end;


procedure TVAMMUsageTracker.btnUpdateClick(Sender: TObject);
begin
  UpdateVAMMStats();
end;

procedure TVAMMUsageTracker.cbAutoUpdateClick(Sender: TObject);
begin
  timAutoUpdater.Enabled := cbAutoUpdate.Checked;
end;

procedure TVAMMUsageTracker.timAutoUpdaterTimer(Sender: TObject);
begin
  UpdateVAMMStats();
end;

function IntToByteSizeStr(const AByteCount: UInt64): string;
const
  SizeUnits: array[0..5] of string = ('B', 'kB', 'MB', 'GB', 'TB', 'PB');
var
  Size: Double;
  UnitIndex: Integer;
begin
  Size := AByteCount;
  UnitIndex := 0;

  // Teile die Größe, bis sie kleiner als 1024 ist, und erhöhe den Einheit-Index
  while (Size >= 1024) and (UnitIndex < High(SizeUnits)) do
  begin
    Size := Size / 1024;
    Inc(UnitIndex);
  end;

  // Ausgabe formatieren mit maximal zwei Dezimalstellen
  Result := FormatFloat('0.##', Size) + ' ' + SizeUnits[UnitIndex];
end;

procedure TVAMMUsageTracker.UpdateVAMMStats;
var
  LStats: TVAMMStats;
  LLogDepth: Integer;
  LMaxPages: UInt64;
begin
  try
    LStats := GetVAMMStats();

    LMaxPages := ((NativeUInt(GSysInfo.lpMaximumApplicationAddress) - NativeUInt(GSysInfo.lpMinimumApplicationAddress)) div GSysInfo.dwAllocationGranularity) + 1;

    mVAMMStats.Lines.BeginUpdate();
    mVAMMStats.Lines.Clear();
    mVAMMStats.Lines.Add(' --== Virtual Alloc Memory Manager Live Informations ==--');
    mVAMMStats.Lines.Add(' ');
    mVAMMStats.Lines.Add(' Page Size                  : ' + IntToByteSizeStr( GSysInfo.dwPageSize ));
    mVAMMStats.Lines.Add(' Alloc. Granularity (Block) : ' + IntToByteSizeStr( GSysInfo.dwAllocationGranularity ));
    mVAMMStats.Lines.Add(' VAMM Byte Alignement       : ' + IntToByteSizeStr( GVAMMDefaultByteAlignement ));
    mVAMMStats.Lines.Add(' ');
    mVAMMStats.Lines.Add(' GetMem  Calls: ' + IntToStr( LStats.GetMemCalls ));
    mVAMMStats.Lines.Add(' FreeMem Calls: ' + IntToStr( LStats.FreeMemCalls ));
    mVAMMStats.Lines.Add(' Realloc Calls: ' + IntToStr( LStats.ReallocCalls ));
    mVAMMStats.Lines.Add(' ');
    mVAMMStats.Lines.Add(' Allocated Bytes : ' + IntToByteSizeStr( LStats.AllocatedBytes ) + ' (' + IntToStr( LStats.AllocatedBytes ) + ')' );
    mVAMMStats.Lines.Add(' Allocated Pages : ' + IntToStr(LStats.AllocatedPages));
    mVAMMStats.Lines.Add(' Allocated Blocks: ' + IntToStr(LStats.Allocated64ks) + ' of: ' + IntToStr( LMaxPages )
                      + ' (' + IntToStr(Round( 100 * (LStats.Allocated64ks/LMaxPages))) + ' %)');

    if (LStats.Reserved4KPages > 0) then
    begin
      mVAMMStats.Lines.Add(' ');
      mVAMMStats.Lines.Add('   -- 4k Allocation Optimization: enabled --');
      mVAMMStats.Lines.Add(' Reserved  4K Pages: ' + IntToStr( LStats.Reserved4KPages ));
      mVAMMStats.Lines.Add(' Allocated 4K Bytes: ' + IntToByteSizeStr( LStats.Allocated4KBytes ) + ' (' + IntToStr( LStats.Allocated4KBytes ) + ')');
      mVAMMStats.Lines.Add(' Free 4K Pages     : ' + IntToStr( LStats.Free4KPages ) + ' (' + IntToStr(Round( 100 * (LStats.Free4KPages/LStats.Reserved4KPages))) +' %)');
    end;

    mVAMMStats.Lines.Add(' ');
    mVAMMStats.Lines.Add('   -- Allocation Page Size Overview --');
    mVAMMStats.Lines.Add(' ');
    mVAMMStats.Lines.Add(' [Pagesize]:  Active  - Abs.Max  - Total');

    for LLogDepth := 0 to GMaxStatLogDepth do
    begin
      mVAMMStats.Lines.Add( Format(' [%8d]: %8d - %8d - %8d',[
        Round(Power(2,LLogDepth)),
        LStats.AllocPage2nA[LLogDepth],
        LStats.AllocPage2nM[LLogDepth],
        LStats.AllocPage2nT[LLogDepth]]));
    end;

  finally
    mVAMMStats.Lines.EndUpdate();
  end;
end;

Initialization
  GetSystemInfo(GSysInfo);
Finalization
end.
