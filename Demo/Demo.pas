unit Demo;

interface

uses
  VirtualAllocMM,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  System.Generics.Collections;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    FList: TList<Pointer>;
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

uses
  VirtualAllocMM.UsageTracker;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FList := TList<Pointer>.Create;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowVAMMUsageTracker();
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FList.Add( GetMemory( Random(1023) + 1 ));
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  FList.Add( GetMemory( (1+Random(1023)) * 1024 ));
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  LBPtr: PByte;
  LByte: Byte;
  LOffset: Integer;
begin
  LBPtr := GetMemory(16);

  for LOffset := 0 to 4096 do
    LByte := LBPtr[LOffset];
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  LBPtr: PByte;
  LByte: Byte;
  LOffset: Integer;
begin
  LBPtr := GetMemory(16);

  for LOffset := 0 to 4096 do
    LByte := LBPtr[0-LOffset];
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  if FList.Count <= 0 then
    Exit;

  FreeMemory(FList.Last);
  FList.Delete(FList.Count-1);
end;

end.
