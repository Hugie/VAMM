program DemoProject;

{$DEFINE useVAMM}

uses
  VirtualAllocMM,
  Vcl.Forms,
  Demo in 'Demo.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
