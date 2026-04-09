program DemoProject;

{$DEFINE useVAMM}

uses
  VirtualAllocMM,
  Forms,
  Demo in 'Demo.pas' {Form1},
  VirtualAllocMM.UsageTracker in '..\VirtualAllocMM.UsageTracker.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
