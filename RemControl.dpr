program RemControl;

uses
  Forms,
  MainRemote in 'MainRemote.pas' {FMainRemote},
  RemoteControl in 'RemoteControl.pas' {FRemoteControl};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFMainRemote, FMainRemote);
  Application.CreateForm(TFRemoteControl, FRemoteControl);
  Application.Run;
end.
