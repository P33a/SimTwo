program SimTwo;

uses
  FastMM4,
  SyncObjs,
  Forms,
  Viewer in 'Viewer.pas' {FViewer},
  Params in 'Params.pas' {FParams},
  Editor in 'Editor.pas' {FEditor},
  FastChart in 'FastChart.pas' {FFastChart},
  RemoteControl in 'RemoteControl.pas' {FRemoteControl},
  Remote in 'Remote.pas',
  ODERobots in 'ODERobots.pas',
  ODERobotsPublished in 'ODERobotsPublished.pas',
  uPSI_ODERobotsPublished in 'uPSI_ODERobotsPublished.pas',
  ProjConfig in 'ProjConfig.pas',
  Utils in 'Utils.pas',
  WayPointsEdit in 'WayPointsEdit.pas' {FWayPointsEdit},
  VerInfo in 'VerInfo.pas',
  AStar in 'AStar.pas',
  PathFinder in 'PathFinder.pas',
  SceneEdit in 'SceneEdit.pas' {FSceneEdit},
  Sheets in 'Sheets.pas' {FSheets},
  ChooseScene in 'ChooseScene.pas' {FChooseScene},
  dynmatrix in 'dynmatrix.pas',
  odeimport in 'ODEImport.pas',
  SimpleParser in 'SimpleParser.pas',
  uPSI_PathFinder in 'uPSI_PathFinder.pas',
  uPSI_dynmatrix in 'uPSI_dynmatrix.pas',
  cameras in 'cameras.pas' {FCameras};

{$R *.res}

begin
  RegisterExpectedMemoryLeak(TCriticalSection, 1);

  Application.Initialize;
  Application.CreateForm(TFViewer, FViewer);
  Application.CreateForm(TFParams, FParams);
  Application.CreateForm(TFEditor, FEditor);
  Application.CreateForm(TFChart, FChart);
  Application.CreateForm(TFRemoteControl, FRemoteControl);
  Application.CreateForm(TFWayPointsEdit, FWayPointsEdit);
  Application.CreateForm(TFSceneEdit, FSceneEdit);
  Application.CreateForm(TFSheets, FSheets);
  Application.CreateForm(TFChooseScene, FChooseScene);
  Application.CreateForm(TFCameras, FCameras);
  Application.Run;
end.
