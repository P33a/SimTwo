unit ChooseScene;

{$MODE Delphi}

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, IniPropStorage;

type

  { TFChooseScene }

  TFChooseScene = class(TForm)
    IniPropStorage: TIniPropStorage;
    LBScenes: TListBox;
    ImageSnapshot: TImage;
    BOK: TButton;
    BCancel: TButton;
    ImageEmpty: TImage;
    procedure FormShow(Sender: TObject);
    procedure BOKClick(Sender: TObject);
    procedure LBScenesDblClick(Sender: TObject);
    procedure LBScenesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    SelectedDir: string;
  end;

var
  FChooseScene: TFChooseScene;

implementation

uses ProjConfig;

{$R *.lfm}

procedure TFChooseScene.FormShow(Sender: TObject);
var i: integer;
    sr: TSearchRec;
    FileAttrs: Integer;
begin
  LBScenes.Clear;

  FileAttrs := faDirectory;
  if FindFirst('..\*', FileAttrs, sr) = 0 then begin
    repeat
      if (sr.Attr and FileAttrs) = FileAttrs then begin   // Only directories
        if pos('.',sr.Name) = 1 then continue; // no dot files
        if sr.Name = 'base' then continue; // "base" is a reserved name
        if not fileexists('..\' + sr.Name + '\scene.xml') then continue; // must have a scene.xml file
        LBScenes.Items.Add(sr.Name);
      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;

  for i := 0 to LBScenes.Count - 1 do begin
    if ExpandFileName('..\' + LBScenes.Items[i]) = GetCurrentDir then begin
      LBScenes.ItemIndex := i;
      LBScenesClick(sender);
      break;
    end;
  end;

  //LBScenes.Items.Add(GetCurrentDir);
  //LBScenes.Items.Add(ExpandFileName('.'));

end;

procedure TFChooseScene.BOKClick(Sender: TObject);
var i: integer;
begin
  i := LBScenes.ItemIndex;
  if i >= 0 then begin
    SelectedDir := LBScenes.Items[i];
  end else begin
    SelectedDir := GetCurrentDir;
  end;
  IniPropStorage.Save;
end;

procedure TFChooseScene.LBScenesDblClick(Sender: TObject);
begin
  BOK.Click;
end;

procedure TFChooseScene.LBScenesClick(Sender: TObject);
var s: string;
begin
  if LBScenes.ItemIndex < 0 then exit;
  s := '..\' + LBScenes.Items[LBScenes.ItemIndex] + '\Snapshot.jpg';
  if fileexists(s) then begin
    ImageEmpty.Visible := false;
    ImageSnapshot.Picture.LoadFromFile(s);
    ImageSnapshot.Visible := true;
  end else begin
    ImageSnapshot.Visible := false;
    ImageEmpty.Visible := true;
  end;
end;

procedure TFChooseScene.FormCreate(Sender: TObject);
begin
  IniPropStorage.IniFileName := GetIniFineName;
end;

end.
