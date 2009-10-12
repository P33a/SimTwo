unit SceneEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, SynEditHighlighter, SynHighlighterXML, SynEdit, Menus,
  ExtCtrls, StdCtrls, ShellAPI, SynEditTypes, SynEditMiscClasses, SynEditSearch,
  rxPlacemnt, ProjConfig, StrUtils;

type
  TFSceneEdit = class(TForm)
    StatusBar: TStatusBar;
    LBErrors: TListBox;
    Splitter: TSplitter;
    MainMenu: TMainMenu;
    MenuFile: TMenuItem;
    MenuNew: TMenuItem;
    MenuOpen: TMenuItem;
    N2: TMenuItem;
    MenuSave: TMenuItem;
    MenuSaveAs: TMenuItem;
    N3: TMenuItem;
    MenuPrintSource: TMenuItem;
    N1: TMenuItem;
    MenuExit: TMenuItem;
    MenuEdit: TMenuItem;
    MenuUndo: TMenuItem;
    MenuRedo: TMenuItem;
    N4: TMenuItem;
    MenuFind: TMenuItem;
    MenuReplace: TMenuItem;
    MenuScene: TMenuItem;
    MenuReBuild: TMenuItem;
    MenuWindow: TMenuItem;
    MenuSceneXML: TMenuItem;
    SynXMLSyn: TSynXMLSyn;
    PageControlXML: TPageControl;
    TabScene: TTabSheet;
    SynEditXML: TSynEdit;
    ReplaceDialog: TReplaceDialog;
    FindDialog: TFindDialog;
    SynEditSearch: TSynEditSearch;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    FormStorage: TFormStorage;
    MenuChange: TMenuItem;
    MenuNewScene: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure MenuReBuildClick(Sender: TObject);
    procedure SynEditXMLStatusChange(Sender: TObject;
      Changes: TSynStatusChanges);
    procedure PageControlXMLChange(Sender: TObject);
    procedure MenuUndoClick(Sender: TObject);
    procedure MenuRedoClick(Sender: TObject);
    procedure MenuFindClick(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    procedure MenuReplaceClick(Sender: TObject);
    procedure ReplaceDialogAction(Sender: TObject);
    procedure MenuSaveClick(Sender: TObject);
    procedure MenuSaveAsClick(Sender: TObject);
    procedure MenuNewClick(Sender: TObject);
    procedure MenuOpenClick(Sender: TObject);
    procedure MenuExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure LBErrorsDblClick(Sender: TObject);
    procedure MenuChangeClick(Sender: TObject);
    procedure MenuNewSceneClick(Sender: TObject);
  private
    function GetSynEdit: TSynEdit;
    function ActSynEdit: TSynEdit;
    procedure FindReplaceDialog(TmpSynEdit: TSynEdit; Dialog: TFindDialog);
    procedure CreateXMLTabEdit(SL: TStringList; i: integer);
    function CheckModified: boolean;
    { Private declarations }
  public
    { Public declarations }
    MustReSpawn: boolean;
    ReSpawnPars: string;

    procedure ReSpawn;
  end;

var
  FSceneEdit: TFSceneEdit;

implementation

uses Viewer, Editor, ChooseScene;

{$R *.dfm}

procedure TFSceneEdit.CreateXMLTabEdit(SL: TStringList; i: integer);
var NewTabSheet: TTabSheet;
    NewSynEdit: TSynEdit;
begin
    NewTabSheet := TTabSheet.Create(self);
    NewTabSheet.Name := 'TS' + inttostr(i);
    NewTabSheet.PageControl := PageControlXML;
    NewTabSheet.Caption := SL[i];
    NewSynEdit := TSynEdit.Create(self);
    NewSynEdit.Name := 'SE' + inttostr(i);
    SL.Objects[i] := NewSynEdit;
    NewSynEdit.Parent := NewTabSheet;
    NewSynEdit.Align := alClient;
    NewSynEdit.Highlighter := SynXMLSyn;
    NewSynEdit.OnStatusChange := SynEditXMLStatusChange;
    NewSynEdit.Lines.LoadFromFile(SL[i]);
end;


procedure TFSceneEdit.FormShow(Sender: TObject);
var i: integer;
//    NewTabSheet: TTabSheet;
//    NewSynEdit: TSynEdit;
   TmpSynEdit: TSynEdit;
//   coord: TBufferCoord;
begin
  TabScene.TabVisible := false;
  SynEditXML.Lines.AddStrings(WorldODE.XMLFiles);
  for i := 0 to WorldODE.XMLFiles.Count -1 do begin
    if WorldODE.XMLFiles.IndexOf(WorldODE.XMLFiles[i]) = i then begin
      if WorldODE.XMLFiles.Objects[i] = nil then  // only if they were not already created (ex: a cycle show/close/show again)
        CreateXMLTabEdit(WorldODE.XMLFiles, i);
    end;
  end;
  FormStorage.RestoreFormPlacement;

  TmpSynEdit := GetSynEdit();
  if TmpSynEdit = nil then exit;
  TmpSynEdit.CaretY := FormStorage.ReadInteger('CursorLine',TmpSynEdit.CaretY);
  TmpSynEdit.CaretX := FormStorage.ReadInteger('CursorCol',TmpSynEdit.CaretX);
  TmpSynEdit.UpdateCaret;
  //coord.Line := 100;
  //SynEditXML.SetCaretAndSelection(coord, coord, coord);
  TmpSynEdit.SetFocus;
end;

procedure TFSceneEdit.MenuReBuildClick(Sender: TObject);
var i: integer;
    TmpSynEdit: TSynEdit;
begin
  for i := 0 to WorldODE.XMLFiles.Count -1 do begin
    if not (WorldODE.XMLFiles.Objects[i] is TSynEdit) then continue;
    TmpSynEdit := TSynEdit(WorldODE.XMLFiles.Objects[i]);
    if TmpSynEdit.Modified then begin
      TmpSynEdit.Lines.SaveToFile(WorldODE.XMLFiles[i]);
      TmpSynEdit.Modified := false;
    end;
  end;
  FEditor.MenuSaveClick(Sender);

  MustReSpawn := true;
  FViewer.Close;
end;

function TFSceneEdit.GetSynEdit: TSynEdit;
var i: integer;
begin
  result := nil; 
  i := PageControlXML.TabIndex;
  if i<0 then exit;
  if (WorldODE.XMLFiles.Objects[i] is TSynEdit) then begin
    result := TSynEdit(WorldODE.XMLFiles.Objects[i]);
  end;
end;

// Same as GetSynEdit but returns a dummy SynEdit instead of nil
function TFSceneEdit.ActSynEdit: TSynEdit;
var i: integer;
begin
  i := PageControlXML.TabIndex;
  if (WorldODE.XMLFiles.Objects[i] is TSynEdit) then begin
    result := TSynEdit(WorldODE.XMLFiles.Objects[i]);
  end else result := SynEditXML;
end;


procedure TFSceneEdit.SynEditXMLStatusChange(Sender: TObject; Changes: TSynStatusChanges);
var TmpSynEdit: TSynEdit;
begin
  TmpSynEdit := GetSynEdit();
  if TmpSynEdit = nil then exit;

  if (scCaretX in Changes) or (scCaretY in Changes) or (scAll in Changes) then
    StatusBar.Panels[0].Text := format('%6d: %3d',[TmpSynEdit.CaretY, TmpSynEdit.CaretX]);

  if (scInsertMode in Changes) or (scAll in Changes)then begin
    if TmpSynEdit.InsertMode then begin
      StatusBar.Panels[2].Text := 'Insert';
    end else begin
      StatusBar.Panels[2].Text := 'Overwrite';
    end;
  end;

  if (scModified in Changes) or (scAll in Changes) then begin
    if TmpSynEdit.Modified then begin
      StatusBar.Panels[1].Text := 'Modified';
    end else begin
      StatusBar.Panels[1].Text := '';
    end;
  end;
  StatusBar.Invalidate;
  TmpSynEdit.Invalidate;

end;

procedure TFSceneEdit.PageControlXMLChange(Sender: TObject);
var TmpSynEdit: TSynEdit;
begin
  TmpSynEdit := GetSynEdit();
  if TmpSynEdit = nil then exit;

  TmpSynEdit.OnStatusChange(self, [scAll]);
end;

procedure TFSceneEdit.MenuUndoClick(Sender: TObject);
begin
  ActSynEdit.Undo;
end;

procedure TFSceneEdit.MenuRedoClick(Sender: TObject);
begin
  ActSynEdit.Redo;
end;

procedure TFSceneEdit.MenuFindClick(Sender: TObject);
begin
  FindDialog.Execute;
end;

procedure TFSceneEdit.FindDialogFind(Sender: TObject);
var TmpSynEdit: TSynEdit;
begin
  TmpSynEdit := GetSynEdit();
  if TmpSynEdit = nil then exit;

  FindReplaceDialog(TmpSynEdit, FindDialog);
end;

procedure TFSceneEdit.MenuReplaceClick(Sender: TObject);
begin
  ReplaceDialog.Execute;
end;

//  TFindOption = (frDown, frFindNext, frHideMatchCase, frHideWholeWord,
//    frHideUpDown, frMatchCase, frDisableMatchCase, frDisableUpDown,
//    frDisableWholeWord, frReplace, frReplaceAll, frWholeWord, frShowHelp);

//  TSynSearchOption = (ssoMatchCase, ssoWholeWord, ssoBackwards,
//    ssoEntireScope, ssoSelectedOnly, ssoReplace, ssoReplaceAll, ssoPrompt);

procedure TFSceneEdit.FindReplaceDialog(TmpSynEdit: TSynEdit; Dialog: TFindDialog);
var SynSearchOptions: TSynSearchOptions;
    replace_txt: string;
begin
  TmpSynEdit.SearchEngine := SynEditSearch;
  SynSearchOptions := [];
  if not (frDown in Dialog.Options) then
    SynSearchOptions := SynSearchOptions + [ssoBackwards];

  if (frReplace in Dialog.Options) then
    SynSearchOptions := SynSearchOptions + [ssoReplace];
  if (frReplaceAll in Dialog.Options) then
    SynSearchOptions := SynSearchOptions + [ssoReplaceAll];

  if (frWholeWord in Dialog.Options) then
    SynSearchOptions := SynSearchOptions + [ssoWholeWord];
  if (frMatchCase in Dialog.Options) then
    SynSearchOptions := SynSearchOptions + [ssoMatchCase];

  if Dialog is TReplaceDialog then begin
    replace_txt := TReplaceDialog(Dialog).ReplaceText;
    // Just to make the next Replace hit the selected word by find
    if [ssoReplace, ssoReplaceAll] * SynSearchOptions <> [] then
      SynSearchOptions := SynSearchOptions + [ssoSelectedOnly];
  end else begin
    replace_txt := '';
  end;
  TmpSynEdit.SearchReplace(Dialog.FindText, replace_txt, SynSearchOptions)
end;


procedure TFSceneEdit.ReplaceDialogAction(Sender: TObject);
var TmpSynEdit: TSynEdit;
begin
  TmpSynEdit := GetSynEdit();
  if TmpSynEdit = nil then exit;

  FindReplaceDialog(TmpSynEdit, ReplaceDialog);
end;

procedure TFSceneEdit.MenuSaveClick(Sender: TObject);
var i: integer;
    TmpSynEdit: TSynEdit;
begin
  i := PageControlXML.TabIndex;
  if (WorldODE.XMLFiles.Objects[i] is TSynEdit) then begin
    TmpSynEdit := TSynEdit(WorldODE.XMLFiles.Objects[i]);
  end else exit;

  TmpSynEdit.Lines.SaveToFile(WorldODE.XMLFiles[i]);
  TmpSynEdit.Modified := false;
end;

procedure TFSceneEdit.MenuSaveAsClick(Sender: TObject);
var i: integer;
    TmpSynEdit: TSynEdit;
begin
  i := PageControlXML.TabIndex;
  if (WorldODE.XMLFiles.Objects[i] is TSynEdit) then begin
    TmpSynEdit := TSynEdit(WorldODE.XMLFiles.Objects[i]);
  end else exit;

  if SaveDialog.initialDir ='' then SaveDialog.initialDir := GetCurrentDir;
  SaveDialog.FileName := WorldODE.XMLFiles[i];
  if not SaveDialog.Execute then exit;

  WorldODE.XMLFiles[i] := ExtractFileName(SaveDialog.FileName);
  (TmpSynEdit.Parent as TTabSheet).Caption := WorldODE.XMLFiles[i];
  TmpSynEdit.Lines.SaveToFile(WorldODE.XMLFiles[i]);
  TmpSynEdit.Modified := false;
end;


procedure TFSceneEdit.MenuNewClick(Sender: TObject);
var i: integer;
    sname: string;
    SL: TStringlist;
begin
  //i := PageControlXML.TabIndex;
  i := WorldODE.XMLFiles.Count;
  sname := 'untitled.xml';
  while true do begin
    if not InputQuery('Select file Name', 'New File to be created:', sname) then exit;
    if fileexists(sname) then begin
      Showmessage('File: '+ sname + ' already exists');
      continue;
    end;
    SL := TStringlist.Create;
    try
      SL.Add('<?xml version="1.0" ?>');
      SL.SaveToFile(sname);
    finally
      SL.Free;
    end;
    break;
  end;
  WorldODE.XMLFiles.Insert(i, sname);
  CreateXMLTabEdit(WorldODE.XMLFiles, i);
end;

procedure TFSceneEdit.MenuOpenClick(Sender: TObject);
var i: integer;
    sname: string;
begin
  if OpenDialog.initialDir ='' then OpenDialog.initialDir := GetCurrentDir;
  if not OpenDialog.Execute then exit;

  sname := ExtractFileName(OpenDialog.FileName);
  if not fileexists(sName) then exit; // TODO: queixar ao utilizador

  i := WorldODE.XMLFiles.Count;
  WorldODE.XMLFiles.Insert(i, sname);
  CreateXMLTabEdit(WorldODE.XMLFiles, i);

end;


procedure TFSceneEdit.MenuExitClick(Sender: TObject);
begin
  FViewer.close;
end;

procedure TFSceneEdit.FormCreate(Sender: TObject);
begin
  FormStorage.IniFileName := GetIniFineName;
  MustReSpawn := false;
  ReSpawnPars := '';
end;

procedure TFSceneEdit.FormClose(Sender: TObject; var Action: TCloseAction);
var TmpSynEdit: TSynEdit;
begin
  TmpSynEdit := GetSynEdit();
  if TmpSynEdit = nil then exit;
  FormStorage.WriteInteger('CursorLine',TmpSynEdit.CaretY);
  FormStorage.WriteInteger('CursorCol',TmpSynEdit.CaretX);
  //while PageControlXML.PageCount > 0 do begin
  //  PageControlXML.Pages[0].Free;
  //end;
end;

procedure TFSceneEdit.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if CheckModified then begin
    if MessageDlg('Some XML Files were changed.'+crlf+
                  'Exit anyway?',
                  mtConfirmation , [mbOk,mbCancel], 0)
       = mrCancel then CanClose:=false;
  end;
end;


function TFSceneEdit.CheckModified: boolean;
var i: integer;
    TmpSynEdit: TSynEdit;
    AnyModified: boolean;
begin
  AnyModified := false;
  for i := 0 to WorldODE.XMLFiles.Count -1 do begin
    if (WorldODE.XMLFiles.Objects[i] is TSynEdit) then begin
      TmpSynEdit := TSynEdit(WorldODE.XMLFiles.Objects[i]);
      AnyModified := AnyModified or TmpSynEdit.Modified;
    end;
  end;
  result := AnyModified;
end;

function GetLineInErrorString(const txt : string): integer;
var p1, p2: integer;
    s: string;
begin
  result := -1;

  p1:= pos('(',txt);
  p2:= pos(')',txt);
  if (p1 > 0) and (p2 > 0) then begin
    s := copy(txt, p1+1, p2-(p1+1));
    result := strToIntdef(s,-1);
  end;
end;


function GetTokenInErrorString(const txt : string): string;
var p1, p2: integer;
begin
  result := '';

  p1:= pos('"', txt);
  p2:= posex('"', txt, p1+1);
  if (p1 > 0) and (p2 > 0) then begin
    result := copy(txt, p1+1, p2-(p1+1));
  end;
end;

procedure TFSceneEdit.LBErrorsDblClick(Sender: TObject);
var i, col, idx: integer;
    TmpSynEdit: TSynEdit;
    ErrLineNumber: integer;
    tok, lin: string;
    BufferCoord: TBufferCoord;
begin
  TmpSynEdit := GetSynEdit();
  if TmpSynEdit = nil then exit;

  i:= LBErrors.ItemIndex;
  if i<0 then exit;

  ErrLineNumber := GetLineInErrorString(LBErrors.Items[i]);
  tok := GetTokenInErrorString(LBErrors.Items[i]);

  if (ErrLineNumber <> -1) then begin
    TmpSynEdit.caretY := ErrLineNumber;

    lin :=  TmpSynEdit.LineText;
    col := pos(tok, lin);
    if col <> 0 then
      TmpSynEdit.caretX := col;

  end else begin
    idx := pos(tok, TmpSynEdit.Text);
    BufferCoord := TmpSynEdit.CharIndexToRowCol(idx);
    TmpSynEdit.SetCaretAndSelection(BufferCoord, BufferCoord, BufferCoord);
  end;

  TmpSynEdit.UpdateCaret;
  TmpSynEdit.setfocus;
end;

procedure TFSceneEdit.ReSpawn;
var s: string;
begin
  if not MustReSpawn then exit;

  if ReSpawnPars = '' then begin
    if ParamCount > 0 then
      s:=ansiquotedstr(ParamStr(1),'"')
    else
      s:='';
    //ShellExecute(Handle, 'open', pchar(Application.ExeName), pchar(AnsiquotedStr(AnsiDequotedStr(s,''''),'''')) , pchar(ExtractFilePath(Application.ExeName)),  SW_SHOWNORMAL);
    ShellExecute(Handle, 'open', pchar(Application.ExeName), pchar(s) , pchar(ExtractFilePath(Application.ExeName)),  SW_SHOWNORMAL);
  end else begin
    ShellExecute(Handle, 'open', pchar(Application.ExeName), pchar(ReSpawnPars), pchar(ExtractFilePath(Application.ExeName)),  SW_SHOWNORMAL);
  end;
end;

procedure TFSceneEdit.MenuChangeClick(Sender: TObject);
begin
  FChooseScene.showmodal;

  if FChooseScene.ModalResult = mrCancel then exit;
  if FChooseScene.SelectedDir = '' then exit;

  ReSpawnPars := ansiquotedstr(FChooseScene.SelectedDir,'"');
  MustReSpawn := true;
  FViewer.Close;
end;

procedure TFSceneEdit.MenuNewSceneClick(Sender: TObject);
var s, od: string;
    i: integer;
    fo: _SHFILEOPSTRUCT;
begin
  i := 1;
  s := 'Project' + inttostr(i);
  while true do begin
    if not (DirectoryExists('..\' + s) or FileExists('..\' + s)) then break;
    inc(i);
    s := 'Project' + inttostr(i);
  end;

  inputQuery('Choose new project name', '', s);

  if DirectoryExists('..\' + s) or FileExists('..\' + s) then begin
    showMessage('Project: ' + s  + ' already exists!');
    exit;
  end;

  CreateDir('..\' + s);

  od := GetCurrentDir;

  fo.Wnd := handle;
  fo.wFunc := FO_COPY;
  fo.pFrom := pchar(od + '\..\base\*.*');
  fo.pTo := pchar(od + '\..\' + s);
  fo.fFlags := FOF_NOCONFIRMATION or FOF_SILENT;
  fo.hNameMappings := nil;
  fo.lpszProgressTitle := nil;
  SHFileOperation(fo);

  ReSpawnPars := ansiquotedstr(s,'"');
  MustReSpawn := true;
  FViewer.Close;
end;

end.
