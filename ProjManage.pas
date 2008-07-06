unit ProjManage;

interface

uses Forms, Classes, SysUtils, IniFiles, Projconfig;

type
  TProjectConfig = record
    FileName : string;
    Modified: boolean; // Always compile
    //ChangedSinceCompile: boolean; // Always compile !
    Author: string;
    Comments: string;
  end;


var Project: TProjectConfig;

procedure ProjectNew;
function ProjectOpen(aFileName: string):boolean;
function ProjectSave(aFileName: string): boolean;

procedure SaveStringsToMemIni( MemIni: TMemIniFile; section, Ident: string; SL: TStrings);
procedure LoadStringsFromMemIni( MemIni: TMemIniFile; section, Ident: string; SL: TStrings);
procedure SaveFormGeometryToMemIni( MemIni: TMemIniFile; const aForm: TForm);
procedure LoadFormGeometryFromMemIni( MemIni: TMemIniFile; aForm: TForm);

implementation


uses Viewer, Editor, Params;

function ProjectSave(aFileName: string):boolean;
var ProjMemIni : TMemIniFile;
begin
  result:=false;
  ProjMemIni:= TMemIniFile.Create(afileName);
  try
  try
    //FViewer.FormSave(ProjMemIni);
    FEditor.FormSave(ProjMemIni);
    //FIOLEDs.FormSave(ProjMemIni);

    ProjMemIni.UpdateFile;

    with Project do begin
      FileName:=aFileName;
      Modified:=false;
    end;

    FEditor.SynEditST.Modified:=false;
    FEditor.UpdateStatusLine;
    FEditor.Caption := FormEditorCaption + ExtractFileName(Project.FileName);
    result:=true;

  except

  end;
  finally
    ProjMemIni.Free;
  end;
end;



procedure QuoteSTTextInIniFile(aFileName: string);
var
  lines : TStringList;
  CurLine,StartLine,separator, NumLines : integer;

begin
  lines:=TStringList.Create;
  try
    lines.LoadFromFile(aFileName);
    StartLine:=0;

    while (lines.Strings[StartLine]<>'[Main\STText]') and (StartLine<lines.Count) do
      Inc(StartLine);

    if (StartLine=lines.Count) then begin  // not found, strange ! Empty File ?
      exit;
    end;

    Inc(StartLine);
    if (Copy(lines.Strings[StartLine],1,6)<>'count=') then begin  // not found, strange !
      exit;
    end;

    NumLines:=StrToIntDef(copy(lines.Strings[StartLine],7,length(lines.Strings[StartLine])),0);

    CurLine:=StartLine+1;

    repeat
      separator:=pos('=',lines.Strings[curline]);
      if copy(lines.Strings[curline],separator+1,1)='"' then exit; // already quoted file...
      lines.Strings[CurLine]:=copy(lines.Strings[curline],1,separator)+'"'+copy(lines.Strings[curline],separator+1,9999)+'"';
      inc(CurLine);
    until CurLine=StartLine+NumLines+1;

    RenameFile(aFileName,aFileName+'.bkp');
    lines.SaveToFile(aFileName);
  finally
    lines.Free;
  end;
end;



function ProjectOpen(aFileName: string):boolean;
var ProjMemIni : TMemIniFile;
begin
  result:=false;
  if not fileExists(afileName) then exit;

  QuoteSTTextInIniFile(aFileName);

  ProjMemIni:= TMemIniFile.Create(afileName);
  try
  try
    //FViewer.FormLoad(ProjMemIni);
    FEditor.FormLoad(ProjMemIni);
    //FIOLEDs.FormLoad(ProjMemIni);

    FEditor.SynEditST.ReadOnly:=False;

    with Project do begin
      fileName:=aFileName;
      FEditor.EditAuthors.text:=Author;
      FEditor.EditComments.Text:=Comments;
      Modified:=False;
    end;
    result:=true;
  except

  end;
  finally
    //SynEditSTStatusChange(FMain,[scCaretX, scCaretY,scInsertMode,scModified]);
    FEditor.UpdateStatusLine;
    FEditor.Caption := FormEditorCaption+ExtractFileName(Project.FileName);
    ProjMemIni.Free;
  end;
end;


procedure SaveFormGeometryToMemIni( MemIni: TMemIniFile; const aForm: TForm);
begin
  MemIni.WriteInteger(aForm.Name,'top',aForm.Top);
  MemIni.WriteInteger(aForm.Name,'left',aForm.Left);
  MemIni.WriteInteger(aForm.Name,'height',aForm.Height);
  MemIni.WriteInteger(aForm.Name,'width',aForm.Width);
end;


procedure LoadFormGeometryFromMemIni( MemIni: TMemIniFile; aForm: TForm);
begin
  aForm.Top    := MemIni.ReadInteger(aForm.Name,'top',aForm.Top);
  aForm.Left   := MemIni.ReadInteger(aForm.Name,'left',aForm.Left);
  aForm.Height := MemIni.ReadInteger(aForm.Name,'height',aForm.Height);
  aForm.Width  := MemIni.ReadInteger(aForm.Name,'width',aForm.Width);
end;


procedure SaveStringsToMemIni( MemIni: TMemIniFile; section, Ident: string; SL: TStrings);
var i: integer;
    key: string;
begin
  key:=section+'\'+Ident;
  MemIni.WriteInteger(key,'count',SL.count);
  for i:=0 to SL.count-1 do begin
    MemIni.WriteString(key,'line'+inttostr(i), AnsiQuotedStr(SL.strings[i],'"'));
  end;
end;


procedure LoadStringsFromMemIni( MemIni: TMemIniFile; section, Ident: string; SL: TStrings);
var i, count: integer;
    key: string;
    txt: string;
begin
  key:=section+'\'+Ident;

  count := MemIni.ReadInteger(key,'count',-1);
  SL.Clear;
  for i:=0 to count-1 do begin
    txt:=MemIni.ReadString(key,'line'+inttostr(i),'');
    if txt='""' then txt:=''
    else txt:=AnsiDequotedStr(txt,'"');
    SL.Add(txt);
  end;
end;

procedure ProjectNew;
begin
  with Project do begin
    FileName:='Untitled';
    Author:='';
    Comments:='';
  end;

  //FVariables.FormInit;  //FVariables.LoadVariables(nil);
  // ResetPLCState;
  //FIOLEDs.FormInit;
  //FMain.FormInit;

  FEditor.EditAuthors.text:=Project.Author;
  FEditor.EditComments.Text:=Project.Comments;
  FEditor.SynEditST.ReadOnly:=False;
  FEditor.SynEditST.Modified:=False;

  FEditor.UpdateStatusLine;
  //SynEditSTStatusChange(FMain,[scCaretX, scCaretY,scInsertMode,scModified]);
  FEditor.caption := FormEditorCaption + ExtractFileName(Project.FileName);

  Project.Modified:=False;

end;


end.

