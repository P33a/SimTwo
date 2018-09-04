unit ProjConfig;

{$MODE Delphi}

interface

uses Forms, SysUtils;

const  crlf=#13+#10;
       FormEditorCaption = 'Code Editor: ';

var SimTwoVersion: string = 'SimTwo v1';

function GetIniFineName: string;

implementation

function GetIniFineName: string;
begin
  //result := extractfilepath(application.ExeName)+'\SimTwo.ini';
  result :=  GetCurrentDir + PathDelim + 'SimTwo.ini';
end;

end.
