unit ProjConfig;

interface

uses Forms, SysUtils;

const  crlf=#13+#10;
       FormEditorCaption = 'Code Editor: ';

var SimTwoVersion: string = 'SimTwo v0.97';

function GetIniFineName: string;

implementation

function GetIniFineName: string;
begin
  result := extractfilepath(application.ExeName)+'\SimTwo.ini';
end;

end.
