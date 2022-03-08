unit ProjConfig;

{$MODE Delphi}

interface

uses Forms, SysUtils;

const  crlf = #13 + #10;
       FormEditorCaption = 'Code Editor: ';

var SimTwoVersion: string = 'SimTwo 64';

function GetIniFineNameOld: string;
function GetIniFineName(sufix: string): string;

implementation

function GetIniFineNameOld: string;
begin
  //result := extractfilepath(application.ExeName)+'\SimTwo.ini';
  result :=  GetCurrentDir + PathDelim + 'SimTwo.ini';
end;

function GetIniFineName(sufix: string): string;
begin
  //result := extractfilepath(application.ExeName)+'\SimTwo.ini';
  result :=  GetCurrentDir + PathDelim + 'config_' + sufix + '.ini';
end;


end.
