unit VerInfo;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms;

const
  InfoNum = 10;
  InfoKey: array[1..InfoNum] of string = ('CompanyName', 'FileDescription', 'FileVersion', 'InternalName', 'LegalCopyright', 'LegalTradeMarks', 'OriginalFileName', 'ProductName', 'ProductVersion', 'Comments');

var
  InfoData: array[1..InfoNum] of string;

procedure GetVersionInfo;

implementation

procedure GetVersionInfo;
var
  S: string;
  n, Len, i: DWORD;
  Buf: PChar;
  Value: PChar;
begin
  S := Application.ExeName;
  n := GetFileVersionInfoSize(PChar(S), n);
  if n > 0 then begin
    Buf := AllocMem(n);
    //Memo1.Lines.Add('VersionInfoSize = ' + IntToStr(n));
    GetFileVersionInfo(PChar(S), 0, n, Buf);
    for i := 1 to InfoNum do begin
      if VerQueryValue(Buf, PChar('StringFileInfo\040904E4\' + InfoKey[i]), Pointer(Value), Len) then begin
        //Memo1.Lines.Add(InfoStr[i] + ' = ' + Value);
        InfoData[i] := value;
      end;
    end;
    FreeMem(Buf, n);
  end;// else
    //Memo1.Lines.Add('No version information found');
end;

end.
