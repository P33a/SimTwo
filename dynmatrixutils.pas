{ dynmatrixutils v0.1

  CopyRight (C) 2008 Paulo Costa, Armando Sousa

  This library is Free software; you can rediStribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; version 2 of the License.

  This program is diStributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; withOut even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a Copy of the GNU Library General Public License
  along with This library; if not, Write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  This license has been modified. See File LICENSE.ADDON for more inFormation.
}

unit dynmatrixutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dynmatrix, Grids;

function  Mload(fname: string): TDMatrix;
procedure Msave(M: TDmatrix; fname: string);

procedure MMatrix2Grid(SG: TStringGrid; M: TDMatrix);
function  MGrid2Matrix(SG: TStringGrid) : TDMatrix;
function  MReadFromStringList(var SL : TStrings): TDMatrix;

// Adds to TS all the elements from matrix A, line by line
procedure MAddToStringList(const m : TDMatrix; var TS: TStrings; FormatString: string = '%.6g'; ItemSeparator: string = ' ');

function MEqual( const m1,m2 : TDMatrix ; AllowErr : double ) : boolean;



implementation


function Mload(fname: string): TDMatrix;
var
  r,c,lines,rxc: integer;
  F: TextFile;
  dum: double;
begin
  result := Mzeros(0,0);
  AssignFile(F, fname);
  Reset(F);
  lines:=0;
  while not eof(F) do begin
    readln(F);
    inc(lines);
  end;
  CloseFile(F);

  AssignFile(F, fname);
  Reset(F);
  rxc:=-1;
  dum:=0;  // para cortar com o warning
  while not eof(F) do begin
    read(F,dum);
    inc(rxc);
  end;
  CloseFile(F);

  if (lines<=0) or (rxc<=0) or (((rxc div lines)*lines)<>rxc) then
     raise  Exception.Create('Bad file format: cannot load matrix');

  result:=Mzeros(lines,rxc div lines);

  AssignFile(F, fname);
  Reset(F);
  for r:=0 to lines-1 do begin
    for c:=0 to (rxc div lines)-1 do begin
      read(F,dum);
        result.setv(r,c,dum);
    end;
  end;
  CloseFile(F);

end;

procedure Msave(M: TDmatrix; fname: string);
var
  r,c: integer;
  F: TextFile;
begin
  AssignFile(F, fname);
  Rewrite(F);
  for r:=0 to M.NumRows-1 do begin
    for c:=0 to M.NumRows-1 do begin
      write(F,M.getv(r,c));
      write(F,' ');
    end;
    write(F,chr($0d)+chr($0a));
  end;
  CloseFile(F);
end;

// Preenche a StringGrid T com os elementos da matrix A
procedure MMatrix2Grid(SG: TStringGrid; M: TDMatrix);
var
  r,c: integer;
begin
  SG.RowCount := integer(M.NumRows) + SG.FixedRows;
  SG.ColCount := integer(M.NumCols) + SG.FixedCols;

  for r:=0 to M.NumRows-1 do begin
    for c:=0 to M.NumCols-1 do begin
      SG.cells[c+SG.FixedCols,r+SG.FixedRows]:=FloatToStr(M.getv(r,c));
    end;
  end;
end;


// Da matrix A com os elementos da StringGrid T
function MGrid2Matrix(SG: TStringGrid) : TDMatrix;
var
  r,c: integer;
begin

  result := Mzeros(SG.RowCount-SG.FixedRows, SG.ColCount-SG.FixedCols);

  for r:=0 to result.NumRows-1 do begin
    for c:=0 to result.NumCols-1 do begin
      result.setv(r,c, StrToFloat( SG.Cells[c+SG.Fixedcols,r+SG.Fixedrows] ) );
    end;
  end;

end;

//procedure ParseString(s,sep: string; sl: TStringList);
//var p,i,last: integer;
//begin
  //sl.Clear;
  //last:=1;
  //for i:=1 to length(s) do begin
    //p:=Pos(s[i],sep);
    //if p>0 then begin
      //if i<>last then
        //sl.add(copy(s,last,i-last));
      //last:=i+1;
    //end;
  //end;
  //if last<=length(s) then
    //sl.add(copy(s,last,length(s)-last+1));
//end;


//// Preenche a String S com os elementos da matrix A: a11,a12,a13; a21,a22,a23; ...
//function M2String(A: matrix): string;
//var
  //da: PdoubleArray;
  //rows,cols,r,c: integer;
  //sall,sr: string;
//begin
  //Mdata(A,rows,cols,da);

  //sall:='';
  //for r:=0 to rows-1 do begin
    //sr:='';
    //for c:=0 to cols-1 do begin
      //if sr<>'' then sr:=sr+' ';
      //sr:=sr+floattostr(da^[c+r*cols]);
    //end;
    //sall:=sall+sr;
    //if r<>(rows-1) then sall:=sall+';';
  //end;
  //result:=sall;
//end;


//// <- matrix com os elementos da String T
//function MString2Matrix(S: String): matrix;
//var
  //Mdr: PdoubleArray;
  //rows,cols,r,c: integer;
  //Linetokens,tokens: TStringList;
  //str: string;
//begin
  //result:='';
  //tokens:=TStringList.Create;
  //Linetokens:=TStringList.Create;
  //try
////    str:=uppercase(S);
    //ParseString(S,';'+#13+#10,Linetokens);
    //if Linetokens.count<=0 then
      //raise  EMatrixFile.Create('Bad string format: cannot parse matrix');
    //r:=LineTokens.Count;

    //ParseString(Linetokens.strings[0],' ',tokens);
    //if tokens.count<=0 then
      //raise  EMatrixFile.Create('Bad string format: cannot parse matrix');
    //c:=Tokens.Count;;

    //result:=Mnew(r,c);
    //Mdata(result,rows,cols,Mdr);

    //for r:=0 to rows-1 do begin
      //ParseString(Linetokens.strings[r],' ',tokens);
      //for c:=0 to cols-1 do begin
        //str:=tokens.Strings[c];
        //Mdr^[c+r*cols]:=strtofloat(str);
////        Mdr^[c+r*cols]:=strtofloat(tokens.Strings[r]);
      //end;
    //end;
  //finally
    //tokens.free;
    //LineTokens.free;
  //end;
//end;


// Input : a Tstrings Objects with ascci lines
// one string line per matrix line, separator is one of "!| ,;"
// Obs1: ";" is equal to ","
// Obs2: changes incoming SL... !!!!!!!! (by converting all separators into SPACEs)

// Output: the read TDynMatrix

function MReadFromStringList(var SL : TStrings): TDMatrix;
var
  r,c,lines,rxc : integer;
  s : string;
  SLRow : TStringList;
begin

  result := Mzeros(0,0);

  r := 0;
  while r<SL.Count do begin
    s:=SL.Strings[r];
    if s='' then begin SL.Delete(r); continue;end;
    for c:=1 to length(SL.Strings[r]) do
      if IsDelimiter('|!,;',s,c) then
        s[c]:=' ';
    s := trim(s);
    if s='' then begin SL.Delete(r); continue;end;
    SL.Strings[r] := s;
    inc(r);
  end;
  
  lines := SL.Count;
  
  rxc:=0;
  slRow := TStringList.Create;
  slRow.Delimiter := ' '; //SPACE
  try
    //for each SPACE delimited element
    for r := 0 to sl.Count-1 do begin
      //"load" the line into a stringlist
      slRow.DelimitedText := SL[r];
      if r=0 then result:=Mzeros(lines,slRow.Count);
      for c := 0 to slRow.Count-1 do begin
        result.setv(r,c,StrToFloat(slRow[c]));
        // FMain.Memo.Append(format ('mat[%d,%d]=%s=%g ',[r,c,SLRow[c],result.getv(r,c)])); // Debug only
        inc (rxc);
      end;
    end;

  finally
    if (lines<=0) or (rxc<=0) or (((rxc div lines)*lines)<>rxc) then
       raise  Exception.Create('Bad file format: cannot load matrix');
    slRow.Free;
  end;

end;


// Adds to TS all the elements from matrix A, line by line
procedure MAddToStringList(const m : TDMatrix; var TS: TStrings; FormatString: string = '%.6g'; ItemSeparator: string = ' ');
var
    r,c,rows, cols: Longword;
    sr: string;
begin
  rows := m.NumRows;
  cols := m.NumCols;

  for r:=0 to rows-1 do begin
    sr:='';
    for c:=0 to cols-1 do begin
      if sr <> '' then sr := sr + ItemSeparator;
      sr := sr + format(FormatString, [m.getv(r,c)]);
    end;
    TS.add(sr);
  end;
end;

function MEqual( const m1,m2 : TDMatrix ; AllowErr : double ) : boolean;
var
    mtemp : TDMatrix;
begin
  result := false;
  if (m1.NumRows<>m2.NumRows) or (m1.NumCols<>m2.NumCols) then exit;

  mtemp := m1-m2;
  if MmaxAbs(mtemp)>AllowErr then exit;

  result:=true;
end;


end.

