{ dynmatrix v0.1

  Copyright (C) 2008 Paulo Costa

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

  This license has been modified. See File LICENSE.ADDON for more information.
}

unit dynmatrix;

{$MODE Delphi}

interface

uses
  SysUtils;

type

  { Matrix }

  Matrix = record
    data: array of double;
    rows: Longword;
    cols: Longword;
  end;

//  TDoubleFunc = function(v: double): double;


procedure MInit(var Mat: Matrix; newrows, newcols: Longword);
procedure MTestData(const Mat: Matrix; out NRows, NCols: Longword);

procedure MSetSize(var Mat: Matrix; newrows, newcols: Longword);
procedure Msetv(var Mat: Matrix; r, c: Longword; v: double);
function Mgetv(const Mat: Matrix; r, c: Longword): double;
procedure MUsetv(var Mat: Matrix; r, c: Longword; v: double);
function MUgetv(const Mat: Matrix; r, c: Longword): double;

function MIsGood(const Mat: Matrix): boolean;
function MNumCols(const Mat: Matrix): Longword;
function MNumRows(const Mat: Matrix): Longword;

function Mzeros(numrows, numcols: LongWord): Matrix;
function Meye(n: Longword): Matrix;
function Mrandom(numrows, numcols: LongWord): Matrix;
function Minc(numrows, numcols: LongWord): Matrix;

procedure ArrayToMatrix(var M: Matrix; const D: array of double);

function Madd(const A, B: Matrix): Matrix;
function MaddReal(const A: Matrix; k: double): Matrix;
function Mneg(const A: Matrix): Matrix;
function Msub(const A, B: Matrix): Matrix;
function MsubReal(const A: Matrix; k: double): Matrix;
function MmultReal(const A: Matrix; k: double): Matrix;
function Mmult(const A, B: Matrix): Matrix;
function MPower(const M: Matrix; const n: integer): Matrix;

function Mtran(const M: Matrix): Matrix;
function Minv(const M: Matrix): Matrix;
function Minv_fast(const M: Matrix): Matrix;

function MelementMult(const A, B: Matrix): Matrix;

function Mmin(const M: Matrix): double;
function Mmax(const M: Matrix): double;
function MmaxAbs(const M: Matrix): double;
function MallNorm(const M: Matrix): double;

//function Mfunc(const A: Matrix; f: TDoubleFunc): Matrix;

function MHflip(const M: Matrix): Matrix;
function MConv(const A, B: Matrix): Matrix;

function MCrop(const M: Matrix; uprow, leftcol, downrow, rightcol: Longword): Matrix;
function MOneCol(const M:Matrix; col: Longword): Matrix;
function MOneRow(const M:Matrix; row: Longword): Matrix;

function MStamp(const M, S: Matrix; drow, dcol: Longword): Matrix;
function MStampCol(const M, S: Matrix; col: Longword): Matrix;
function MStampRow(const M, S: Matrix; row: Longword): Matrix;

function MColsum(const M: Matrix): Matrix;
function MRowsum(const M: Matrix): Matrix;

function  Mload(const fname: string): Matrix;
procedure Msave(M: Matrix; const fname: string);

// Missing:
//function Mvflip(M:Matrix): Matrix;

//function MColNorm2(M:Matrix): Matrix;

//procedure MShape(M:Matrix; newrow,newcol: integer);
//procedure MShape2col(M:Matrix);
//procedure MShape2row(M:Matrix);

//function VDotProduct(A: Matrix; B: Matrix): Matrix;
//function VExtProduct(A: Matrix; B: Matrix): Matrix;
//function VNorm1(A: Matrix; B: Matrix): Matrix;
//function VNorm2(A: Matrix; B: Matrix): Matrix;
//function VNormInf(A: Matrix; B: Matrix): Matrix;


implementation

uses math;

//{ $RANGECHECKS OFF }

// <- Transpose of M
function MTran(const M: Matrix): Matrix;
var
  r,c: Longword;
begin
  MInit(result, M.cols, M.rows);
  for c:=0 to M.cols-1 do
    for r:=0 to M.rows-1 do begin
      result.data[r + c * M.rows] := M.data[c + r * M.cols];
    end;
end;

// Zeros matrix
function Mzeros(numrows, numcols: LongWord): Matrix;
var r,c: Longword;
begin
  MSetSize(result, numrows, numcols);
  if (numrows>0) and (numcols>0) then begin
    for c := 0 to numcols - 1 do
      for r := 0 to numrows - 1 do begin
        result.data[r + c * numrows] := 0;
      end;
  end;
end;

// Identity matrix
function Meye(n: Longword): Matrix;
var i: Longword;
begin
  MInit(result, n, n);
  for i := 0 to n - 1 do begin
    result.data[i + i * n] := 1;
  end;
end;

// Returns a Matrix with (numrows, numcols) elements with random values between 0 e 1
function Mrandom(numrows, numcols: LongWord): Matrix;
var i: Longword;
begin
  MInit(result, numrows, numcols);
  for i := 0 to numrows * numcols - 1 do begin
    result.data[i] := random;
  end;
end;

function Minc(numrows, numcols: LongWord): Matrix;
var i: Longword;
begin
  MInit(result, numrows, numcols);
  for i := 0 to numrows * numcols - 1 do begin
    result.data[i] := i;
  end;
end;


// <- M^n (power n of a square matrix M) with non-negative, integer n.
//function Mpow(const M: Matrix; n: longword): Matrix;
//begin
//  result := M ** n;
//end;


function Madd(const A, B: Matrix): Matrix;
var i : LongWord;
begin
  if (A.rows <> B.rows) or (A.cols <> B.cols) then
    raise  Exception.Create(format('Cannot add matrix (%d,%d) with matrix (%d,%d)',[A.rows, A.Cols, B.rows, B.cols]));

  MInit(result, A.rows,A.cols);

  for i := 0 to A.rows * A.cols - 1 do begin
    result.data[i] := A.data[i] + B.data[i];
  end;

end;


function MaddReal(const A: Matrix; k: double): Matrix;
var i: LongWord;
begin
  MInit(result, A.rows, A.cols);

  for i := 0 to A.rows * A.cols - 1 do begin
    result.data[i] := A.data[i] + k;
  end;

end;



// <- -A  ie R(i,j) := -A(i,j)
function Mneg(const A: Matrix): Matrix;
var i: LongWord;
begin
  MInit(result, A.rows, A.cols);

  for i := 0 to A.rows * A.cols - 1 do begin
    result.data[i] := - A.data[i];
  end;

end;

// <- A-B  ie R(i,j) := A(i,j) - B(i,j)
function Msub(const A, B: Matrix): Matrix;
var i: LongWord;
begin
  if (A.rows <> B.rows) or (A.cols <> B.cols) then
    raise  Exception.Create(format('Cannot subtract matrix (%d,%d) with matrix (%d,%d)',[A.rows, A.Cols, B.rows, B.cols]));

  MInit(result, A.rows, A.cols);

  for i := 0 to A.rows * A.cols - 1 do begin
    result.data[i] := A.data[i] - B.data[i];
  end;

end;

function MsubReal(const A: Matrix; k: double): Matrix;
var i: LongWord;
begin
  MInit(result, A.rows, A.cols);

  for i := 0 to A.rows * A.cols - 1 do begin
    result.data[i] := A.data[i] - k;
  end;

end;


// <- A * k (k: double) ie R(i,j) := A(i,j) * k
function MmultReal(const A: Matrix; k: double): Matrix;
var i: LongWord;
begin
  MInit(result, A.rows, A.cols);

  for i := 0 to A.rows * A.cols - 1 do begin
    result.data[i] := A.data[i] * k;
  end;

end;


// <- A*B
function Mmult(const A, B: Matrix): Matrix;
var r,c,i: LongWord;
  sum: double;
begin
  if A.cols <> B.rows then
    raise Exception.Create(format('Cannot multiply matrix (%d,%d) with matrix (%d,%d)',[A.rows, A.Cols, B.rows, B.cols]));

  MInit(result, A.rows, B.cols);

  for r := 0 to A.rows-1 do begin
    for c := 0 to B.cols-1 do begin
      sum := 0;
      for i :=0 to A.cols-1 do begin
        sum := sum + A.data[r*A.cols + i] * B.data[c + i*B.cols];
      end;
      result.data[c + r*B.cols] := sum;
    end;
  end;
end;


// <- M^n (power n of a square matrix M) with non-negative, integer n.
function MPower(const M: Matrix; const n: integer): Matrix;
var np: longword;
    P: Matrix;
begin
  if n < 0 then begin
    result := Mpower(Minv(M), -n);
    exit;
  end;
  // Must handle special cases: n = 0, and n = 1
  if n = 0 then begin
    result := Meye(n);
    exit;
  end;

  result := M;

  if n = 1 then exit;

  // General case: n >= 2
  P := M;                         // P holds the current square
  np := n - 1;
  while (np >= 1) do begin
    if (np and 1) = 0 then begin  // np is even, we have a zero in the binary expansion
      np := np div 2;
    end else begin                // np is odd, we have a one in the binary expansion
      np := (np - 1) div 2;
      result := MMult(result, P);
    end;
    P := MMult(P, P);
  end;
end;


{ Matrix }

procedure MInit(var Mat: Matrix; newrows, newcols: Longword);
begin
  with Mat do begin
    rows := NewRows;
    cols := NewCols;
    Setlength(data, rows * cols);
  end;
end;

// Inicializes a matrix with numrows lines and numcols columns
procedure MSetSize(var Mat: Matrix; newrows, newcols: Longword);
begin
  with Mat do begin
    rows := NewRows;
    cols := NewCols;
    Setlength(data, rows * cols);
  end;
end;


// Write v to Element [r,c]
procedure Msetv(var Mat: Matrix; r, c: Longword; v: double);
begin
  with Mat do begin
    Setlength(data, rows * cols); // Make unique
    if (r >= Rows) or (c >= Cols) then
      raise Exception.Create(format('Invalid (row,col) value. Matrix is (%d,%d), element required is (%d,%d)',[Rows, Cols, r,c]));
    data[c + r*Cols] := v;
  end;
end;

// Get Element [r,c]
function Mgetv(const Mat: Matrix; r, c: Longword): double;
begin
  with Mat do begin
    if (r >= Rows) or (c >= Cols) then
      raise Exception.Create(format('Invalid (row,col) value. Matrix is (%d,%d), element required is (%d,%d)',[Rows, Cols, r,c]));
    result := data[c + r*Cols];
  end;
end;

// Write to v Element [r,c] , ignore operation if r,c is out of bounds
procedure MUsetv(var Mat: Matrix; r, c: Longword; v: double);
begin
  with Mat do begin
    Setlength(data, rows * cols);  // Make unique
    if (r >= Rows) or (c >= Cols) then exit;
    data[c + r*Cols] := v;
  end;
end;

// Get Element [r,c], 0 if r,c out of bounds
function MUgetv(const Mat: Matrix; r, c: Longword): double;
begin
  with Mat do begin
    if (r >= Rows) or (c >= Cols) then begin
      result := 0;
      exit;
    end;
    result := data[c + r*Cols];
  end;
end;


procedure MTestData(const Mat: Matrix; out NRows, NCols: Longword);
begin
  with Mat do begin
    NRows := rows;
    NCols := cols;

    if data=nil then
      raise Exception.Create('Invalid matrix: nil data');

    if not (rows>0) then
      raise  Exception.Create('Invalid number of rows:'+inttostr(rows));

    if not (cols>0) then
      raise  Exception.Create('Invalid number of columns:'+inttostr(cols));

    if longword(length(data)) <> (rows * cols) * sizeof(double) then
      raise  Exception.Create('Invalid matrix: incompatible data size');
  end;
end;

// Get total number of columns
function MNumCols(const Mat: Matrix): Longword;
begin
  result := Mat.cols;
end;


// Get total number of rows
function MNumRows(const Mat: Matrix): Longword;
begin
  result := Mat.rows;
end;

// Test the matrix goodness:
//  if the number of row and cols is not zero
//  if the string size is compatible with expected embeded array
// <- true if it is good
function MIsGood(const Mat: Matrix): boolean;
begin
  with Mat do begin
    result:=false;
    if (pointer(data) = nil) then exit;
    if (rows > 0) and (cols > 0) and (longword(length(data)) = ((rows*cols)*sizeof(double))) then
      result:=True
  end;
end;


// <- A+B
//function MAdd(A: Matrix; B: Matrix): Matrix; inline;
//begin
//  result := A + B;
//end;


// <- M^-1
function Minv(const M: Matrix): Matrix;
var
  ROW, COL: array of Longword;
  MatINV, MatTMP: Matrix;
  HOLD , I_pivot , J_pivot: Longword;
  fv, pivot, abs_pivot, rel_eps: double;
  n, i, j, k, {r, c,} rin, rkn, ck, cj: Longword;
begin
//  M.GetData(r, c, Mda);
  if M.cols <> M.rows then // c:= M.cols r := M.rows
    raise Exception.Create('Cannot invert non-square matrix');

  n := M.cols;
  SetLength(ROW, n);
  SetLength(COL, n);
  MatTMP := MZeros(n, n);
  MatINV := M;

  SetLength(MatINV.data, MatINV.rows * MatINV.cols);  // Make unique

  // Set up row and column interchange vectors
  for k := 0  to n-1 do begin
    ROW[k] := k;
    COL[k] := k;
  end;

  // Find largest element
  rel_eps := 0;
  for i := 0 to n-1 do begin
    for j := 0  to n-1 do begin
      fv := abs(MatINV.data[ROW[i]*n + COL[j]]);
      if  fv > rel_eps then begin
        rel_eps := fv ;
      end;
    end;
  end;
  rel_eps := rel_eps * 1e-15;


  // Begin main reduction loop
  for k := 0  to n-1 do begin
    // Find largest element for pivot
    pivot := MatINV.data[ROW[k]*n+COL[k]];
    abs_pivot := abs(pivot);
    I_pivot := k;
    J_pivot := k;
    for i := k to n-1 do begin
      for j := k  to n-1 do begin
        //abs_pivot := abs(pivot);
        fv := MatINV.data[ROW[i]*n+COL[j]];
        if  abs(fv) > abs_pivot then begin
          I_pivot := i;
          J_pivot := j;
          pivot := fv;
          abs_pivot := abs(pivot);
        end;
      end;
    end;
    if abs(pivot) < rel_eps then
      raise Exception.Create(format('Singular matrix: Pivot is %g, max element = %g',[pivot, rel_eps]));

    HOLD := ROW[k];
    ROW[k] := ROW[I_pivot];
    ROW[I_pivot] := HOLD;

    HOLD := COL[k];
    COL[k] := COL[J_pivot];
    COL[J_pivot] := HOLD;

    rkn := ROW[k]*n;
    ck := COL[k];

    // Reduce around pivot
    MatINV.data[rkn + ck] := 1.0 / pivot ;
    for j :=0 to n-1 do begin
      if j <> k  then begin
        cj := COL[j];
        MatINV.data[rkn + cj] := MatINV.data[rkn + cj] * MatINV.data[rkn + ck];
      end;
    end;

    // Inner reduction loop
    for i := 0 to n-1 do begin
      rin := ROW[i]*n;
      if k <> i then begin
        fv := MatINV.data[rin + ck];
        for j := 0 to n-1 do begin
          if  k <> j then begin
            cj := COL[j];
            MatINV.data[rin + cj] := MatINV.data[rin + cj] - fv * MatINV.data[rkn + cj] ;
          end;
        end;
        MatINV.data[rin + ck] := - MatINV.data[rin + ck] * MatINV.data[rkn + ck];
      end;
    end;
  end; // end of main reduction loop

  // Unscramble rows
  for j := 0  to n-1 do begin
    for i := 0 to n-1 do begin
      MatTMP.data[COL[i]] := MatINV.data[ROW[i]*n + j];
    end;
    for i := 0 to n-1 do begin
      MatINV.data[i*n + j] := MatTMP.data[i];
    end;
  end;

  // Unscramble columns
  for i := 0 to n-1 do begin
    for j := 0 to n-1 do begin
      MatTMP.data[ROW[j]] := MatINV.data[i*n + COL[j]];
    end;
    for j := 0 to n-1 do begin
      MatINV.data[i*n+j] := MatTMP.data[j];
    end;
  end;

  result := MatInv;
end;


// <- M^-1
// Faster and less acurate version
function Minv_fast(const M: Matrix): Matrix;
var dim,r,c,t,pivrow,k: Longword;
  pivmax,pivot: double;
  INV,TMP: Matrix;
  ex,pdisp,cdisp:Longword;
  dtmp,victim,rk,norm,invnorm: double;
  Mzero : double;
begin
  if M.cols <> M.rows then
    raise Exception.Create('Cannot invert non-square matrix');

  dim := M.rows;
  INV := Meye(dim);
  TMP := M;

  setlength(TMP.data, TMP.cols * TMP.rows);  // Make unique

  MZero := 1e-10;
  for c := 0 to dim - 1 do begin
    // find the greatest pivot in the remaining columns
    pivmax := abs(TMP.data[c + c*dim]);
    pivrow := c;
    for k := c + 1 to dim - 1 do begin
      if abs(TMP.data[c + k*dim]) > pivmax then begin
        pivmax := abs(TMP.data[c + k*dim]);
	pivrow:=k;
      end;
    end;
    pivot:= TMP.data[c + pivrow*dim];
    if abs(pivot) < Mzero then
      raise Exception.Create('Singular matrix: Pivot is '+floattostr(pivot));

    if pivrow <> c then begin
      // swap lines
      pdisp:=pivrow*dim;
      cdisp:=c*dim;
      for ex:=c to dim-1 do begin
        dtmp:=TMP.data[cdisp+ex];
        TMP.data[cdisp+ex]:=TMP.data[pdisp+ex];
        TMP.data[pdisp+ex]:=dtmp;
      end;
      for ex:=0 to dim-1 do begin
	dtmp:=INV.data[cdisp+ex];
        INV.data[cdisp+ex]:=INV.data[pdisp+ex];
        INV.data[pdisp+ex]:=dtmp;
      end;
    end;

    for r:=0 to dim-1 do begin
      if r<>c then begin
        victim:=TMP.data[c+r*dim];
        rk:=-victim/pivot;
        for t:=0 to dim-1 do INV.data[r*dim+t]:= INV.data[r*dim+t] + rk * INV.data[c*dim+t];
        for t:=c+1 to dim-1 do TMP.data[r*dim+t]:= TMP.data[r*dim+t] + rk * TMP.data[c*dim+t];
      end;
    end;
  end;

  // normalize the pivots
  for r := 0 to dim - 1 do begin
    norm := TMP.data[r + r*dim];
    if abs(norm) < Mzero then
       raise Exception.Create('Singular matrix: Pivot has been '+floattostr(norm));
    invnorm := 1.0 / norm;
    for c := 0 to dim - 1 do
	    INV.data[c + r*dim] := INV.data[c + r*dim] * invnorm;
  end;
  result:=INV;
end;


// <- max M(i,j)
function Mmax(const M: Matrix): double;
var i: Longword;
begin
  result := M.data[0];
  for i := 1 to M.rows * M.cols - 1 do begin
    if (result < M.data[i]) then result := M.data[i];
  end;
end;

// <- sqrt(sum all elements)
function MallNorm(const M: Matrix): double;
var i: Longword;
begin
  result := M.data[0] * M.data[0];
  for i := 1 to M.rows * M.cols - 1 do begin
    result := result + M.data[i] * M.data[i];
  end;
  result := sqrt(result);
end;


// <- max |M(i,j)|
function MmaxAbs(const M: Matrix): double;
var i: Longword;
begin
  result := abs(M.data[0]);
  for i := 1 to M.rows * M.cols - 1 do begin
    if (result < abs(M.data[i])) then result := abs(M.data[i]);
  end;
end;


// <- A .* B (Element-wise mutiplication)
function MelementMult(const A, B: Matrix): Matrix;
var i: LongWord;
begin
  if (A.rows <> B.rows) or (A.cols <> B.cols) then
    raise  Exception.Create(format('Cannot Element-wise mutiply matrix (%d,%d) with matrix (%d,%d)',[A.rows, A.Cols, B.rows, B.cols]));

  MInit(result, A.rows,A.cols);

  for i := 0 to A.rows * A.cols - 1 do begin
    result.data[i] := A.data[i] * B.data[i];
  end;

end;

// <- min M(i,j)
function Mmin(const M: Matrix): double;
var i: Longword;
begin
  result := M.data[0];
  for i := 1 to M.rows * M.cols - 1 do begin
    if (result > M.data[i]) then result := M.data[i];
  end;
end;


{function Mfunc(const A: Matrix; f: TDoubleFunc): Matrix;
var i: Longword;
begin
  result := A;
  SetLength(result.data, result.rows * result.cols);  // Make unique

  for i := 0 to result.rows * result.cols - 1 do begin
    result.data[i] := f(result.data[i]);
  end;
end;}




// <- Reverse Columns
function Mhflip(const M: Matrix): Matrix;
var r,c: Longword;
begin
  MInit(result, M.rows, M.cols);
  for r := 0 to M.rows - 1 do begin
    for c:= 0 to M.cols-1 do begin
      result.data[c + r * M.rows] := M.data[(M.cols - 1) - c + r * M.cols];
    end;
  end;
end;


// <- row convolution between A and B
function MConv(const A, B: Matrix): Matrix;
var ar,br,disp,r,c,i: Longword;
    pivot,prod: double;
begin
  result := MZeros(A.rows * B.rows, A.cols + B.cols - 1);

  for ar:=0 to A.rows-1 do begin
    for br:=0 to B.rows-1 do begin
      for disp:=0 to A.cols-1 do begin
        r:=ar*B.rows+br;
        pivot:= A.data[disp+ar*A.cols];
        for i:=0 to B.cols-1 do begin
          prod := pivot * B.data[i+br*B.cols];
          c:=disp+i;
          result.data[c+r*result.cols]:=result.data[c+r*result.cols] + prod;
        end;
      end;
    end;
  end;
end;


// Fill matrix A with the elements from array D
procedure ArrayToMatrix(var M: Matrix; const D: array of double);
var i: Longword;
begin
  if M.rows * M.cols <> Longword(high(D) - low(D) + 1) then
    raise  Exception.Create('Const Array size does not match Matrix size');

  for i := 0 to M.cols * M.rows - 1 do begin
    M.data[i] := D[i];
  end;
end;



// <- A submatrix from M
function MCrop(const M: Matrix; uprow, leftcol, downrow, rightcol: Longword): Matrix;
var rowsize,colsize,r,c: Longword;
begin
  rowsize:=downrow-uprow+1;
  colsize:=rightcol-leftcol+1;
  if (rowsize < 1) or (colsize < 1) then
     raise  Exception.Create('Invalid number of rows/cols:'+inttostr(rowsize)+'/'+inttostr(colsize));

  if (downrow > M.rows-1) or (rightcol > M.cols-1) then
     raise  Exception.Create('Invalid number of rows/cols:'+inttostr(downrow)+'/'+inttostr(rightcol));

  MInit(result, rowsize,colsize);

  for r:=0 to result.rows-1 do begin
    for c:=0 to result.cols-1 do begin
      result.data[c+r*result.cols]:= M.data[c+leftcol+(r+uprow)*M.cols];
    end
  end;
end;


// <- one col from M
function MOneCol(const M:Matrix; col: Longword): Matrix;
begin
  result := Mcrop(M, 0, col, M.rows - 1, col)
end;


// <- one row from M
function MOneRow(const M:Matrix; row: Longword): Matrix;
begin
  result := Mcrop(M, row, 0, row, M.cols - 1)
end;



// <- Replace part of matrix M with matrix S
function MStamp(const M, S: Matrix; drow, dcol: Longword): Matrix;
var r,c: Longword;
begin
  if (drow + S.rows > M.rows) or (dcol + S.cols > M.cols) then
     raise  Exception.Create(format('Matrix(%d,%d) does not fit im matrix(%d,%d)!',[M.rows, M.cols, S.rows, S.cols]));

  result := M;
  SetLength(result.data, result.rows * result.cols);  // Make unique

  for c:=0 to S.cols-1 do begin
    for r:=0 to S.rows-1 do begin
      result.data[c+dcol+(r+drow)*result.cols]:= S.data[c+r*S.cols];
    end
  end;
end;

// <-  (column of M with index col) := S
function MStampCol(const M, S: Matrix; col: Longword): Matrix;
begin
  result := MStamp(M, S, 0, col);
end;

// <- (row of M with index row) := S
function MStampRow(const M, S: Matrix; row: Longword): Matrix;
begin
  result := MStamp(M, S, row, 0);
end;



// <- matrix with the sum of all M columns
function MColsum(const M: Matrix): Matrix;
var r,c: Longword;
begin
  result := MZeros(1, M.cols);

  for c:=0 to M.cols-1 do begin
    for r:=0 to M.rows-1 do begin
      result.data[c] := result.data[c] + M.data[c + r * M.cols];
    end
  end;
end;

// <- matrix with the sum of all M rows
function MRowsum(const M: Matrix): Matrix;
var r,c: Longword;
begin
  result := MZeros(M.rows, 1);

  for r:=0 to M.rows-1 do begin
    for c:=0 to M.cols-1 do begin
      result.data[r] := result.data[r]+ M.data[c + r * M.cols];
    end
  end;
end;


function Mload(const fname: string): Matrix;
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
  //dum:=0;  // para cortar com o warning
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
      Msetv(result, r,c,dum);
    end;
  end;
  CloseFile(F);

end;

procedure Msave(M: matrix; const fname: string);
var
  r,c: integer;
  F: TextFile;
begin
  AssignFile(F, fname);
  Rewrite(F);
  for r:=0 to MNumRows(M)-1 do begin
    for c:=0 to MNumCols(M)-1 do begin
      write(F,Mgetv(M, r,c));
      write(F,' ');
    end;
    write(F,chr($0d)+chr($0a));
  end;
  CloseFile(F);
end;

//{$R+}

initialization

end.
