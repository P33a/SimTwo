unit SimpleParser;

interface

uses classes,sysutils,
{$ifdef WIN32}
  windows,
{$endif}
{$ifdef LINUX}

{$endif}
  math;

const
  maxDoubleStack = 256;


type
  TSimpleAction=(saCheckSintax, saEval, saCompile);

  TParserFunction= function(const v: array of double): double;

  THash256 = array[0..255] of integer;

  TSimpleParser = class
    Look: char;              // Lookahead Character
    lookIndex: integer;
    line: string;
    ResultList, VarsList, FuncsList: TStrings;
    LastError: string;
    Action:TSimpleAction;

    //Stack, Consts: array[0..maxDoubleStack-1] of double;   // TODO: change to dynamic array
    Stack, Consts: array of double;   // TODO: change to dynamic array
    StackIndex: integer;

    HashFuncs, HashVars: THash256;
    HashFuncsColisions: integer;
  private
    procedure Push(v: double);
    function Pop: double;

    function GetVariable(VarName: string): pdouble;
    function GetFunction(FuncName: string): TParserFunction;
    procedure GetFunctionData(const FuncName: string; out pf: TParserFunction; out NumArgs: integer);

    procedure GetChar;
    procedure RaiseError(s: string);
    procedure Expected(s: string);
    procedure SkipWhite;
    procedure Match(x: char);
    function GetName: string;
    function GetNum: string;
    procedure EmitAction(command: char; data: string);
    procedure Ident;
    procedure Factor;
    procedure SignedFactor;
    procedure Divide;
    procedure Multiply;
    procedure Term;
    procedure Add;
    procedure Subtract;
    procedure Expression;
    function BoolExpressionList: integer;
    function GetRelOp: char;
    procedure Relation;
    procedure BoolExpression;

    procedure Init(e: string; nResultList: TStrings);

  public
    constructor Create;
    destructor Destroy; override;

    procedure CopyVarList(const SourceParser: TSimpleParser);

    procedure Check(e: string);
    function Calc(e: string): double;
    function Assign(e: string): double;

    procedure Compile(e: string; nResultList: TStrings);
    function Run(const rpnList: TStrings): double;

    procedure RegisterVariable(VarName: string; pvar: pdouble);
    procedure RegisterConst(ConstName: string; value: double);
    function RegisterFunction(FuncName: string; pFunc: TParserFunction; numArgs: integer): integer;
    function ConstIsDefined(ConstName: string): boolean;

  end;



implementation

function BuildHashTable(const SList: TStrings; var HashTable: THash256): integer; forward;


procedure TSimpleParser.Push(v: double);
begin
  if StackIndex >= length(Stack) then begin
    SetLength(Stack, 2 * length(Stack));
    //exit;
  end;
  Stack[StackIndex]:=v;
  inc(StackIndex);
end;

function TSimpleParser.Pop: double;
begin
  if StackIndex<=0 then begin
    result:=0;
    exit;
  end;
  dec(StackIndex);
  result:=Stack[StackIndex];
end;


//procedure RegisterVariable(Vars: TStrings; pvar: pdouble; VarName: string);
procedure TSimpleParser.RegisterVariable(VarName: string; pvar: pdouble);
begin
 //TODO: Test if varname already exists
  VarsList.AddObject(uppercase(VarName), Tobject(pvar));
//  BuildHashTable(VarsList,HashVars);
end;

procedure TSimpleParser.RegisterConst(ConstName: string; value: double);
var idx, ConstsCount: integer;
begin
  //Test if constname already exists
  idx := VarsList.IndexOf(uppercase(ConstName));
  if idx = -1 then begin
    ConstsCount := VarsList.Count;
    if ConstsCount >=  length(consts) then begin //exit; //BAaaaad...
      SetLength(consts, 2 * length(consts));
    end;
    consts[ConstsCount] := value;

    VarsList.AddObject(uppercase(ConstName), Tobject(@consts[ConstsCount]));
  end else begin
    consts[idx] := value;
  end;
end;


function TSimpleParser.ConstIsDefined(ConstName: string): boolean;
var idx: integer;
begin
  //Test if constname already exists
  idx := VarsList.IndexOf(uppercase(ConstName));
  result := (idx <> -1);
end;


function TSimpleParser.GetVariable(VarName: string): pdouble;
var idx: integer;
begin
  if VarsList = nil then
    raise Exception.Create('Variables Not Defined!');
  idx := VarsList.IndexOf(VarName);
  if idx < 0 then
    raise Exception.Create('Variable "' + Varname + '" Not Found!');
//  result:=pdouble(VarsList.Objects[idx])^;
  result:=pdouble(VarsList.Objects[idx]);
end;

//procedure RegisterFunction(Funcs: TStrings; pFunc: TParserFunction; FuncName: string; numArgs: integer);
function TSimpleParser.RegisterFunction(FuncName: string; pFunc: TParserFunction; numArgs: integer): integer;
begin
  result := FuncsList.AddObject(uppercase(FuncName)+'_'+inttostr(numargs), Tobject(@pFunc));
  HashFuncsColisions := BuildHashTable(FuncsList, HashFuncs);
end;

// Generic hash functions
//  hash(i) = hash(i - 1) * 65599 + str[i];
function CalcHash(const s: string): byte;
var i: integer;
    hash: Longword;
begin
  hash:=0;
  for i:=1 to length(s) do begin
    hash := hash*65599 + ord(s[i]);
  end;
  result:=hash and 255;
end;


function BuildHashTable(const SList: TStrings; var HashTable: THash256): integer;
var i,idx: integer;
begin
  result:=0;
  for i:=0 to 255 do HashTable[i]:=-1;

  for i:=0 to SList.Count-1 do begin
    idx:=CalcHash(SList.strings[i]);
    if HashTable[idx]<>-1 then inc(result);
    HashTable[idx]:=i;
  end;
end;

function HashIndexOf(SList: TStrings; var HashTable: THash256; const name: string): integer;
begin
  result:=HashTable[CalcHash(name)];
  if result>=0 then
    if SList.Strings[result] = name then exit; //Found!
  // Linear search fallback
  result:=SList.IndexOf(name);
end;

//function GetFunction(Funcs: TStrings; FuncName: string): TParserFunction;
function TSimpleParser.GetFunction(FuncName: string): TParserFunction;
var idx: integer;
begin
  if FuncsList = nil then
    raise Exception.Create('Functions Not Defined!');

  idx := HashIndexOf(FuncsList, HashFuncs, FuncName);
  if idx < 0 then begin
    idx:=length(FuncName);
    raise Exception.Create('Function "' + copy(FuncName,1,idx-2) + ' with ' + FuncName[idx] + ' args Not Found!');
  end;
  result := TParserFunction(FuncsList.Objects[idx]);
end;


//procedure GetFunctionData(Funcs: TStrings; const FuncName: string; out pf: TParserFunction; out NumArgs: integer);
procedure TSimpleParser.GetFunctionData(const FuncName: string; out pf: TParserFunction; out NumArgs: integer);
var idx: integer;
    s: string;
begin
  if FuncsList = nil then
    raise Exception.Create('Functions Not Defined!');
  //idx:=Funcs.IndexOf(FuncName);
  idx := HashIndexOf(FuncsList, HashFuncs, FuncName);
  if idx < 0 then begin
    idx := length(FuncName);
    raise Exception.Create('Function "' + copy(FuncName,1,idx-2) + ' with ' + FuncName[idx] + ' args Not Found!');
  end;
  s := FuncsList.Strings[idx];
  NumArgs := strtoint(s[length(s)]);
  pf := TParserFunction(FuncsList.Objects[idx]);
end;


{
function IsValidFuncChar(c: char; first_char: boolean): boolean;
begin
  result:=false;
  if (first_char) and (IsDigit(c)) then exit;
  result:=((c>='a') and (c<='z')) or ((c>='A') and (c<='Z')) or
          IsDigit(c) or (c='_');
end;
}

//--------------------------------------------------------------
// Read New Character From Input Stream

procedure TSimpleParser.GetChar;
begin
 //  Read(Look);
  if lookIndex<=length(line) then begin
    Look:=line[lookIndex];
    inc(lookIndex);
  end else begin
    look:=#0;
  end;
end;


//--------------------------------------------------------------
// Report Error and Halt

procedure TSimpleParser.RaiseError(s: string);
begin
   LastError := s;
   raise Exception.Create(s);
//   Halt;
end;


//--------------------------------------------------------------
// Report What Was Expected

procedure TSimpleParser.Expected(s: string);
begin
   RaiseError(s + ' Expected');
end;


//--------------------------------------------------------------
// Recognize some Character

function IsAlpha(c: char): boolean;
begin
   IsAlpha := upcase(c) in ['A'..'Z'];
end;

function IsDigit(c: char): boolean;
begin
   IsDigit := c in ['0'..'9'];
end;

function IsAlNum(c: char): boolean;
begin
   IsAlNum := IsAlpha(c) or IsDigit(c) or (c='_');
end;

function IsWhite(c: char): boolean;
begin
   IsWhite := c in [' ', #9];
end;

//--------------------------------------------------------------
// Skip Over Leading White Space

procedure TSimpleParser.SkipWhite;
begin
  while IsWhite(Look) do
    GetChar;
end;

//--------------------------------------------------------------
// Match a Specific Input Character

procedure TSimpleParser.Match(x: char);
begin
   if Look <> x then begin
     if x<>#0 then Expected('''' + x + '''')
     else Expected('End of expression');
   end else begin
      GetChar();
      SkipWhite();
   end;
end;

//--------------------------------------------------------------
// Get an Identifier

function TSimpleParser.GetName: string;
var Token: string;
begin
   Token := '';
   if not IsAlpha(Look) then Expected('Name');
   while IsAlNum(Look) do begin
      Token := Token + UpCase(Look);
      GetChar();
   end;
   Result := Token;
   SkipWhite();
end;


//--------------------------------------------------------------
// Get a Number



function TSimpleParser.GetNum: string;
var SepCount, ExpCount: integer;
    Value: string;
begin
  SepCount:=0;
  ExpCount:=0; //0 - 'E' not fount, 1 - 'E' found, 2 - '-/+' found after 'E', 3 - digit found after 'E' or '+/-'
  while Look<>#0 do begin
    if IsDigit(Look) then begin
      if ExpCount in [1,2] then ExpCount:=3;
      if SepCount=1 then SepCount:=3;
    end else if (SepCount=0) and (Look in [decimalSeparator]) then begin
      inc(SepCount);
    end else if (ExpCount=0) and (Look in ['e','E']) then begin
      ExpCount:=1;
    end else if (ExpCount=1) and (Look in['-','+']) then begin
      ExpCount:=2;
    end else begin
      break;
    end;
    Value := Value + Look;
    GetChar();
  end;
  if Value='' then RaiseError('Missing Number');
  if (not (SepCount in [0,3])) or (not (ExpCount in [0,3])) then RaiseError(Value+ ' not a valid number');
  result:=Value;
  SkipWhite();
end;




//--------------------------------------------------------------
// Output a String with Tab and CRLF

procedure TSimpleParser.EmitAction(command: char; data: string);
var v1,v2: double;
    pf: TParserFunction;
    i,num: integer;
    vs: array[0..7] of double;
begin
  case Action of
    saCompile:
      if ResultList <> nil then begin
        ResultList.AddObject(data, TObject(ord(command)));
      end;
    saEval: begin
      case command of

        'S': Push(strtofloat(data));

        'V': begin
          Push(GetVariable(data)^);
        end;

        'F': begin
          GetFunctionData(data, pf, num);
          for i:=num-1 downto 0 do begin
            vs[i]:=pop();
          end;
          Push(pf(vs));
        end;

        'O': begin
          if data='' then exit;
          v2:=Pop();
          v1:=Pop();
          case data[1] of
            '+': Push(v1+v2);
            '-': Push(v1-v2);
            '/': Push(v1/v2);
            '*': Push(v1*v2);
          end;
        end;

        'U': begin
          if data='' then exit;
          v2:=Pop();
          case data[1] of
            'N': Push(-v2);
          end;
        end;

        'R': begin
          if data='' then exit;
          v2:=Pop();
          v1:=Pop();
          case data[1] of
            '>': Push(ord(v1>v2));
            '»': Push(ord(v1>=v2));
            '<': Push(ord(v1<v2));
            '«': Push(ord(v1<=v2));
            '#': Push(ord(v1<>v2));
            '=': Push(ord(v1=v2));
          end;
        end;

        'B': begin
          if data='' then exit;
          v2:=Pop();
          v1:=Pop();
          case data[1] of
            'o': Push(ord((v1<>0) or (v2<>0)));
            'a': Push(ord((v1<>0) and (v2<>0)));
            'x': Push(ord((v1<>0) xor (v2<>0)));
          end;
        end;

      end;
    end;
  end;
end;


//function BoolExpressionList: integer; forward

//---------------------------------------------------------------
// Parse and Translate an Identifier

procedure TSimpleParser.Ident;
var Name, s: string;
    args: integer;
begin
  Name := GetName;
  if Look = '(' then begin
    Match('(');
    if look<>')' then args:=BoolExpressionList() else args:=0;
    Match(')');
    s:=Name+'_'+inttostr(args);
    if Action=saCheckSintax then
      GetFunction(s);
    EmitAction('F', s);
  end else
    EmitAction('V', Name);
end;

//procedure BoolExpression; Forward;

procedure TSimpleParser.Factor;
begin
   if Look = '(' then begin
      Match('(');
      BoolExpression;
      Match(')');
   end else if IsAlpha(Look) then
      Ident()
   else
      EmitAction('S', GetNum);
end;


procedure TSimpleParser.SignedFactor;
begin
  if Look ='-' then begin
    Match('-');
    Factor;
    EmitAction('U', 'Neg');
  end else if Look ='+' then begin
    Match('+');
    Factor;
  end else begin
    Factor;
  end;
end;

//--------------------------------------------------------------
// Recognize and Translate a Multiply

procedure TSimpleParser.Multiply;
begin
   Match('*');
   SignedFactor;
   EmitAction('O', '*');
end;


//-------------------------------------------------------------
// Recognize and Translate a Divide

procedure TSimpleParser.Divide;
begin
   Match('/');
   SignedFactor;
   EmitAction('O', '/');
end;


//---------------------------------------------------------------
// Parse and Translate a Math Term

procedure TSimpleParser.Term;
begin
   SignedFactor;
   while Look in ['*', '/'] do begin
      //EmitAction('MOVE D0,-(SP)');
      case Look of
       '*': Multiply;
       '/': Divide;
      else Expected('Mulop');
      end;
   end;
end;





//--------------------------------------------------------------
// Recognize and Translate an Add

procedure TSimpleParser.Add;
begin
   Match('+');
   Term;
   EmitAction('O', '+');
end;


//-------------------------------------------------------------
// Recognize and Translate a Subtract

procedure TSimpleParser.Subtract;
begin
   Match('-');
   Term;
   EmitAction('O', '-');
end;

//---------------------------------------------------------------
// Parse and Translate an Expression

procedure TSimpleParser.Expression;
begin
   Term;
   while Look in ['+', '-'] do begin
      case Look of
       '+': Add;
       '-': Subtract;
      //else Expected('Addop');
      end;
   end;
end;


function TSimpleParser.BoolExpressionList: integer;
begin
  BoolExpression();
  result:=1;
  while Look in [','] do begin
    Match(',');
    BoolExpression();
    inc(result);
  end;
end;


// RelOps: > < = >= <= <>
function TSimpleParser.GetRelOp: char;
var rop: char;
begin
   if not (Look in ['>','<','=']) then Expected('Relop');
   rop:=Look;
   if Look = '>' then begin
     GetChar();
     if Look = '=' then begin
       //rop:=rop +look;
       rop:='»';
       match('=');
     end;
   end else if Look = '<' then begin
     GetChar();
     if Look ='=' then begin
       rop:='«';
       match('=');
     end else if Look ='>' then begin
       rop:='#';
       match('>');
     end;
   end else if Look='=' then begin
     match('=');
     rop:='=';
   end;
   Result := rop;
   SkipWhite();
end;


procedure TSimpleParser.Relation;
var rop: char;
begin
  Expression();
//  if Look in ['>','<','='] then begin
  while Look in ['>','<','='] do begin
    rop:=GetRelOp();
    Expression();
    EmitAction('R', rop);
  end;
end;


procedure TSimpleParser.BoolExpression;
begin
  relation();
  while Look = '.' do begin
    match('.');
    if Look = 'a' then begin
      match('a');
      match('n');
      match('d');
      relation();
      EmitAction('B', 'a');
    end else if Look = 'o' then begin
      match('o');
      match('r');
      relation();
      EmitAction('B', 'o');
    end else if Look = 'x' then begin
      match('x');
      match('o');
      match('r');
      relation();
      EmitAction('B', 'x');
    end;
  end;
end;

//--------------------------------------------------------------
// Initialize

procedure TSimpleParser.Init(e: string; nResultList: TStrings);
begin
  line := e;
  lookIndex := 1;
  ResultList := nResultList;
end;

{procedure SetVarList(nVarsList: TStrings);
begin
  VarsList:=nVarsList;
end;}


procedure TSimpleParser.Compile(e: string; nResultList: TStrings);
begin
  init(e,nResultList);
  Action:=saCOmpile;
  GetChar();
  SkipWhite();
  BoolExpression();
  match(#0);
end;


function TSimpleParser.Calc(e: string): double;
begin
  init(e, nil);
  Action := saEval;
  StackIndex := 0;
  GetChar();
  SkipWhite();
  BoolExpression();
  match(#0);
  result:=Stack[0];
end;

procedure TSimpleParser.Check(e: string);
begin
  init(e,nil);
  Action:=saCheckSintax;
  GetChar();
  SkipWhite();
  BoolExpression();
  match(#0);
end;

//function SimpleRun(const rpnOps: string; rpnList, nVarsList: TStrings): double;
//function TSimpleParser.Run(const rpnOps: string; rpnList: TStrings): double;
function TSimpleParser.Run(const rpnList: TStrings): double;
var i: integer;
//    s: string;
begin
  Action := saEval;
  StackIndex := 0;
  //for i:=0 to length(rpnOps)-1 do begin
  for i := 0 to rpnList.Count - 1 do begin

    //s:=rpnOps.Strings[i];
    //if s='' then break;
    //EmitAction(s[1],copy(s,2,length(s)-1));

    //if rpnList[i]='' then break;

//    EmitAction(rpnOps[i+1],rpnList[i]);
    EmitAction(chr(integer(rpnList.objects[i])), rpnList[i]);

    //EmitAction(rpnOps[i][1],copy(rpnOps[i],2,10000));
  end;
  result := Stack[0];
end;

function TSimpleParser.Assign(e: string): double;
var disp: integer;
    sv,se: string;
    pv: pdouble;
begin
  disp := pos(':=',e);
  if disp<0 then
    raise Exception.Create(e + ' is Not an assignment');
  sv := copy(e,1,disp-1);
  se := copy(e,disp+2,length(e));
  pv := GetVariable(sv);
  pv^ := Calc(se);
  result := pv^;
end;


//------------------------------------------------------------------------------
// Parser Functions


function PaPi(const v: array of double): double;
begin
  result:=pi;
end;

function PaE(const v: array of double): double;
begin
  result:=exp(1);
end;

function PaSqrt(const v: array of double): double;
begin
  result:=sqrt(v[0]);
end;

function PaSqr(const v: array of double): double;
begin
  result:=sqr(v[0]);
end;

function PaSin(const v: array of double): double;
begin
  result:=sin(v[0]);
end;

function PaCos(const v: array of double): double;
begin
  result:=cos(v[0]);
end;

function PaTan(const v: array of double): double;
begin
  result:=tan(v[0]);
end;

function PaAtan(const v: array of double): double;
begin
  result:=arctan(v[0]);
end;

function PaArcSin(const v: array of double): double;
begin
  result:=arcsin(v[0]);
end;

function PaArcCos(const v: array of double): double;
begin
  result:=arccos(v[0]);
end;


function PaDeg(const v: array of double): double;
begin
  result:=RadTodeg(v[0]);
end;

function PaRad(const v: array of double): double;
begin
  result:=degToRad(v[0]);
end;

function PaMax(const v: array of double): double;
begin
  result:=max(v[0],v[1]);
end;

function PaMin(const v: array of double): double;
begin
  result:=min(v[0],v[1]);
end;

function PaInt(const v: array of double): double;
begin
  result:=round(v[0]);
end;

function PaAbs(const v: array of double): double;
begin
  result:=abs(v[0]);
end;

function PaIf(const v: array of double): double;
begin
  if v[0]<>0 then result:=v[1] else result:=v[2];
end;

function PaNot(const v: array of double): double;
begin
  if v[0]<>0 then result:=0 else result:=1;
end;

function PaPow(const v: array of double): double;
begin
  result:=Power(v[0], v[1]);
end;

function PaHypot(const v: array of double): double;
begin
  result:=Hypot(v[0], v[1]);
end;

function PaExp(const v: array of double): double;
begin
  result:=exp(v[0]);
end;

function PaFrac(const v: array of double): double;
begin
  result:=Frac(v[0]);
end;

function PaLn(const v: array of double): double;
begin
  result:=ln(v[0]);
end;

function PaLog10(const v: array of double): double;
begin
  result:=log10(v[0]);
end;


function PaSign(const v: array of double): double;
begin
  result:=sign(v[0]);
end;

{ TSimpleParser }

procedure TSimpleParser.CopyVarList(const SourceParser: TSimpleParser);
var i: integer;
begin
  // Copy VarList
  VarsList.Assign(SourceParser.VarsList);
  for i := 0 to VarsList.Count -1 do begin
    // Copy values
    Consts[i] := SourceParser.Consts[i];
    // Copy Reference
    VarsList.Objects[i] := TObject(@Consts[i]);
  end;
end;

constructor TSimpleParser.Create;
begin
  VarsList := TStringList.create;
  FuncsList := TStringList.create;

  SetLength(Stack, maxDoubleStack);
  SetLength(Consts, maxDoubleStack);

  RegisterFunction('pi', PaPi, 0);
  RegisterFunction('e', PaE, 0);

  RegisterFunction('sqrt', PaSqrt, 1);
  RegisterFunction('sqr', PaSqr, 1);
  RegisterFunction('pow', PaPow, 2);
  RegisterFunction('dist2d', PaHypot, 2);

  RegisterFunction('sin', PaSin, 1);
  RegisterFunction('cos', PaCos, 1);
  RegisterFunction('tan', PaTan, 1);

  RegisterFunction('atan', PaAtan, 1);
  RegisterFunction('arcsin', PaArcSin, 1);
  RegisterFunction('arccos', PaArcCos, 1);

  RegisterFunction('deg', PaDeg, 1);
  RegisterFunction('rad', PaRad, 1);

  RegisterFunction('not', PaNot, 1);
  RegisterFunction(' if', PaIf,3);

  RegisterFunction('max', PaMax, 2);
  RegisterFunction('min', PaMin, 2);

  RegisterFunction('int', PaInt, 1);
  RegisterFunction('abs', PaAbs, 1);
  RegisterFunction('sign', PaSign, 1);

  RegisterFunction('exp', PaExp, 1);
  RegisterFunction('frac', PaFrac, 1);

  RegisterFunction('ln', PaLn, 1);
  RegisterFunction('log10', PaLog10, 1);
// RoundTo

  HashFuncsColisions := BuildHashTable(FuncsList, HashFuncs);
end;

destructor TSimpleParser.Destroy;
begin
  FuncsList.Free;
  VarsList.Free;
  inherited;
end;


end.
