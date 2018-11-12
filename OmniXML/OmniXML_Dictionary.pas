{ $OmniXML: OmniXML/OmniXML_Dictionary.pas,v 1.5 2008/09/12 11:25:17 mremec Exp $ }

(*******************************************************************************
* The contents of this file are subject to the Mozilla Public License Version
* 1.1 (the "License"); you may not use this file except in compliance with the
* License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
*
* Software distributed under the License is distributed on an "AS IS" basis,
* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
* the specific language governing rights and limitations under the License.
*                                                                              
* The Original Code is OmniXML_Dictionary.pas
*
* The Initial Developer of the Original Code is Miha Remec
*   http://omnixml.com/
*******************************************************************************)
unit OmniXML_Dictionary;

interface

{$I OmniXML.inc}

{$IFDEF OmniXML_DXE4_UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

uses
  Classes, SysUtils, {$IFDEF FPC} OmniXML_IniHash,{$ELSE} IniFiles,{$ENDIF}
  OmniXML_Types;

const
  CInvalidDicId = High(LongWord);

type
  TDicId = LongWord;
  TDictionary = class
  private
    FHashTable: TStringHash;
    FTextList: TStringList;
    FMaxItemsBeforeResize: Integer;
    procedure Resize;
  protected
    function StorageToStr(const aStorageString: XmlFastString): XmlString; virtual;
    function StrToStorage(const aString: XmlString): XmlFastString; virtual;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function Add(const Text: XmlString): TDicId; overload;
    function Add(const Text: XmlString; out aNewEntry: Boolean): TDicId; overload;
    function Get(const Id: TDicId): XmlString;
    procedure SetObject(const Id: TDicId; const aObject: TObject);
    function GetObject(const Id: TDicId): TObject;
    procedure SetPObject(const Id: TDicId; const aObject: Pointer);
    function GetPObject(const Id: TDicId): Pointer;
    function Count: Integer;
    procedure Clear;
  end;

implementation

const
  // customized version from http://planetmath.org/encyclopedia/GoodHashTablePrimes.html
  CGoodHashTablePrimes: array [6..30] of Cardinal = (97, 193, 389, 769, 1543, 3079, 6151,
    12289, 24593, 49157, 98317, 196613, 393241, 786433, 1572869, 3145739, 6291469,
    12582917, 25165843, 50331653, 100663319, 201326611, 402653189, 805306457, 1610612741);

function GetGoodHashSize(const Size: Cardinal): Cardinal;
var
  UpToSize: Cardinal;
  TableIndex: Integer;
begin
  TableIndex := Low(CGoodHashTablePrimes);
  UpToSize := 1 shl TableIndex;
  while Size > UpToSize do
  begin
    Inc(TableIndex);
    UpToSize := UpToSize shl 1;
  end;
  Result := CGoodHashTablePrimes[TableIndex];
end;

{ TDictionary }

function TDictionary.Add(const Text: XmlString;
  out aNewEntry: Boolean): TDicId;
var
  Value: Integer;
  xStorageText: XmlFastString;
begin
  xStorageText := StrToStorage(Text);
  Value := FHashTable.ValueOf(xStorageText);

  if Value >= 0 then begin
    aNewEntry := False;
    Result := Value;
  end else begin
    aNewEntry := True;
    Value := FTextList.Add(xStorageText);

    if FTextList.Count < FMaxItemsBeforeResize then
      FHashTable.Add(xStorageText, Value)
    else
      Resize;

    Result := Value;
  end;
end;

procedure TDictionary.Clear;
begin
  FHashTable.Clear;
  FTextList.Clear;

  Resize;
end;

function TDictionary.Count: Integer;
begin
  Result := FTextList.Count;
end;

constructor TDictionary.Create;
begin
  inherited;
  
  FTextList := TStringList.Create;
  Resize;
end;

destructor TDictionary.Destroy;
begin
  FreeAndNil(FHashTable);
  FreeAndNil(FTextList);

  inherited;
end;

function TDictionary.Add(const Text: XmlString): TDicId;
var
  x: Boolean;
begin
  Result := Add(Text, x);
end;

function TDictionary.Get(const Id: TDicId): XmlString;
begin
  Result := StorageToStr(FTextList[Id]);
end;

function TDictionary.GetObject(const Id: TDicId): TObject;
begin
  Result := FTextList.Objects[Id];
end;

function TDictionary.GetPObject(const Id: TDicId): Pointer;
begin
  Result := Pointer(FTextList.Objects[Id]);//unsave but fine
end;

procedure TDictionary.Resize;
var
  ItemIndex: Integer;
  HashSize: Cardinal;
begin
  FHashTable.Free;

  HashSize := GetGoodHashSize(FTextList.Count);

  FHashTable := TStringHash.Create(HashSize);
  FMaxItemsBeforeResize := Trunc((HashSize / 3) * 2);

  // re-add items to hash
  for ItemIndex := 0 to FTextList.Count - 1 do
    FHashTable.Add(FTextList[ItemIndex], ItemIndex);
end;

procedure TDictionary.SetObject(const Id: TDicId; const aObject: TObject);
begin
  FTextList.Objects[Id] := aObject;
end;

procedure TDictionary.SetPObject(const Id: TDicId; const aObject: Pointer);
begin
  FTextList.Objects[Id] := TObject(aObject);//unsave but fine!
end;

function TDictionary.StorageToStr(
  const aStorageString: XmlFastString): XmlString;
begin
  {$IFDEF OmniXML_Unicode}
  Result := aStorageString;
  {$ELSE}
  Result := UTF8Decode(aStorageString);
  {$ENDIF}
end;

function TDictionary.StrToStorage(const aString: XmlString): XmlFastString;
begin
  {$IFDEF OmniXML_Unicode}
  Result := aString;
  {$ELSE}
  Result := UTF8Encode(aString);
  {$ENDIF}
end;

end.

