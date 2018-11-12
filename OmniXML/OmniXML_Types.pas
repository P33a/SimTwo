{ $OmniXML: OmniXML/OmniXML_Types.pas,v 1.1 2008/08/31 15:28:15 mremec Exp $ }
(*******************************************************************************
* The contents of this file are subject to the Mozilla Public License Version
* 1.1 (the "License"); you may not use this file except in compliance with the
* License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
*
* Software distributed under the License is distributed on an "AS IS" basis,
* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
* the specific language governing rights and limitations under the License.
*
* The Original Code is OmniXML_Types.pas
*
* The Initial Developer of the Original Code is Miha Remec
*   http://omnixml.com/
*******************************************************************************)
unit OmniXML_Types;

interface

{$I OmniXML.inc}

uses SysUtils;

type
  {$IFDEF OmniXML_Unicode}
    XmlString = string;
    PXmlString = ^string;
    XmlChar = Char;
    PXmlChar = PChar;
    XmlFastString = string;
    {$IF DEFINED(FPC)}
    XmlRawByteString = AnsiString;
    {$ELSEIF NOT DEFINED(NEXTGEN)}
    XmlRawByteString = RawByteString;
    {$IFEND}
  {$ELSE}
    XmlString = WideString;
    PXmlString = PWideString;
    XmlChar = WideChar;
    PXmlChar = PWideChar;
    XmlRawByteString = AnsiString;
    XmlFastString = UTF8String;
  {$ENDIF}  // OmniXML_Unicode

  {$IFDEF OmniXML_D2009_UP}
    //DELPHI 2009+
    XmlWideString = string;
    XmlWideChar = Char;
    PXmlWideChar = PChar;
  {$ELSE}
    //FPC, DELPHI 7
    XmlWideString = WideString;
    XmlWideChar = WideChar;
    PXmlWideChar = PWideChar;
  {$ENDIF}

implementation

end.
