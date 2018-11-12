{ $OmniXML: OmniXML/OmniXMLUtils.pas,v 1.13 2010/07/21 07:45:38 mremec Exp $ }

(*:XML helper unit. Contains routines to convert data, retrieve/set data of
   different types, manipulate nodes, load/save XML documents.
   @author Primoz Gabrijelcic
   @desc <pre>
   (c) 2008 Primoz Gabrijelcic
   Free for personal and commercial use. No rights reserved.

   Author            : Primoz Gabrijelcic
   Creation date     : 2001-10-25
   Last modification : 2010-07-06
   Version           : 1.29
</pre>*)(*
   History:
     1.29: 2010-07-06
       - Overloaded SelectNode.
     1.28: 2010-07-05
       - GetNodeText works when nil is passed as the 'parentNode' parameter. 
     1.27: 2010-05-27
       - (ia) Added handling of boolean strings 'true' and 'false'
     1.26: 2009-12-25
       - (mr) Base64 code optimization.
       - (mr) Overloaded Base64Encode, Base64Decode to work with buffer.
     1.25: 2008-04-09
       - Implemented enumerator XMLEnumNodes. Now you can do:
           for nodePassword in XMLEnumNodes(xmlConfig, '//*/PasswordHash') do
             SetTextChild(nodePassword, '(removed)');
     1.24: 2007-01-09
       - Added two additional overloads for Base64Encode and one for Base64Decode.
     1.23b: 2004-07-29
       - Updated GetNodeText to handle a case when #text subnode doesn't exist.
     1.23a: 2004-04-21
       - Updated GetNodeText, GetNodeCData to use .NodeValue instead of .Text internally.
     1.23: 2004-04-07
       - Added functions GetNodeTextFont and SetNodeTextFont.
     1.22a: 2004-04-07
       - Modified XMLBinaryToStr to always process entire stream.
       - Modified XMLStrToBinary to clear output stream at beginning.
     1.22: 2004-04-05
       - Added overloaded versions of GetNodeText and GetNodeCData.
     1.21: 2004-03-27
       - Added function AppendNode.
     1.20a: 2004-03-25
       - Fixed broken format strings (used for error reporting) in various XMLStrTo*
         functions.
     1.20: 2004-03-23
       - Added two more variants of Base64Encode and Base64Decode.
     1.19: 2004-03-01
       - GetNodeText*, GetNodeAttr*, and XMLStrTo* families extended with overloaded
         versions without a default value, raising exception on invalid/missing XML node.
     1.18: 2004-01-16
       - Functions OwnerDocument and DocumentElement made public.
     1.17: 2004-01-05
       - Remove some unnecessary 'overload' directives.
       - Added functions XMLStrToCurrency, XMLStrToCurrencyDef, XMLVariantToStr,
         and XMLCurrencyToStr.
       - Added function FindProcessingInstruction.
       - Added functions XMLSaveToAnsiString, XMLLoadFromAnsiString.
       - Fixed XMLSaveToString which incorrectly returned UTF8 string instead of
         UTF16.
     1.16: 2003-12-12
       - GetTextChild and SetTextChild made public.
       - New functions GetCDataChild and SetCDataChild.
       - New functions GetNodeCData and SetNodeCData.
       - New functions MoveNode and RenameNode.
       - Added functions XMLStrToExtended, XMLStrToExtendedDef, and
         XMLExtendedToStr.
     1.15b: 2003-10-01
       - Fixed another bug in SelectNode and EnsureNode (broken since 1.15).
     1.15a: 2003-09-22
       - Fixed bug in SelectNode and EnsureNode (broken since 1.15).
     1.15: 2003-09-21
       - Added function SelectNode.
     1.14: 2003-05-08
       - Overloaded Base64Encode, Base64Decode to work with strings too.
     1.13: 2003-04-01
       - Filter* and Find* routines modified to skip all non-ELEMENT_NODE nodes.
     1.12b: 2003-01-15
       - Safer implementation of some internal functions.
     1.12a: 2003-01-13
       - Adapted for latest fixes in OmniXML 2002-01-13.
     1.12: 2003-01-13
       - CopyNode, and CloneDocument made MS XML compatible.
       - Automatic DocumentElement dereferencing now works with MS XML.
     1.11: 2003-01-13
       - Fixed buggy GetNode(s)Text*/SetNode(s)Text* functions.
       - Fixed buggy CopyNode and CloneDocument.
     1.10a: 2003-01-09
       - Fixed filterProc support in the CopyNode.
     1.10: 2003-01-07
       - Added functions XMLLoadFromRegistry and XMLSaveToRegistry.
       - Added function CloneDocument.
       - Added parameter filterProc to the CopyNode procedure.
       - Smarter GetNodeAttr (automatically dereferences DocumentElement if
         root xml node is passed to it).
     1.09: 2002-12-26
       - Added procedure CopyNode that copies contents of one node into another.
       - Modified DeleteAllChildren to preserve Text property.
     1.08: 2002-12-21
       - Smarter GetNodeText (automatically dereferences DocumentElement if
         root xml node is passed to it).
     1.07a: 2002-12-10
       - Bug fixed in XMLSaveToString (broken since 1.06).
     1.07: 2002-12-09
       - Added XMLLoadFromFile and XMLSaveToFile.
       - Small code cleanup.
     1.06: 2002-12-09
       - MSXML compatible (define USE_MSXML).
     1.05a: 2002-11-23
       - Fixed bug in Base64Decode.
     1.05: 2002-11-05
       - Added function ConstructXMLDocument.
     1.04: 2002-10-03
       - Added function EnsureNode.
     1.03: 2002-09-24
       - Added procedure SetNodesText.
     1.02: 2002-09-23
       - SetNode* familiy of procedures changed into functions returning the
         modified node.
     1.01: 2001-11-07
       - (mr) Added function XMLDateTimeToStrEx.
       - (mr) ISODateTime2DateTime enhanced.
       - (mr) Bug fixed in Str2Time.
     1.0: 2001-10-25
       - Created by extracting common utilities from unit GpXML.
*)

unit OmniXMLUtils.Vcl;

interface

{$I OmniXML.inc}

{$IFDEF OmniXML_DXE4_UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF (CompilerVersion >= 17)} //Delphi 2005 or newer
    {$DEFINE OmniXmlUtils_Enumerators}
  {$IFEND}
  {$IF CompilerVersion >= 20.0}  // Delphi 2009 or newer
    {$DEFINE OmniXmlUtils_Base64UsePointerMath}
  {$IFEND}
{$ENDIF}

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils,
  Classes,
  Graphics,
  OmniXML_Types,
  OmniXML
{$IFDEF USE_MSXML}
  ,OmniXML_MSXML
{$ENDIF USE_MSXML}
{$IFDEF HAS_UNIT_VARIANTS}
  ,Variants
{$ENDIF DELPHI6_UP}
  ;

implementation

uses
  OmniXMLUtils;

function GetNodeTextFont(parentNode: IXMLNode; nodeTag: string; value: TFont): boolean;
var
  fontNode: IXMLNode;
  fStyle  : TFontStyles;
  iStyle  : integer;
begin
  Result := false;
  fontNode := SelectNode(parentNode, nodeTag);
  if assigned(fontNode) then begin
    value.Name := GetNodeTextStr(fontNode, 'Name', value.Name);
    value.Charset := GetNodeAttrInt(fontNode, 'Charset', value.Charset);
    value.Color := GetNodeAttrInt(fontNode, 'Color', value.Color);
    value.Height := GetNodeAttrInt(fontNode, 'Height', value.Height);
    value.Pitch := TFontPitch(GetNodeAttrInt(fontNode, 'Pitch', Ord(value.Pitch)));
    value.Size := GetNodeAttrInt(fontNode, 'Size', value.Size);
    fStyle := value.Style;
    iStyle := 0;
    Move(fStyle, iStyle, SizeOf(TFontStyles));
    iStyle := GetNodeAttrInt(fontNode, 'Style', iStyle);
    Move(iStyle, fStyle, SizeOf(TFontStyles));
    value.Style := fStyle;
    Result := true;
  end;
end; { GetNodeTextFont }

function SetNodeTextFont(parentNode: IXMLNode; nodeTag: string;
  value: TFont): IXMLNode;
var
  fontNode  : IXMLNode;
  fStyle    : TFontStyles;
  iStyle    : integer;
begin
  fontNode := EnsureNode(parentNode, nodeTag);
  SetNodeTextStr(fontNode, 'Name', value.Name);
  SetNodeAttrInt(fontNode, 'Charset', value.Charset);
  SetNodeAttrInt(fontNode, 'Color', value.Color);
  SetNodeAttrInt(fontNode, 'Height', value.Height);
  SetNodeAttrInt(fontNode, 'Pitch', Ord(value.Pitch));
  SetNodeAttrInt(fontNode, 'Size', value.Size);
  fStyle := value.Style;
  iStyle := 0;
  Move(fStyle, iStyle, SizeOf(TFontStyles));
  SetNodeAttrInt(fontNode, 'Style', iStyle);
end; { SetNodeTextFont }

end.
