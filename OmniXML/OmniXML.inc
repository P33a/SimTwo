{ $OmniXML: OmniXML/OmniXML.inc,v 1.4 2008/09/12 11:25:17 mremec Exp $ }

{$DEFINE OmniXML_InterfaceListOptimalization}

{$IFDEF CONDITIONALEXPRESSIONS}

  {$IF (CompilerVersion >= 25.0)}//Delphi XE4 and newer
    {$LEGACYIFEND ON}
    {$IFDEF NEXTGEN}//Delphi XE4 NEXTGEN/iOS compiler
      {$UNDEF OmniXML_InterfaceListOptimalization}//TInterfaceList is fast enough in XE4
    {$ENDIF}
  {$IFEND}

  {$IF Declared(CompilerVersion) and (CompilerVersion >= 14.0)}  // Delphi 6 or newer
    {$DEFINE HAS_UNIT_VARIANTS}
    {$DEFINE DELPHI6_UP}
  {$IFEND}  // Declared(CompilerVersion) and (CompilerVersion >= 14.0)

  {$IF Declared(CompilerVersion) and (CompilerVersion < 15.0)}  // Delphi 6 or older
    {$DEFINE DELPHI6_DOWN}
  {$IFEND}

  {$IFDEF UNICODE}  // Delphi with Unicode support
    {$DEFINE OmniXML_Unicode}
  {$ENDIF}  // UNICODE

  {$IF (CompilerVersion >= 24.0)}
    {$DEFINE OmniXML_DXE3_UP}
  {$IFEND}

  {$IF (CompilerVersion >= 25.0)}
    {$DEFINE OmniXML_DXE4_UP}
  {$IFEND}

  {$IF (CompilerVersion >= 20.0)}
    {$DEFINE OmniXML_D2009_UP}
  {$IFEND}

  {$IF DEFINED(NEXTGEN)}
    {$DEFINE OmniXML_NEXTGEN}
  {$IFEND}

{$ENDIF}  // CONDITIONALEXPRESSIONS

{$IFDEF LINUX}
  {$DEFINE OmniXML_WantWideString}
{$ENDIF}  // LINUX

{$IFDEF OmniXML_WantWideString}
  {$UNDEF OmniXML_Unicode}
{$ENDIF}  // OmniXML_WantWideString

{$IFDEF FPC}
  {$DEFINE OmniXML_Unicode}
  {$DEFINE UNICODE}

  //{$DEFINE OmniXML_InterfaceListOptimalization}
{$ENDIF}

{$IF NOT DEFINED(FPC) AND DEFINED(MACOS)}
  {$DEFINE OmniXml_XE2_OSX}
{$IFEND}

{$I OmniXML_JEDI.inc}

//USE GpTextStream? -> SEE LICENSE TEXT!!!
{$IFDEF MSWINDOWS}
  {.$DEFINE OmniXML_UseGP}
{$ENDIF}
