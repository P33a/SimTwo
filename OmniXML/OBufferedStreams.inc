
{$IFNDEF FPC}
  {$IF (CompilerVersion >= 25.0)}
    {$LEGACYIFEND ON}
  {$IFEND}

  {$IF CompilerVersion >= 18.5}
    {$DEFINE O_DELPHI_2007}
  {$IFEND}
  {$IF CompilerVersion >= 20}
    {$DEFINE O_DELPHI_2009}
  {$ELSE}
    {$DEFINE O_DELPHI_PRE2009}
  {$IFEND}
  {$IF CompilerVersion >= 22}
    {$DEFINE O_DELPHI_XE}
  {$IFEND}
  {$IF CompilerVersion >= 23}
    {$DEFINE O_DELPHI_XE2}
  {$IFEND}
  {$IF CompilerVersion >= 24}
    {$DEFINE O_DELPHI_XE3}
  {$IFEND}
  {$IF CompilerVersion >= 25}
    {$DEFINE O_DELPHI_XE4}
  {$IFEND}

  {$IFNDEF O_DELPHI_2007}//FALLBACK TO Delphi7; + handle Delphi2005/2006 as Delphi7
    {$DEFINE O_DELPHI_7}

    {$IF CompilerVersion < 15}
      {$DEFINE O_DELPHI_6}
    {$IFEND}
  {$ENDIF}

  {$IFDEF UNICODE}
    {$DEFINE O_UNICODE}
  {$ENDIF}
{$ENDIF}

{$IFDEF FPC}
  {$DEFINE O_UNICODE}
{$ENDIF}