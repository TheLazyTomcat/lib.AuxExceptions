//--- platform -----------------------------------------------------------------

{$IF Defined(CPU64) or Defined(CPU64BITS)}
  {$DEFINE 64bit}
{$ELSEIF Defined(CPU16)}
  {$DEFINE 16bit}
{$ELSE}
  {$DEFINE 32bit}
{$IFEND}

//--- processor ----------------------------------------------------------------

{$IF Defined(CPUX86_64) or Defined(CPUX64)}
  {$DEFINE x64}
{$ELSEIF Defined(CPU386)}
  {$DEFINE x86}
{$ELSE}
  {$DEFINE PurePascal}
{$IFEND}

//--- operating system ---------------------------------------------------------

{$IF Defined(WINDOWS) or Defined(MSWINDOWS)}
  {$DEFINE Windows}
{$ELSEIF Defined(LINUX)}
  {$DEFINE Linux}
{$ELSE}
  {$MESSAGE FATAL 'Unsupported operating system.'}
{$IFEND}

//--- compiler -----------------------------------------------------------------

{$IFDEF FPC}
  {$MODE ObjFPC}{$H+}
  {$IFNDEF PurePascal}
    {$ASMMODE Intel}
  {$ENDIF}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ENDIF}

//--- user-definable -----------------------------------------------------------

{
  ExtendedException

  When defined, EGeneralException and all its subclasses provide extended
  error information (stack trace, CPU registers snapshot, ...).

  Enabled by default.
}
{$DEFINE EnableExtendedException}
