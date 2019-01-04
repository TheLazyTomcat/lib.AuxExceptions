unit AuxExceptions;

{$IF defined(CPU64) or defined(CPU64BITS)}
  {$DEFINE 64bit}
{$ELSEIF defined(CPU16)}
  {$DEFINE 16bit}
{$ELSE}
  {$DEFINE 32bit}
{$IFEND}

{$IF Defined(CPUX86_64) or Defined(CPUX64)}
  {$DEFINE x64}
{$ELSEIF Defined(CPU386)}
  {$DEFINE x86}
{$ELSE}
  {$DEFINE PurePascal}
{$IFEND}

{$IF Defined(WINDOWS) or Defined(MSWINDOWS)}
  {$DEFINE Windows}
{$ELSE}
  {$IF not (Defined(UNIX) or Defined(POSIX))}
    {$MESSAGE FATAL 'Unsupported operating system.'}
  {$IFEND}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}{$H+}
  {$INLINE ON}
  {$DEFINE CanInline}
  //{$ASMMODE Intel}
  //{$DEFINE FPC_DisableWarns}
  //{$MACRO ON}
{$ELSE}
  {$IF CompilerVersion >= 17 then}  // Delphi 2005+
    {$DEFINE CanInline}
  {$ELSE}
    {$UNDEF CanInline}
  {$IFEND}
{$ENDIF}

{.$DEFINE EnbaleStackTrace}

{$IF not Defined(PurePascal) and Defined(EnbaleStackTrace)}
  {$DEFINE StackTraceEnabled}  
{$ELSE}
  {$UNDEF StackTraceEnabled}
{$IFEND}

interface

uses
  {$IFDEF Windows}Windows,{$ELSE}{$MESSAGE ERROR 'implement for lin'}{$ENDIF} SysUtils,
  AuxTypes;

type
{$IFDEF Windows}
  TThreadID = DWORD;
{$ELSE}
{$MESSAGE ERROR 'implement for lin'}
{$ENDIF}

  ECustomException = class(Exception)
  protected
    fTime:      TDateTime;
    fThreadID:  TThreadID;
  public
    constructor CreateFmt(const Msg: String; Args: array of const);
    property Time: TDateTime read fTime;
    property ThreadID: TThreadID read fThreadID;
  end;

{$IFDEF StackTraceEnabled}
  ETraceException = class(ECustomException);  // later implement stack trace

  EBaseException = ETraceException;
{$ELSE}
  EBaseException = ECustomException;
{$ENDIF}

  EGeneralException = class(EBaseException)
  private
    fFaultingObject:    String;
    fFaultingFunction:  String;
    fFullMessage:       String;
  public
    constructor CreateFmt(const Msg: String; Args: array of const; FaultingObject: TObject; const FaultingFunction: String); overload;
    constructor Create(const Msg: String; FaultingObject: TObject; const FaultingFunction: String); overload;
    property FaultingObject: String read fFaultingObject;
    property FaultingFunction: String read fFaultingFunction;
    property FullMessage: String read fFullMessage;
  end;

  ESystemError = class(EGeneralException)
  private
    fErrorCode: UInt32;
  public
    constructor Create(FullSysMsg: Boolean; FaultingObject: TObject; const FaultingFunction: String); overload;
    property ErrorCode: UInt32 read fErrorCode;
  end;

  EIndexException = class(EGeneralException)
  protected
    fIndex: Integer;
    class Function GetDefaultMessage: String; virtual;
  public
    constructor Create(const Msg: String; Index: Integer; FaultingObject: TObject; const FaultingFunction: String); overload;
    constructor Create(Index: Integer; FaultingObject: TObject; const FaultingFunction: String); overload;
    property Index: Integer read fIndex;
  end;

  EIndexOutOfBounds = class(EIndexException)
  protected
    class Function GetDefaultMessage: String; override;
  end;

  EIndexTooLow = class(EIndexException)
  protected
    class Function GetDefaultMessage: String; override;
  end;

  EIndexTooHigh = class(EIndexException)
  protected
    class Function GetDefaultMessage: String; override;
  end;

  EIndexInvalid = class(EIndexException)
  protected
    class Function GetDefaultMessage: String; override;
  end;

  EValueException = class(EGeneralException)
  protected
    fValueName: String;
    fValue:     Variant;
    class Function VariantArrayToStr(Value: Variant): String; virtual;
    class Function GetDefaultMessage(ValueString: Boolean): String; virtual;
  public
    constructor Create(const Msg,ValueName: String; Value: Variant; FaultingObject: TObject; const FaultingFunction: String); overload;
    constructor Create(const Msg,ValueName: String; FaultingObject: TObject; const FaultingFunction: String); overload;  
    constructor Create(const ValueName: String; Value: Variant; FaultingObject: TObject; const FaultingFunction: String); overload;
    constructor Create(const ValueName: String; FaultingObject: TObject; const FaultingFunction: String); overload;
    property ValueName: String read FValueName;
    property Value: Variant read fValue;
  end;

  EValueInvalidException = class(EValueException)
  protected
    class Function GetDefaultMessage(ValueString: Boolean): String; override;
  end;

  EValueInvalidNameOnlyException = class(EValueException)
  protected
    class Function GetDefaultMessage(ValueString: Boolean): String; override;
  end;

implementation

uses
  Variants;

constructor ECustomException.CreateFmt(const Msg: String; Args: array of const);
begin
inherited CreateFmt(Msg,Args);
fTime := Now;
{$IFDEF Windows}
fThreadID := GetCurrentThreadID;
{$ELSE}
{$MESSAGE ERROR 'implement for lin'}
{$ENDIF}
end;

//==============================================================================

constructor EGeneralException.CreateFmt(const Msg: String; Args: array of const; FaultingObject: TObject; const FaultingFunction: String);

  Function InstanceString(Obj: TObject): String;
  begin
    Result := Format('%s(%p)',[Obj.ClassName,Pointer(Obj)]);
  end;
  
begin
inherited CreateFmt(Msg,Args);
If Assigned(FaultingObject) then
  fFaultingObject := InstanceString(FaultingObject)
else
  fFaultingObject := '';
fFaultingFunction := FaultingFunction;
If Length(fFaultingObject) > 0 then
  begin
    If Length(fFaultingFunction) > 0 then
      fFullMessage := Format(Format('%s.%s: %s',[fFaultingObject,fFaultingFunction,Msg]),Args)
    else
      fFullMessage := Format(Format('%s: %s',[fFaultingObject,Msg]),Args);
  end
else
  begin
    If Length(FaultingFunction) > 0 then
      fFullMessage := Format(Format('%s: %s',[fFaultingFunction,Msg]),Args)
    else
      fFullMessage := Format(Msg,Args);
  end;
end;

//------------------------------------------------------------------------------

constructor EGeneralException.Create(const Msg: String; FaultingObject: TObject; const FaultingFunction: String);
begin
CreateFmt(Msg,[],FaultingObject,FaultingFunction);
end;

//==============================================================================

constructor ESystemError.Create(FullSysMsg: Boolean; FaultingObject: TObject; const FaultingFunction: String);
var
  ErrCode:  UInt32;
begin
ErrCode := GetLastError;
If FullSysMsg then
  inherited CreateFmt('System error 0x%.8x: %s',[ErrCode,SysErrorMessage(ErrCode)],FaultingObject,FaultingFunction)
else
  inherited CreateFmt('System error occured (0x%.8x).',[ErrCode],FaultingObject,FaultingFunction);
fErrorCode := ErrCode;
end;

//==============================================================================

class Function EIndexException.GetDefaultMessage: String;
begin
Result := 'Index (%d) error.';
end;

//------------------------------------------------------------------------------

constructor EIndexException.Create(const Msg: String; Index: Integer; FaultingObject: TObject; const FaultingFunction: String);
begin
inherited CreateFmt(Msg,[Index],FaultingObject,FaultingFunction);
fIndex := Index;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor EIndexException.Create(Index: Integer; FaultingObject: TObject; const FaultingFunction: String);
begin
Create(GetDefaultMessage,Index,FaultingObject,FaultingFunction);
end;

//==============================================================================

class Function EIndexOutOfBounds.GetDefaultMessage: String;
begin
Result := 'Index (%d) out of bounds.';
end;

//==============================================================================

class Function EIndexTooLow.GetDefaultMessage: String;
begin
Result := 'Index (%d) too low.';
end;

//==============================================================================

class Function EIndexTooHigh.GetDefaultMessage: String;
begin
Result := 'Index (%d) too high.';
end;

//==============================================================================

class Function EIndexInvalid.GetDefaultMessage: String;
begin
Result := 'Index (%d) is invalid.';
end;

//==============================================================================

class Function EValueException.VariantArrayToStr(Value: Variant): String;
var
  Dimensions: Integer;
  Indices:    array of Integer;

  procedure ConvertVarArrayDimension(var Str: String; Dim: Integer);
  var
    Index:  Integer;
  begin
    Str := Str + '[';
    For Index := VarArrayLowBound(Value,Dim) to VarArrayHighBound(Value,Dim) do
      begin
        Indices[Pred(Dim)] := Index;
        If Dim >= Dimensions then
          begin
            If Index <> VarArrayHighBound(Value,Dim) then
              Str := Str + VarToStrDef(VarArrayGet(Value,Indices),'ERROR') + ','
            else
              Str := Str + VarToStrDef(VarArrayGet(Value,Indices),'ERROR');
          end
        else ConvertVarArrayDimension(Str,Dim + 1);
      end;
    Str := Str + ']';
  end;

begin
Result := '';
Dimensions := VarArrayDimCount(Value);
If Dimensions > 0 then
  begin
    SetLength(Indices,Dimensions);
    ConvertVarArrayDimension(Result,1);
  end;
end;

//------------------------------------------------------------------------------

class Function EValueException.GetDefaultMessage(ValueString: Boolean): String;
begin
If ValueString then
  Result := 'Value %s error (%s).'
else
  Result := 'Value %s error.';
end;

//------------------------------------------------------------------------------

constructor EValueException.Create(const Msg,ValueName: String; Value: Variant; FaultingObject: TObject; const FaultingFunction: String);
begin
If (VarType(Value) and varArray) <> 0 then
  inherited CreateFmt(Msg,[ValueName,VariantArrayToStr(Value)],FaultingObject,FaultingFunction)
else
  inherited CreateFmt(Msg,[ValueName,VarToStrDef(Value,'ERROR')],FaultingObject,FaultingFunction);
fValue := Value;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor EValueException.Create(const Msg,ValueName: String; FaultingObject: TObject; const FaultingFunction: String);
begin
inherited CreateFmt(Msg,[ValueName],FaultingObject,FaultingFunction);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor EValueException.Create(const ValueName: String; Value: Variant; FaultingObject: TObject; const FaultingFunction: String);
begin
Create(GetDefaultMessage(True),ValueName,Value,FaultingObject,FaultingFunction);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor EValueException.Create(const ValueName: String; FaultingObject: TObject; const FaultingFunction: String);
begin
Create(GetDefaultMessage(False),ValueName,FaultingObject,FaultingFunction);
end;

//==============================================================================

class Function EValueInvalidException.GetDefaultMessage(ValueString: Boolean): String;
begin
If ValueString then
  Result := 'Invalid %s value (%s).'
else
  Result := 'Invalid %s value.';
end;

//==============================================================================

class Function EValueInvalidNameOnlyException.GetDefaultMessage(ValueString: Boolean): String;
begin
If ValueString then
  Result := 'Invalid %s (%s).'
else
  Result := 'Invalid %s.';
end;


end.
