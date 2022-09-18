{$IFDEF AE_Include_Defs}

{$IF Defined(WINDOWS) or Defined(MSWINDOWS)}
  {$DEFINE Windows}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
{$ENDIF}
{$H+}

{$ENDIF AE_Include_Defs}

{===============================================================================
********************************************************************************
===============================================================================}

{$IFDEF AE_Include_Interface_Uses}
uses
  SysUtils {$IFDEF Windows}, Windows{$ENDIF};

{$ENDIF AE_Include_Interface_Uses}

{===============================================================================
********************************************************************************
===============================================================================}

{$IFDEF AE_Include_Interface}
{===============================================================================
    Common types
===============================================================================}
type
  TAEThreadID = {$IFDEF Windows}DWORD{$ELSE}pthread_t{$ENDIF};
  TAESysErrCode = {$IFDEF Windows}DWORD{$ELSE}cInt{$ENDIF};

{===============================================================================
--------------------------------------------------------------------------------
                               EAECustomException
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAECustomException - class declaration
===============================================================================}
type
  EAECustomException = class(EAEBaseException)
  protected
    fTime:      TDateTime;
    fThreadID:  TAEThreadID;
  public
    constructor CreateFmt(const Msg: String; Args: array of const);
    property Time: TDateTime read fTime;
    property ThreadID: TAEThreadID read fThreadID;
  end;


{===============================================================================
--------------------------------------------------------------------------------
                               EAEExtendedException
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEExtendedException - class declaration
===============================================================================}
type
  EAEExtendedException = class(EAECustomException)
  protected
    //fRegisters: TRegisters;
    //procedure GetFloatRegisters; virtual;
    //procedure GetFloatVectorRegisters_SSE; virtual;
    //procedure GetFloatVectorRegisters_AVX; virtual;
    //procedure GetFloatVectorRegisters_AVX512; virtual;
  public
    constructor CreateFmt(const Msg: String; Args: array of const);
    //property Registers: TRegisters read fRegisters;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                               EAEGeneralException
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEGeneralException - class declaration
===============================================================================}
type
{$IFDEF ExtendedException}
  EAEGeneralException = class(EAEExtendedException)
{$ELSE ExtendedException}
  EAEGeneralException = class(EAECustomException)
{$ENDIF ExtendedException}
  protected
    fFaultingObject:    String;
    fFaultingFunction:  String;
    fFullMessage:       String;
  public
    constructor CreateFmt(const Msg: String; Args: array of const; FaultObject: TObject; const FaultFunction: String); overload;
    constructor Create(const Msg: String; FaultObject: TObject; const FaultFunction: String); overload;
    property FaultingObject: String read fFaultingObject;
    property FaultingFunction: String read fFaultingFunction;
    property FullMessage: String read fFullMessage;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 EAESystemError
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAESystemError - class declaration
===============================================================================}
type
  EAESystemError = class(EAEGeneralException)
  protected
    fErrorCode: TAESysErrCode;
  public
    constructor Create(ErrorCode: TAESysErrCode; FullSysMsg: Boolean; FaultObject: TObject; const FaultFunction: String); overload;
    constructor Create(FullSysMsg: Boolean; FaultObject: TObject; const FaultFunction: String); overload;
    property ErrorCode: TAESysErrCode read fErrorCode;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                EAEIndexException
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEIndexException - class declaration
===============================================================================}
type
  EAEIndexException = class(EAEGeneralException)
  protected
    fIndex: Integer;
    class Function GetDefaultMessage: String; virtual;
  public
    constructor Create(const Msg: String; Index: Integer; FaultObject: TObject; const FaultFunction: String); overload;
    constructor Create(Index: Integer; FaultObject: TObject; const FaultFunction: String); overload;
    property Index: Integer read fIndex;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                               EAEIndexOutOfBounds
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEIndexOutOfBounds - class declaration
===============================================================================}
type
  EAEIndexOutOfBounds = class(EAEIndexException)
  protected
    class Function GetDefaultMessage: String; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 EAEIndexTooLow
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEIndexTooLow - class declaration
===============================================================================}
type
  EAEIndexTooLow = class(EAEIndexException)
  protected
    class Function GetDefaultMessage: String; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 EAEIndexTooHigh
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEIndexTooHigh - class declaration
===============================================================================}
type
  EAEIndexTooHigh = class(EAEIndexException)
  protected
    class Function GetDefaultMessage: String; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 EAEIndexInvalid
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEIndexInvalid - class declaration
===============================================================================}
type
  EAEIndexInvalid = class(EAEIndexException)
  protected
    class Function GetDefaultMessage: String; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                EAEValueException
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEValueException - class declaration
===============================================================================}
type
  EAEValueException = class(EAEGeneralException)
  protected
    fValueName: String;
    fValue:     Variant;
    class Function VariantArrayToStr(Value: Variant): String; virtual;
    class Function GetDefaultMessage(ValueString: Boolean): String; virtual;
  public
    constructor Create(const Msg,ValueName: String; Value: Variant; FaultObject: TObject; const FaultFunction: String); overload;
    constructor Create(const Msg,ValueName: String; FaultObject: TObject; const FaultFunction: String); overload;
    constructor Create(const ValueName: String; Value: Variant; FaultObject: TObject; const FaultFunction: String); overload;
    constructor Create(const ValueName: String; FaultObject: TObject; const FaultFunction: String); overload;
    property ValueName: String read FValueName;
    property Value: Variant read fValue;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 EAEValueInvalid
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEValueInvalid - class declaration
===============================================================================}
type
  EAEValueInvalid = class(EAEValueException)
  protected
    class Function GetDefaultMessage(ValueString: Boolean): String; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                             EAEValueInvalidNameOnly                             
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEValueInvalidNameOnly - class declaration
===============================================================================}
type
  EAEValueInvalidNameOnly = class(EAEValueException)
  protected
    class Function GetDefaultMessage(ValueString: Boolean): String; override;
  end;

{$ENDIF AE_Include_Interface}

{===============================================================================
********************************************************************************
===============================================================================}

{$IFDEF AE_Include_Implementation_Uses}
uses
  Variants;

{$ENDIF AE_Include_Implementation_Uses}

{===============================================================================
********************************************************************************
===============================================================================}

{$IFDEF AE_Include_Implementation}
{===============================================================================
--------------------------------------------------------------------------------
                               EAECustomException
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAECustomException - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    EAECustomException - public methods
-------------------------------------------------------------------------------}

constructor EAECustomException.CreateFmt(const Msg: String; Args: array of const);
begin
inherited CreateFmt(Msg,Args);
fTime := Now;
{$IFDEF Windows}
fThreadID := Windows.GetCurrentThreadID;
{$ELSE}
fThreadID := pthreads.pthread_self;
{$ENDIF}
end;


{===============================================================================
--------------------------------------------------------------------------------
                               EAEExtendedException
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEExtendedException - class declaration
===============================================================================}

constructor EAEExtendedException.CreateFmt(const Msg: String; Args: array of const);
begin
// get ALL registers to local storage
// inherited constructor
// copy registers from local storage to objects storage
end;

{===============================================================================
--------------------------------------------------------------------------------
                               EAEGeneralException
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEGeneralException - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    EAEGeneralException - public methods
-------------------------------------------------------------------------------}

constructor EAEGeneralException.CreateFmt(const Msg: String; Args: array of const; FaultObject: TObject; const FaultFunction: String);
begin
inherited CreateFmt(Msg,Args);
If Assigned(FaultObject) then
  fFaultingObject := Format('%s(%p)',[FaultObject.ClassName,Pointer(FaultObject)])
else
  fFaultingObject := '';
fFaultingFunction := FaultFunction;
If Assigned(FaultObject) then
  begin
    If Length(fFaultingFunction) > 0 then
      fFullMessage := Format(Format('%s.%s: %s',[fFaultingObject,fFaultingFunction,Msg]),Args)
    else
      fFullMessage := Format(Format('%s: %s',[fFaultingObject,Msg]),Args);
  end
else
  begin
    If Length(fFaultingFunction) > 0 then
      fFullMessage := Format(Format('%s: %s',[fFaultingFunction,Msg]),Args)
    else
      fFullMessage := Format(Msg,Args);
  end;
end;

//------------------------------------------------------------------------------

constructor EAEGeneralException.Create(const Msg: String; FaultObject: TObject; const FaultFunction: String);
begin
CreateFmt(Msg,[],FaultObject,FaultFunction);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 EAESystemError
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAESystemError - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    EAESystemError - public methods
-------------------------------------------------------------------------------}

constructor EAESystemError.Create(ErrorCode: TAESysErrCode; FullSysMsg: Boolean; FaultObject: TObject; const FaultFunction: String);
begin
If FullSysMsg then
  inherited CreateFmt({$IFDEF Windows}'System error %u: %s'{$ELSE}'System error %d: %s'{$ENDIF},
                      [ErrorCode,SysErrorMessage(ErrorCode)],FaultObject,FaultFunction)
else
  inherited CreateFmt({$IFDEF Windows}'System error occured (%u).'{$ELSE}'System error occured (%d).'{$ENDIF},
                      [ErrorCode],FaultObject,FaultFunction);
fErrorCode := ErrorCode;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor EAESystemError.Create(FullSysMsg: Boolean; FaultObject: TObject; const FaultFunction: String);
begin
Create({$IFDEF Windows}GetLastError{$ELSE}errno{$ENDIF},FullSysMsg,FaultObject,FaultFunction);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                EAEIndexException
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEIndexException - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    EAEIndexException - protected methods
-------------------------------------------------------------------------------}

class Function EAEIndexException.GetDefaultMessage: String;
begin
Result := 'Index (%d) error.';
end;

{-------------------------------------------------------------------------------
    EAEIndexException - public methods
-------------------------------------------------------------------------------}

constructor EAEIndexException.Create(const Msg: String; Index: Integer; FaultObject: TObject; const FaultFunction: String);
begin
inherited CreateFmt(Msg,[Index],FaultObject,FaultFunction);
fIndex := Index;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor EAEIndexException.Create(Index: Integer; FaultObject: TObject; const FaultFunction: String);
begin
Create(GetDefaultMessage,Index,FaultObject,FaultFunction);
end;


{===============================================================================
--------------------------------------------------------------------------------
                               EAEIndexOutOfBounds
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEIndexOutOfBounds - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    EAEIndexOutOfBounds - protected methods
-------------------------------------------------------------------------------}

class Function EAEIndexOutOfBounds.GetDefaultMessage: String;
begin
Result := 'Index (%d) out of bounds.';
end;

{===============================================================================
--------------------------------------------------------------------------------
                                 EAEIndexTooLow
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEIndexTooLow - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    EAEIndexTooLow - protected methods
-------------------------------------------------------------------------------}

class Function EAEIndexTooLow.GetDefaultMessage: String;
begin
Result := 'Index (%d) too low.';
end;

{===============================================================================
--------------------------------------------------------------------------------
                                 EAEIndexTooHigh
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEIndexTooHigh - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    EAEIndexTooHigh - protected methods
-------------------------------------------------------------------------------}

class Function EAEIndexTooHigh.GetDefaultMessage: String;
begin
Result := 'Index (%d) too high.';
end;

{===============================================================================
--------------------------------------------------------------------------------
                                 EAEIndexInvalid
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEIndexInvalid - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    EAEIndexInvalid - protected methods
-------------------------------------------------------------------------------}

class Function EAEIndexInvalid.GetDefaultMessage: String;
begin
Result := 'Index (%d) is invalid.';
end;


{===============================================================================
--------------------------------------------------------------------------------
                                EAEValueException
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEValueException - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    EAEValueException - protected methods
-------------------------------------------------------------------------------}

class Function EAEValueException.VariantArrayToStr(Value: Variant): String;
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

class Function EAEValueException.GetDefaultMessage(ValueString: Boolean): String;
begin
If ValueString then
  Result := 'Value %s error (%s).'
else
  Result := 'Value %s error.';
end;

{-------------------------------------------------------------------------------
    EAEValueException - public methods
-------------------------------------------------------------------------------}

constructor EAEValueException.Create(const Msg,ValueName: String; Value: Variant; FaultObject: TObject; const FaultFunction: String);
begin
If (VarType(Value) and varArray) <> 0 then
  inherited CreateFmt(Msg,[ValueName,VariantArrayToStr(Value)],FaultObject,FaultFunction)
else
  inherited CreateFmt(Msg,[ValueName,VarToStrDef(Value,'ERROR')],FaultObject,FaultFunction);
fValue := Value;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor EAEValueException.Create(const Msg,ValueName: String; FaultObject: TObject; const FaultFunction: String);
begin
inherited CreateFmt(Msg,[ValueName],FaultObject,FaultFunction);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor EAEValueException.Create(const ValueName: String; Value: Variant; FaultObject: TObject; const FaultFunction: String);
begin
Create(GetDefaultMessage(True),ValueName,Value,FaultObject,FaultFunction);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor EAEValueException.Create(const ValueName: String; FaultObject: TObject; const FaultFunction: String);
begin
Create(GetDefaultMessage(False),ValueName,FaultObject,FaultFunction);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 EAEValueInvalid
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEValueInvalid - class implementation
===============================================================================}

class Function EAEValueInvalid.GetDefaultMessage(ValueString: Boolean): String;
begin
If ValueString then
  Result := 'Invalid %s value (%s).'
else
  Result := 'Invalid %s value.';
end;

{===============================================================================
--------------------------------------------------------------------------------
                             EAEValueInvalidNameOnly
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    EAEValueInvalidNameOnly - class implementation
===============================================================================}

class Function EAEValueInvalidNameOnly.GetDefaultMessage(ValueString: Boolean): String;
begin
If ValueString then
  Result := 'Invalid %s (%s).'
else
  Result := 'Invalid %s.';
end;

{$ENDIF AE_Include_Implementation}
