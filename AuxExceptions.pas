{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  AuxExceptions

    Set of exception classes designed to simplify exception creation in
    specific situations (eg. index out of bounds, invalid variable value,
    system error, ...).

  ©František Milt 2019-03-27

  Version 0.9.1 (under development)
  
  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting the author by
    making a small donation using following link(s):

      https://www.paypal.me/FMilt

  Dependencies:
    AuxTypes - github.com/ncs-sniper/Lib.AuxTypes

===============================================================================}
(*******************************************************************************
--------------------------------------------------------------------------------
  Inclusion template
--------------------------------------------------------------------------------

unit <unit_name>;

{$DEFINE AE_Included}

{$DEFINE AE_Include_Defines}
{$INCLUDE '..\Dev\AuxExceptions.pas'}
{$UNDEF AE_Include_Defines}

{$DEFINE AE_Include_Defines_Auto}
{$INCLUDE '..\Dev\AuxExceptions.pas'}
{$UNDEF AE_Include_Defines_Auto}

interface

{$DEFINE AE_Include_Interface_Uses}
uses
  {$INCLUDE '..\Dev\AuxExceptions.pas'};
{$UNDEF AE_Include_Interface_Uses}

type
  E<prefix>BaseException = class(Exception);

  EBaseException = EIBaseException;

{$DEFINE AE_Include_Interface}
{$INCLUDE '..\Dev\AuxExceptions.pas'}
{$UNDEF AE_Include_Interface}

type
  E<prefix>GeneralException     = EGeneralException;
  E<prefix>SystemError          = ESystemError;
  E<prefix>IndexException       = EIndexException;
  E<prefix>IndexOutOfBounds     = EIndexOutOfBounds;
  E<prefix>IndexTooLow          = EIndexTooLow;
  E<prefix>IndexTooHigh         = EIndexTooHigh;
  E<prefix>IndexInvalid         = EIndexInvalid;
  E<prefix>ValueException       = EValueException;
  E<prefix>ValueInvalid         = EValueInvalid;
  E<prefix>ValueInvalidNameOnly = EValueInvalidNameOnly;

implementation

{$DEFINE AE_Include_Implementation_Uses}
uses
  {$INCLUDE '..\Dev\AuxExceptions.pas'};
{$UNDEF AE_Include_Implementation_Uses}

{$DEFINE AE_Include_Implementation}
{$INCLUDE '..\Dev\AuxExceptions.pas'}
{$UNDEF AE_Include_Implementation}

end.

*******************************************************************************)
{$IFNDEF AE_Included}
unit AuxExceptions;
{$ENDIF AE_Included}

{$IF Defined(AE_Include_Defines) or not Defined(AE_Included)}

{$IF Defined(CPU64) or Defined(CPU64BITS)}
  {$DEFINE 64bit}
{$ELSEIF Defined(CPU16)}
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
{$ELSEIF Defined(LINUX)}
  // when compiled for linux, some of the features might not be available atm.
  {$DEFINE Linux}
{$ELSE}
  {$MESSAGE FATAL 'Unsupported operating system.'}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}{$H+}
{$ENDIF}

{
  ExtendedException

  When defined, EGeneralException and all its subclasses provide extended
  error information (stack trace, CPU registers snapshot, ...).

  NOTE - currently not implemented.

  Enabled by default.
}
{$DEFINE EnableExtendedException}

{$IFEND}  // defines block end

{$IF Defined(AE_Include_Defines_Auto) or not Defined(AE_Included)}

// do not touch following...
{$IF not Defined(PurePascal) and Defined(EnableExtendedException)}
  {$DEFINE ExtendedException}
{$ELSE}
  {$UNDEF ExtendedException}
{$IFEND}

{$IFEND}  // auto defines block end

{$IFNDEF AE_Included}
interface
{$ENDIF AE_Included}

{$IFNDEF AE_Included}
uses
{$ENDIF AE_Included}
{$IF Defined(AE_Include_Interface_Uses) or not Defined(AE_Included)}
  {$IFDEF Windows}Windows{$ELSE}baseunix, pthreads{$ENDIF}, SysUtils, AuxTypes
{$IFEND}  // interface uses block end
{$IFNDEF AE_Included};{$ENDIF AE_Included}

{$IFNDEF AE_Included}
type
  EBaseException = class(Exception);
{$ENDIF AE_Included}

{$IF Defined(AE_Include_Interface) or not Defined(AE_Included)}
{===============================================================================
--------------------------------------------------------------------------------
                             Base exception classes
--------------------------------------------------------------------------------
===============================================================================}

type
  TAEThreadID = {$IFDEF Windows}DWORD{$ELSE}pthread_t{$ENDIF};
  TAESysErrCode = {$IFDEF Windows}DWORD{$ELSE}cint{$ENDIF};

{===============================================================================
    ECustomException - class declaration
===============================================================================}

  ECustomException = class(EBaseException)
  protected
    fTime:      TDateTime;
    fThreadID:  TAEThreadID;
  public
    constructor CreateFmt(const Msg: String; Args: array of const);
    property Time: TDateTime read fTime;
    property ThreadID: TAEThreadID read fThreadID;
  end;

{$IFDEF ExtendedException}
{===============================================================================
    EExtendedException - internal types
===============================================================================}

type
  TNativeRegister = PtrUInt;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  TGeneralPurposeRegister = packed record
    case Integer of
      0:  (LoByte:  UInt8;
           HiByte:  UInt8);
      1:  (Word:    UInt16);
      2:  (Long:    UInt32);
    {$IFDEF x64}
      3:  (Quad:    UInt64);
    {$ENDIF}
      4:  (Native:  TNativeRegister)
  end;

  TGeneralPurposeRegisters = packed record
    case Integer of
      0:  (A,B,C,D,SI,DI,BP,SP:           TGeneralPurposeRegister;
        {$IFDEF x64}
           R8,R9,R10,R11,R12,R13,R14,R15: TGeneralPurposeRegister;
        {$ENDIF});
      1:  (R0,R1,R2,R3,R4,R5,R6,R7:       TGeneralPurposeRegister;
        {$IFDEF x64}
           R8,R9,R10,R11,R12,R13,R14,R15: TGeneralPurposeRegister;
        {$ENDIF});
      2:  (Regs:  array[0..{$IFDEF x64}15{$ELSE}7{$ENDIF}] of TGeneralPurposeRegister);
  end;

  TSegmentRegisters = packed record
    case Integer of
      0:  (CS,DS,SS,ES,FS,GS: UInt16);
      1:  (Regs:              packed array[0..5] of UInt16);
  end;

  TBasicRegisters = record
    GeneralPurpose:     TGeneralPurposeRegisters;
    Segment:            TSegmentRegisters;
    Flags:              TNativeRegister;
    InstructionPointer: TNativeRegister;
  end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  TFloatRegister = record
    Float80:  Float80;
    Float64:  Float64;
    Float32:  Float32;
  end;

  TFloatRegisters = record
    Data:                       array[0..7] of TFloatRegister;
    Control,Status,TagWord:     UInt16;
    LastInstructionPointerSel:  UInt16; // selector
    LastInstructionPointerOff:  UInt32; // offset
    LastDataPointerSel:         UInt16; // selector
    LastDataPointerOff:         UInt32; // offset
    OpCode:                     UInt16; // only lower 11 bits are walid
  end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  TIntegerVectorRegister = packed record
    case Integer of
      0:  (Vec_8b:  packed array[0..7] of UInt8);
      1:  (Vec_16b: packed array[0..3] of UInt16);
      2:  (Vec_32b: packed array[0..1] of UInt32);
      3:  (Vec_64b: UInt64);
  end;

  TIntegerVectorRegisters = packed record
    MM: packed array[0..7] of TIntegerVectorRegister;
  end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  TFloatVectorRegister = packed record
    case Integer of
      0:  (Vec_32b: packed array[0..15] of Float32);
      1:  (Vec_64b: packed array[0..7] of Float64);
      2:  (XMM_32b: packed array[0..3] of Float32);
      3:  (XMM_64b: packed array[0..1] of Float64);
      4:  (YMM_32b: packed array[0..7] of Float32);
      5:  (YMM_64b: packed array[0..3] of Float64);
      6:  (ZMM_32b: packed array[0..15] of Float32);
      7:  (ZMM_64b: packed array[0..7] of Float64)
  end;

  TFloatVectorRegisters = record
    Length: Integer;                // number of valid 32bit floats in each register
    Count:  Integer;                // number of valid register
    Regs:   array[0..31] of TFloatVectorRegister;
    MXCSR:  UInt32;
    OPMask: array[0..7] of UInt64;  // K0-K7 registers
  end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  TPresentRegister = (
    prBasic,  // general purpose, flags, segment and instruction pointer registers
    prFPU,    // x87 FPU registers (ST0-ST7, control, status, ...)
    prMMX,    // MMX registers (MM0-MM7)
    prSSE,    // SSE-SSE4.x registers (XMM0-XMM7/15)
    prAVX,    // AVX and AVX2 registers (YMM0-YMM7/15)
    prAVX512  // AVX-512 registers (ZMM0-ZMM7/31, K0-K7)
  );

  TPresentRegisters = set of TPresentRegister;

  TRegisters = record
    PresentRegs:    TPresentRegisters;
    Basic:          TBasicRegisters;
    Float:          TFloatRegisters;          // x87 FPU
    IntegerVector:  TIntegerVectorRegisters;  // MMX
    FloatVector:    TFloatVectorRegisters;    // SSE, AVX
  end;

{===============================================================================
    EExtendedException - class declaration
===============================================================================}

type
  EExtendedException = class(ECustomException)
  private
    fRegisters: TRegisters;
  protected
    procedure GetFloatRegister; virtual;
  public
    constructor CreateFmt(const Msg: String; Args: array of const);
    property Registers: TRegisters read fRegisters; 
  end;

{$ENDIF ExtendedException}

{===============================================================================
    EGeneralException - class declaration
===============================================================================}
{$IFDEF ExtendedException}
  EGeneralException = class(EExtendedException)
{$ELSE ExtendedException}
  EGeneralException = class(ECustomException)
{$ENDIF ExtendedException}
  private
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
                                 System errors
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    ESystemError - class declaration
===============================================================================}

  ESystemError = class(EGeneralException)
  private
    fErrorCode: TAESysErrCode;
  public
    constructor Create(FullSysMsg: Boolean; FaultObject: TObject; const FaultFunction: String); overload;
    property ErrorCode: TAESysErrCode read fErrorCode;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                  Index errors
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    EIndexException - class declaration
===============================================================================}

  EIndexException = class(EGeneralException)
  protected
    fIndex: Integer;
    class Function GetDefaultMessage: String; virtual;
  public
    constructor Create(const Msg: String; Index: Integer; FaultObject: TObject; const FaultFunction: String); overload;
    constructor Create(Index: Integer; FaultObject: TObject; const FaultFunction: String); overload;
    property Index: Integer read fIndex;
  end;

{===============================================================================
    EIndexOutOfBounds - class declaration
===============================================================================}

  EIndexOutOfBounds = class(EIndexException)
  protected
    class Function GetDefaultMessage: String; override;
  end;

{===============================================================================
    EIndexTooLow - class declaration
===============================================================================}

  EIndexTooLow = class(EIndexException)
  protected
    class Function GetDefaultMessage: String; override;
  end;

{===============================================================================
    EIndexTooHigh - class declaration
===============================================================================}

  EIndexTooHigh = class(EIndexException)
  protected
    class Function GetDefaultMessage: String; override;
  end;

{===============================================================================
    EIndexInvalid - class declaration
===============================================================================}

  EIndexInvalid = class(EIndexException)
  protected
    class Function GetDefaultMessage: String; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                  Value errors
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    EValueException - class declaration
===============================================================================}

  EValueException = class(EGeneralException)
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
    EValueInvalid - class declaration
===============================================================================}

  EValueInvalid = class(EValueException)
  protected
    class Function GetDefaultMessage(ValueString: Boolean): String; override;
  end;

{===============================================================================
    EValueInvalidNameOnly - class declaration
===============================================================================}

  EValueInvalidNameOnly = class(EValueException)
  protected
    class Function GetDefaultMessage(ValueString: Boolean): String; override;
  end;

{$IFEND}  // interface block end

{$IFNDEF AE_Included}
implementation
{$ENDIF AE_Included}

{$IFNDEF AE_Included}
uses
{$ENDIF AE_Included}
{$IF Defined(AE_Include_Implementation_Uses) or not Defined(AE_Included)}
  Variants
{$IFDEF ExtendedException}
  , SimpleCPUID
{$ENDIF ExtendedException}
{$IFEND}  // implementation uses block end
{$IFNDEF AE_Included};{$ENDIF AE_Included}

{$IF Defined(AE_Include_Implementation) or not Defined(AE_Included)}
{===============================================================================
--------------------------------------------------------------------------------
                             Base exception classes
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    ECustomException - class implementation
===============================================================================}

constructor ECustomException.CreateFmt(const Msg: String; Args: array of const);
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
    EGeneralException - class implementation
===============================================================================}

constructor EGeneralException.CreateFmt(const Msg: String; Args: array of const; FaultObject: TObject; const FaultFunction: String);

  Function InstanceString(Obj: TObject): String;
  begin
    Result := Format('%s(%p)',[Obj.ClassName,Pointer(Obj)]);
  end;
  
begin
inherited CreateFmt(Msg,Args);
If Assigned(FaultObject) then
  fFaultingObject := InstanceString(FaultObject)
else
  fFaultingObject := '';
fFaultingFunction := FaultFunction;
If Length(fFaultingObject) > 0 then
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

constructor EGeneralException.Create(const Msg: String; FaultObject: TObject; const FaultFunction: String);
begin
CreateFmt(Msg,[],FaultObject,FaultFunction);
end;

{$IFDEF ExtendedException}
{===============================================================================
    EExtendedException - class implementation
===============================================================================}

{-------------------------------------------------------------------------------
    EExtendedException - auxiliary routines
-------------------------------------------------------------------------------}

{$IFOPT W+}
  {$DEFINE StackFramesEnabled}
{$ELSE}
  {$UNDEF StackFramesEnabled}
{$ENDIF}

{$STACKFRAMES OFF}

Function GetInstructionPointer: Pointer; assembler; register;{$IFDEF FPC} nostackframe; {$ENDIF}
asm
{$IF Defined(x64)}
  {$IFNDEF FPC}.NOFRAME{$ENDIF}
    MOV   qword ptr RAX, [RSP]
{$ELSEIF Defined(x86)}
    MOV   dword ptr EAX, [ESP]
{$ELSE}
  {$MESSAGE FATAL 'Unsupported architecture.'}
{$IFEND}
end;

//------------------------------------------------------------------------------

procedure GetGeneralPurposeRegisters(Mem: Pointer); assembler; register;{$IFDEF FPC} nostackframe; {$ENDIF}
asm
{$IF Defined(x64)}
  {$IFNDEF FPC}.NOFRAME{$ENDIF}
    MOV   qword ptr [Mem + 00], RAX
    MOV   qword ptr [Mem + 08], RBX
    MOV   qword ptr [Mem + 16], RCX
    MOV   qword ptr [Mem + 24], RDX
    MOV   qword ptr [Mem + 32], RSI
    MOV   qword ptr [Mem + 40], RDI
    MOV   qword ptr [Mem + 48], RBP
    MOV   qword ptr [Mem + 56], RSP
{$ELSEIF Defined(x86)}
    MOV   dword ptr [Mem + 00], EAX
    MOV   dword ptr [Mem + 04], EBX
    MOV   dword ptr [Mem + 08], ECX
    MOV   dword ptr [Mem + 12], EDX
    MOV   dword ptr [Mem + 16], ESI
    MOV   dword ptr [Mem + 20], EDI
    MOV   dword ptr [Mem + 24], EBP
    MOV   dword ptr [Mem + 28], ESP
{$ELSE}
  {$MESSAGE FATAL 'Unsupported architecture.'}
{$IFEND}
end;

//------------------------------------------------------------------------------

Function GetFLAGSRegister: PtrUInt; assembler; register;{$IFDEF FPC} nostackframe; {$ENDIF}
asm
{$IF Defined(x64)}
  {$IFNDEF FPC}.NOFRAME{$ENDIF}
    PUSHFQ
    MOV   RAX, qword ptr [RSP]
    ADD   RSP, 8
{$ELSEIF Defined(x86)}
    PUSHFD
    MOV   EAX, dword ptr [ESP]
    ADD   ESP, 4
{$ELSE}
  {$MESSAGE FATAL 'Unsupported architecture.'}
{$IFEND}
end;

//------------------------------------------------------------------------------

//CS,DS,SS,ES,FS,GS
procedure GetSegmentRegisters(Mem: Pointer); assembler; register;{$IFDEF FPC} nostackframe; {$ENDIF}
asm
{$IF Defined(x64)}
  {$IFNDEF FPC}.NOFRAME{$ENDIF}
{$ELSEIF not Defined(x86)}
  {$MESSAGE FATAL 'Unsupported architecture.'}
{$IFEND}
    MOV   word ptr [Mem + 00],  CS
    MOV   word ptr [Mem + 02],  DS
    MOV   word ptr [Mem + 04],  SS
    MOV   word ptr [Mem + 06],  ES
    MOV   word ptr [Mem + 08],  FS
    MOV   word ptr [Mem + 10],  GS
end;

//------------------------------------------------------------------------------

procedure Getx87Registers(Mem: Pointer); assembler; register;{$IFDEF FPC} nostackframe; {$ENDIF}
asm
{$IF Defined(x64)}
  {$IFNDEF FPC}.NOFRAME{$ENDIF}
{$ELSEIF not Defined(x86)}
  {$MESSAGE FATAL 'Unsupported architecture.'}
{$IFEND}
    FSTENV  [Mem]
end;

//------------------------------------------------------------------------------

procedure Float80toFloat64(F80,F64: Pointer); assembler; register;{$IFDEF FPC} nostackframe; {$ENDIF}
asm
{$IF Defined(x64)}
  {$IFNDEF FPC}.NOFRAME{$ENDIF}
{$ELSEIF not Defined(x86)}
  {$MESSAGE FATAL 'Unsupported architecture.'}
{$IFEND}
    FLD     tbyte ptr [F80]
    FSTP    qword ptr [F64]
    FWAIT
end;

//------------------------------------------------------------------------------

procedure GetMMXRegisters(Mem: Pointer); assembler; register;{$IFDEF FPC} nostackframe; {$ENDIF}
asm
{$IF Defined(x64)}
  {$IFNDEF FPC}.NOFRAME{$ENDIF}
{$ELSEIF not Defined(x86)}
  {$MESSAGE FATAL 'Unsupported architecture.'}
{$IFEND}
    MOVQ  qword ptr [Mem + 00], MM0
    MOVQ  qword ptr [Mem + 08], MM1
    MOVQ  qword ptr [Mem + 16], MM2
    MOVQ  qword ptr [Mem + 24], MM3
    MOVQ  qword ptr [Mem + 32], MM4
    MOVQ  qword ptr [Mem + 40], MM5
    MOVQ  qword ptr [Mem + 48], MM6
    MOVQ  qword ptr [Mem + 56], MM7
end;

{$IFDEF StackFramesEnabled}
  {$STACKFRAMES ON}
{$ENDIF}

{-------------------------------------------------------------------------------
    EExtendedException - protected methods
-------------------------------------------------------------------------------}

procedure EExtendedException.GetFloatRegister;
var
  FPUData: packed record
    CW,pad0:  UInt16;   // control word
    SW,pad1:  UInt16;   // status word
    TW,pad2:  UInt16;   // tag word
    FIP:      UInt32;   // FPU Instruction Pointer Offset
    FCS:      UInt16;   // FPU Instruction Pointer Selector
    OpCode:   UInt16;   // only lower 10 bits used
    FDP:      UInt32;   // FPU Data Pointer Offset
    FDS,pad3: UInt16;   // FPU Data Pointer Selector
    Stack:    packed array[0..7] of Float80;
  end;
  i:  Integer;
begin
Getx87Registers(@FPUData);
// parse the obtained data
For i := Low(FPUData.Stack) to High(FPUData.Stack) do
  begin
    Move(FPUData.Stack[i],fRegisters.Float.Data[i].Float80,SizeOf(Float80));
    Float80toFloat64(Addr(fRegisters.Float.Data[i].Float80),Addr(fRegisters.Float.Data[i].Float64));
    fRegisters.Float.Data[i].Float32 := fRegisters.Float.Data[i].Float64;
  end;
fRegisters.Float.Control := FPUData.CW;
fRegisters.Float.Status := FPUData.SW;
fRegisters.Float.TagWord := FPUData.TW;
fRegisters.Float.LastInstructionPointerSel := FPUData.FCS;
fRegisters.Float.LastInstructionPointerOff := FPUData.FIP;
fRegisters.Float.LastDataPointerSel := FPUData.FDS;
fRegisters.Float.LastDataPointerOff := FPUData.FDP;
fRegisters.Float.OpCode := FPUData.OpCode and $07FF;
end;

{-------------------------------------------------------------------------------
    EExtendedException - public methods
-------------------------------------------------------------------------------}

(*
    prSSE,    // SSE-SSE4.x registers (XMM0-XMM7/15)
    prAVX,    // AVX and AVX2 registers (YMM0-YMM7/15)
    prAVX512  // AVX-512 registers (ZMM0-ZMM7/31, K0-K7)
*)
constructor EExtendedException.CreateFmt(const Msg: String; Args: array of const);
begin                  
inherited CreateFmt(Msg,Args);
FillChar(fRegisters,SizeOf(TRegisters),0);
// get basic registers (I know they are now polluted by arguments and other things, but to be complete)
fRegisters.Basic.InstructionPointer := TNativeRegister(GetInstructionPointer);
GetGeneralPurposeRegisters(Addr(fRegisters.Basic.GeneralPurpose));
fRegisters.Basic.Flags := GetFLAGSRegister;
GetSegmentRegisters(Addr(fRegisters.Basic.Segment));
Include(fRegisters.PresentRegs,prBasic);
with TSimpleCPUID.Create do
try
  If Info.SupportedExtensions.X87 then
    begin
      // get FPU registers
      GetFloatRegister;
      Include(fRegisters.PresentRegs,prFPU);
    end;
  If Info.SupportedExtensions.MMX then
    begin
      // get MMX registers
      GetMMXRegisters(Addr(fRegisters.IntegerVector.MM));
      Include(fRegisters.PresentRegs,prMMX);
    end;
finally
  Free;
end;
end;

{$ENDIF ExtendedException}

{===============================================================================
--------------------------------------------------------------------------------
                                 System errors                                  
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    ESystemError - class implementation
===============================================================================}

constructor ESystemError.Create(FullSysMsg: Boolean; FaultObject: TObject; const FaultFunction: String);
var
  ErrCode:  TAESysErrCode;
begin
{$IFDEF Windows}
ErrCode := GetLastError;
{$ELSE}
ErrCode := errno;
{$ENDIF}
If FullSysMsg then
  inherited CreateFmt('System error 0x%.8x: %s',[ErrCode,SysErrorMessage(ErrCode)],FaultObject,FaultFunction)
else
  inherited CreateFmt('System error occured (0x%.8x).',[ErrCode],FaultObject,FaultFunction);
fErrorCode := ErrCode;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                  Index errors
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    EIndexException - class implementation
===============================================================================}

{-------------------------------------------------------------------------------
    EIndexException - protected methods
-------------------------------------------------------------------------------}

class Function EIndexException.GetDefaultMessage: String;
begin
Result := 'Index (%d) error.';
end;

{-------------------------------------------------------------------------------
    EIndexException - public methods
-------------------------------------------------------------------------------}

constructor EIndexException.Create(const Msg: String; Index: Integer; FaultObject: TObject; const FaultFunction: String);
begin
inherited CreateFmt(Msg,[Index],FaultObject,FaultFunction);
fIndex := Index;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor EIndexException.Create(Index: Integer; FaultObject: TObject; const FaultFunction: String);
begin
Create(GetDefaultMessage,Index,FaultObject,FaultFunction);
end;

{===============================================================================
    EIndexOutOfBounds - class implementation
===============================================================================}

class Function EIndexOutOfBounds.GetDefaultMessage: String;
begin
Result := 'Index (%d) out of bounds.';
end;

{===============================================================================
    EIndexTooLow - class implementation
===============================================================================}

class Function EIndexTooLow.GetDefaultMessage: String;
begin
Result := 'Index (%d) too low.';
end;

{===============================================================================
    EIndexTooHigh - class implementation
===============================================================================}

class Function EIndexTooHigh.GetDefaultMessage: String;
begin
Result := 'Index (%d) too high.';
end;

{===============================================================================
    EIndexInvalid - class implementation
===============================================================================}

class Function EIndexInvalid.GetDefaultMessage: String;
begin
Result := 'Index (%d) is invalid.';
end;


{===============================================================================
--------------------------------------------------------------------------------
                                  Value errors
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    EValueException - class implementation
===============================================================================}

{-------------------------------------------------------------------------------
    EValueException - protected methods
-------------------------------------------------------------------------------}

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

{-------------------------------------------------------------------------------
    EValueException - public methods
-------------------------------------------------------------------------------}

constructor EValueException.Create(const Msg,ValueName: String; Value: Variant; FaultObject: TObject; const FaultFunction: String);
begin
If (VarType(Value) and varArray) <> 0 then
  inherited CreateFmt(Msg,[ValueName,VariantArrayToStr(Value)],FaultObject,FaultFunction)
else
  inherited CreateFmt(Msg,[ValueName,VarToStrDef(Value,'ERROR')],FaultObject,FaultFunction);
fValue := Value;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor EValueException.Create(const Msg,ValueName: String; FaultObject: TObject; const FaultFunction: String);
begin
inherited CreateFmt(Msg,[ValueName],FaultObject,FaultFunction);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor EValueException.Create(const ValueName: String; Value: Variant; FaultObject: TObject; const FaultFunction: String);
begin
Create(GetDefaultMessage(True),ValueName,Value,FaultObject,FaultFunction);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor EValueException.Create(const ValueName: String; FaultObject: TObject; const FaultFunction: String);
begin
Create(GetDefaultMessage(False),ValueName,FaultObject,FaultFunction);
end;

{===============================================================================
    EValueInvalid - class implementation
===============================================================================}

class Function EValueInvalid.GetDefaultMessage(ValueString: Boolean): String;
begin
If ValueString then
  Result := 'Invalid %s value (%s).'
else
  Result := 'Invalid %s value.';
end;

{===============================================================================
    EValueInvalidNameOnly - class implementation
===============================================================================}

class Function EValueInvalidNameOnly.GetDefaultMessage(ValueString: Boolean): String;
begin
If ValueString then
  Result := 'Invalid %s (%s).'
else
  Result := 'Invalid %s.';
end;

{$IFEND}  // implementation block end

{$IFNDEF AE_Included}
{$WARNINGS OFF}
end.
{$ENDIF AE_Included}
