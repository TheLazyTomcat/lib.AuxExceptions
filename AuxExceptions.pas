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
    AuxTypes    - github.com/ncs-sniper/Lib.AuxTypes
  * SimpleCPUID - github.com/ncs-sniper/Lib.SimpleCPUID

  SimpleCPUID is required only when symbol PurePascal is defined or symbol
  EnableExtendedException is not defined.

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

{$DEFINE AE_Include_Interface_Defines}
{$INCLUDE '..\Dev\AuxExceptions.pas'}
{$UNDEF AE_Include_Interface_Defines}

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

{$DEFINE AE_Include_Implementation_Defines}
{$INCLUDE '..\Dev\AuxExceptions.pas'}
{$UNDEF AE_Include_Implementation_Defines}

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
  {$DEFINE Linux}
{$ELSE}
  {$MESSAGE FATAL 'Unsupported operating system.'}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}{$H+}
  {$IFNDEF PurePascal}
    {$ASMMODE Intel}
  {$ENDIF}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ENDIF}

{
  ExtendedException

  When defined, EGeneralException and all its subclasses provide extended
  error information (stack trace, CPU registers snapshot, ...).

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
  {$IFDEF Windows}Windows{$ELSE}baseunix, pthreads{$ENDIF}, SysUtils
  {$IFDEF ExtendedException}
  , AuxTypes
  {$ENDIF}
{$IFEND}  // interface uses block end
{$IFNDEF AE_Included};{$ENDIF AE_Included}

{$IF Defined(AE_Include_Interface_Defines) or not Defined(AE_Included)}
{$IFEND}  // interface defines block end

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
      1:  (R0,R1,R2,R3,R4,R5,R6,R7:       TGeneralPurposeRegister);
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
    Regs:   packed array[0..31] of TFloatVectorRegister;
    MXCSR:  UInt32;
    OPMask: packed array[0..7] of UInt64;  // K0-K7 registers
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
    procedure GetFloatRegisters; virtual;
    procedure GetFloatVectorRegisters_SSE; virtual;
    procedure GetFloatVectorRegisters_AVX; virtual;
    procedure GetFloatVectorRegisters_AVX512; virtual;
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

{$IFDEF FPC}{$WARN 2005 OFF}{$ENDIF}
{$IF (Defined(AE_Include_Implementation_Defines) or not Defined(AE_Included))}
  {$IFDEF FPC_DisableWarns}
    {$DEFINE FPCDWM}
    {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
    {$IF Defined(FPC) and (FPC_FULLVERSION >= 30000)}
      {$DEFINE W7122:={$WARN 7122 OFF}} // Warning: Check size of memory operand "op: memory-operand-size is X bits, but expected [Y bits + Z byte offset]"
    {$ELSE}
      {$DEFINE W7122:=}
    {$IFEND}
  {$ENDIF}
{$IFEND}  // implementation defines block end
{$IFDEF FPC}{$WARN 2005 ON}{$ENDIF}

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
    MOV   RAX, qword ptr [RSP]
{$ELSEIF Defined(x86)}
    MOV   EAX, dword ptr [ESP]
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

{$IFDEF FPCDWM}{$PUSH}W7122{$ENDIF}
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
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function GetMXCSR: UInt32; assembler; register;{$IFDEF FPC} nostackframe; {$ENDIF}
asm
{$IF Defined(x64)}
  {$IFNDEF FPC}.NOFRAME{$ENDIF}
    SUB       RSP,  4
    STMXCSR   dword ptr [RSP]
    MOV       EAX,  dword ptr [RSP]
    ADD       RSP,  4
{$ELSEIF Defined(x86)}
    SUB       ESP,  4
    STMXCSR   dword ptr [ESP]
    MOV       EAX,  dword ptr [ESP]
    ADD       ESP,  4
{$ELSE}
  {$MESSAGE FATAL 'Unsupported architecture.'}
{$IFEND}
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W7122{$ENDIF}
procedure GetSSERegisters(Mem: Pointer); assembler; register;{$IFDEF FPC} nostackframe; {$ENDIF}
asm
{$IF Defined(x64)}
  {$IFNDEF FPC}.NOFRAME{$ENDIF}
{$ELSEIF not Defined(x86)}
  {$MESSAGE FATAL 'Unsupported architecture.'}
{$IFEND}
    MOVUPS  dqword ptr [Mem + 000], XMM0
    MOVUPS  dqword ptr [Mem + 064], XMM1
    MOVUPS  dqword ptr [Mem + 128], XMM2
    MOVUPS  dqword ptr [Mem + 192], XMM3
    MOVUPS  dqword ptr [Mem + 256], XMM4
    MOVUPS  dqword ptr [Mem + 320], XMM5
    MOVUPS  dqword ptr [Mem + 384], XMM6
    MOVUPS  dqword ptr [Mem + 448], XMM7
  {$IFDEF 64bit}
    MOVUPS  dqword ptr [Mem + 512], XMM8
    MOVUPS  dqword ptr [Mem + 576], XMM9
    MOVUPS  dqword ptr [Mem + 640], XMM10
    MOVUPS  dqword ptr [Mem + 704], XMM11
    MOVUPS  dqword ptr [Mem + 768], XMM12
    MOVUPS  dqword ptr [Mem + 832], XMM13
    MOVUPS  dqword ptr [Mem + 896], XMM14
    MOVUPS  dqword ptr [Mem + 960], XMM15
  {$ENDIF}
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure GetAVXRegisters(Mem: Pointer); assembler; register;{$IFDEF FPC} nostackframe; {$ENDIF}
asm
{$IF Defined(x64)}
  {$IFNDEF FPC}.NOFRAME{$ENDIF}
  {$IFDEF Windows}
    // Win64
    DB  $C5, $FC, $11, $01                      // VMOVUPS ymmword ptr [RCX + 000], YMM0
    DB  $C5, $FC, $11, $49, $40                 // VMOVUPS ymmword ptr [RCX + 064], YMM1
    DB  $C5, $FC, $11, $91, $80, $00, $00, $00  // VMOVUPS ymmword ptr [RCX + 128], YMM2
    DB  $C5, $FC, $11, $99, $C0, $00, $00, $00  // VMOVUPS ymmword ptr [RCX + 192], YMM3
    DB  $C5, $FC, $11, $A1, $00, $01, $00, $00  // VMOVUPS ymmword ptr [RCX + 256], YMM4
    DB  $C5, $FC, $11, $A9, $40, $01, $00, $00  // VMOVUPS ymmword ptr [RCX + 320], YMM5
    DB  $C5, $FC, $11, $B1, $80, $01, $00, $00  // VMOVUPS ymmword ptr [RCX + 384], YMM6
    DB  $C5, $FC, $11, $B9, $C0, $01, $00, $00  // VMOVUPS ymmword ptr [RCX + 448], YMM7

    DB  $C5, $7C, $11, $81, $00, $02, $00, $00  // VMOVUPS ymmword ptr [RCX + 512], YMM8
    DB  $C5, $7C, $11, $89, $40, $02, $00, $00  // VMOVUPS ymmword ptr [RCX + 576], YMM9
    DB  $C5, $7C, $11, $91, $80, $02, $00, $00  // VMOVUPS ymmword ptr [RCX + 640], YMM10
    DB  $C5, $7C, $11, $99, $C0, $02, $00, $00  // VMOVUPS ymmword ptr [RCX + 704], YMM11
    DB  $C5, $7C, $11, $A1, $00, $03, $00, $00  // VMOVUPS ymmword ptr [RCX + 768], YMM12
    DB  $C5, $7C, $11, $A9, $40, $03, $00, $00  // VMOVUPS ymmword ptr [RCX + 832], YMM13
    DB  $C5, $7C, $11, $B1, $80, $03, $00, $00  // VMOVUPS ymmword ptr [RCX + 896], YMM14
    DB  $C5, $7C, $11, $B9, $C0, $03, $00, $00  // VMOVUPS ymmword ptr [RCX + 960], YMM15
  {$ELSE}
    // Lin64
    DB  $C5, $FC, $11, $07                      // VMOVUPS ymmword ptr [RDI + 000], YMM0
    DB  $C5, $FC, $11, $4F, $40                 // VMOVUPS ymmword ptr [RDI + 064], YMM1
    DB  $C5, $FC, $11, $97, $80, $00, $00, $00  // VMOVUPS ymmword ptr [RDI + 128], YMM2
    DB  $C5, $FC, $11, $9F, $C0, $00, $00, $00  // VMOVUPS ymmword ptr [RDI + 192], YMM3
    DB  $C5, $FC, $11, $A7, $00, $01, $00, $00  // VMOVUPS ymmword ptr [RDI + 256], YMM4
    DB  $C5, $FC, $11, $AF, $40, $01, $00, $00  // VMOVUPS ymmword ptr [RDI + 320], YMM5
    DB  $C5, $FC, $11, $B7, $80, $01, $00, $00  // VMOVUPS ymmword ptr [RDI + 384], YMM6
    DB  $C5, $FC, $11, $BF, $C0, $01, $00, $00  // VMOVUPS ymmword ptr [RDI + 448], YMM7

    DB  $C5, $7C, $11, $87, $00, $02, $00, $00  // VMOVUPS ymmword ptr [RDI + 512], YMM8
    DB  $C5, $7C, $11, $8F, $40, $02, $00, $00  // VMOVUPS ymmword ptr [RDI + 576], YMM9
    DB  $C5, $7C, $11, $97, $80, $02, $00, $00  // VMOVUPS ymmword ptr [RDI + 640], YMM10
    DB  $C5, $7C, $11, $9F, $C0, $02, $00, $00  // VMOVUPS ymmword ptr [RDI + 704], YMM11
    DB  $C5, $7C, $11, $A7, $00, $03, $00, $00  // VMOVUPS ymmword ptr [RDI + 768], YMM12
    DB  $C5, $7C, $11, $AF, $40, $03, $00, $00  // VMOVUPS ymmword ptr [RDI + 832], YMM13
    DB  $C5, $7C, $11, $B7, $80, $03, $00, $00  // VMOVUPS ymmword ptr [RDI + 896], YMM14
    DB  $C5, $7C, $11, $BF, $C0, $03, $00, $00  // VMOVUPS ymmword ptr [RDI + 960], YMM15
  {$ENDIF}
{$ELSEIF Defined(x86)}
    // Win32, Lin32
    DB  $c5, $fc, $11, $00                      // VMOVUPS ymmword ptr [EAX + 000], YMM0
    DB  $c5, $fc, $11, $48, $40                 // VMOVUPS ymmword ptr [EAX + 064], YMM1
    DB  $c5, $fc, $11, $90, $80, $00, $00, $00  // VMOVUPS ymmword ptr [EAX + 128], YMM2
    DB  $c5, $fc, $11, $98, $c0, $00, $00, $00  // VMOVUPS ymmword ptr [EAX + 192], YMM3
    DB  $c5, $fc, $11, $a0, $00, $01, $00, $00  // VMOVUPS ymmword ptr [EAX + 256], YMM4
    DB  $c5, $fc, $11, $a8, $40, $01, $00, $00  // VMOVUPS ymmword ptr [EAX + 320], YMM5
    DB  $c5, $fc, $11, $b0, $80, $01, $00, $00  // VMOVUPS ymmword ptr [EAX + 384], YMM6
    DB  $c5, $fc, $11, $b8, $c0, $01, $00, $00  // VMOVUPS ymmword ptr [EAX + 448], YMM7
{$ELSE}
  {$MESSAGE FATAL 'Unsupported architecture.'}
{$IFEND}
end;

//------------------------------------------------------------------------------

procedure GetAVX512Registers(Mem: Pointer); assembler; register;{$IFDEF FPC} nostackframe; {$ENDIF}
asm
{$IF Defined(x64)}
  {$IFNDEF FPC}.NOFRAME{$ENDIF}
  {$IFDEF Windows}
    // Win64
    DB  $62, $f1, $7c, $48, $11, $01      // VMOVUPS zmmword ptr [RCX + 0000], ZMM0
    DB  $62, $f1, $7c, $48, $11, $49, $01 // VMOVUPS zmmword ptr [RCX + 0064], ZMM1
    DB  $62, $f1, $7c, $48, $11, $51, $02 // VMOVUPS zmmword ptr [RCX + 0128], ZMM2
    DB  $62, $f1, $7c, $48, $11, $59, $03 // VMOVUPS zmmword ptr [RCX + 0192], ZMM3
    DB  $62, $f1, $7c, $48, $11, $61, $04 // VMOVUPS zmmword ptr [RCX + 0256], ZMM4
    DB  $62, $f1, $7c, $48, $11, $69, $05 // VMOVUPS zmmword ptr [RCX + 0320], ZMM5
    DB  $62, $f1, $7c, $48, $11, $71, $06 // VMOVUPS zmmword ptr [RCX + 0384], ZMM6
    DB  $62, $f1, $7c, $48, $11, $79, $07 // VMOVUPS zmmword ptr [RCX + 0448], ZMM7

    DB  $62, $71, $7c, $48, $11, $41, $08 // VMOVUPS zmmword ptr [RCX + 0512], ZMM8
    DB  $62, $71, $7c, $48, $11, $49, $09 // VMOVUPS zmmword ptr [RCX + 0576], ZMM9
    DB  $62, $71, $7c, $48, $11, $51, $0a // VMOVUPS zmmword ptr [RCX + 0640], ZMM10
    DB  $62, $71, $7c, $48, $11, $59, $0b // VMOVUPS zmmword ptr [RCX + 0704], ZMM11
    DB  $62, $71, $7c, $48, $11, $61, $0c // VMOVUPS zmmword ptr [RCX + 0768], ZMM12
    DB  $62, $71, $7c, $48, $11, $69, $0d // VMOVUPS zmmword ptr [RCX + 0832], ZMM13
    DB  $62, $71, $7c, $48, $11, $71, $0e // VMOVUPS zmmword ptr [RCX + 0896], ZMM14
    DB  $62, $71, $7c, $48, $11, $79, $0f // VMOVUPS zmmword ptr [RCX + 0960], ZMM15

    DB  $62, $E1, $7C, $48, $11, $41, $10 // VMOVUPS zmmword ptr [RCX + 1024], ZMM16
    DB  $62, $E1, $7C, $48, $11, $49, $11 // VMOVUPS zmmword ptr [RCX + 1088], ZMM17
    DB  $62, $E1, $7C, $48, $11, $51, $12 // VMOVUPS zmmword ptr [RCX + 1152], ZMM18
    DB  $62, $E1, $7C, $48, $11, $59, $13 // VMOVUPS zmmword ptr [RCX + 1216], ZMM19
    DB  $62, $E1, $7C, $48, $11, $61, $14 // VMOVUPS zmmword ptr [RCX + 1280], ZMM20
    DB  $62, $E1, $7C, $48, $11, $69, $15 // VMOVUPS zmmword ptr [RCX + 1344], ZMM21
    DB  $62, $E1, $7C, $48, $11, $71, $16 // VMOVUPS zmmword ptr [RCX + 1408], ZMM22
    DB  $62, $E1, $7C, $48, $11, $79, $17 // VMOVUPS zmmword ptr [RCX + 1472], ZMM23

    DB  $62, $61, $7C, $48, $11, $41, $18 // VMOVUPS zmmword ptr [RCX + 1536], ZMM24
    DB  $62, $61, $7C, $48, $11, $49, $19 // VMOVUPS zmmword ptr [RCX + 1600], ZMM25
    DB  $62, $61, $7C, $48, $11, $51, $1A // VMOVUPS zmmword ptr [RCX + 1664], ZMM26
    DB  $62, $61, $7C, $48, $11, $59, $1B // VMOVUPS zmmword ptr [RCX + 1728], ZMM27
    DB  $62, $61, $7C, $48, $11, $61, $1C // VMOVUPS zmmword ptr [RCX + 1792], ZMM28
    DB  $62, $61, $7C, $48, $11, $69, $1D // VMOVUPS zmmword ptr [RCX + 1856], ZMM29
    DB  $62, $61, $7C, $48, $11, $71, $1E // VMOVUPS zmmword ptr [RCX + 1920], ZMM30
    DB  $62, $61, $7C, $48, $11, $79, $1F // VMOVUPS zmmword ptr [RCX + 1984], ZMM31
  {$ELSE}
    // Lin64
    DB  $62, $F1, $7C, $48, $11, $07      // VMOVUPS zmmword ptr [RDI + 0000], ZMM0
    DB  $62, $F1, $7C, $48, $11, $4F, $01 // VMOVUPS zmmword ptr [RDI + 0064], ZMM1
    DB  $62, $F1, $7C, $48, $11, $57, $02 // VMOVUPS zmmword ptr [RDI + 0128], ZMM2
    DB  $62, $F1, $7C, $48, $11, $5F, $03 // VMOVUPS zmmword ptr [RDI + 0192], ZMM3
    DB  $62, $F1, $7C, $48, $11, $67, $04 // VMOVUPS zmmword ptr [RDI + 0256], ZMM4
    DB  $62, $F1, $7C, $48, $11, $6F, $05 // VMOVUPS zmmword ptr [RDI + 0320], ZMM5
    DB  $62, $F1, $7C, $48, $11, $77, $06 // VMOVUPS zmmword ptr [RDI + 0384], ZMM6
    DB  $62, $F1, $7C, $48, $11, $7F, $07 // VMOVUPS zmmword ptr [RDI + 0448], ZMM7

    DB  $62, $71, $7C, $48, $11, $47, $08 // VMOVUPS zmmword ptr [RDI + 0512], ZMM8
    DB  $62, $71, $7C, $48, $11, $4F, $09 // VMOVUPS zmmword ptr [RDI + 0576], ZMM9
    DB  $62, $71, $7C, $48, $11, $57, $0A // VMOVUPS zmmword ptr [RDI + 0640], ZMM10
    DB  $62, $71, $7C, $48, $11, $5F, $0B // VMOVUPS zmmword ptr [RDI + 0704], ZMM11
    DB  $62, $71, $7C, $48, $11, $67, $0C // VMOVUPS zmmword ptr [RDI + 0768], ZMM12
    DB  $62, $71, $7C, $48, $11, $6F, $0D // VMOVUPS zmmword ptr [RDI + 0832], ZMM13
    DB  $62, $71, $7C, $48, $11, $77, $0E // VMOVUPS zmmword ptr [RDI + 0896], ZMM14
    DB  $62, $71, $7C, $48, $11, $7F, $0F // VMOVUPS zmmword ptr [RDI + 0960], ZMM15

    DB  $62, $E1, $7C, $48, $11, $47, $10 // VMOVUPS zmmword ptr [RDI + 1024], ZMM16
    DB  $62, $E1, $7C, $48, $11, $4F, $11 // VMOVUPS zmmword ptr [RDI + 1088], ZMM17
    DB  $62, $E1, $7C, $48, $11, $57, $12 // VMOVUPS zmmword ptr [RDI + 1152], ZMM18
    DB  $62, $E1, $7C, $48, $11, $5F, $13 // VMOVUPS zmmword ptr [RDI + 1216], ZMM19
    DB  $62, $E1, $7C, $48, $11, $67, $14 // VMOVUPS zmmword ptr [RDI + 1280], ZMM20
    DB  $62, $E1, $7C, $48, $11, $6F, $15 // VMOVUPS zmmword ptr [RDI + 1344], ZMM21
    DB  $62, $E1, $7C, $48, $11, $77, $16 // VMOVUPS zmmword ptr [RDI + 1408], ZMM22
    DB  $62, $E1, $7C, $48, $11, $7F, $17 // VMOVUPS zmmword ptr [RDI + 1472], ZMM23

    DB  $62, $61, $7C, $48, $11, $47, $18 // VMOVUPS zmmword ptr [RDI + 1536], ZMM24
    DB  $62, $61, $7C, $48, $11, $4F, $19 // VMOVUPS zmmword ptr [RDI + 1600], ZMM25
    DB  $62, $61, $7C, $48, $11, $57, $1A // VMOVUPS zmmword ptr [RDI + 1664], ZMM26
    DB  $62, $61, $7C, $48, $11, $5F, $1B // VMOVUPS zmmword ptr [RDI + 1728], ZMM27
    DB  $62, $61, $7C, $48, $11, $67, $1C // VMOVUPS zmmword ptr [RDI + 1792], ZMM28
    DB  $62, $61, $7C, $48, $11, $6F, $1D // VMOVUPS zmmword ptr [RDI + 1856], ZMM29
    DB  $62, $61, $7C, $48, $11, $77, $1E // VMOVUPS zmmword ptr [RDI + 1920], ZMM30
    DB  $62, $61, $7C, $48, $11, $7F, $1F // VMOVUPS zmmword ptr [RDI + 1984], ZMM31
  {$ENDIF}
{$ELSEIF Defined(x86)}
    // Win32, Lin32
    DB  $62, $f1, $7c, $48, $11, $00      // VMOVUPS zmmword ptr [EAX + 0000], ZMM0
    DB  $62, $f1, $7c, $48, $11, $48, $01 // VMOVUPS zmmword ptr [EAX + 0064], ZMM1
    DB  $62, $f1, $7c, $48, $11, $50, $02 // VMOVUPS zmmword ptr [EAX + 0128], ZMM2
    DB  $62, $f1, $7c, $48, $11, $58, $03 // VMOVUPS zmmword ptr [EAX + 0192], ZMM3
    DB  $62, $f1, $7c, $48, $11, $60, $04 // VMOVUPS zmmword ptr [EAX + 0256], ZMM4
    DB  $62, $f1, $7c, $48, $11, $68, $05 // VMOVUPS zmmword ptr [EAX + 0320], ZMM5
    DB  $62, $f1, $7c, $48, $11, $70, $06 // VMOVUPS zmmword ptr [EAX + 0384], ZMM6
    DB  $62, $f1, $7c, $48, $11, $78, $07 // VMOVUPS zmmword ptr [EAX + 0448], ZMM7
{$ELSE}
  {$MESSAGE FATAL 'Unsupported architecture.'}
{$IFEND}
end;

//------------------------------------------------------------------------------

procedure GetAVX512MaskRegisters(Mem: Pointer); assembler; register;{$IFDEF FPC} nostackframe; {$ENDIF}
asm
{$IF Defined(x64)}
  {$IFNDEF FPC}.NOFRAME{$ENDIF}
  {$IFDEF Windows}
    // Win64
    DB  $C4, $E1, $F8, $91, $01       // KMOVQ [RCX + 00], K0
    DB  $C4, $E1, $F8, $91, $49, $08  // KMOVQ [RCX + 08], K1
    DB  $C4, $E1, $F8, $91, $51, $10  // KMOVQ [RCX + 16], K2
    DB  $C4, $E1, $F8, $91, $59, $18  // KMOVQ [RCX + 24], K3
    DB  $C4, $E1, $F8, $91, $61, $20  // KMOVQ [RCX + 32], K4
    DB  $C4, $E1, $F8, $91, $69, $28  // KMOVQ [RCX + 40], K5
    DB  $C4, $E1, $F8, $91, $71, $30  // KMOVQ [RCX + 48], K6
    DB  $C4, $E1, $F8, $91, $79, $38  // KMOVQ [RCX + 56], K7
  {$ELSE}
    // Lin64
    DB  $C4, $E1, $F8, $91, $07       // KMOVQ [RDI + O0], K0
    DB  $C4, $E1, $F8, $91, $4F, $08  // KMOVQ [RDI + O8], K1
    DB  $C4, $E1, $F8, $91, $57, $10  // KMOVQ [RDI + 16], K2
    DB  $C4, $E1, $F8, $91, $5F, $18  // KMOVQ [RDI + 24], K3
    DB  $C4, $E1, $F8, $91, $67, $20  // KMOVQ [RDI + 32], K4
    DB  $C4, $E1, $F8, $91, $6F, $28  // KMOVQ [RDI + 40], K5
    DB  $C4, $E1, $F8, $91, $77, $30  // KMOVQ [RDI + 48], K6
    DB  $C4, $E1, $F8, $91, $7F, $38  // KMOVQ [RDI + 56], K7
  {$ENDIF}
{$ELSEIF Defined(x86)}
    // Win32, Lin32
    DB  $C4, $E1, $F8, $91, $00       // KMOVQ [EAX + 00], K0
    DB  $C4, $E1, $F8, $91, $48, $08  // KMOVQ [EAX + 08], K1
    DB  $C4, $E1, $F8, $91, $50, $10  // KMOVQ [EAX + 16], K2
    DB  $C4, $E1, $F8, $91, $58, $18  // KMOVQ [EAX + 24], K3
    DB  $C4, $E1, $F8, $91, $60, $20  // KMOVQ [EAX + 32], K4
    DB  $C4, $E1, $F8, $91, $68, $28  // KMOVQ [EAX + 40], K5
    DB  $C4, $E1, $F8, $91, $70, $30  // KMOVQ [EAX + 48], K6
    DB  $C4, $E1, $F8, $91, $78, $38  // KMOVQ [EAX + 56], K7
{$ELSE}
  {$MESSAGE FATAL 'Unsupported architecture.'}
{$IFEND}
end;

{$IFDEF StackFramesEnabled}
  {$STACKFRAMES ON}
{$ENDIF}

{-------------------------------------------------------------------------------
    EExtendedException - protected methods
-------------------------------------------------------------------------------}

procedure EExtendedException.GetFloatRegisters;
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

//------------------------------------------------------------------------------

procedure EExtendedException.GetFloatVectorRegisters_SSE;
begin
fRegisters.FloatVector.Length := 4;
fRegisters.FloatVector.Count := {$IFDEF 64bit}16{$ELSE}8{$ENDIF};
GetSSERegisters(Addr(fRegisters.FloatVector.Regs));
fRegisters.FloatVector.MXCSR := GetMXCSR;
end;

//------------------------------------------------------------------------------

procedure EExtendedException.GetFloatVectorRegisters_AVX;
begin
fRegisters.FloatVector.Length := 8;
fRegisters.FloatVector.Count := {$IFDEF 64bit}16{$ELSE}8{$ENDIF};
GetAVXRegisters(Addr(fRegisters.FloatVector.Regs));
fRegisters.FloatVector.MXCSR := GetMXCSR;
end;

//------------------------------------------------------------------------------

procedure EExtendedException.GetFloatVectorRegisters_AVX512;
begin
fRegisters.FloatVector.Length := 16;
fRegisters.FloatVector.Count := {$IFDEF 64bit}32{$ELSE}8{$ENDIF};
GetAVX512Registers(Addr(fRegisters.FloatVector.Regs));
fRegisters.FloatVector.MXCSR := GetMXCSR;
GetAVX512MaskRegisters(Addr(fRegisters.FloatVector.OPMask));
end;

{-------------------------------------------------------------------------------
    EExtendedException - public methods
-------------------------------------------------------------------------------}

constructor EExtendedException.CreateFmt(const Msg: String; Args: array of const);
begin                  
inherited CreateFmt(Msg,Args);
FillChar(fRegisters,SizeOf(TRegisters),0);
// get basic registers (I know they are now polluted by arguments and other things, but to be complete)
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
fRegisters.Basic.InstructionPointer := TNativeRegister(GetInstructionPointer);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
GetGeneralPurposeRegisters(Addr(fRegisters.Basic.GeneralPurpose));
fRegisters.Basic.Flags := GetFLAGSRegister;
GetSegmentRegisters(Addr(fRegisters.Basic.Segment));
Include(fRegisters.PresentRegs,prBasic);
with TSimpleCPUID.Create do
try
  If Info.SupportedExtensions.X87 then
    begin
      // get FPU registers
      GetFloatRegisters;
      Include(fRegisters.PresentRegs,prFPU);
    end;
  If Info.SupportedExtensions.MMX then
    begin
      // get MMX registers
      GetMMXRegisters(Addr(fRegisters.IntegerVector.MM));
      Include(fRegisters.PresentRegs,prMMX);
    end;
  If Info.SupportedExtensions.AVX512F then
    begin
      // AVX512 is supported => AVX and SSE registers are available too
      GetFloatVectorRegisters_AVX512;
      Include(fRegisters.PresentRegs,prSSE);
      Include(fRegisters.PresentRegs,prAVX);
      Include(fRegisters.PresentRegs,prAVX512);
    end
  else If Info.SupportedExtensions.AVX then
    begin
      // AVX is supported => SSE registers are available too
      GetFloatVectorRegisters_AVX;
      Include(fRegisters.PresentRegs,prSSE);
      Include(fRegisters.PresentRegs,prAVX);
    end
  else If Info.SupportedExtensions.SSE then
    begin
      GetFloatVectorRegisters_SSE;
      Include(fRegisters.PresentRegs,prSSE);
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
