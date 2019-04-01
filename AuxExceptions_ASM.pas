{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  AuxExceptions - auxiliary assembly routines

  ©František Milt 2019-04-01

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

  SimpleCPUID is required only when symbol PurePascal is not defined and symbol
  EnableExtendedException is defined.

===============================================================================}
unit AuxExceptions_ASM;

{$INCLUDE '.\AuxExceptions_defs.inc'}

interface

{$IFNDEF PurePascal}

uses
  AuxTypes;

Function GetInstructionPointer: Pointer; assembler; register;
procedure GetGeneralPurposeRegisters(Mem: Pointer); assembler; register;
Function GetFLAGSRegister: PtrUInt; assembler; register;
procedure GetSegmentRegisters(Mem: Pointer); assembler; register;
procedure Getx87Registers(Mem: Pointer); assembler; register;
procedure Float80toFloat64(F80,F64: Pointer); assembler; register;
procedure GetMMXRegisters(Mem: Pointer); assembler; register;
Function GetMXCSR: UInt32; assembler; register;
procedure GetSSERegisters(Mem: Pointer); assembler; register;
procedure GetAVXRegisters(Mem: Pointer); assembler; register;
procedure GetAVX512Registers(Mem: Pointer); assembler; register;
procedure GetAVX512MaskRegisters(Mem: Pointer); assembler; register;

{$ENDIF}

implementation

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$PUSH}{$WARN 2005 OFF}
  {$IF Defined(FPC) and (FPC_FULLVERSION >= 30000)}
    {$DEFINE W7122:={$WARN 7122 OFF}} // Warning: Check size of memory operand "op: memory-operand-size is X bits, but expected [Y bits + Z byte offset]"
  {$ELSE}
    {$DEFINE W7122:=}
  {$IFEND}
  {$POP}
{$ENDIF}

{$IFNDEF PurePascal}  // must be here, not before warns disable

{$IFOPT W+}
  {$DEFINE AE_StackFramesEnabled}
{$ELSE}
  {$UNDEF AE_StackFramesEnabled}
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

{$IFDEF AE_StackFramesEnabled}
  {$STACKFRAMES ON}
{$ENDIF}

{$ENDIF}

end.

