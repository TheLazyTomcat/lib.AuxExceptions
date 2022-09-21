unit AuxExceptions;

{$DEFINE AllowExtendedException}

{$DEFINE AE_Include_Defs}
  {$INCLUDE '.\AuxExceptions.inc'}
{$UNDEF AE_Include_Defs}

interface

{$DEFINE AE_Include_Interface_Uses}
  {$INCLUDE '.\AuxExceptions.inc'}
{$UNDEF AE_Include_Interface_Uses}

type
  EBaseException = class(Exception);

  EAEBaseException = EBaseException;

{$DEFINE AE_Include_Interface}
  {$INCLUDE '.\AuxExceptions.inc'}
{$UNDEF AE_Include_Interface}

type
  EGeneralException     = EAEGeneralException;
  ESystemError          = EAESystemError;
  EIndexException       = EAEIndexException;
  EIndexOutOfBounds     = EAEIndexOutOfBounds;
  EIndexTooLow          = EAEIndexTooLow;
  EIndexTooHigh         = EAEIndexTooHigh;
  EIndexInvalid         = EAEIndexInvalid;
  EValueException       = EAEValueException;
  EValueInvalid         = EAEValueInvalid;
  EValueInvalidNameOnly = EAEValueInvalidNameOnly;

implementation

{$DEFINE AE_Include_Implementation_Uses}
  {$INCLUDE '.\AuxExceptions.inc'}
{$UNDEF AE_Include_Implementation_Uses}

{$DEFINE AE_Include_Implementation}
  {$INCLUDE '.\AuxExceptions.inc'}
{$UNDEF AE_Include_Implementation}

{$DEFINE AE_Include_UnitInitFinal}
  {$INCLUDE '.\AuxExceptions.inc'}
{$UNDEF AE_Include_UnitInitFinal}

end.
