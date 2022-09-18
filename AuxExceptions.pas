unit AuxExceptions;
{$DEFINE AE_Include_Defs}
  {$INCLUDE '.\AuxExceptions.inc'}
{$UNDEF AE_Include_Defs}

{$DEFINE ExtendedException}

interface

{$DEFINE AE_Include_Interface_Uses}
  {$INCLUDE '.\AuxExceptions.inc'}
{$UNDEF AE_Include_Interface_Uses}

type
  EBaseException = class(Exception);

  EAEBaseException = class(EBaseException);

{$DEFINE AE_Include_Interface}
  {$INCLUDE '.\AuxExceptions.inc'}
{$UNDEF AE_Include_Interface}

implementation

{$DEFINE AE_Include_Implementation_Uses}
  {$INCLUDE '.\AuxExceptions.inc'}
{$UNDEF AE_Include_Implementation_Uses}

{$DEFINE AE_Include_Implementation}
  {$INCLUDE '.\AuxExceptions.inc'}
{$UNDEF AE_Include_Implementation}

end.








