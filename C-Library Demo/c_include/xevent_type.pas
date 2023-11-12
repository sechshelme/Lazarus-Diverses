
unit xevent_type;

interface

{$L xevent_type.o}
{$LinkLib c}

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

function getEventType(event: longint): PChar; cdecl; external;

implementation

end.
