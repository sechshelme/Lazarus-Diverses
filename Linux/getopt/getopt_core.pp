unit getopt_core;

interface

const
  lib_stdio = 'c';

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

const
  no_argument = 0;
  required_argument = 1;
  optional_argument = 2;

type
  Poption = ^Toption;

  Toption = record
    Name: PChar;
    has_arg: longint;
    flag: Plongint;
    val: longint;
  end;

var
  optarg: PChar; cvar;external;
  optind: longint; cvar;external;
  opterr: longint; cvar;external;
  optopt: longint; cvar;external;

// getopt_core.h
function getopt(___argc: longint; ___argv: PPchar; __shortopts: PChar): longint; cdecl; external lib_stdio;

// getopt_ext.h
function getopt_long(___argc: longint; ___argv: PPchar; __shortopts: PChar; __longopts: Poption; __longind: Plongint): longint; cdecl; external;
function getopt_long_only(___argc: longint; ___argv: PPchar; __shortopts: PChar; __longopts: Poption; __longind: Plongint): longint; cdecl; external;

implementation

end.
