program Project1;

uses
//  libc,
  initc,
  ctypes;


// /home/tux/fpcupdeluxe_stable/fpcsrc/packages/libc/src/bconfnameh.inc

  function sysconf(i: cint): clong; cdecl; external Name 'sysconf';

begin
  writeln('We have ', sysconf(83), ' cores'); //83 should be beautified into a nice const      ( _SC_NPROCESSORS_ONLN )
end.
