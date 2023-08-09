program Project1;

uses
  ctypes,
  Strings,
  BaseUnix,
  Unix,
  confname;

// https://www.ibm.com/docs/en/i/7.4?topic=ssw_ibm_i_74/apis/sysconf.html

function sysconf(__name:cint):cint; cdecl; external 'c';


  function main: cint;
  begin
    WriteLn('_SC_ARG_MAX: ',sysconf(_SC_ARG_MAX));
    WriteLn('_SC_CHILD_MAX: ',sysconf(_SC_CHILD_MAX));
    WriteLn('_SC_PAGESIZE: ',sysconf(_SC_PAGESIZE));
    WriteLn('_SC_VERSION: ',sysconf(_SC_VERSION));
    Result:=0;
  end;

begin
  main;
end.
