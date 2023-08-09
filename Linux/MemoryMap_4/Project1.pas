program Project1;

uses
  Strings,
  BaseUnix,
  Unix,
  confname  ;

function sysconf(__name:cint):cint; cdecl; external 'c';


  procedure errx(const s: string);
  begin
    WriteLn(s);
    Halt(1);
  end;


  function main: cint;
  var
    fd: cint;
    sb: stat;
    offset: culong=3;
    len:SizeInt=10;
    pa_offset: culong;
  begin
    fd := FpOpen('Project1.pas', O_RDONLY);
    if fd = -1 then begin
      errx('open');
    end;

    if FpFStat(fd, sb) = -1 then begin
      errx('fstat');
    end;

    offset:=0;
    pa_offset:=offset and not (sysconf(_SC_PAGESIZE)-1);

    if offset>=sb.st_size then errx('Versatz ist hinter dem Dateiende');





    FpClose(fd);
    Result := 0;
  end;

begin
  main;
end.
