program Project1;

uses
  Strings,
  BaseUnix,
  Unix;

procedure errx(const s:String);
begin
  WriteLn(s);
  Halt(1);
  end;


  function main:cint;
  var
    parpid, childpid: TPid;
    fd: cint = -1;
    zero, anon: PChar;
    str1: PChar = 'string 1';
    str2: PChar = 'string 2';
  begin
    parpid := FpGetpid;
    childpid:=0;
    fd := FpOpen('/dev/zero', O_RDWR, 0);
    if fd = -1 then begin
      errx('Konnte Datei nicht öffnen');
    end;
    anon := Fpmmap(nil, 4096, PROT_READ or PROT_WRITE, MAP_ANON or MAP_SHARED, -1, 0);
    zero := Fpmmap(nil, 4096, PROT_READ or PROT_WRITE, MAP_SHARED, fd, 0);

    if (anon = MAP_FAILED) or (zero = MAP_FAILED) then begin
      errx('either mmap');
    end;

    strcopy(anon, str1);
    strcopy(zero, str1);

    WriteLn('PID ', parpid, ':   anonymous ', anon, ', zero-backed ', zero);
    if childpid = FpFork then begin
      errx('fork');
    end else begin
      childpid := FpGetpid;
      WriteLn('PID ', childpid, ':   anonymous ', anon, ', zero-backed ', zero);
      FpSleep(3);
      WriteLn('PID ', childpid, ':   anonymous ', anon, ', zero-backed ', zero);
      Fpmunmap(anon, 4096);
      Fpmunmap(zero, 4096);
      FpClose(fd);
      exit(0);
    end;

    FpSleep(2);
    strcopy(anon, str2);
    strcopy(zero, str2);

    WriteLn('PID ', parpid, ':   anonymous ', anon, ', zero-backed ', zero);
    Fpmunmap(anon, 4096);
    Fpmunmap(zero, 4096);
    FpClose(fd);
    exit(0);
  end;

begin
  main;
end.
