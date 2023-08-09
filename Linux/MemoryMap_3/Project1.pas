program Project1;

uses
  Strings,
  BaseUnix,
  Unix;

  procedure errx(const s: string);
  begin
    WriteLn(s);
    Halt(1);
  end;


  function main: cint;
  var
    parpid, childpid: TPid;
    fd: cint = -1;
    zero, anon: PChar;
    str1: PChar = 'string 1';
    str2: PChar = 'string 2';
    i: integer;
  begin
    parpid := FpGetpid;
    childpid := 0;
    fd := FpOpen('/dev/stdout', O_RDWR, 0);
    if fd = -1 then begin
      errx('Konnte Datei nicht öffnen');
    end;
    zero := nil;
    anon := nil;

    anon := Fpmmap(nil, 4096, PROT_READ or PROT_WRITE, MAP_ANON or MAP_SHARED, fd, 0);
    zero := Fpmmap(nil, 4096, PROT_READ or PROT_WRITE, MAP_ANON or MAP_SHARED, fd, 0);

    for i := 0 to 20 do begin
      anon[i] := char(byte('A') + i);
    end;

    Write('anon: ');
    for i := 0 to 20 do begin
      Write(anon[i]);
    end;
    WriteLn();
    WriteLn();


    Write('zero: ');
    for i := 0 to 20 do begin
      Write(zero[i]);
    end;
    WriteLn();
    WriteLn();
    WriteLn();
    WriteLn();
    WriteLn();

    strcopy(zero, str2);

    Fpmunmap(anon, 4096);
    Fpmunmap(zero, 4096);
    FpClose(fd);
    exit(0);
  end;

begin
  main;
end.
