program Project1;

uses
  BaseUnix,
  Unix;

var
  S: string;
  fd: cint;
  //    args : tmmapargs;
  P: PChar;
  i: Integer;

begin

  fd := fpOpen('Project1.pas', O_rdOnly);
//  fd := fpOpen('/dev/stdout', O_rdOnly);
  if fd = -1 then begin
    WriteLn('Fehler: Open');
  end;
  P := PChar(fpmmap(nil, 10, PROT_READ or PROT_WRITE, MAP_PRIVATE, fd, 0));

  if PtrUInt(P) = -1 then begin
    WriteLn('Fehler');
  end;
  Writeln('Read in memory  :'#10);

  for i:= 0 to 20 do Write(p[i]);


  fpclose(fd);
  if fpMUnMap(P, 10) <> 0 then begin
    Halt(fpgeterrno);
  end;


  WriteLn('ende');
end.
