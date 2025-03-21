program project1;

uses
  SysUtils,
  unix,
  BaseUnix;

  procedure Test(path: PChar);
  var
    filestat: Stat;
  begin
    if fpLstat(path, @filestat) = -1 then begin
      WriteLn('lstat');
    end;
    if fpS_ISREG(filestat.st_mode) then begin
      WriteLn('normale Datei');
    end else if fpS_ISDIR(filestat.st_mode) then begin
      WriteLn('Ordner');
    end else if fpS_ISLNK(filestat.st_mode) then begin
      WriteLn('S-Link');
    end else begin
      WriteLn('Anderer Typ');
    end;
  end;

begin
  Test('/home/tux/Schreibtisch/filestest/main.c');   // nornal
  Test('/home/tux/Schreibtisch/filestest/test');     // Ordner
  Test('/home/tux/Schreibtisch/filestest/test.c');   // Link
end.
