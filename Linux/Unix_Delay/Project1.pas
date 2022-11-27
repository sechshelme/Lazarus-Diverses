program Project1;

{ Program to demonstrate the NanoSleep function. }
// https://www.freepascal.org/docs-html/rtl/baseunix/fpnanosleep.html

uses
  BaseUnix;

var
  Req, Rem: TimeSpec;
  Res: longint;

begin
  with Req do begin
    tv_sec := 10;
    tv_nsec := 100;
  end;
  Write('NanoSleep returned : ');
  Flush(Output);
  Res := (fpNanoSleep(@Req, @rem));
  Writeln(res);
  if (res <> 0) then begin
    with rem do begin
      Writeln('Remaining seconds     : ', tv_sec);
      Writeln('Remaining nanoseconds : ', tv_nsec);
    end;
  end;
end.
