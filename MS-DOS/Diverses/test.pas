program test;
{$ASMMODE   intel}
uses
  Crt, ports;
begin
  asm
    mov ax,$13
    int $10
  end;
  repeat
    Mem[$A000:random(64000)] := random(256);
  until KeyPressed;
end.
