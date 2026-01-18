program project1;

type
  TTimeVal = record
    tv_sec: int64;
    tv_usec: int64;
  end;
  PTimeVal = ^TTimeVal;

  // /usr/include/x86_64-linux-gnu/asm/unistd_64.h

  {$asmmode intel}

  function GetTime(tv: PTimeVal): int64; assembler; nostackframe;
  asm
           Mov     Rax, 96
           Mov     Rdi, tv
           Xor     Rsi, Rsi
           Syscall
  end;


  function Print(c: pchar; len: int64): int64; assembler; nostackframe;
  asm
           Mov     Rax, 1
           Mov     Rdx, len
           Mov     Rsi, c
           Mov     Rdi, 1
           Syscall
  end;

  procedure main;
  var
    start, ende: TTimeVal;
    c: double;
    i: integer;
  var
    demo: pchar = 'syscall demo'#10;
  begin
    Print(demo, Length(demo));

    GetTime(@start);

    for i := 0 to 1000000 do begin
      c := cos(c);
    end;

    GetTime(@ende);

    WriteLn('Rechenzeit: ', ende.tv_usec - start.tv_usec);
  end;

begin
  main;
end.
