program project1;

type
  TTimeVal = record
    tv_sec: int64;
    tv_usec: int64;
  end;

// /usr/include/x86_64-linux-gnu/asm/unistd_64.h

  {$asmmode intel}

  function ManualGetTimeOfDay(var tv: TTimeVal): int64; assembler; nostackframe;
  asm
           Mov     Rax, 96      // Syscall-Nummer 96 (sys_gettimeofday)
           Mov     Rdi, tv      // Erster Parameter: Zeiger auf die Struktur
           Xor     Rsi, Rsi     // Zweiter Parameter: Zeitzone (NULL/0)
           Syscall          // Den Kernel aufrufen
  end;



  procedure main;
  var
    start, ende: TTimeVal;
    c: double;
    i: integer;
  begin

    ManualGetTimeOfDay(start);

    for i := 0 to 1000000 do begin
      c := cos(c);
    end;

    ManualGetTimeOfDay(ende);

    WriteLn('Rechenzeit: ', ende.tv_usec - start.tv_usec);
  end;

begin
  main;
end.
