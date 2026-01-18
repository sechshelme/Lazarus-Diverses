program project1;

const
  {$IFDEF linux}
  libc = 'c';
  {$ENDIF}

  {$IFDEF windows}
  libc = 'msvcrt.dll';
  {$ENDIF}


type
  PFILE = Pointer;

  function fprintf(stream: PFILE; fmt: pchar): integer; cdecl; varargs; external libc;

  {$IFDEF linux}
var
  stdout: PFILE; cvar; external libc;
  {$ENDIF}

  {$IFDEF windows}
  function __iob_func: PFILE; cdecl; external libc;

  function stdout: PFILE; inline;
  begin
    Result := __iob_func + 48;
  end;
  {$ENDIF}

  procedure main;
  const
    zahl: integer = 42;
    text: pchar = 'Hello World';
  begin
    fprintf(stdout, 'Text: %s'#10, text);
    fprintf(stdout, 'Zahl: %d'#10, zahl);
  end;

begin
  main;
end.
