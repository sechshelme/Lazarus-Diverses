program project1;

uses
  DynLibs;

var
  LibHandle: TLibHandle;
  printf: function(format: PChar): integer; varargs; cdecl;

begin
  LibHandle := LoadLibrary('libc.so.6');

  if LibHandle = NilHandle then begin
    Writeln('Konnte Bibliothek nicht laden.');
    Halt(1);
  end;

  Pointer(printf) := GetProcAddress(LibHandle, 'printf');
  if printf = nil then begin
    Writeln('Konnte printf nicht finden.');
    UnloadLibrary(LibHandle);
    Halt(1);
  end;

  printf(#10#10'String: %s     Zahl: %d'#10#10, 'abc', 123);

  UnloadLibrary(LibHandle);
end.
