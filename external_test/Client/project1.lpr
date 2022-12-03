program project1;

//{$L '/n4800/DATEN/Programmierung/mit_GIT/Lazarus/Diverses/external_test/liberty/libmylib.so'}

procedure Ausgabe(s:String); cdecl; external '/n4800/DATEN/Programmierung/mit_GIT/Lazarus/Diverses/external_test/liberty/libmylib.so';
begin
  WriteLn('Hello World');
  Ausgabe('Hello World');
end.
