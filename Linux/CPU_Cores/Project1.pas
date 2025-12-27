program Project1;

  function sysconf(i: Integer): Int64; cdecl; external 'c';

begin
  writeln('We have ', sysconf(83), ' cores');
end.
