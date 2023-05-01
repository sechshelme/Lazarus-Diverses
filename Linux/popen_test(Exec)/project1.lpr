program project1;

uses
  Unix;
var
  f: Text;
  s: string;
  i: integer = 1;

begin
  popen(f, 'ls /home/tux  --color', 'R');

  while not EOF(f) do begin
    ReadLn(f, s);
    WriteLn(i: 4, ': ', s);
//    Write(s,#9);
    Inc(i);
  end;

  PClose(f);
end.
