program project1;

uses
  xevent_type;

var
  i: integer;
begin
  for i := 0 to 40 do begin
    WriteLn(getEventType(i));
  end;
end.
