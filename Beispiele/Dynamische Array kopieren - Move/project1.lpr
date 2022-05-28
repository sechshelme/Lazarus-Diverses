program project1;

type
  TIntArray = array of integer;
var
  q, z1, z2: TIntArray;
  i: integer;


  procedure CopyArray(a: TIntArray);
  begin
    Move(a[0], z2[0], Length(a) * SizeOf(integer));
  end;

begin
  SetLength(q, 10);
  SetLength(z1, 10);
  SetLength(z2, 10);
  for i := 0 to Length(q) - 1 do begin
    q[i] := i;
  end;

  CopyArray(q);

  Move(q[0], z1[0], Length(q) * SizeOf(integer));

  for i := 0 to Length(q) - 1 do begin
    z2[i]:=0;
    Write(q[i], '-', z1[i], '-');
    WriteLn(z2[i]);
  end;
  ReadLn;
end.
