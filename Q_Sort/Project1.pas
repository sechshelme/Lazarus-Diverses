program Project1;

  procedure QuickSort(var ia: array of integer; ALo, AHi: integer);
  var
    Lo, Hi, Pivot, T: integer;
  begin
    Lo := ALo;
    Hi := AHi;
    Pivot := ia[(Lo + Hi) div 2];
    repeat
      while ia[Lo] < Pivot do begin
        Inc(Lo);
      end;
      while ia[Hi] > Pivot do begin
        Dec(Hi);
      end;
      if Lo <= Hi then begin
        T := ia[Lo];
        ia[Lo] := ia[Hi];
        ia[Hi] := T;
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    if Hi > ALo then begin
      QuickSort(ia, ALo, Hi);
    end;
    if Lo < AHi then begin
      QuickSort(ia, Lo, AHi);
    end;
  end;

var
  ia: array of integer;
  i: integer;
  Len: integer;
begin
  Randomize;
  Len := 2000;
  SetLength(ia, Len);
  for i := 0 to Len - 1 do begin
    ia[i] := Random(len);
  end;

  QuickSort(ia, 0, Len - 1);

  for i := 0 to Len - 1 do begin
    Writeln(i:8, '.', ia[i]: 8);
  end;
  Readln;
end.
































































