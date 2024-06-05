program project1;

  procedure GetListe(list: PPPChar);
  const
    Data: array of string = ('1', '12', '123', '1234', '12345', 'a', 'ab', 'abc', 'abcd');
  var
    i: integer;
  begin
    Getmem(list^, SizeOf(PPChar) * Length(Data) + 1);
    for i := 0 to Length(Data) - 1 do begin
      Getmem(list^[i], SizeOf(PPChar) * Length(Data[i]) + 1);
      Move(Data[i, 1], list^[i]^, Length(Data[i]));
      list^[i, Length(Data[i])] := #0;
    end;
    list^[Length(Data)] := nil;
  end;

var
  list: PPChar = nil;
  index: integer = 0;

begin
  GetListe(@list);

  while list[index] <> nil do begin
    WriteLn(index: 3, ': ', list[index]);
    Inc(index);
  end;
end.
