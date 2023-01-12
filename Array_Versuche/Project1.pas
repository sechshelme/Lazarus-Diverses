program Project1;

type
  TByteArray = array of byte;

var
  staticArray: array[0..5] of byte;
  dynamicArray: array of byte;
  i: integer;


  procedure Test1(a: array of byte);
  var
    i: integer;
  begin
//    SetLength(a, 8);
    for i := 0 to Length(a) - 1 do begin
      a[i] := i + 10;
    end;
  end;

  procedure Test2(var a: array of byte);
  var
    i: integer;
  begin
//    SetLength(a, 8);
    for i := 0 to Length(a) - 1 do begin
      a[i] := i + 10;
    end;
  end;

  procedure Test3(a: TByteArray);
  var
    i: integer;
  begin
    SetLength(a, 8);
    for i := 0 to Length(a) - 1 do begin
      a[i] := i + 20;
    end;
  end;

  procedure Test4(var a: TByteArray);
  var
    i: integer;
  begin
    SetLength(a, 8);
    for i := 0 to Length(a) - 1 do begin
      a[i] := i + 30;
    end;
  end;

begin
  // --- Inizialisieren

  for i := 0 to Length(staticArray) - 1 do begin
    staticArray[i] := i;
  end;

  SetLength(dynamicArray, 6);
  for i := 0 to Length(dynamicArray) - 1 do begin
    dynamicArray[i] := i;
  end;

  /// --- Tests
  WriteLn('--- Static ---');

  Test1(staticArray);
  for i := 0 to Length(staticArray) - 1 do begin
    WriteLn(staticArray[i]);
  end;
  WriteLn('--------');

  Test2(staticArray);
  for i := 0 to Length(staticArray) - 1 do begin
    WriteLn(staticArray[i]);
  end;
  WriteLn('--------');

  Test3(staticArray);
  for i := 0 to Length(staticArray) - 1 do begin
    WriteLn(staticArray[i]);
  end;

  //Test4(staticArray);
  //for i := 0 to Length(staticArray) - 1 do begin
  //  WriteLn(staticArray[i]);
  //end;

  WriteLn('--- Dynamisch ---');
  Test1(dynamicArray);
  for i := 0 to Length(dynamicArray) - 1 do begin
    WriteLn(dynamicArray[i]);
  end;
  WriteLn('--------');

  Test2(dynamicArray);
  for i := 0 to Length(dynamicArray) - 1 do begin
    WriteLn(dynamicArray[i]);
  end;
  WriteLn('--------');

  Test3(dynamicArray);
  for i := 0 to Length(dynamicArray) - 1 do begin
    WriteLn(dynamicArray[i]);
  end;
  WriteLn('--------');

  Test4(dynamicArray);
  for i := 0 to Length(dynamicArray) - 1 do begin
    WriteLn(dynamicArray[i]);
  end;
  WriteLn('--------');
end.
