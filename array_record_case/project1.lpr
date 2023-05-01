program project1;

type
  TArg = record
    Name: PChar;
    case byte of
      0: (valueP: PChar);
      1: (valueI: PtrUInt);
      2: (valueF: Single);
  end;
  TArgs = array of TArg;

var
  Arg: TArgs = (
    (Name: 'PChar';   valueP: 'Ich bin ein PChar'),
    (Name: 'Single';  valueF: 12.45),
    (Name: 'PtrUInt'; valueI: 1234));


begin
end.
