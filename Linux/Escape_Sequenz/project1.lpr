program project1;

//uses
//  crt;

begin
//  TextAttr := 5;
//  GotoXY(35, 12);
  WriteLn('Hallo World');


//  WriteLn(#27'[6n'#27'[H'#27'[m'#27'[12;35H'#27'[35mHello World');

  WriteLn(#27'[12;35H'#27'[32mHello World');

  //  WriteLn(#27'[6n'#27'[H'#27'[m'#27'[35mHello World');

end.

/// [6n[H[m[35mHallo

// [6n[H[m[12;35H[35mHallo World
