program project1;

uses
  Crt;

var
  ch: char;
  i: integer;
  Feld: array[0..150] of integer;
  f: array[1..7] of byte;
  gewonnen: boolean;
  s: string[10];

const
  Anz_Minen: integer = 1;
  menupos: integer = 1;
  ae = #132;
  oe = #148;
  ue = #129;
  MenuPkt: array[1..5] of string =
    ('      Spiel        ',
    '      Hilfe        ',
    '   Minen erh' + oe + 'hen   ',
    ' Minen verkleinern ',
    '   Spiel beenden   ');

  procedure OutTextXY(x, y: integer; s: string); inline;
  begin
    GotoXY(x, y);
    Write(s);
  end;

  procedure Color(vg, hg: byte); inline;
  begin
    Textattr := vg + hg shl 4;
  end;

  procedure ZKette(Zeichen: char; anz: byte); inline;
  begin
    Write(StringOfChar(Zeichen, anz));
  end;


  function Menu(Startpos, anz: integer): integer;
  var
    y: integer;
    ch: char;
    Ende: boolean;
  begin
    Ende := False;
    repeat
      for y := 1 to anz do begin
        if y <> Startpos then begin
          Color(f[7] + 8, 0);
        end else begin
          Color(1, f[7]);
        end;
        GotoXY(30, y + 10);
        Write(Menupkt[y]);
      end;
      ch := ReadKey;
      if ch = #0 then begin
        ch := ReadKey;
        case ch of
          #71: begin
            Startpos := 1;
          end;
          #79: begin
            Startpos := anz;
          end;
          #72: begin
            if Startpos > 1 then begin
              Dec(Startpos);
            end else begin
              Startpos := anz;
            end;
          end;
          #80: begin
            if Startpos < anz then begin
              Inc(Startpos);
            end else begin
              Startpos := 1;
            end;
          end;
        end;
      end else begin
        case ch of
          #13: begin
            Ende := True;
          end;
        end;
      end;
    until ch = #13;
    Menu := Startpos;
  end;

  procedure Hilfe; { --- Hilfe, Spielanleitung --- }
  var
    ch: char;
  begin
    Color(f[7], 0);
    GotoXY(60, 20);
    ZKette(' ', 20);
    OutTextXY(10, 10, #218 + #196 + ' Hilfe ');
    ZKette(#196, 50);
    Write(#191);
    for i := 11 to 23 do begin
      OutTextXY(10, i, #179);
      OutTextXY(69, i, #179);
    end;
    GotoXY(10, 24);
    Write(#192);
    ZKette(#196, 58);
    Write(#217);
    Color(f[2], 0);
    GotoXY(21, 11);
    ZKette(' ', 38);
    OutTextXY(30, 12, '   Spielanleitung');
    OutTextXY(30, 13, '   ==============   ');
    OutTextXY(23, 14, 'Mit Nummerblock den Cursor bewegen');
    OutTextXY(30, 15, '  (7,8,9,4,6,1,2,3)     ');
    OutTextXY(16, 16, 'Ziel des Spieles ist den Cursor von der oberen-');
    OutTextXY(16, 17, 'linken Ecke in die rechte-untere Ecke zu bringen');
    OutTextXY(26, 18, 'ohne in eine Mine zu fahren.');
    OutTextXY(16, 19, 'Die roten Ziffern zeigen an mit wie vielen Minen,');
    OutTextXY(31, 20, 'man Beruehrung hat.');
    OutTextXY(34, 22, 'Viel Glueck');
    ch := ReadKey;
    if ch = #0 then begin
      ch := ReadKey;
    end;
  end;

  procedure Minen_anzeigen;
  var
    i, x, y: integer;
  begin
    Color(f[7], f[4]);
    for i := 1 to 150 do begin
      if Feld[i] <> 0 then begin
        y := i div 15 + 1;
        x := i - (y - 1) * 15;
        if x = 0 then begin
          x := 15;
          Dec(y);
        end;
        GotoXY(2 + x * 4, 2 + y * 2);
        Write('   ');
      end;
    end;
    if gewonnen then begin
      gewonnen := False;
    end;
    ch := ReadKey;
    if ch = #0 then begin
      ch := ReadKey;
    end;
  end;

  procedure Spielen;
  var
    fe, mine, i, j, x, y, x1, y1: integer;
  begin
    Color(f[2], 0);
    ClrScr;
    OutTextXY(20, 1, '* Landmine *');
    OutTextXY(60, 1, 'Anzahl Minen <   >');
    Color(f[7], 0);
    GotoXY(74, 1);
    Write(anz_Minen: 3);
    GotoXY(1, 2);
    Color(f[3], 0);
    ZKette(#196, 80);
    Color(f[1], 0);
    OutTextXY(5, 3, #218#196#196#196);
    for i := 1 to 14 do begin
      Write(#194#196#196#196);
    end;
    Write(#191);
    for i := 1 to 10 do begin
      GotoXY(4, i * 2 + 3 - 1);
      for j := 1 to 16 do begin
        Write(' '#179'  ');
      end;
      if i < 10 then begin
        OutTextXY(5, i * 2 - 1 + 4, #195);
        for j := 1 to 14 do begin
          Write(#196#196#196#197);
        end;
        Write(#196#196#196#180);
      end;
    end;
    OutTextXY(5, 23, #192#196#196#196);
    for i := 1 to 14 do begin
      Write(#193#196#196#196);
    end;
    Write(#217);
    Color(f[5], 0);
    OutTextXY(6, 4, '   ');
    Color(f[7], 0);
    OutTextXY(7, 4, '0');
    OutTextXY(63, 22, '*');
    Color(f[7], 0);
    OutTextXY(1, 25, '<ESC>= abbrechen');
    Color(f[2], 0);
    OutTextXY(35, 25, 'Copyright 1998, 2014 by Mathias Burkhard');

    { --- Minen definieren --- }

    for i := 1 to 150 do begin
      Feld[i] := 0;
    end;
    for i := 1 to anz_Minen do begin
      repeat
        j := Random(150);
      until not (j in [1, 2, 16, 17, 134, 135, 149, 150]) and (Feld[j] = 0);
      Feld[j] := 1;
    end;
    x := 1;
    y := 1;
    fe := 1;
    repeat
      ch := ReadKey;
      if ch = #0 then begin
        ch := ReadKey;
        if ch = #27 then begin
          ch := #0;
        end;
      end;
      mine := 0;
      x1 := x;
      y1 := y;
      case ch of
        #27: begin
          Exit;
        end;
        '1': begin
          if (x <> 1) and (y <> 10) then begin
            Dec(x);
            Inc(y);
            Inc(fe, 14);
          end;
        end;
        '2', #80: begin
          if y <> 10 then begin
            Inc(y);
            Inc(fe, 15);
          end;
        end;
        '3': begin
          if (x <> 15) and (y <> 10) then begin
            Inc(x);
            Inc(y);
            Inc(fe, 16);
          end;
        end;
        '4', #75: begin
          if x <> 1 then begin
            Dec(x);
            Dec(fe);
          end;
        end;
        '6', #77: begin
          if x <> 15 then begin
            Inc(x);
            Inc(fe);
          end;
        end;
        '7': begin
          if (x <> 1) and (y <> 1) then begin
            Dec(x);
            Dec(y);
            Dec(fe, 16);
          end;
        end;
        '8', #72: begin
          if y <> 1 then begin
            Dec(y);
            Dec(fe, 15);
          end;
        end;
        '9': begin
          if (x <> 15) and (y <> 1) then begin
            Inc(x);
            Dec(y);
            Dec(fe, 15);
          end;
        end;
        else begin
          Write(^g);
        end;
      end;

      if Feld[fe] = 1 then begin  { Mine gefangen }
        Color(f[7], 0);
        OutTextXY(1, 25, 'Mine gefangen         ');
        Minen_anzeigen;
        Exit;
      end;
      if (x = 15) and (y = 10) then begin { --- Spiel gewonnen --- }
        Color(f[7], 0);
        OutTextXY(1, 25, 'Sie haben gewonnen');
        gewonnen := True;
        Inc(anz_Minen);
        Minen_anzeigen;
        Exit;
      end;
      Mine := 0;
      if (x <> 1) and (y <> 1) and (Feld[fe - 16] = 1) then begin
        Inc(Mine);
      end;
      if (y <> 1) and (Feld[fe - 15] = 1) then begin
        Inc(Mine);
      end;
      if (x <> 15) and (y <> 1) and (Feld[fe - 14] = 1) then begin
        Inc(Mine);
      end;
      if (x <> 1) and (Feld[fe - 1] = 1) then begin
        Inc(Mine);
      end;
      if (x <> 15) and (Feld[fe + 1] = 1) then begin
        Inc(Mine);
      end;
      if (x <> 1) and (y <> 10) and (Feld[fe + 14] = 1) then begin
        Inc(Mine);
      end;
      if (y <> 10) and (Feld[fe + 15] = 1) then begin
        Inc(Mine);
      end;
      if (x <> 15) and (y <> 10) and (Feld[fe + 16] = 1) then begin
        Inc(Mine);
      end;
      ;
      Color(f[7], 0);
      OutTextXY(2 + x1 * 4, y1 * 2 + 2, ' ');
      OutTextXY(4 + x1 * 4, y1 * 2 + 2, ' ');
      if Mine = 0 then begin
        Color(f[7], 0);
      end else begin
        Color(f[4], 0);
      end;
      GotoXY(3 + x * 4, y * 2 + 2);
      Write(Mine);
      Color(f[7], f[5]);
      OutTextXY(2 + x * 4, y * 2 + 2, 'I');
      OutTextXY(4 + x * 4, y * 2 + 2, 'I');
    until 1 = 2;
  end;

begin
  ClrScr;
  Randomize;
  OutTextXY(5, 5, 'Haben Sie einen <F>arb- oder <M>ono-Bildschirm (default Farb) ?');
  repeat
    ch := UpCase(ReadKey);
    if ch = #0 then begin
      ch := UpCase(ReadKey);
    end;
    case ch of
      'F', #13: begin
        f[1] := 1;
        f[2] := 2;
        f[3] := 3;
        f[4] := 4;
        f[5] := 5;
        f[6] := 6;
        f[7] := 7;
      end;
      'M': begin
        for i := 1 to 7 do begin
          f[i] := $07;
        end;
      end;
    end;
  until ch in ['F', 'M', #13];

  { --- Titelbild --- }

  Color(f[2], 0);
  ClrScr;
  Color(f[6], f[1]);
  if ch = 'M' then begin
    Color(f[6], 0);
  end;
  CursorOff;
  s := #219#219#179;
  OutTextXY(6, 2, #218#196#191);
  OutTextXY(14, 2, #218#196#191);
  OutTextXY(22, 2, #218#196#191);
  OutTextXY(30, 2, #218#196#191);
  OutTextXY(38, 2, #218#196#191);
  OutTextXY(46, 2, #218#196#196#196#196#196#196#196#196#196#191);
  OutTextXY(62, 2, #218#196#191);
  OutTextXY(70, 2, #218#196#191);
  OutTextXY(6, 3, #219#219#192#196#191);
  OutTextXY(12, 3, #218#196 + s);
  OutTextXY(22, 3, s);
  OutTextXY(30, 3, #219#219#192#196#191);
  OutTextXY(38, 3, s);
  OutTextXY(46, 3, #219#219#219#219#219#219#219#219#219#219#217);
  OutTextXY(62, 3, #219#219#192#196#191);
  OutTextXY(70, 3, s);
  OutTextXY(6, 4, #219#219#219#219#192#196#191);
  OutTextXY(12, 4, #219#219 + s);
  OutTextXY(22, 4, s);
  OutTextXY(30, 4, #219#219#219#219#192#196#191);
  OutTextXY(38, 4, s);
  OutTextXY(46, 4, s);
  OutTextXY(62, 4, #219#219#219#219#192#196#191);
  OutTextXY(70, 4, s);
  OutTextXY(6, 5, s);
  OutTextXY(10, 5, #219#219#217);
  OutTextXY(14, 5, s);
  OutTextXY(22, 5, s);
  OutTextXY(30, 5, s);
  OutTextXY(34, 5, s);
  OutTextXY(38, 5, s);
  OutTextXY(46, 5, #219#219#192#196#196#196#196#196#191);
  OutTextXY(62, 5, s);
  OutTextXY(66, 5, s);
  OutTextXY(70, 5, s);
  OutTextXY(6, 6, s);
  OutTextXY(14, 6, s);
  OutTextXY(22, 6, s);
  OutTextXY(30, 6, s);
  OutTextXY(34, 6, s);
  OutTextXY(38, 6, s);
  OutTextXY(46, 6, #219#219#219#219#219#219#219#219);
  OutTextXY(62, 6, s);
  OutTextXY(66, 6, s);
  OutTextXY(70, 6, s);
  OutTextXY(6, 7, s);
  OutTextXY(14, 7, s);
  OutTextXY(22, 7, s);
  OutTextXY(30, 7, s);
  OutTextXY(34, 7, #219#219#192#196);
  OutTextXY(38, 7, s);
  OutTextXY(46, 7, s);
  OutTextXY(62, 7, s);
  OutTextXY(66, 7, #219#219#192#196 + s);
  OutTextXY(6, 8, s);
  OutTextXY(14, 8, s);
  OutTextXY(22, 8, s);
  OutTextXY(30, 8, s);
  OutTextXY(36, 8, #219#219 + s);
  OutTextXY(46, 8, #219#219#192#196#196#196#196#196#196#196#191);
  OutTextXY(62, 8, s);
  OutTextXY(68, 8, #219#219 + s);
  OutTextXY(6, 9, #219#219#217);
  OutTextXY(14, 9, #219#219#217);
  OutTextXY(22, 9, #219#219#217);
  OutTextXY(30, 9, #219#219#217);
  OutTextXY(38, 9, #219#219#217);
  OutTextXY(46, 9, #219#219#219#219#219#219#219#219#219#219#217);
  OutTextXY(62, 9, #219#219#217);
  OutTextXY(70, 9, #219#219#217);
  OutTextXY(27, 22, #218);
  ZKette(#196, 25);
  Write(#191);
  OutTextXY(27, 23, #179' Beliebige Taste= weiter '#179);
  OutTextXY(27, 24, #192);
  ZKette(#196, 25);
  Write(#217);

  { --- Haup-Menu --- }

  ch := Readkey;
  if ch = #0 then begin
    ch := Readkey;
  end;
  ch := ' ';
  repeat
    if not (ch in ['+', '-']) then begin
      CursorOff;
      Color(f[5], 0);
      ClrScr;
      OutTextXY(35, 1, '32-Bit Minen-K' + oe + 'nig f' + ue + 'r Win32 V1.10');
      OutTextXY(35, 2, 'Copyright 1998, 2014 (C) by Mathias Burkhard');
      Color(f[2], 0);
      OutTextXY(5, 4, #219#220' '#220#219'   '#219'   '#219#220'  '#219'   '#219#223#223#223#223'   '#219#220'  '#219'      '#219'  '#220#223'   '#220#223#223#223#220'   '#219#220'  '#219'   '#219'   '#220#223#223#223#220);
      OutTextXY(5, 5, #219' '#223' '#219'   '#219'   '#219' '#219' '#219'   '#219#220#220#220'    '#219' '#219' '#219'      '#219#220#223'     '#219'   '#219'   '#219' '#219' '#219'   '#219'   '#219' '#220#220#220);
      OutTextXY(5, 6, #219'   '#219'   '#219'   '#219' '#223#220#219'   '#219'       '#219' '#223#220#219'      '#219' '#223#220'    '#219'   '#219'   '#219' '#223#220#219'   '#219'   '#219'   '#219);
      OutTextXY(5, 7, #223'   '#223'   '#223'   '#223'   '#223'   '#223#223#223#223#223'   '#223'   '#223'      '#223'   '#223'    '#223#223#223'    '#223'   '#223'   '#223'    '#223#223#223' ');
      OutTextXY(53, 3, #223' '#223);
      GotoXY(1, 8);
      ZKette('-', 80);
    end;
    Color(f[5], 0);
    OutTextXY(60, 20, 'Anzahl Minen:');
    Color(f[7], 0);
    Write(anz_Minen, '  ');
    ch := #0;
    menupos := menu(menupos, 5);
    case menupos of
      1: begin
        Spielen;
      end;
      2: begin
        Hilfe;
      end;
      3: begin
        Inc(anz_Minen);
        if anz_Minen = 101 then begin
          anz_Minen := 1;
        end;
        ch := '+';
      end;
      4: begin
        Dec(anz_Minen);
        if anz_Minen = 0 then begin
          anz_Minen := 100;
        end;
        ch := '-';
      end;
      5: begin
        Color(f[6], 0);
        ClrScr;
        OutTextXY(30, 12, 'Sind Sie sicher (J/N) ?');
        repeat
          ch := UpCase(ReadKey);
          if ch = #0 then begin
            ch := UpCase(ReadKey);
          end;
          if ch = 'J' then begin
            Color(7, 0);
            ClrScr;
            Halt;
          end;
        until ch = 'N';
      end;
    end;
  until 1 = 2;
  CursorOn;
end.
