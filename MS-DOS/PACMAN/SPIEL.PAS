function RichtungsTest : Boolean;
begin
  RichtungsTest := false;
  case richtung[i] of
    1 : begin
          Dec(yneu[i]);
          If attributlesen(xneu[i] + 1,yneu[i] DIV 2 + 1) in [5..13] then
            Inc(yneu[i])
          else RichtungsTest := true;
        end;
    2 : begin
          Inc(xneu[i]);
          If attributlesen(xneu[i] + 2,yneu[i] DIV 2 + 1) in [5..13] then
            Dec(xneu[i])
          else RichtungsTest := true;
        end;
    3 : begin
          Inc(yneu[i]);
          if Odd (yneu[i]) then begin
            If attributlesen(xneu[i] + 1,yneu[i] DIV 2 + 2) in [5..13] then
              Dec(yneu[i])
            else RichtungsTest := true;
          end
        end;
    4 : begin
          Dec(xneu[i]);
          If attributlesen(xneu[i] + 1,yneu[i] DIV 2 + 1) in [5..13] then
            Inc(xneu[i])
          else RichtungsTest := true;
        end;
   end;
end;



procedure Spiel( Level : Integer );
var
  LevelBild       : Bildpointer;
  Gefaehrlich     : Boolean;                        { Zustand des Feindes }
  Farbe,
  Merker          : Byte;
  x,y,
  FeindZaehler,
  Zaehler         : Integer;
  Zellenzaehler   : Array[1..3] of Integer;
begin
  Gefaehrlich := False;
  CursorAus;
  xneu[0] := 26; yneu[0] := 36;
  xneu[1] := 24; yneu[1] := 24;
  xneu[2] := 26; yneu[2] := 24;
  xneu[3] := 28; yneu[3] := 24;
  Richtung[0] := 2;
  Richtung[1] := 2;
  Richtung[2] := 1;
  Richtung[3] := 4;
  FeindZaehler := 1;
  Zaehler := 0;
  Regs.ah := $03;
  Regs.al := $05;
  Regs.bh := $00;
  Regs.bl := $00;
  Intr($16,Regs);
  for i := 1 to 3 do
    Zellenzaehler [i] := 0;
  Taste := #0; ch := #0;
  Randomize;
  Textcolor(31); TextBackground(0);
  GotoXY(68, 7); Write(Spielpunkte : 8);
  GotoXY(68, 9); Write(Level       : 8);
  GotoXY(68,11); Write(Leben       : 8);
  BildSpeichern(LevelBild);
  Textcolor(14); 
    GotoXY(55,24); Zkette(32,24,0,0);
    for x := 0 to Leben - 2 do begin
      GotoXY(55 + 2 * x,24); Write('ά');
    end;
  GotoXY(27,19); Write('ά');
  GotoXY(25,13);
  Textcolor(4); Write('ީ');
  Textcolor(2); Write('ީ');
  Textcolor(3); Write('ީ');
  repeat
  until ((joyvorhanden) AND (JoyKnopf1)) OR (Keypressed);
  repeat
    asm
      DB $fa
    end;
//    Inline( $FA );                      { Tastatur buffer leeren }
    MemW[$40:$1A] := MemW[$40:$1C];
    asm
      DB $fb
    end;
//    Inline( $FB );
    for i := 0 to 3 do begin
      xalt[i] := xneu[i];
      yalt[i] := yneu[i];
    end;
    delay(Geschwindigkeit * 2);                         { Verz�gerung }
    GameOver := false;
    if ch = #27 then begin
      GameOver := true;
      Leben := 0;
    end;
    if port[$60] = 1 then ch := #27
                     else taste := chr(port[$60]);
    if joyvorhanden then begin
      if Joyoben Then Taste := #72;
      if Joylinks then taste := #75;
      if joyrechts then taste := #77;
      if joyunten then taste := #80;
    end;
    case Taste of
      #72 : begin                       { Cursor Rauf }
              if Not (Odd(Xneu[0])) AND
              ( NOT (attributlesen(xneu[0] + 1,yneu[0] DIV 2 + 0) in [5..13]))
              then richtung[0] := 1;
            end;
      #75 : begin                       { Cursor Links }
              if Not (Odd(Yneu[0])) AND
              ( NOT (attributlesen(xneu[0] + 0,yneu[0] DIV 2 + 1) in [5..13]))
              then richtung[0] := 4;
            end;
      #77 : begin                       { Cursor Rechts }
              if Not Odd(Yneu[0]) AND
              ( NOT (attributlesen(xneu[0] + 3,yneu[0] DIV 2 + 1) in [5..13]))
              then richtung[0] := 2;
            end;
      #80 : begin                       { Cursor Runter }
              if Not Odd(Xneu[0]) AND
              ( NOT (attributlesen(xneu[0] + 1,yneu[0] DIV 2 + 2) in [5..13]))
              then richtung[0] := 3;
            end;
    end;
    if NOT (Taste in [#72,#75,#77,#80]) then richtung[0] := Alterichtung[0]
                                        else Alterichtung[0] := richtung[0];
    for i := 1 to 3 do begin
      AlteRichtung[i] := Richtung[i];
        case Richtung[i] of
          1 : begin
               if Odd(yneu[i]) then
                 Dec(yneu[i])
               else begin
                 repeat
                   case Round(INT(Random(21))) of
                      0..15 : Richtung[i] := 1;
                     16..17 : Richtung[i] := 4;
                     18..19 : Richtung[i] := 2;
                         20 : Richtung[i] := 3;
                   end;
                 until RichtungsTest;
               end;
              end;
          2 : begin
               if Odd(xneu[i]) then
                 Inc(xneu[i])
               else begin
                 repeat
                   case Round(INT(Random(21))) of
                     0..15 : Richtung[i] := 2;
                    16..17 : Richtung[i] := 1;
                    18..19 : Richtung[i] := 3;
                        20 : Richtung[i] := 4;
                   end;
                 until RichtungsTest;
               end;
              end;
          3 : begin
               if Odd(yneu[i]) then
                 Inc(yneu[i])
               else begin
                 repeat
                   case Round(INT(Random(21))) of
                     0..15 : Richtung[i] := 3;
                    16..17 : Richtung[i] := 4;
                    18..19 : Richtung[i] := 2;
                        20 : Richtung[i] := 1;
                   end;
                 until RichtungsTest;
               end;
              end;
          4 : begin
               if Odd(xneu[i]) then
                 Dec(xneu[i])
               else begin
                 repeat
                   case Round(INT(Random(21))) of
                     0..15 : Richtung[i] := 4;
                    16..17 : Richtung[i] := 1;
                    18..19 : Richtung[i] := 3;
                        20 : Richtung[i] := 2;
                   end;
                 until RichtungsTest;
               end;
              end;
        end;
    end;
    case richtung[0] of
    1 : begin
          Dec(yneu[0]);
          If attributlesen(xneu[0] + 1,yneu[0] DIV 2 + 1) in [5..13] then
            Inc(yneu[0]);
        end;
    2 : begin
          Inc(xneu[0]);
          If attributlesen(xneu[0] + 2,yneu[0] DIV 2 + 1) in [5..13] then
            Dec(xneu[0]);
        end;
    3 : begin
          Inc(yneu[0]);
          if Odd (yneu[0]) then begin
            If attributlesen(xneu[0] + 1,yneu[0] DIV 2 + 2) in [5..13] then
              Dec(yneu[0]);
          end;
        end;
    4 : begin
          Dec(xneu[0]);
          If attributlesen(xneu[0] + 1,yneu[0] DIV 2 + 1) in [5..13] then
            Inc(xneu[0]);
        end;
    end;
    TextColor(14);
    for i := 0 to 3 do begin
      if xneu[i] = 50 then xneu[i] :=  1;
      if xneu[i] =  0 then xneu[i] := 49;
      if yneu[i] =  0 then yneu[i] := 47;
      if yneu[i] = 48 then yneu[i] :=  1;
    end;
    if (yneu[0] = 21) AND (Xneu[0] = 26) then Dec(yneu[0]);
    if (yneu[0] <> yalt[0]) OR (xneu[0] <> xalt[0]) then
      for i:=2000 downto 1000 do
        Sound(i);
    Nosound;
    for i := 1 to 3 do begin                    { Kolosion Mit Feind }
      if Zellenzaehler[i] > 0 then begin
        Dec (Zellenzaehler[i]);
        xneu[i] := xalt[i];
        yneu[i] := yalt[i];
      end;
      x := xneu[0] - xneu[i];
      y := yneu[0] - yneu[i];
      if (x > -2) AND (x < 2) AND (y > -2) AND (y < 2) then begin
        if Gefaehrlich then begin
          Rahmen(29,12,50,14,2,0,7,true,true);
          GotoXY(31,13);
          for k := 100 to 800 do begin
            Sound(k * 10    ); delay(4);
            Sound(k * 10 * 2); delay(4);
            case k of
              100 : Write('Le');
              200 : Write('id');
              300 : Write('er ');
              400 : Write('Pe');
              500 : Write('ch ');
              600 : Write('ge');
              700 : Write('ha');
              800 : Write('bt');
            end;
          end;
          Nosound;
          TextBackground(0);
          repeat
          until ((joyvorhanden) AND (Joyknopf1)) OR (KeyPressed) OR (Leben = 1);
          if Leben <> 0 then Dec(Leben);
          if Leben = 0 then GameOver := true
          else begin
            xneu[0] := 26; yneu[0] := 36;
            xneu[1] := 24; yneu[1] := 24;
            xneu[2] := 26; yneu[2] := 24;
            xneu[3] := 28; yneu[3] := 24;
            Richtung[0] := 2;
            Richtung[1] := 2;
            Richtung[2] := 1;
            Richtung[3] := 4;
          end;
        end
        else begin
          for x := 1000 to 5000 do
            for y := 1 to 4 do
              Sound(y * x);
          yneu[i] := 24;
          Zellenzaehler[i] := 300;
          Inc(SpielPunkte,500 * Feindzaehler * Faktor);
          Inc(Feindzaehler);
          case i of
            1 : xneu[i] := 24;
            2 : xneu[i] := 26;
            3 : xneu[i] := 28;
          end;
        end;
      end;
    end;
    if Leben <> 0 then Bildholen(LevelBild);
    TextColor(14);
    GotoXY(55,24); Zkette(32,24,0,0);
    for x := 0 to Leben - 2 do begin
      GotoXY(55 + 2 * x,24); Write('ά');
    end;
    if Odd(yalt[0]) then begin
      GotoXY(xalt[0] + 1, yalt[0] DIV 2 + 1); Write('  ');
      GotoXY(xalt[0] + 1, yalt[0] DIV 2 + 2); Write('  ');
    end
    else begin
      GotoXY(xalt[0] + 1,yalt[0] DIV 2 + 1); Write('  ');
    end;
    Merker := 0; 
    if richtung[0] in [1,3] then begin
      if Odd(yneu[0]) then begin
        if richtung[0] = 1 then begin
          Merker := Zeichenlesen(xneu[0] + 1, yneu[0] DIV 2 + 1);
          GotoXY(xneu[0] + 1, yneu[0] DIV 2 + 1); Write('ܫ');
          GotoXY(xneu[0] + 1, yneu[0] DIV 2 + 2); Write('ح');
        end;
        if richtung[0] = 3 then begin
          Merker := Zeichenlesen(xneu[0] + 1, yneu[0] DIV 2 + 2);
          GotoXY(xneu[0] + 1, yneu[0] DIV 2 + 1); Write('ŧ');
          GotoXY(xneu[0] + 1, yneu[0] DIV 2 + 2); Write('ݪ');
        end;
      end
      else begin
        GotoXY(xneu[0] + 1,yneu[0] DIV 2 + 1); Write('Φ');
      end;
    end
    else begin
      if Odd(xneu[0]) then begin
        GotoXY(xneu[0] + 1,yneu[0] DIV 2 + 1);
        if Richtung[0] = 2 then begin
          Merker := Zeichenlesen(xneu[0] + 2, yneu[0] DIV 2 + 1);
          Write('ά');
        end;
        if Richtung[0] = 4 then begin
          Merker := Zeichenlesen(xneu[0] , yneu[0] DIV 2 + 1);
          Write('˦');
        end;
      end
      else begin
        GotoXY(xneu[0] + 1,yneu[0] DIV 2 + 1); Write('Φ');
      end;
    end;
    Textcolor(31);
    GotoXY(68, 7); Write(Spielpunkte : 8);
    GotoXY(68,11); Write(Leben       : 8);
    if Merker in [214,215] then Dec(FeldPunkte);
    if Merker = 214 then begin
      for i := 1000 to 7000 do
        Sound(i);
      nosound;
      Inc(Zaehler,200);
      Inc(Spielpunkte,50 * Faktor); end;
    if Merker = 215 then Inc(Spielpunkte,10 * Faktor);
    if Merker = 215 then for i := 200 to 2500 do Sound(i *10);
    nosound;
    if Zaehler > 0 then begin
      dec(Zaehler); Gefaehrlich := false; end
    else Gefaehrlich := true;
    BildSpeichern(LevelBild);
    for i := 1 to 3 do begin
      if (Zaehler > 50) OR (Odd(Zaehler)) then Textcolor(1)
      else
        case i of
          1 : Textcolor(4);
          2 : Textcolor(2);
          3 : Textcolor(3);
        end;
      if richtung[i] in [1,3] then begin
        if Odd(yneu[i]) then begin
          GotoXY(xneu[i] + 1, yneu[i] DIV 2 + 1); Write('̥');
          GotoXY(xneu[i] + 1, yneu[i] DIV 2 + 2); Write('ߨ');
        end
        else begin
          GotoXY(xneu[i] + 1,yneu[i] DIV 2 + 1);
          if Odd(Zellenzaehler[i]) then Write('ʤ')
                                   else Write('ީ');
        end;
      end
      else begin
        if Odd(xneu[i]) then begin
          GotoXY(xneu[i] + 1,yneu[i] DIV 2 + 1);
          Write('ʤ');
        end
        else begin
          GotoXY(xneu[i] + 1,yneu[i] DIV 2 + 1);
          if Odd(Zellenzaehler[i]) then Write('ʤ')
                                   else Write('ީ');
        end;
      end;
    end;
  until (GameOver) OR (FeldPunkte = 0);
  Bildholen(LevelBild);
end;

