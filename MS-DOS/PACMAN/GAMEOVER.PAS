procedure GameOverBild;
const
  maxEintrag = 20;
  Datei      = 'Liste.hic';
type
  Listentyp = Record
                Name : String[20];
                Wert : String[10];
                Zahl : LongInt;
              end;
var
  Rang,
  i       : Byte;
  k       : Integer;
  ch      : char;
  Bild    : BildPointer;
  f       : Text;
  Str1    : String[20];
  Eintrag : Array[1..maxEintrag] of Listentyp;
begin
  Assign(f,Datei);
  {$I-} Reset(f); {$I+}
  if IOResult <> 0 then Exit;
  for i:=1 to 20 do begin
    Readln(f,Eintrag[i].Name);
    Readln(f,Eintrag[i].Wert);
    Val(Eintrag[i].Wert,Eintrag[i].Zahl,k);
  end;
  close(f);
  Rahmen(30,8,48,12,2,0,7,true,true);
  GotoXY(35,10);
  for k := 100 to 800 do begin
    Sound(k * 10    ); delay(4);
    Sound(k * 10 * 2); delay(4);
    case k of
      100 : Write('G');
      200 : Write('a');
      300 : Write('m');
      400 : Write('e ');
      500 : Write('O');
      600 : Write('v');
      700 : Write('e');
      800 : Write('r');
    end;
  end;
  Nosound;
  BildSpeichern(Bild);
  if SpielPunkte > Eintrag[i].Zahl then begin
    Rahmen(24,14,53,16,2,0,7,true,true);
    GotoXY(26,15);
    TextColor(15); Write ('Name: ');
    Textcolor( 0); Readln(Str1);
    Rang := 21;
    for i := 1 to maxEintrag do begin
      if SpielPunkte > Eintrag[i].Zahl then begin
        Rang := i;
        i := maxEintrag;
        if Rang <> 20 then
          for k := maxEintrag downto Rang + 1 do begin
            Eintrag[k].Name := Eintrag[k-1].Name;
            Eintrag[k].Wert := Eintrag[k-1].Wert;
          end;
        Eintrag[Rang].Name := Str1;
        Str(Spielpunkte,Eintrag[Rang].Wert);
      end
      else Inc(Rang);
    end;
  end
  else begin
    ch := ReadKey;
    if ch = #0 then ch := ReadKey;
  end;
  Rahmen(19,1,59,24,2,23,0,true,true);
  TextColor(15);
  Scrolloben(22,4,41,23,0);
  Scrolloben(46,4,55,23,0);
  GotoXY(22,2); Write('            RangListe             ');
  for i := 1 to maxEintrag do begin
    if i = Rang then TextColor(15)
                else TextColor( 7);
    GotoXY(22,i+3); Write(Eintrag[i].Name);
    GotoXY(46,i+3); Write(Eintrag[i].Wert : 10);
  end;
  Assign(f,Datei);
  {$I-} ReWrite(f); {$I+}
  for i:=1 to 20 do begin
    Writeln(f,Eintrag[i].Name);
    Writeln(f,Eintrag[i].Wert);
  end;
  close(f);
  ch := ReadKey;
  if ch = #0 then ch := ReadKey;
  BildHolen(Bild);
end;