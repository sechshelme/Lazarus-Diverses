program pacman;
{$mode tp}
{$ASMMODE   intel}
uses
  crt,dos,bild,joystick,ascii,ports  ;
const
  Levelpfad      = '';
  maxLevelBilder = 9;
type
  BildTyp = Array[0..3999] of Byte;
var
Pos,
  Leben,
  Faktor,
  Geschwindigkeit   : Byte;
  Level,
  LevelBildNr,
  FeldPunkte,                                      { WÅrfel zum fressen }
  i, k              : integer;
  fLevel            : file of Bildtyp;             { FÅr Level Bilder ab Disk }
  LevelBild         : Bildtyp absolute $B800:0000; { BS Adresse }
  Spielpunkte       : LongInt;
  ch, Taste         : char;
  regs              : Registers;
  Menupkt           : Menupunkte;
  Ende,
  GameOver          : Boolean;
  Str1,
  DateiName         : String;
  xalt, yalt,
  AlteRichtung,
  Richtung,
  xneu, yneu        : Array[0..3] of Byte;

{$I zeichen.pas  }
{$I spiel.pas    }
{$I gameover.pas }

begin
  blinken(false);
  clrscr;
  zeichenaendern;
  Menupkt[1] := 'Einfach';
  Menupkt[2] := 'Mittel';
  Menupkt[3] := 'Schwierig';
  Menupkt[4] := 'Hilfe';
  Menupkt[5] := 'Ende';
  LevelBildNr := 1;
  Str(LevelBildNr,Str1);
  DateiName := LevelPfad + 'Level' + Str1 + '.SCR';
  assign(fLevel,DateiName);                         { Level Laden }
  reset(flevel);
  read(fLevel, Levelbild);
  close(flevel);
  move(LevelBild, Mem[$B800:$0000], 4000);
  pos := 0;
  repeat
    Ende := false;
    Level := 1;
    Leben := 4;
    LevelBildNr := 1;
    Spielpunkte := 0;
    GameOver := false;
    pos := Menufenster(30,8,5,pos,0,7,15,1,0,Menupkt);
    case pos of
        1 : begin Geschwindigkeit := 200; Faktor := 1; end;
        2 : begin Geschwindigkeit := 100; Faktor := 2; end;
        3 : begin Geschwindigkeit := 50;  Faktor := 5; end;
        4 : begin
            end;
      0,5 : Ende := true;
    end;
    if Not Ende then begin
     repeat
      if pos in [1..3] then begin
        case LevelBildNr of
          1 : Feldpunkte := 242;
          2 : Feldpunkte := 242;
          3 : Feldpunkte := 242;
          4 : Feldpunkte := 242;
          5 : Feldpunkte := 242;
          6 : Feldpunkte := 242;
          7 : Feldpunkte := 242;
          8 : Feldpunkte := 242;
          9 : Feldpunkte := 242;
        end;
        Str(LevelBildNr,Str1);
        DateiName := LevelPfad + 'Level' + Str1 + '.SCR';
        assign(fLevel,DateiName);                         { Level Laden }
        reset(flevel); read(fLevel,Levelbild); close(flevel);
        Spiel( Level );
        Inc(Level);
        Geschwindigkeit := Geschwindigkeit * 20 DIV 21;
        if Geschwindigkeit < 25 then Geschwindigkeit := 25;
        if leben = 12 then Inc(Spielpunkte,10000)
                      else Inc(Leben);
        if LevelBildNr = maxLevelBilder then LevelBildNr := 1
                                        else Inc(LevelBildNr);
        if Not GameOver then begin
          Rahmen(29,12,42,14,2,0,7,true,true);
          GotoXY(31,13);
          for k := 100 to 1000 do begin
            Sound(k * 10    ); delay(2);
            Sound(k * 10 * 2); delay(2);
            case k of
              100 : Write('G');
              200 : Write('r');
              300 : Write('a');
              400 : Write('t');
              500 : Write('u');
              600 : Write('l');
              700 : Write('i');
              800 : Write('e');
              900 : Write('r');
             1000 : Write('e');
            end;
          end;
          Textcolor(31); TextBackground(0);
          Inc(Spielpunkte,90 * Faktor);
          for i := 1 to 11 do
            for k := 101 to 400 do begin
              inc(Spielpunkte,3 * Faktor);
              GotoXY(68, 7); Write(Spielpunkte : 8);
              Sound(k * i * 5); delay(1);
            end;
        end;
        repeat
        until ((joyvorhanden) AND (JoyKnopf1)) OR (Keypressed);
        if keypressed then begin
          ch := ReadKey;
          if ch = #0 then ch := Readkey;
        end;
      end;
     until (Gameover) OR (pos = 4);
     GameOverBild;
    end;
  until Ende;
  Cursorein;
  Textmode(3);
end.
