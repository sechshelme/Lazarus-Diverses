unit Tastatur;
interface
uses
  dos, crt, Maus, Bild;
function JaNein( x, y, RT, Fr, Fh, Fhg : Byte;
                 Text                  : String;
                 Schatten              : Boolean) : Boolean;
procedure Beenden( ErrorLevel : Byte );
procedure InsAus;
procedure InsEin;
procedure Pause;
function Eingabe( var Text           : string;
                  var Cursorpos      : Byte;
                      maxTextlaenge,
                      x, y, Fvg, Fhg : Byte ) : Byte;

implementation
var
  Regs   : Registers;
  stat   : Byte absolute $0040:$0017;
  taste2 : procedure;
  neuint : pointer;

function Ins : Boolean;
begin
  if Stat and 128 = 128 then Ins := true
                        else Ins := false;
end;

procedure Cursor; interrupt;
begin
  Regs.ah := $01;
  if Ins then begin
    Regs.ch := 7;
    Regs.cl := 8;
  end
  else begin
    Regs.ch := 3;
    Regs.cl := 8;
  end;
  Intr($10,Regs);
  inline($9c);
  taste2;
end;

function JaNein( x, y, RT, Fr, Fh, Fhg : Byte;
                 Text                  : String;
                 Schatten              : Boolean) : Boolean;
var
  Bildschirm : BildPointer;
  pos, pos1  : Byte;
  Ende       : Boolean;
  ch         : char;
  Laenge     : Byte;
begin
  BildSpeichern(Bildschirm);
  Laenge := Length(Text) + 4;
  CursorAus;
  Hinweis( x, y, RT, Fr, Fh, Fhg, Text + '  ja  nein', Schatten);
  GotoXY(x + Laenge, y + 1); Write(' ja ');
  Textcolor(Fh); TextBackground(4); Write('nein');
  GotoMaus(x + Laenge + 5,y + 1);
  Ende := false; pos := 2;
  repeat
    pos1 := pos;
    if Keypressed then begin
      ch := Upcase(ReadKey);
      if ch = #0 then begin
        ch := ReadKey;
        case ch of
          #75 : pos := 1;
          #77 : pos := 2;
        end;
      end
      else begin
        case ch of
          #13 : Ende := true;
          #27 : Ende := true;
          'J' : begin
                  pos := 1;
                  Ende := true;
                end;
          'N' : begin
                  pos := 2;
                  Ende := true;
                end;
        end;
      end;
      if pos = 2 then GotoMaus(x + Laenge + 5, y + 1)
                 else GotoMaus(x + Laenge + 1, y + 1);
    end;
    if Mausvorhanden then begin
      if MausX < x + Laenge then GotoMaus(x + Laenge, y + 1);
      if MausX > x + Laenge + 7 then GotoMaus(x + Laenge + 6, y + 1);
      if MausX in[x + Laenge..x + Laenge + 3] then pos := 1
                                              else pos := 2;
    end;
    if pos <> pos1 then begin
      GotoXY(X + Laenge, y + 1);
      if pos = 1 then TextBackGround(4)
                 else TextBackGround(Fhg);
      Write(' ja ');
      if pos = 1 then TextBackground(Fhg)
                 else TextBackground(4);
      Write('nein');
    end;
    if Mausknopf(0) then Ende := true;
    if Mausknopf(1) then begin
      Ende := true;
      pos := 2; end;
  until Ende;
  if ch = #27 then pos := 2;
  if pos = 1 then JaNein := true
             else JaNein := false;
  Bildholen(Bildschirm);
end;

procedure Beenden( ErrorLevel : Byte );
begin
  Insaus;
  Textattr := 7;
  CursorEin;
  Halt( ErrorLevel );
end;

procedure InsAus;
begin
  SetIntVec($9,@Taste2);
  CursorAus;
end;

procedure InsEin;
begin
  SetIntVec($9,@Cursor);
  Regs.ah := $01;
  if Ins then begin
    Regs.ch := 7;
    Regs.cl := 8;
  end
  else begin
    Regs.ch := 3;
    Regs.cl := 8;
  end;
  Intr($10,Regs);
end;

procedure Pause;
var
  ch : char;
begin
  CursorAus;
  repeat
  until (Keypressed) OR (Mausknopf(0)) OR (Mausknopf(1));
  if KeyPressed then begin
    ch := ReadKey;
    if ch = #0 then ch := ReadKey;
  end;
end;

function Eingabe( var Text           : string;
                  var Cursorpos      : Byte;
                      maxTextlaenge,
                      x, y, Fvg, Fhg : Byte ) : Byte;
var
  i     : Byte;
  ch,
  Taste : char;
begin
  InsEin;
  Textcolor(Fvg); Textbackground(Fhg);
  if maxTextlaenge < Length(Text) then
    Delete(Text, maxTextlaenge + 1, Length(Text) - maxTextlaenge);
  if Cursorpos < 1 then Cursorpos := 1;
  if Cursorpos > Length(Text) + 1 then Cursorpos := Length(Text) + 1;
  GotoXY(x, y);
  Write(Text);
  for i := Length(Text) to maxTextlaenge - 1 do Write(' ');
  GotoXY(x + Cursorpos - 1, y );
  repeat
    ch := #0;
    repeat
      if Mausknopf(1) then ch := #27;
      if MausKnopf(0) then ch := #10;
    until (ch <> #0) OR Keypressed;
    if NOT (ch in[#10,#27]) then ch := ReadKey;
    if ch = #0 then Taste := ReadKey
               else Taste := #0;
    if ch = #7  then write(#7);
    if ch = #8 then
      if Cursorpos <> 1 then begin
        Delete(Text, Cursorpos - 1, 1);
        Dec(Cursorpos);
      end;
    if Taste = #71 then Cursorpos := 1;
    if Taste = #79 then Cursorpos := Length(Text) + 1;
    if Taste = #75 then
      if Cursorpos <> 1 then Dec(Cursorpos);
    if Taste = #77 then
      if Cursorpos <> Length(Text) + 1 then Inc(Cursorpos);
    if Taste = #83 then
      Delete(Text, Cursorpos, 1);
    if Taste = #82 then Taste := #0;
    if (ch in [#32..#255]) AND (Cursorpos <= maxTextlaenge) then begin
      if Ins then Insert(ch, Text, Cursorpos)
             else if Cursorpos > Length(Text) then
             Text := Text + ch else Text[Cursorpos] := ch;
      Inc(Cursorpos);
      if Length(Text) > maxTextlaenge then
        Delete(Text, maxTextlaenge + 1, 1);
    end;
    GotoXY(x, y); Write(Text);
    if Length(Text) < maxTextlaenge then Write(' ');
    GotoXY(x + Cursorpos - 1, y );
  until (ch in [#9,#10,#13,#27]) OR (Not (Taste in [#0,#71,#75,#77,#79,#82,#83]));
  if ch = #0 then Eingabe := Ord(Taste)
             else Eingabe := Ord(ch);
  while Text[Length(Text)] = #32 do
    Delete(Text, Length(Text), 1);
  InsAus;
  CursorAus;
end;

begin
{  neuint := @Cursor;}
  GetintVec($9,@taste2);
end.
