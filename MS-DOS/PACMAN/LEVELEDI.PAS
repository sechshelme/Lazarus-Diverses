{ šbersetzungs Programm fr Pac - Man - Leval }

program LevelEdit;
uses
  dos,crt,ascii,bild;
type
  Mustertyp      = Array[1..25] of String[30];
  Bildtyp        = Array[1..4000] of char;
const
  Pfad = 'c:\tp\pacman\';
var
  Muster       : Array[1..10] of Mustertyp;
  LevelBild    : Bildtyp absolute $B800:0000;
  f            : file of Bildtyp;
  i,
  RahmenFarbe  : Byte;

{$I pacman\zeichen.pas }

procedure Level1( var Muster : Mustertyp );
begin
  Muster[ 1] := '           j j j           ';      { 27 x 25 Punkte }
  Muster[ 2] := ' akkkkkkkkki j gkkkkkkkkkc ';
  Muster[ 3] := ' j*++++++++++j++++++++++*j ';
  Muster[ 4] := ' j+akkc+akkc+j+akkc+akkc+j ';
  Muster[ 5] := ' j+j  j+j  j+j+j  j+j  j+j ';
  Muster[ 6] := ' j+gkki+gkki+j+gkki+gkki+j ';
  Muster[ 7] := ' j+++++++++++j+++++++++++j ';
  Muster[ 8] := ' j+mkkl+o+mkkekkl+o+mkkl+j ';
  Muster[ 9] := ' j++++++j++++j++++j++++++j ';
  Muster[10] := ' dkkkkc+dkkl+n+mkkf+akkkkf ';
  Muster[11] := ' j    j+j++++ ++++j+j    j ';
  Muster[12] := 'khkkkki+n+akl mkc+n+gkkkkhk';
  Muster[13] := '  ++++++++j     j++++++++  ';
  Muster[14] := 'kbkkkkc+o+gkkkkki+o+akkkkbk';
  Muster[15] := ' j    j+j++++ ++++j+j    j ';
  Muster[16] := ' dkkkki+n+mkkbkkl+n+gkkkkf ';
  Muster[17] := ' j+++++++++++j+++++++++++j ';
  Muster[18] := ' j+mkkc+mkkl+n+mkkl+akkl+j ';
  Muster[19] := ' j++++j++++++ ++++++j++++j ';
  Muster[20] := ' dkkl+n+o+mkkbkkl+o+n+mkkf ';
  Muster[21] := ' j++++++j++++j++++j++++++j ';
  Muster[22] := ' j+mkkkkhkkl+j+mkkhkkkkl+j ';
  Muster[23] := ' j*++++++++++j++++++++++*j ';
  Muster[24] := ' gkkkkkkkkkc+j+akkkkkkkkki ';
  Muster[25] := '           j j j           ';
  RahmenFarbe := 5;
end;

procedure Level2( var Muster : Mustertyp );
begin
  Muster[ 1] := '           j j j           ';      { 27 x 25 Punkte }
  Muster[ 2] := ' akkkkkkkkki j gkkkkkkkkkc ';
  Muster[ 3] := ' j*++++++++++j++++++++++*j ';
  Muster[ 4] := ' j+akkc+akkc+j+akkc+akkc+j ';
  Muster[ 5] := ' j+j  j+j  j+j+j  j+j  j+j ';
  Muster[ 6] := ' j+gkki+gkki+j+gkki+gkki+j ';
  Muster[ 7] := ' j+++++++++++j+++++++++++j ';
  Muster[ 8] := ' j+mkkl+o+mkkekkl+o+mkkl+j ';
  Muster[ 9] := ' j++++++j++++j++++j++++++j ';
  Muster[10] := ' dkkkkc+dkkl+n+mkkf+akkkkf ';
  Muster[11] := ' j    j+j++++ ++++j+j    j ';
  Muster[12] := 'khkkkki+n+akl mkc+n+gkkkkhk';
  Muster[13] := '  ++++++++j     j++++++++  ';
  Muster[14] := 'kbkkkkc+o+gkkkkki+o+akkkkbk';
  Muster[15] := ' j    j+j++++ ++++j+j    j ';
  Muster[16] := ' dkkkki+n+mkkbkkl+n+gkkkkf ';
  Muster[17] := ' j+++++++++++j+++++++++++j ';
  Muster[18] := ' j+mkkc+mkkl+n+mkkl+akkl+j ';
  Muster[19] := ' j++++j++++++ ++++++j++++j ';
  Muster[20] := ' dkkl+n+o+mkkbkkl+o+n+mkkf ';
  Muster[21] := ' j++++++j++++j++++j++++++j ';
  Muster[22] := ' j+mkkkkhkkl+j+mkkhkkkkl+j ';
  Muster[23] := ' j*++++++++++j++++++++++*j ';
  Muster[24] := ' gkkkkkkkkkc+j+akkkkkkkkki ';
  Muster[25] := '           j j j           ';
  RahmenFarbe := 6;
end;

procedure Level3( var Muster : Mustertyp );
begin
  Muster[ 1] := '           j j j           ';      { 27 x 25 Punkte }
  Muster[ 2] := ' akkkkkkkkki j gkkkkkkkkkc ';
  Muster[ 3] := ' j*++++++++++j++++++++++*j ';
  Muster[ 4] := ' j+akkc+akkc+j+akkc+akkc+j ';
  Muster[ 5] := ' j+j  j+j  j+j+j  j+j  j+j ';
  Muster[ 6] := ' j+gkki+gkki+j+gkki+gkki+j ';
  Muster[ 7] := ' j+++++++++++j+++++++++++j ';
  Muster[ 8] := ' j+mkkl+o+mkkekkl+o+mkkl+j ';
  Muster[ 9] := ' j++++++j++++j++++j++++++j ';
  Muster[10] := ' dkkkkc+dkkl+n+mkkf+akkkkf ';
  Muster[11] := ' j    j+j++++ ++++j+j    j ';
  Muster[12] := 'khkkkki+n+akl mkc+n+gkkkkhk';
  Muster[13] := '  ++++++++j     j++++++++  ';
  Muster[14] := 'kbkkkkc+o+gkkkkki+o+akkkkbk';
  Muster[15] := ' j    j+j++++ ++++j+j    j ';
  Muster[16] := ' dkkkki+n+mkkbkkl+n+gkkkkf ';
  Muster[17] := ' j+++++++++++j+++++++++++j ';
  Muster[18] := ' j+mkkc+mkkl+n+mkkl+akkl+j ';
  Muster[19] := ' j++++j++++++ ++++++j++++j ';
  Muster[20] := ' dkkl+n+o+mkkbkkl+o+n+mkkf ';
  Muster[21] := ' j++++++j++++j++++j++++++j ';
  Muster[22] := ' j+mkkkkhkkl+j+mkkhkkkkl+j ';
  Muster[23] := ' j*++++++++++j++++++++++*j ';
  Muster[24] := ' gkkkkkkkkkc+j+akkkkkkkkki ';
  Muster[25] := '           j j j           ';
  RahmenFarbe := 7;
end;

procedure Level4( var Muster : Mustertyp );
begin
  Muster[ 1] := '           j j j           ';      { 27 x 25 Punkte }
  Muster[ 2] := ' akkkkkkkkki j gkkkkkkkkkc ';
  Muster[ 3] := ' j*++++++++++j++++++++++*j ';
  Muster[ 4] := ' j+akkc+akkc+j+akkc+akkc+j ';
  Muster[ 5] := ' j+j  j+j  j+j+j  j+j  j+j ';
  Muster[ 6] := ' j+gkki+gkki+j+gkki+gkki+j ';
  Muster[ 7] := ' j+++++++++++j+++++++++++j ';
  Muster[ 8] := ' j+mkkl+o+mkkekkl+o+mkkl+j ';
  Muster[ 9] := ' j++++++j++++j++++j++++++j ';
  Muster[10] := ' dkkkkc+dkkl+n+mkkf+akkkkf ';
  Muster[11] := ' j    j+j++++ ++++j+j    j ';
  Muster[12] := 'khkkkki+n+akl mkc+n+gkkkkhk';
  Muster[13] := '  ++++++++j     j++++++++  ';
  Muster[14] := 'kbkkkkc+o+gkkkkki+o+akkkkbk';
  Muster[15] := ' j    j+j++++ ++++j+j    j ';
  Muster[16] := ' dkkkki+n+mkkbkkl+n+gkkkkf ';
  Muster[17] := ' j+++++++++++j+++++++++++j ';
  Muster[18] := ' j+mkkc+mkkl+n+mkkl+akkl+j ';
  Muster[19] := ' j++++j++++++ ++++++j++++j ';
  Muster[20] := ' dkkl+n+o+mkkbkkl+o+n+mkkf ';
  Muster[21] := ' j++++++j++++j++++j++++++j ';
  Muster[22] := ' j+mkkkkhkkl+j+mkkhkkkkl+j ';
  Muster[23] := ' j*++++++++++j++++++++++*j ';
  Muster[24] := ' gkkkkkkkkkc+j+akkkkkkkkki ';
  Muster[25] := '           j j j           ';
  RahmenFarbe := 8;
end;

procedure Level5( var Muster : Mustertyp );
begin
  Muster[ 1] := '           j j j           ';      { 27 x 25 Punkte }
  Muster[ 2] := ' akkkkkkkkki j gkkkkkkkkkc ';
  Muster[ 3] := ' j*++++++++++j++++++++++*j ';
  Muster[ 4] := ' j+akkc+akkc+j+akkc+akkc+j ';
  Muster[ 5] := ' j+j  j+j  j+j+j  j+j  j+j ';
  Muster[ 6] := ' j+gkki+gkki+j+gkki+gkki+j ';
  Muster[ 7] := ' j+++++++++++j+++++++++++j ';
  Muster[ 8] := ' j+mkkl+o+mkkekkl+o+mkkl+j ';
  Muster[ 9] := ' j++++++j++++j++++j++++++j ';
  Muster[10] := ' dkkkkc+dkkl+n+mkkf+akkkkf ';
  Muster[11] := ' j    j+j++++ ++++j+j    j ';
  Muster[12] := 'khkkkki+n+akl mkc+n+gkkkkhk';
  Muster[13] := '  ++++++++j     j++++++++  ';
  Muster[14] := 'kbkkkkc+o+gkkkkki+o+akkkkbk';
  Muster[15] := ' j    j+j++++ ++++j+j    j ';
  Muster[16] := ' dkkkki+n+mkkbkkl+n+gkkkkf ';
  Muster[17] := ' j+++++++++++j+++++++++++j ';
  Muster[18] := ' j+mkkc+mkkl+n+mkkl+akkl+j ';
  Muster[19] := ' j++++j++++++ ++++++j++++j ';
  Muster[20] := ' dkkl+n+o+mkkbkkl+o+n+mkkf ';
  Muster[21] := ' j++++++j++++j++++j++++++j ';
  Muster[22] := ' j+mkkkkhkkl+j+mkkhkkkkl+j ';
  Muster[23] := ' j*++++++++++j++++++++++*j ';
  Muster[24] := ' gkkkkkkkkkc+j+akkkkkkkkki ';
  Muster[25] := '           j j j           ';
  RahmenFarbe := 9;
end;

procedure Level6( var Muster : Mustertyp );
begin
  Muster[ 1] := '           j j j           ';      { 27 x 25 Punkte }
  Muster[ 2] := ' akkkkkkkkki j gkkkkkkkkkc ';
  Muster[ 3] := ' j*++++++++++j++++++++++*j ';
  Muster[ 4] := ' j+akkc+akkc+j+akkc+akkc+j ';
  Muster[ 5] := ' j+j  j+j  j+j+j  j+j  j+j ';
  Muster[ 6] := ' j+gkki+gkki+j+gkki+gkki+j ';
  Muster[ 7] := ' j+++++++++++j+++++++++++j ';
  Muster[ 8] := ' j+mkkl+o+mkkekkl+o+mkkl+j ';
  Muster[ 9] := ' j++++++j++++j++++j++++++j ';
  Muster[10] := ' dkkkkc+dkkl+n+mkkf+akkkkf ';
  Muster[11] := ' j    j+j++++ ++++j+j    j ';
  Muster[12] := 'khkkkki+n+akl mkc+n+gkkkkhk';
  Muster[13] := '  ++++++++j     j++++++++  ';
  Muster[14] := 'kbkkkkc+o+gkkkkki+o+akkkkbk';
  Muster[15] := ' j    j+j++++ ++++j+j    j ';
  Muster[16] := ' dkkkki+n+mkkbkkl+n+gkkkkf ';
  Muster[17] := ' j+++++++++++j+++++++++++j ';
  Muster[18] := ' j+mkkc+mkkl+n+mkkl+akkl+j ';
  Muster[19] := ' j++++j++++++ ++++++j++++j ';
  Muster[20] := ' dkkl+n+o+mkkbkkl+o+n+mkkf ';
  Muster[21] := ' j++++++j++++j++++j++++++j ';
  Muster[22] := ' j+mkkkkhkkl+j+mkkhkkkkl+j ';
  Muster[23] := ' j*++++++++++j++++++++++*j ';
  Muster[24] := ' gkkkkkkkkkc+j+akkkkkkkkki ';
  Muster[25] := '           j j j           ';
  RahmenFarbe := 10;
end;

procedure Level7( var Muster : Mustertyp );
begin
  Muster[ 1] := '           j j j           ';      { 27 x 25 Punkte }
  Muster[ 2] := ' akkkkkkkkki j gkkkkkkkkkc ';
  Muster[ 3] := ' j*++++++++++j++++++++++*j ';
  Muster[ 4] := ' j+akkc+akkc+j+akkc+akkc+j ';
  Muster[ 5] := ' j+j  j+j  j+j+j  j+j  j+j ';
  Muster[ 6] := ' j+gkki+gkki+j+gkki+gkki+j ';
  Muster[ 7] := ' j+++++++++++j+++++++++++j ';
  Muster[ 8] := ' j+mkkl+o+mkkekkl+o+mkkl+j ';
  Muster[ 9] := ' j++++++j++++j++++j++++++j ';
  Muster[10] := ' dkkkkc+dkkl+n+mkkf+akkkkf ';
  Muster[11] := ' j    j+j++++ ++++j+j    j ';
  Muster[12] := 'khkkkki+n+akl mkc+n+gkkkkhk';
  Muster[13] := '  ++++++++j     j++++++++  ';
  Muster[14] := 'kbkkkkc+o+gkkkkki+o+akkkkbk';
  Muster[15] := ' j    j+j++++ ++++j+j    j ';
  Muster[16] := ' dkkkki+n+mkkbkkl+n+gkkkkf ';
  Muster[17] := ' j+++++++++++j+++++++++++j ';
  Muster[18] := ' j+mkkc+mkkl+n+mkkl+akkl+j ';
  Muster[19] := ' j++++j++++++ ++++++j++++j ';
  Muster[20] := ' dkkl+n+o+mkkbkkl+o+n+mkkf ';
  Muster[21] := ' j++++++j++++j++++j++++++j ';
  Muster[22] := ' j+mkkkkhkkl+j+mkkhkkkkl+j ';
  Muster[23] := ' j*++++++++++j++++++++++*j ';
  Muster[24] := ' gkkkkkkkkkc+j+akkkkkkkkki ';
  Muster[25] := '           j j j           ';
  RahmenFarbe := 11;
end;

procedure Level8( var Muster : Mustertyp );
begin
  Muster[ 1] := '           j j j           ';      { 27 x 25 Punkte }
  Muster[ 2] := ' akkkkkkkkki j gkkkkkkkkkc ';
  Muster[ 3] := ' j*++++++++++j++++++++++*j ';
  Muster[ 4] := ' j+akkc+akkc+j+akkc+akkc+j ';
  Muster[ 5] := ' j+j  j+j  j+j+j  j+j  j+j ';
  Muster[ 6] := ' j+gkki+gkki+j+gkki+gkki+j ';
  Muster[ 7] := ' j+++++++++++j+++++++++++j ';
  Muster[ 8] := ' j+mkkl+o+mkkekkl+o+mkkl+j ';
  Muster[ 9] := ' j++++++j++++j++++j++++++j ';
  Muster[10] := ' dkkkkc+dkkl+n+mkkf+akkkkf ';
  Muster[11] := ' j    j+j++++ ++++j+j    j ';
  Muster[12] := 'khkkkki+n+akl mkc+n+gkkkkhk';
  Muster[13] := '  ++++++++j     j++++++++  ';
  Muster[14] := 'kbkkkkc+o+gkkkkki+o+akkkkbk';
  Muster[15] := ' j    j+j++++ ++++j+j    j ';
  Muster[16] := ' dkkkki+n+mkkbkkl+n+gkkkkf ';
  Muster[17] := ' j+++++++++++j+++++++++++j ';
  Muster[18] := ' j+mkkc+mkkl+n+mkkl+akkl+j ';
  Muster[19] := ' j++++j++++++ ++++++j++++j ';
  Muster[20] := ' dkkl+n+o+mkkbkkl+o+n+mkkf ';
  Muster[21] := ' j++++++j++++j++++j++++++j ';
  Muster[22] := ' j+mkkkkhkkl+j+mkkhkkkkl+j ';
  Muster[23] := ' j*++++++++++j++++++++++*j ';
  Muster[24] := ' gkkkkkkkkkc+j+akkkkkkkkki ';
  Muster[25] := '           j j j           ';
  RahmenFarbe := 12;
end;

procedure Level9( var Muster : Mustertyp );
begin
  Muster[ 1] := '           j j j           ';      { 27 x 25 Punkte }
  Muster[ 2] := ' akkkkkkkkki j gkkkkkkkkkc ';
  Muster[ 3] := ' j*++++++++++j++++++++++*j ';
  Muster[ 4] := ' j+akkc+akkc+j+akkc+akkc+j ';
  Muster[ 5] := ' j+j  j+j  j+j+j  j+j  j+j ';
  Muster[ 6] := ' j+gkki+gkki+j+gkki+gkki+j ';
  Muster[ 7] := ' j+++++++++++j+++++++++++j ';
  Muster[ 8] := ' j+mkkl+o+mkkekkl+o+mkkl+j ';
  Muster[ 9] := ' j++++++j++++j++++j++++++j ';
  Muster[10] := ' dkkkkc+dkkl+n+mkkf+akkkkf ';
  Muster[11] := ' j    j+j++++ ++++j+j    j ';
  Muster[12] := 'khkkkki+n+akl mkc+n+gkkkkhk';
  Muster[13] := '  ++++++++j     j++++++++  ';
  Muster[14] := 'kbkkkkc+o+gkkkkki+o+akkkkbk';
  Muster[15] := ' j    j+j++++ ++++j+j    j ';
  Muster[16] := ' dkkkki+n+mkkbkkl+n+gkkkkf ';
  Muster[17] := ' j+++++++++++j+++++++++++j ';
  Muster[18] := ' j+mkkc+mkkl+n+mkkl+akkl+j ';
  Muster[19] := ' j++++j++++++ ++++++j++++j ';
  Muster[20] := ' dkkl+n+o+mkkbkkl+o+n+mkkf ';
  Muster[21] := ' j++++++j++++j++++j++++++j ';
  Muster[22] := ' j+mkkkkhkkl+j+mkkhkkkkl+j ';
  Muster[23] := ' j*++++++++++j++++++++++*j ';
  Muster[24] := ' gkkkkkkkkkc+j+akkkkkkkkki ';
  Muster[25] := '           j j j           ';
  RahmenFarbe := 13;
end;

procedure Uebersetzen( Muster : Mustertyp;
                       Level  : Byte );
var
  x,y : Byte;
  s   : String[2];
  ch  : char;
begin
  TextBackground(0);
  for y := 1 to 25 do begin
    GotoXY(1,y);
    for x := 1 to 27 do begin
      ch := Muster[y][x];
      if ch in ['+','*',' ']  then Textcolor(15);
      if ch in ['a'..'o'] then Textcolor(RahmenFarbe);
      case ch of
        ' ' : s := '  ';   { Leer }
        '+' : s := '×®';
        '*' : s := 'Ö¯';
        'a' : s := 'ÓÐ';   { É }
        'b' : s := 'ÒÐ';   { Ë }
        'c' : s := 'Ò·';   { » }
        'd' : s := 'µÇ';   { Ì }
        'e' : s := '¶Ç';   { Î }
        'f' : s := '¶µ';   { ¹ }
        'g' : s := 'ÔÏ';   { È }
        'h' : s := 'ÑÏ';   { Ê }
        'i' : s := 'Ñ½';   { ¼ }
        'j' : s := 'µµ';   { º }
        'k' : s := 'ÆÆ';   { Í }
        'l' : s := 'Æ¾';   { > }
        'm' : s := 'ÕÆ';   { < }
        'n' : s := 'Ô½';   { U }
        'o' : s := 'Ó·';   { ï }
      end;
    Write(s);
    end;
  end;
  Hinweis(59,2,2,30,31,0,'P A C - M A N',false);
  Textcolor(30);
  GotoXY(59, 7); Write(' Punkte : '); TextColor(31); Write('        ');
  Textcolor(30);
  GotoXY(59, 9); Write(' Level  : '); Textcolor(31); Write('        ');
  Textcolor(30);
  GotoXY(59,11); Write(' Leben  : '); Textcolor(31); Write('        ');
  Textcolor(14);
  Str(Level,s);
  Assign(f,pfad+'LEVEL'+s+'.SCR');
  Rewrite(f);
  Write(f,LevelBild);
  close(f);
  readln;
end;



begin
  Clrscr;
  CursorAus;
  blinken(false);
  Level1( Muster[1] ); Uebersetzen( Muster[1], 1);
  Level2( Muster[2] ); Uebersetzen( Muster[2], 2);
  Level3( Muster[3] ); Uebersetzen( Muster[3], 3);
  Level4( Muster[4] ); Uebersetzen( Muster[4], 4);
  Level5( Muster[5] ); Uebersetzen( Muster[5], 5);
  Level6( Muster[6] ); Uebersetzen( Muster[6], 6);
  Level7( Muster[7] ); Uebersetzen( Muster[7], 7);
  Level8( Muster[8] ); Uebersetzen( Muster[8], 8);
  Level9( Muster[9] ); Uebersetzen( Muster[9], 9);
  clrscr;
  Textcolor(12);
  Zeichenaendern;
  CursorEin;
end.
