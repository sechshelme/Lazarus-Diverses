{***********************************************************************
*  M I K A D O P : Ein Programm, das die Nutzung des 512-Zeichen-Modus *
*                  bei EGA- und VGA-Karten am Beispiel der Grafik-     *
*                  erzeugung innerhalb des Textmodus demonstriert.     *
**--------------------------------------------------------------------**
*  Autor            : MICHAEL TISCHER                                  *
*  entwickelt am    :  2.04.1990                                       *
*  letztes Update am: 14.01.1991                                       *
***********************************************************************}

program MikadoP;

uses DOS, CRT, Ports;                           { DOS- und CRT-Unit einbinden }

{-- Konstanten --------------------------------------------------------}

const EGAVGA_SEQUENCER = $3C4;            { Sequencer Adre·-/Datenport }
      EGAVGA_MONCTR    = $3D4;                    { Monitor-Controller }
      EGAVGA_GRAPHCTR  = $3CE;  { Graphics-Controller Adre·-/Datenport }
      CHAR_BREITE      = 8;
      CHAR_BYTES       = 32;
      MIKADOS          = 5; { Anzahl der gleichz. gezeichneten Mikados }

{-- Typdeklarationen --------------------------------------------------}

type VEL      = record             { beschreibt eine Zeichen-Attribut- }
                  case boolean of  { Kombination im Video-RAM          }
                    true  : ( Zeichen, Attribut : byte );
                    false : ( Inhalt            : word );
                end;
     VPTR     = ^VEL;                { Zeiger auf ein Zeichen/Attribut }
     VELARRAY = array [1..25,1..80] of VEL;  {Nachbildung des Video-RAM}
     VELARPTR = ^VELARRAY;                  { Zeiger auf den Video-RAM }

     FONT = array[0..255,0..CHAR_BYTES-1] of byte;  { Nachbildung Font }
     FPTR = ^font;                             { Zeiger auf einen Font }

     PALARY = array[ 1..16] of BYTE;      { ein Satz Paletten-Register }

{-- globale Variablen -------------------------------------------------}

const vioptr : VELARPTR = ptr( $B800, $0000 );      { Zeiger auf V-RAM }

var CharHoehe,
    lenx    : byte;             { Breite des Grafikfensters in Zeichen }
    xmax,                   { Max-Pixel-Koordinaten des Grafikfensters }
    ymax    : integer;
    fontptr : fptr;                       { Zeiger auf den Grafik-Font }

procedure CLI; assembler; asm db $FA end ; 
procedure STI; assembler; asm db $FB end;

{***********************************************************************
*  IsEgaVga : Stellt fest, ob eine EGA- oder VGA-Karte installiert ist.*
*             und legt gleichzeitig die Anzahl der Scan-Zeilen pro Zei-*
*             chen in der globalen Variablen CharHoehe ab.             *
**--------------------------------------------------------------------**
*  Eingabe : keine                                                     *
*  Ausgabe : TRUE, wenn EGA- oder VGA-KArte, sonst FALSE               *
***********************************************************************}

function IsEgaVga : boolean;

var Regs : Registers;          { Prozessorregister fÅr Interruptaufruf }

begin
  Regs.AX := $1a00;                 { Funktion 1Ah gibt es nur bei VGA }
  Intr( $10, Regs );
  if ( Regs.AL = $1a ) then              { ist die Funktion verfÅgbar? }
    begin
      IsEgaVGa := TRUE;
      CharHoehe := 16;                                { 16 Scan-Zeilen }
    end
  else
    begin
      Regs.ah := $12;                     { Funktion 12h Unterfunktion }
      Regs.bl := $10;                     { 10h aufrufen               }
      intr($10, Regs);                           { Video-BIOS aufrufen }
      IsEgaVga := ( Regs.bl <> $10 );
      CharHoehe := 14;                                { 14 Scan-Zeilen }
    end;
end;

{***********************************************************************
*  SetCharWidth: Legt die Breite der Zeichen bei VGA-Karten auf 8 oder *
*                9 Punkte fest.                                        *
**--------------------------------------------------------------------**
*  Eingabe : BREITE = Breite der Zeichen (8 oder 9)                    *
***********************************************************************}

procedure SetCharWidth( breite : byte );

var Regs : Registers;          { Prozessorregister zum Interruptaufruf }
    x    : byte;                  { nimmt Wert fÅr Misc-Output-Reg auf }

begin
  if ( breite = 8 ) then Regs.BX := $0001    { BH = horiz. Ausrichtung }
                    else Regs.BX := $0800;   { BL = Wert fÅr Seq. Reg. }

  x := port[ $3CC ] and not(4+8);  { horizontale Auflîsung von 720 auf }
  if ( breite = 9 ) then           { 640 Punkte umschalten und umge-   }
    x := x or 4;                   { kehrt                             }
  port[ $3C2 ] := x;

  CLI;        { Sequencer von 8 auf 9 Punkte nzw. umgekehrt umschalten }
  portw[ EGAVGA_SEQUENCER ] := $0100;
  portw[ EGAVGA_SEQUENCER ] := $01 + Regs.BL shl 8;
  portw[ EGAVGA_SEQUENCER ] := $0300;
  STI;

  Regs.AX := $1000;                   { Bildschirmausrichtung anpassen }
  Regs.BL := $13;
  intr( $10, Regs );
end;

{***********************************************************************
*  SelectMaps : WÑhlt die ZeichensÑtze aus, zwischen denen Åber das    *
*               Bit 3 im Attribut-Byte der Zeichen ausgewÑhlt wird.    *
**--------------------------------------------------------------------**
*  Eingabe : MAP0 = Nummer des ersten Zeichensatzes  (Bit 3 = 0 )      *
*            MAP1 = Nummer des zweiten Zeichensatzes (Bit 3 = 1 )      *
*  Info    : - Bei einer EGA-Karte kînnen die ZeichensÑtze 0 bis 3,    *
*              bei einer VGA-Karte die ZeichensÑtze 0 bis 7 gewÑhlt    *
*              werden                                                  *
***********************************************************************}

procedure SelectMaps( map0, map1 : byte );

var Regs : Registers;          { Prozessorregister zum Interruptaufruf }

begin
  Regs.AX := $1103;  { das Character-Map-Select-Register programmieren }
  Regs.BL := ( ( map0 and 3 ) + ( map0 and 4 ) shl 2 ) +
             ( ( map1 and 3 ) shl 2 + ( map1 and 4 ) shl 3 );
  Intr( $10, Regs );      { BIOS-Funktion $11, Unterfunktion $03 aufr. }
end;

{***********************************************************************
*  GetFontAccess: Erlaubt den direkten Zugriff auf die zweite Memory-  *
*                 map in der die ZeichensÑtze gelagert werden, Åber    *
*                 die Adresse A000:0000                                *
**--------------------------------------------------------------------**
*  Eingabe : keine                                                     *
*  Info    : - Nach dem Aufruf dieser Prozedur ist kein Zugriff auf    *
*              den Video-RAM bei B800:0000 mehr mîglich!               *
***********************************************************************}

procedure GetFontAccess;

const SeqRegs : array[1..4] of word = ( $0100, $0402, $0704, $0300 );
      GCRegs  : array[1..3] of word = ( $0204, $0005, $0406 );

var i : byte;                                        { SchleifenzÑhler }

begin
  CLI;
  for i := 1 to 4 do  { die verschiedenen Register des Sequencer laden }
    portw[ EGAVGA_SEQUENCER ] := SeqRegs[ i ];

  for i := 1 to 3 do     { die Register des Graphics-Controllers laden }
    portw[ EGAVGA_GRAPHCTR ] := GCRegs[ i ];
  STI;
end;

{***********************************************************************
*  ReleaseFontAccess: Gibt den Zugriff auf den Video-RAM bei B800:000  *
*                     wieder frei, versperrt damit aber den Zugang zu  *
*                     den ZeichensÑtzen in der Memory-Page #2.         *
**--------------------------------------------------------------------**
*  Eingabe : keine                                                     *
***********************************************************************}

procedure ReleaseFontAccess;

const SeqRegs : array[1..4] of word = ( $0100, $0302, $0304, $0300 );
      GCRegs  : array[1..3] of word = ( $0004, $1005, $0E06 );

var i : byte;                                        { SchleifenzÑhler }

begin
  for i := 1 to 4 do  { die verschiedenen Register des Sequencer laden }
    portw[ EGAVGA_SEQUENCER ] := SeqRegs[ i ];
  for i := 1 to 3 do     { die Register des Graphics-Controllers laden }
    portw[ EGAVGA_GRAPHCTR ] := GCRegs[ i ];
end;

{***********************************************************************
*  ClearGraphArea: Lîscht den Grafikbereich, indem die Zeichenmuster   *
*                  der darin enthaltenen Zeichen auf 0 gesetzt werden. *
**--------------------------------------------------------------------**
*  Eingabe : keine                                                     *
***********************************************************************}

procedure ClearGraphArea;

var zeichen,                           { die zu durchlaufenden Zeichen }
    zeile    : byte;         { Zeile innerhalb des jeweiligen Zeichens }

begin
  for zeichen := 0 to 255 do                 { die Zeichen durchlaufen }
    for zeile := 0 to CharHoehe-1 do          { die Zeilen durchlaufen }
      fontptr^[ zeichen, zeile ] := 0;              { und auf 0 setzen }
end;

{***********************************************************************
*  InitGraphArea: Bereitet einen Bildschirmbereich auf die Ausgabe von *
*                 grafischen Darstellungen vor.                        *
**--------------------------------------------------------------------**
*  Eingabe : X     = Startspalte des Bereichs (1-80)                   *
*            Y     = Startzeile des Bereichs (1-25)                    *
*            XLEN  = Breite des Bereichs in Zeichen                    *
*            YLEN  = Tiefe des Bereichs in Zeichen                     *
*            MAP   = Nummer des Grafik-Zeichensatzes                   *
*            FARBE = Farbe des Grafikbereichs (0 bis 7 oder $FF)       *
*  Info    : - Wird als Farbe der Wert $FF angegeben, wird ein fort-   *
*              laufender Farbcode erzeugt, der fÅr den 'Mikado'-Effekt *
*              antwortlich ist                                         *
***********************************************************************}

procedure InitGraphArea( x, y, xlen, ylen, map, farbe : byte );

var spalte, zeile : integer;                      { Schleifenvariablen }
    zcode         : byte;                  { fortlaufender Zeichencode }

begin
  if ( xlen * ylen > 256 ) then                     { Bereich zu gro·? }
    writeln( 'Fehler: Der Grafikbereich darf maximal' +
             ' 256 Zeichen umfassen!' )
  else
    begin
      if ( CharHoehe = 16 ) then                                { VGA? }
       SetCharWidth( 8 );                { Ja, Zeichenbreite umstellen }
      SelectMaps( 0, map );                   { ZeichensÑtze auswÑhlen }
      xmax := xlen*CHAR_BREITE;      { Max-Pixel-Koordinaten berechnen }
      ymax := ylen*CharHoehe;
      lenx := xlen;
      fontptr := ptr( $A000, map * $4000 );    { Zeiger auf Grafik-Map }
      GetFontAccess;            { Zugriff auf ZeichensÑtze ermîglichen }
      ClearGraphArea;                            { Zeichensatz lîschen }
      ReleaseFontAccess;       { Zugriff auf Video-RAM wieder erlauben }

      {-- den Grafikbereich mit fortlaufenden Zeichencodes fÅllen -----}

      zcode := 0;
      for zeile := ylen-1 downto 0 do     { Zeilen von unten nach oben }
        for spalte := 0 to xlen-1 do {Sp. von links nach rechts durchl.}
          begin                      { Zeichencode und Attribut setzen }
            vioptr^[zeile+y,spalte+x].Zeichen := zcode;
            if ( farbe = $ff ) then
              vioptr^[zeile+y,spalte+x].Attribut := zcode mod 6 + 1 + 8
            else
              vioptr^[zeile+y,spalte+x].Attribut := farbe or $08;
            inc( zcode );                       { nÑchster Zeichencode }
          end;
    end;
end;

{***********************************************************************
*  CloseGraphArea: Beendet die Arbeit mit dem Grafikbereich.           *
**--------------------------------------------------------------------**
*  Eingabe : keine                                                     *
***********************************************************************}

procedure CloseGraphArea;

begin
  ReleaseFontAccess;                 { Zugriff auf Video-RAM freigeben }
  SelectMaps( 0, 0 );                   { immer Zeichensatz 0 anzeigen }
  if ( CharHoehe = 16 ) then                                    { VGA? }
   SetCharWidth( 9 );                    { Ja, Zeichenbreite umstellen }
end;

{***********************************************************************
*  SetPixel: Setzt oder lîscht einen Pixel innerhalb des Grafikfensters*
**--------------------------------------------------------------------**
*  Eingabe : X,Y   = Pixelkoordinate (0-...)                           *
*            ON    = TRUE fÅr setzen und FALSE fÅr lîschen             *
***********************************************************************}

procedure SetPixel( x, y : integer; on : boolean );

var charnum,                 { Zeichencode fÅr Zeichen Åber Koordinate }
    line     : byte;                    { Zeile innerhalb des Zeichens }

begin
  if ( x < xmax ) and ( y < ymax ) then             { Koordinate o.k.? }
    begin                      { Ja, Zeichennummer und Zeile berechnen }
      charnum := ((x div CHAR_BREITE) + (y div CharHoehe * lenx));
      line    := CharHoehe - ( y mod CharHoehe ) - 1;
      if on then                        { Zeichen setzen oder lîschen? }
        fontptr^[charnum, line] := fontptr^[charnum, line] or
             1 shl (CHAR_BREITE - 1 - ( x mod CHAR_BREITE ) )
      else
        fontptr^[charnum, line] := fontptr^[charnum, line] and
           not( 1 shl (CHAR_BREITE - 1 - ( x mod CHAR_BREITE ) ) );
    end;
end;

{***********************************************************************
*  Line: Zieht eine Linie innerhalb des Grafikfensters nach dem Bresen-*
*        ham-Algorithmus                                               *
**--------------------------------------------------------------------**
*  Eingabe : X1, Y1 = Startkoordinate (0 - ...)                        *
*            X2, Y2 = Endekoordinate                                   *
*            ON     = TRUE fÅr Punkte setzen und FALSE fÅr P. lîschen  *
***********************************************************************}

procedure Line( x1, y1, x2, y2 : integer; on : boolean );

var d, dx, dy,
    aincr, bincr,
    xincr, yincr,
    x, y                 : integer;

{-- Unterprozedur, vertauscht zwei Integer-Variablen ------------------}

procedure SwapInt( var i1, i2: integer );

var dummy : integer;

begin
  dummy := i2;
  i2    := i1;
  i1    := dummy;
end;

{-- Hauptprozedur -----------------------------------------------------}

begin
  if ( abs(x2-x1) < abs(y2-y1) ) then   { Åber X- oder Y-Achse laufen? }
    begin                                           { Åber die Y-Achse }
      if ( y1 > y2 ) then                              { y1 grî·er y2? }
        begin
          SwapInt( x1, x2 );                   { Ja, X1 mit X2 und     }
          SwapInt( y1, y2 );                   { Y1 mit Y2 vertauschen }
        end;

      if ( x2 > x1 ) then xincr := 1      { Spalteninkrement festlegen }
                     else xincr := -1;

      dy := y2 - y1;
      dx := abs( x2-x1 );
      d  := 2 * dx - dy;
      aincr := 2 * (dx - dy);
      bincr := 2 * dx;
      x := x1;
      y := y1;

      SetPixel( x, y, on );                      { ersten Punkt setzen }
      for y:=y1+1 to y2 do              { Line auf X-Achse durchlaufen }
        begin
          if ( d >= 0 ) then
            begin
              inc( x, xincr );
              inc( d, aincr );
            end
          else
            inc( d, bincr );
          SetPixel( x, y, on );
        end;
    end
  else                                              { Åber die X-Achse }
    begin
      if ( x1 > x2 ) then                              { x1 grî·er x2? }
        begin
          SwapInt( x1, x2 );                   { Ja, X1 mit X2 und     }
          SwapInt( y1, y2 );                   { Y1 mit Y2 vertauschen }
        end;

      if ( y2 > y1 ) then yincr := 1       { Zeileninkrement festlegen }
                     else yincr := -1;

      dx := x2 - x1;
      dy := abs( y2-y1 );
      d  := 2 * dy - dx;
      aincr := 2 * (dy - dx);
      bincr := 2 * dy;
      x := x1;
      y := y1;

      SetPixel( x, y, on );                      { ersten Punkt setzen }
      for x:=x1+1 to x2 do              { Line auf X-Achse durchlaufen }
        begin
          if ( d >= 0 ) then
            begin
              inc( y, yincr );
              inc( d, aincr );
            end
          else
            inc( d, bincr );
          SetPixel( x, y, on );
       end;
    end;
end;

{***********************************************************************
*  SetPalCol: definiert eine Farbe aus der 16-teiligen-Farbpalette     *
*             oder die Farbe des Bildschirmrahmens (Overscan-Color)    *
**--------------------------------------------------------------------**
*  Eingabe : RegNr = Nummer des Paletten-Registers (0 bis 15) oder 16  *
*                    fÅr die Farbe des Bildschirmrahmens               *
*            Col   = Farbwert im Bereich 0 bis 15                      *
***********************************************************************}

procedure SetPalCol( RegNr : byte; Col : byte );

var Regs   : Registers;        { Prozessorregister zum Interruptaufruf }

begin
  Regs.AX := $1000;            { Video-Funktion 10h, Unterfunktion 00h }
  Regs.BH := Col;                                           { Farbwert }
  Regs.BL := RegNr;          { Registernummer des Attribut-Controllers }
  intr( $10, Regs );                   { BIOS-Video-Interrupt aufrufen }
end;

{***********************************************************************
*  SetPalAry: installiert eine neue Farbpalette mit 16 Farben, ohne    *
*             allerdings die Farbe des Bildschirmrahmens zu verÑndern. *
**--------------------------------------------------------------------**
*  Eingabe : NewCol = Paletten-Array vom Typ PALARY                    *
***********************************************************************}

procedure SetPalAry( NewCol : PALARY );

var i : byte;                                        { SchleifenzÑhler }

begin
  for i := 1 to 16 do           { die 16 EintrÑge im Array durchlaufen }
    SetPalCol( i-1, NewCol[ i ] );         { jeweils eine Farbe setzen }
end;

{***********************************************************************
*  GetPalCol: ermittelt den Inhalt eines Paletten-Registers            *
**--------------------------------------------------------------------**
*  Eingabe : RegNr = Nummer des Paletten-Registers (0 bis 15) oder 16  *
*                    fÅr die Farbe des Bildschirmrahmens               *
*  Ausgabe : der Farbwert                                              *
*  Info    : Bei der EGA-Karte ist ein Auslesen der Paletten-Register  *
*            nicht mîglich, deshalb wird hier von der normalen Ein-    *
*            stellung der Paletten-Register ausgegangen und der jeweils*
*            entsprechende Wert zurÅckgeliefert.                       *
***********************************************************************}

function GetPalCol( RegNr : byte ) : byte;

var Regs   : Registers;        { Prozessorregister zum Interruptaufruf }

begin
  if ( CharHoehe = 14 ) then                              { EGA-Karte? }
   GetPalCol := RegNr  {Ja, kein Auslesen der Paletten-Register mîglich}
  else                                                     { Nein, VGA }
    begin
      Regs.AX := $1007;        { Video-Funktion 10h, Unterfunktion 07h }
      Regs.BL := RegNr;      { Registernummer des Attribut-Controllers }
      intr( $10, Regs );               { BIOS-Video-Interrupt aufrufen }
      GetPalCol := Regs.BH;  { hier steht der Inhalt des Pal-Registers }
    end;
end;

{***********************************************************************
*  GetPalAry: ermittelt die Inhalt der 16-Paletten-Register und trÑgt  *
*             sie in ein Array des Aufrufers ein.                      *
**--------------------------------------------------------------------**
*  Eingabe : ColAry = Paletten-Array vom Typ PALARY, in das die er-    *
*                     mittelten Farben eingatragen werden              *
***********************************************************************}

procedure GetPalAry( var ColAry : PALARY );

var i : byte;                                        { SchleifenzÑhler }

begin
  for i := 1 to 16 do           { die 16 EintrÑge im Array durchlaufen }
    ColAry[ i ] := GetPalCol( i-1 );    { jeweils eine Farbe ermitteln }
end;

{***********************************************************************
*  Mikado: Demonstriert die Arbeit mit den verschiedenen Routinen aus  *
*          diesem Programm                                             *
**--------------------------------------------------------------------**
*  Eingabe : keine                                                     *
***********************************************************************}

procedure Mikado;

type linie = record                { nimmt Koordinaten einer Linie auf }
               x1, y1,
               x2, y2  : integer;
             end;

const NewCols : PALARY =
        ( {--------- Farben fÅr die normalen Textzeichen --------------}
          BLACK,                                 { vormals ... schwarz }
          BLUE,                                  {                blau }
          GREEN,                                 {                grÅn }
          RED,                                   {              kobalt }
          CYAN,                                  {                 rot }
          MAGENTA,                               {             violett }
          YELLOW,                                {               braun }
          WHITE,                                 {            hellgrau }
          {------------- hier beginnen die Grafik-Farben --------------}
          LIGHTBLUE,                             { vormals  dunkelgrau }
          LIGHTGREEN,                            {            hellblau }
          LIGHTRED,                              {            hellgrÅn }
          LIGHTCYAN,                             {       helles kobalt }
          LIGHTMAGENTA,                          {             hellrot }
          BLUE,                                  {         hellviolett }
          YELLOW,                                {                gelb }
          WHITE );                               {               weiss }

var i,                                               { SchleifenzÑhler }
    first,                         { Array-Index des jÅngesten Mikados }
    last   : integer;               { Array-Index des Ñltesten Mikados }
    clear  : boolean;                                { Mikados lîschen }
    lar    : array[1..MIKADOS] of linie;                { Mikado-Array }
    OldCols: PALARY;                      { nimmt die alten Farben auf }

begin
  GetPalAry( OldCols );        { bisherige Farbeinstellungen ermitteln }
  SetPalAry( NewCols );                { neue Farbpalette installieren }
  TextColor( 7 );
  TextBackGround( 1 );
  ClrScr;                                         { Bildschirm lîschen }
  GotoXY(1,1);                     { und mit Zeichen aus dem Standard- }
  for i:=1 to 25*80-1 do           { Zeichensatz fÅllen                }
   write( chr(32 + i mod 224) );

  {-- Grafikbereich initialisieren und Mikados fallen lassen ----------}

  GotoXY(27,6);
  TextColor( 7 );
  TextBackGround( 3 );
  write('       M I K A D O       ' );
  GotoXY(27,6);
  InitGraphArea( 27, 7, 25, 10, 1,  $FF );
  GetFontAccess;                     { Zugriff auf Zeichensatz sichern }

  clear := false;                         { noch keine Mikados lîschen }
  first := 1;                     { mit erster Array-Position beginnen }
  last := 1;
  repeat                                             { Mikado-Schleife }
    if first = MIKADOS+1 then first := 1;               { Wrap-Around? }
    lar[first].x1 := random( xmax-1 );               { Mikado erzeugen }
    lar[first].x2 := random( xmax-1 );
    lar[first].y1 := random( ymax-1 );
    lar[first].y2 := random( ymax-1 );
    line( lar[first].x1, lar[first].y1,                 { und zeichnen }
          lar[first].x2, lar[first].y2, true );
    inc( first );                                    { nÑchstes Mikado }
    if first = MIKADOS+1 then clear := true;          { schon lîschen? }
    if clear then                                     { jetzt lîschen? }
      begin                                                       { Ja }
        line( lar[last].x1, lar[last].y1,
              lar[last].x2, lar[last].y2, false );
        inc( last );                  { gleich nÑchstes Mikado lîschen }
        if last = MIKADOS+1 then last := 1;
      end;
  until keypressed;                  { wiederholen, bis Taste gedrÅckt }

  {-- Programm beenden ------------------------------------------------}

  CloseGraphArea;
  SetPalAry( OldCols );         { alte Farbpalette wieder restaurieren }
  GotoXY(1, 25 );
  TextColor( 7 );
  TextBackGround( 0 );
  ClrEol;
  writeln( 'Jetzt ist wieder der normale Zeichensaz aktiv.');
end;

{----------------------------------------------------------------------}
{-- H A U P T P R O G R A M M -----------------------------------------}
{----------------------------------------------------------------------}

begin
  if IsEgaVga then         { ist eine EGA- oder VGA-Karte installiert? }
    Mikado                                      { Ja, Demo durchfÅhren }
  else                    { Nein, Programm kann nicht gestartet werden }
    writeln( 'Achtung! Keine EGA- oder VGA-Karte installiert.' );
end.








