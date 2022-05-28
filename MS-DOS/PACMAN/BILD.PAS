unit bild;
interface

uses
  maus,dos,crt;
type
  bildschirm=array[1..2000] of record
                                 zeichen:char;
                                    attr:byte;
                                end;
  bildpointer = ^bildschirm;
  menupunkte  =  array[1..20] of string[30];

var
  regs     :registers;
  farb:bildschirm absolute $b800:0000;

procedure cursoraus;
procedure cursorein;
procedure Rahmnefarbe(farbe:Byte);
procedure Blinken(bl : boolean);
procedure zkette(zeichen,anzahl,fvg,fhg:byte); { Gibt Zeichenketten aus      }
procedure zeichen(zei,fvg,fhg:byte);           { Gibt einzelnes Zeichen aus  }
procedure scrolloben (x1,y1,x2,y2,anzahl:byte); { Scrollt Bildschrim     }
procedure scrollunten(x1,y1,x2,y2,anzahl:byte); {        -"-             }
procedure rahmen(x1,y1,x2,y2,typ,fvg,fhg:byte;      { Zeichnet Rahmen        }
                        fuellen,Schatten:boolean);
procedure Hintergrund(muster,fvg,fhg,musterr,fvgr,fhgr:byte);
procedure Titel(Titeltext : String;
                fvg,fhg   : Byte);
procedure Hinweis(x,y,typ,frahmen,ftext,fhg : byte;
                                        hin : string;
                                   schatten : boolean);
procedure bildspeichern(var bildname:bildpointer);
procedure bildholen(bildname:bildpointer);
function Zeichenlesen ( x, y : Byte ) : Byte;
function Attributlesen( x, y : Byte ) : Byte;
function menufenster(x,y,anzahl,pos,uafvg,uafhg,afvg,afhg,rfarbe:byte;
                                                    menupkt:menupunkte):byte;


implementation


{----------------------------------------------------------------------------}

procedure cursoraus;
begin
  regs.ah:=1;
  regs.ch:=1;
  regs.cl:=0;
  intr($10,regs);
end;

procedure cursorein;
begin
  regs.ah:=1;
  regs.ch:=7;
  regs.cl:=8;
  intr($10,regs);
end;

{----------------------------------------------------------------------------}

procedure Rahmnefarbe(farbe:Byte);
begin
  regs.ah:=$10;
  regs.al:=$01;
  regs.bh:=farbe;
  intr($10,regs);
end;

{----------------------------------------------------------------------------}

procedure Blinken(bl : boolean);
begin
  if bl then regs.bl := 1
        else regs.bl := 0;
  regs.ah:=$10;
  regs.al:=$03;
  intr($10,regs);
end;

{----------------------------------------------------------------------------}

function attribut(fvg,fhg:byte):byte;  { Rechnet Farben in Attribut aus }
var
  attr:byte;
begin
  attr:=0;
  if fvg>15 then begin
     fvg:=fvg-16;attr:=128;
  end;
  if (fhg>7) or (fvg>15) then EXIT;
  attr:=attr+fvg+fhg*16;
  attribut:=attr;
end;

{----------------------------------------------------------------------------}

procedure zkette(zeichen,anzahl,fvg,fhg:byte);  { Gibt Zeichenketten aus }
begin
  regs.ah:=$09;
  regs.al:=zeichen;
  regs.cx:=anzahl;
  regs.bl:=attribut(fvg,fhg);
  regs.bh:=0;
  intr($10,regs);
end;

{----------------------------------------------------------------------------}

procedure zeichen(zei,fvg,fhg:byte);
begin
  zkette(zei,1,fvg,fhg);
end;

{----------------------------------------------------------------------------}

function Zeichenlesen( x, y : Byte ) : Byte;
var
  xalt,yalt : Byte;
begin
  xalt := WhereX;
  yalt := WhereY;
  GotoXY(x,y);
  Regs.ah := $08;
  Regs.bh := $00;
  Intr($10,Regs);
  Zeichenlesen := Regs.al;
  GotoXY(xalt,yalt);
end;

{----------------------------------------------------------------------------}

function Attributlesen( x, y : Byte ) : Byte;
var
  xalt,yalt : Byte;
begin
  xalt := WhereX;
  yalt := WhereY;
  GotoXY(x,y);
  Regs.ah := $08;
  Regs.bh := $00;
  Intr($10,Regs);
  Attributlesen := Regs.ah;
  GotoXY(xalt,yalt);
end;

{----------------------------------------------------------------------------}

Function ZeichenlesenRahmen:Byte;
begin
  regs.ah:=8;
  regs.bh:=0;
  intr($10,regs);
  if Regs.al = 219 then Regs.al := 32;
  ZeichenlesenRahmen:=Regs.al;
end;

{----------------------------------------------------------------------------}

procedure scrolloben(x1,y1,x2,y2,anzahl:byte);
begin
  if (x1>x2)OR(y1>y2) then Exit;
  regs.ah:=6;
  regs.al:=anzahl;
  regs.ch:=y1-1;
  regs.cl:=x1-1;
  regs.dh:=y2-1;
  regs.dl:=x2-1;
  regs.bh:=Textattr;
  intr($10,regs);
end;

{----------------------------------------------------------------------------}

procedure scrollunten(x1,y1,x2,y2,anzahl:byte);
begin
  if (x1>x2)OR(y1>y2) then Exit;
  regs.ah:=7;
  regs.al:=anzahl;
  regs.ch:=y1-1;
  regs.cl:=x1-1;
  regs.dh:=y2-1;
  regs.dl:=x2-1;
  regs.bh:=Textattr;
  intr($10,regs);
end;

{----------------------------------------------------------------------------}

procedure Rahmen(x1,y1,x2,y2,typ,fvg,fhg:byte;fuellen,schatten:boolean);
var
  i: byte;
  z1,z2,z3,z4,z5,z6,z7:char;
begin
  gotoxy(10, 20);
  if schatten then
  begin
    gotoxy(x2,y1);zeichen(0,0,0);
    for i := y1+1 to y2 do begin
      gotoxy(x2+2,i);zeichen(zeichenlesenRahmen,8,0);
      gotoxy(x2+1,i);zeichen(zeichenlesenRahmen,8,0);
    end;
    gotoxy(x1+1,y2+1);
    for i:=x1+2 to x2+2 do
    begin
      gotoxy(i,y2+1);zeichen(zeichenlesenRahmen,8,0);
    end;
  end;
  textcolor(fvg);textbackground(fhg);
  case typ of
    0: begin
         z1:=(' ');z2:=(' ');z3:=(' ');
         z4:=(' ');z5:=(' ');z6:=(' ');z7:=(' ');
       end;
    1: begin
         z1:=('Ú');z2:=('Ä');z3:=('¿');
         z4:=('³');z5:=('À');z6:=('Ù');z7:=('Ä');
       end;
    2: begin
         z1:=('É');z2:=('Í');z3:=('»');
         z4:=('º');z5:=('È');z6:=('¼');z7:=('Í');
       end;
    3: begin
         z1:=('Û');z2:=('ß');z3:=('Û');
         z4:=('Û');z5:=('Û');z6:=('Û');z7:=('Ü');
       end;
  end;
  gotoxy(x1,y1);
  write(z1);for i:=1 to x2-x1-1 do write(z2);write(z3);
  for i:=1 to y2-y1-1 do
  begin
    gotoxy(x1,y1+i);write(z4);
    gotoxy(x2,y1+i);write(z4);
  end;
  gotoxy(x1,y2);
  write(z5);for i:=1 to x2-x1-1 do write(z7);zeichen(ord(z6),fvg,fhg);
  if fuellen then
    scrolloben(x1+1,y1+1,x2-1,y2-1,0);
end;

{----------------------------------------------------------------------------}

procedure Hintergrund(muster,fvg,fhg,musterr,fvgr,fhgr:byte);
var
  i:byte;
begin
  gotoxy(1,1);zkette(musterr,80,fvgr,fhgr);
  gotoxy(1,25);zkette(musterr,80,fvgr,fhgr);
  for i:=2 to 24 do
  begin
    gotoxy(1,i);
    zkette(muster,80,fvg,fhg);
  end;
end;

{----------------------------------------------------------------------------}

procedure Hinweis(x,y,typ,frahmen,ftext,fhg : byte;
                                        hin : string;
                                   schatten : boolean);
begin
  rahmen(x,y,x+length(hin)+3,y+2,typ,frahmen,fhg,false,schatten);
  Textcolor(ftext);
  GotoXY(x+1,y+1);Write(' ',hin,' ');
end;

{----------------------------------------------------------------------------}

procedure Titel(Titeltext : String;
                fvg,fhg   : Byte);
begin
  textattr := attribut(fvg,fhg);
  GotoXY(40 - Length(Titeltext) DIV 2 - 1, 1);
  Write(' ',TitelText,' ');
end;

{----------------------------------------------------------------------------}

procedure bildspeichern(var bildname:bildpointer);
begin
  if maxavail<4000 then begin
    Hinweis(3,10,2,15,15,4,'Programm Fehler !!  Programm abrechen, Absturzgefahr',true);
    Readln;
    exit;
  end;
  new(bildname);
  bildname^:=farb;
end;

procedure bildholen(bildname:bildpointer);
begin
  farb := bildname^;
  Dispose(bildname);
end;

{----------------------------------------------------------------------------}

function menufenster(x,y,anzahl,pos,uafvg,uafhg,afvg,afhg,rfarbe:byte; menupkt:menupunkte):byte;
var
  ch : char;
  Merker, pos1,i,laenge: byte;
  ende1: boolean;

begin
  if pos > 100 then begin
    Merker := pos - 100;
    pos := pos - 100; end
  else Merker := 0;
  laenge:=1;
  ende1 := false;
  if afhg>7 then afhg:=afhg mod 8;
  if uafhg>7 then uafhg:=uafhg mod 8;
  if (pos=0) OR (pos>anzahl) then begin pos:=1;
    merker := 0; end;
  for i:=1 to anzahl do
  begin
    if length(menupkt[i])>laenge
      then laenge:=length(menupkt[i]);
  end;
  laenge:=laenge+2;
  rahmen(x,y,x+laenge+1,y+anzahl+1,2,rfarbe,uafhg,true,true);
  textcolor(uafvg);textbackground(uafhg);
  for i:=1 to anzahl do
  begin
    gotoxy(x+1,y+i);
    write(' ',menupkt[i]);
  end;
  GotoMaus(40,y + pos);
  textcolor(afvg);textbackground(afhg);
  gotoxy(x+1,y+pos);
  i:=laenge-length(menupkt[pos])-1;
  write(' ',menupkt[pos]);zkette(32,i,afhg,afhg);
  repeat
    pos1:=pos;
    if keypressed then
    begin
      ch:=readkey;
      if ch=#0 then
      begin
        ch:=readkey;
        case ch of
          #80:begin
                pos:=pos+1;
                if pos>anzahl then pos:=1;
              end;
          #72:begin
                pos:=pos-1;
                if pos<1 then pos:=anzahl;
              end;
          #71:pos:=1;
        end;
      end else
      case ch of
        #27:begin
              pos:=0;
              menufenster:=pos;
              ende1 := true;
            end;
        #13:begin
              menufenster:=pos;
              ende1 := true;
            end;
      end;
    end
    else begin
      if Merker <> 0 then begin
        pos := merker; ende1 := true;
        menufenster := pos; end;
      if (MausY < y + pos) AND (pos >      1) then pos := pos - 1;
      if (MausY > y + pos) AND (pos < anzahl) then pos := pos + 1;
      if Mausknopf(0) then begin
        Menufenster := pos; ende1 := true; end;
      if Mausknopf(1) then begin
        Menufenster := 0; ende1 := true; End;
    end;
    if (pos <> pos1) AND (POS <> 0) then
    begin
      textcolor(uafvg);textbackground(uafhg);
      gotoxy(x+1,y+pos1);
      i:=laenge-length(menupkt[pos1])-1;
      write(' ',menupkt[pos1]);zkette(32,i,uafhg,uafhg);
      textcolor(afvg);textbackground(afhg);
      gotoxy(x+1,y+pos);
      i:=laenge-length(menupkt[pos])-1;
      write(' ',menupkt[pos]);zkette(32,i,afhg,afhg);
      GotoMaus(40,y + pos);
    end;
  until ende1;
end;

{----------------------------------------------------------------------------}

begin
end.

   
