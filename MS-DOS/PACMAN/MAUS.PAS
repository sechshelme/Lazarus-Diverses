unit Maus;
{$mode tp}
interface
uses
  dos, ports;

var
  Mausvorhanden : Boolean;

procedure Mausein;
procedure Mausaus;
procedure GotoMaus(x, y : Byte);
function MausX : Byte;
function MausY : Byte;
function Mausknopf( Knopf : byte ): boolean;


implementation
var
  MyRegs          : registers;

procedure Mausein;
begin
  if Mausvorhanden then begin
    MyRegs.ax := $01;
    intr($33,MyRegs);
  end;
end;

procedure Mausaus;
begin
  if Mausvorhanden then begin
    MyRegs.ax := $02;
    intr($33,MyRegs);
  end;
end;

procedure GotoMaus(x, y : Byte);
begin
  if Mausvorhanden then begin
    MyRegs.ax := $04;
    MyRegs.cx := x * 8 - 1;
    MyRegs.dx := y * 8 - 1;
    intr($33,MyRegs);
  end;
end;

function MausX : Byte;
begin
  if Mausvorhanden then begin
    MyRegs.ax := $03;
    intr($33,MyRegs);
    MausX := MyRegs.cx DIV 8 + 1
  end;
end;

function MausY : Byte;
begin
  if Mausvorhanden then begin
    MyRegs.ax := $03;
    intr($33,MyRegs);
    MausY := MyRegs.dx DIV 8 + 1;
  end;
end;

function Mausknopf( Knopf : byte ):boolean;
begin
  Mausknopf := false;
  if Mausvorhanden then begin
    MyRegs.ax := $05;
    MyRegs.bx := knopf;
    intr($33,MyRegs);
    if MyRegs.bx > 2 then MyRegs.bx := 2;
    if MyRegs.bx <> 0 then Mausknopf := true;
  end;
end;

{ Hauptteil }

begin
  MyRegs.ax := $00;
  intr($33,MyRegs);
  if MyRegs.ax = $0000 then Mausvorhanden := false
                     else Mausvorhanden := true;
end.

