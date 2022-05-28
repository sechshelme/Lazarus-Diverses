unit ascii;
{$mode tp}
{$ASMMODE   intel}
interface
uses
  crt,dos, ports;
var
  regs : registers;

procedure zeichenbreite(breite : byte);
procedure asciizeichen( ascii  : Byte; punkte : string);

implementation

procedure CLI; assembler;
asm
   cli
end;

procedure STI; assembler;
asm
  sti
end;

procedure zeichenbreite(breite : byte);
var
  x : byte;
begin
  if (lastmode = 258) OR (lastmode = 3) then begin
    if (breite = 8) then regs.bx := $0001
                    else regs.bx := $0800;
    x := port[$3CC] and not(4+8);
    if (breite = 9) then x := x or 4;
    port[$3C2] := x;
    CLI;
    portw[$3C4] := $0100;
    portw[$3C4] := $01 + Regs.bl shl 8;
    portw[$3C4] := $0300;
    STI;
    regs.ax := $100;
    regs.bl := $13;
    intr($10,regs);
    regs.ah:=1;           { Cursor richtig positionieren }
    regs.ch:=$10;
    regs.cl:=$10;
    intr($10,regs);
  end;
end;

procedure asciizeichen( ascii  : Byte; punkte : string);
var
  str   : string;
  str1, zeilen, i,k: byte;
  ch    : char;
begin
  if lastmode = 3 then zeilen := 16
                  else zeilen := 8;
  str := '';
  for k :=  1 to zeilen do begin
    str1 := 0;
    for i := 8 downto 1 do begin
      ch := punkte[1];
      if ch ='*' then begin
        case i of
          1 : inc(str1,1  );
          2 : inc(str1,2  );
          3 : inc(str1,4  );
          4 : inc(str1,8  );
          5 : inc(str1,16 );
          6 : inc(str1,32 );
          7 : inc(str1,64 );
          8 : inc(str1,128);
        end;
      end;
      delete(punkte,1,1);
    end;
    str := str + chr(str1);
  end;
  regs.ax := $1100;
  regs.bh := 16;
  regs.bl := 0;
  regs.cx := 1;
  regs.dx := ascii;
  regs.es := seg(str);
  regs.bp := ofs(str)+1;
  intr($10,regs);
end;

end.

