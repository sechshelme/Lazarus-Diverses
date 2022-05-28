program Project1;
{$mode tp}
{$ASMMODE   intel}
uses
   Crt;

const 
  TextAttr : Byte = 2;
  maxX:Word = 80;
  VioSeg:Word=$B800;
 
procedure OutTextXY(x, y : Word; s : String); assembler;
asm
        mov     bl, textattr    { Attribut in BL sichern }
        push    ds              { Datensegment auf Stack sichern }
        mov     ax, y           { AX = y }
        mul     maxX            { AX = AX * maxX }
        add     ax, x           { AX = AX + x }
        mov     di, ax          { Offset = AX }
        shl     di, 1           { Offset verdoppeln }
        mov     es, VioSeg      { Segment = VioSeg }
        lds     si, s
        xor     ch, ch
        mov     cl, ds:[si]
        cmp     cl, 0           { if Str[0] = 0 then }
        je      @Ende           { Goto @Ende         }
        inc     si
        mov     ah, bl          { Attribut in ah laden }
@L:     lodsb                   { Nchstes zeichen in al laden }
        stosw                   { AX in Vio-Ram schreiben }
        loop    @L
@Ende:  pop     ds              { Datensegment von Stack holen }
end;




Procedure TextOutCrt(X, Y: Integer; S: String);
var
   I: Integer;
begin
   GotoXY(X + 1, Y + 1);
   TextAttr := $65;
   Write(S);
end;
 
Procedure TextOutVRAM(X, Y: integer; S: String);
var
   I, OffSet: Integer;
begin
   OffSet := X * 2 + Y * 160;
   for I:=0 to Byte(S[0]) -1 do
      begin
         MemW[$B800:OffSet + I * 2]:=Byte(S[I + 1]) + $56 SHL 8;
      end;
end;

 Procedure SCP(const Value: String; X, Y: UInt8; col: UInt8);Assembler;
    Asm
      PUSH BP
      MOV  AX, 1300h
      MOV  BH, 00h
      MOV  BL, col
      MOV  DL, X
      MOV  DH, Y


      LES  BP, Value
      XOR  CX, CX
      MOV  CL, ES:[BP]
      INC  BP
      INT  10h
      POP  BP
    End;

Procedure IntTest(const Value: String; X, Y: Byte; col: Byte);
var
   LengTemp: Integer;
   SegTemp: Integer;
   OffTemp: Integer;
begin
   SegTemp := Seg(Value);
   OffTemp := Ofs(Value);
   LengTemp := Length(Value);



 
   asm
      MOV AH, 13h
      MOV AL, 00h
      MOV BH, 00h
      MOV BL, col
      MOV CX, LengTemp
      MOV DL, X
      MOV DH, Y
push bp


les bp, Value
//      MOV ES, SegTemp
//      MOV BP, OffTemp

    inc bp
    INT 10h
    pop bp
  end;
end;
 
procedure ZKette(y:Byte; c:char; col: Byte); assembler;
asm
  mov ah, 02h
  mov dh, y
  mov dl, 0 
  mov bh, 00h
  int 10h

  mov ah, 09h
  mov al, c
  mov bh, 00h
  mov bl, col
  mov cx, 80
  int 10h
end;


var
   Index: LongInt;
 
begin
   ClrScr;

   zKette(0, 'z', $64);   
   zKette(24, 'x', $64);   

   TextAttr := 15;
 
   For Index := 0 to 250 do
      begin
         TextOutCrt(33, 11, 'Hallo Welt!');
      end;
 
   For Index := 0 to 250 do
      begin
         TextOutVRAM(34, 12, 'Hallo Welt!');
      end;
 
   SCP('abcdefgh', 3, 3, $05);
   INTTEST('1234567890', 69, 22, $04);

  OutTextXY(5,5,'Hallo 123456');
 
   Repeat
   Until KeyPressed
end.
