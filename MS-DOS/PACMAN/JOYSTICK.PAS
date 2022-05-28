unit joystick;
{$mode tp}
{$ASMMODE   intel}
interface
uses
  dos,crt;
var
  regs         : registers;
  x            : integer;
  Joyvorhanden : Boolean;
  ch           : char;

function JoyLinks  : boolean;
function JoyRechts : boolean;
function JoyOben   : boolean;
function JoyUnten  : boolean;
function JoyKnopf1 : boolean;
function JoyKnopf2 : boolean;

implementation
function JoyLinks  : boolean;
begin
  asm
    db $1E, $50, $52, $56, $BA, $01, $02,
    db $B4, $01, $BE, $00, $00, $8E, $DE,
    db $FA, $EE, $46, $EC, $84, $C4, $75,
    db $FA, $FB, $89, $36, $64, $12, $5E,
    db $5A, $58, $1F
  end;
  x := MemW[0:$1264];
  if x < 20 then Joylinks := true
            else Joylinks := false;
end;

function JoyRechts : boolean;
begin
  asm
    db $1E, $50, $52, $56, $BA, $01, $02,
    db $B4, $01, $BE, $00, $00, $8E, $DE,
    db $FA, $EE, $46, $EC, $84, $C4, $75,
    db $FA, $FB, $89, $36, $64, $12, $5E,
    db $5A, $58, $1F
  end;
  x := MemW[0:$1264];
  if x > 200 then JoyRechts := true
             else JoyRechts := false;
end;

function JoyOben   : boolean;
begin
  asm
    db $1E, $50, $52, $56, $BA, $01, $02,
    db $B4, $02, $BE, $00, $00, $8E, $DE,
    db $FA, $EE, $46, $EC, $84, $C4, $75,
    db $FA, $FB, $89, $36, $64, $12, $5E,
    db $5A, $58, $1F
  end;
  x := MemW[0:$1264];
  if x < 20 then JoyOben := true
            else JoyOben := false;
end;

function JoyUnten  : boolean;
begin
  asm
    db $1E, $50, $52, $56, $BA, $01, $02,
    db $B4, $02, $BE, $00, $00, $8E, $DE,
    db $FA, $EE, $46, $EC, $84, $C4, $75,
    db $FA, $FB, $89, $36, $64, $12, $5E,
    db $5A, $58, $1F
  end;
  x := MemW[0:$1264];
  if x > 200 then JoyUnten := true
             else JoyUnten := false;
end;

function JoyKnopf1 : boolean;
begin
  regs.ah := $84;
  regs.dx := 0;
  intr($15,regs);
  if (regs.al = 224) OR  (regs.al = 192) then JoyKnopf1 := true
                                         else JoyKnopf1 := false;
end;

function JoyKnopf2 : boolean;
begin
  regs.ah := $84;
  regs.dx := 0;
  intr($15,regs);
  if (regs.al = 208) OR  (regs.al = 192) then JoyKnopf2 := true
                                         else JoyKnopf2 := false;
end;
begin
  Writeln('Joystick angeschlossen (J/N)  ? ');
  ch := ReadKey;
  if ch = #0 then ch := ReadKey;
  if ch in ['j','J'] then Joyvorhanden := true
                     else Joyvorhanden := false;
end.
