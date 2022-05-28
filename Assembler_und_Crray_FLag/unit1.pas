unit Unit1;

{$mode objfpc}{$H+}
{$ASMMODE intel}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  _al, _bl, _cl, _dl: byte;
  i: integer;
begin
  for i := 1 to 20 do begin
    _al := 250;
    _bl := i;
    asm
             Mov     Al, _al
             Add     Al, _bl
             Mov     _cl, Al
             Pushf
             Pop     Ax
             Mov     _al,Al
    end;
    WriteLn('cl: ', _cl, '  al: ', _al, '  Carry flag: ', _al and 1);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  _al, _bl, _cl, _dl: byte;
  i: integer;
begin
  for i := 1 to 20 do begin
    _al := 250;
    _bl := i;
    asm
             Mov     Al, _al
             Add     Al, _bl
             Mov     _cl, Al
             //           Pushf
             //           Pop     Ax

             Mov     Al, 0
             Jae     @ende

             Mov     Al, 1
             @ende:
             Mov     _al,Al
    end;
    WriteLn('cl: ', _cl, '  al: ', _al, '  Carry flag: ', _al and 1);
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  _al, a, b: byte;
  i: integer;
begin
  for i := 1 to 20 do begin

    a := 250;
    b := i;
    asm
             Mov     Ebx, i
             Add     a, Bl
             Mov     Al, 0
             Jae     @ende

             Mov     Al, 1
             @ende:
             Mov     _al,Al
    end;
    if _al and 1 = 1 then begin
      WriteLn('a ', a + 256, '  b: ', b, '  Carry flag: ', _al and 1);
    end else begin
      WriteLn('a ', a, '  b: ', b, '  Carry flag: ', _al and 1);
    end;
  end;

end;

end.
