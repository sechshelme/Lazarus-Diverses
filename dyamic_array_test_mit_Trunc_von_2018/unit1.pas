unit Unit1;

// https://www.freepascal.org/docs-html/ref/refsu48.html

//{$mode delphi}{$H+}
//{$modeswitch ArrayOperators-}

{$mode objfpc}
{$modeswitch arrayoperators}


interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }


procedure Ausgabe(a: array of byte);
var
  i: integer;
begin
  for i := 0 to Length(a) - 1 do begin
    Write(a[i]: 4);
  end;
  WriteLn();
end;

procedure ATest;
type
  tb = array[0..1, 0..1] of byte;
var
  a: array of array of byte;
  b: tb;
begin
  a := [[1, 2], [3, 3]];
//  b := ((1, 2), (3, 3));
  b := [[1, 2], [3, 3]];
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  a, b, c: array of byte;
  d_static: array[0..2] of byte = (6, 7, 8);

var
  d_dynamic: array of integer = (6, 7, 8);
  {$push}
  {$J-}
  e_dynamic: array of integer = (6, 7, 8);
  {$push}

begin
  a := [1, 2, 3];
  Ausgabe(a);           // io.

  WriteLn('d', SizeOf(d_dynamic));
  WriteLn('e', SizeOf(e_dynamic));

  b := a +  [123, 222];

  b := a + d_static + [123, 222];
  Ausgabe(b);           // error  --> Output:  1   2   3 123 222 123 222

  b := a + d_dynamic + [123, 222];
  Ausgabe(b);           // error  --> Output:  1   2   3 123 222 123 222

  b := a + [123, 222];
  c := b + b;
  Ausgabe(c);           // io.
  Delete(c, 2, 2);
  Ausgabe(c);           // io.

  c := Concat(a, d_static, a); // io.
  Ausgabe(c);

  c := a + d_static + a;       // io.
  Ausgabe(c);


  //  d_static := [5, 6, 7]; // geht nicht
end;

end.
