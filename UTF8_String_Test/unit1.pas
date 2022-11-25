unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LazStringUtils, LazUTF8;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  UTF8ToString('abc');
end;

procedure TForm1.Button1Click(Sender: TObject);
type
  MYString = type  ansistring(CP_UTF16);
var
  s: string;
  us: WideString;
  ms: MYString;
begin

  SetLength(ms, 10);
  ms[1] := 'a';
  ms[3] := 'b';
  ms[5] := 'c';

  WriteLn(ms);
  WriteLn(Length(ms));

end;

procedure TForm1.Button2Click(Sender: TObject);
type
  U16String = type  ansistring(CP_UTF16);
  U8String = type  ansistring(CP_UTF8);
var
  s: UnicodeString;
  s2: U16String;
  i: integer;
  chw:WideChar;
  sw:WideString;
begin
  s := 'äöüäöüäöüäöü   abcabcabcabc';
  s2 := UTF16ToUTF8('äüö');
  WriteLn(s2);
  WriteLn(Length(s2));
  WriteLn();

  sw:= StringToWideChar();  'abcöäü';
  WriteLn(Length(sw));


  WriteLn(sizeof(Char));
  WriteLn(sizeof(WideChar));

  for i := 1 to Length(sw) do begin
    Write(byte(sw[i]), ' ');
  end;

end;

end.
