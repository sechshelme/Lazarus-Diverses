unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, Forms, Controls, Graphics,
  Dialogs, StdCtrls;

const
  max = 100;

type
  TData = array[0..max - 1] of single;
  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Chart1LineSeries2: TLineSeries;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function Ausmittem(min, max: integer; DataIn: TData): TData;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

var
  Data1, Data2: TData;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  i: integer;
const
  range = 5;

begin
  Chart1LineSeries1.Clear;
  Chart1LineSeries2.Clear;
  for i := 0 to Length(TData) - 1 do begin
    Data1[i] := Random(1000);
  end;
  Data2 := Ausmittem(range, range, Data1);
  for i := 0 to Length(TData) - 1 do begin
    Chart1LineSeries2.AddY(Data1[i]);
    Chart1LineSeries1.AddY(Data2[i]);
  end;
end;

function TForm1.Ausmittem(min, max: integer; DataIn: TData): TData;
var
  dummy: single;
  i, p: integer;

  function Test(p: integer): integer;
  begin
    if p < 0 then begin
      p := p + Length(DataIn);
    end;
    if p >= Length(DataIn) then begin
      p := p - Length(DataIn);
    end;
    Result := p;
  end;

begin
  for p := 0 to Length(DataIn) - 1 do begin
    dummy := 0.0;
    for i := p - min to p + max do begin
      dummy := dummy + DataIn[Test(i)];
    end;
    Result[p] := dummy / (min + max);
  end;
end;

end.
