unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    StringGrid1: TStringGrid;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure StringGrid1CompareCells(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; var Result: integer);
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
  StringGrid1.FixedCols := 0;
  StringGrid1.FixedRows := 0;
  StringGrid1.ColCount := 2;
  StringGrid1.RowCount := 12;
  StringGrid1.Cols[0].AddCommaText('4,5,6,1,2,3,4,5,6,1,2,3');
  StringGrid1.Cols[1].AddCommaText('b,b,c,c,a,a,b,b,c,c,a,a');
end;

procedure TForm1.StringGrid1CompareCells(Sender: TObject; ACol, ARow, BCol,
  BRow: Integer; var Result: integer);
begin
  Result := CompareStr(StringGrid1.Cells[ACol, ARow], StringGrid1.Cells[BCol, BRow]);
  if Result = 0 then begin
    if ACol = 0 then
      Result := CompareStr(StringGrid1.Cells[1, ARow], StringGrid1.Cells[1, BRow])
    else
      Result := CompareStr(StringGrid1.Cells[0, ARow], StringGrid1.Cells[0, BRow])
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  StringGrid1.SortColRow(True, 0, 0, StringGrid1.RowCount - 1);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  StringGrid1.SortColRow(True, 1, 0, StringGrid1.RowCount - 1);
end;

end.

