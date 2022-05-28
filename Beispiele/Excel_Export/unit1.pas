unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLIntf;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  fpspreadsheet, fpsTypes, fpsallformats;


{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  MyWorkbook: TsWorkbook;
  MyWorksheet: TsWorksheet;
  i: integer;
begin
  MyWorkbook := TsWorkbook.Create;

  MyWorksheet := MyWorkbook.AddWorksheet('abc');
  MyWorksheet.WriteColWidth(0, 5, suChars);

  MyWorksheet.WriteBorders(0, 3, [cbDiagDown, cbDiagUp]);

  MyWorksheet.CopyCell(0, 3, 0, 4);
  //  MyWorksheet.WriteImage(9, 9, 'project1.ico');
  MyWorksheet.WriteImage(4, 4, 'test.bmp', 0.0, 0.0, 0.5, 0.5);

  MyWorksheet.WriteFontSize(1, 1, 20);
  MyWorksheet.WriteFontColor(1, 1, clRed);
  MyWorksheet.WriteText(1, 3, 'Fehler');

  MyWorksheet.WriteBorders(1, 3, [cbNorth, cbSouth, cbWest, cbEast]);
  MyWorksheet.WriteFormula(2, 3, '=D2');

  MyWorksheet.WriteText(0, 0, 'Test');
  MyWorksheet.WriteText(2, 2, 'Test');
  MyWorksheet.WriteBorderLineStyle(5, 9, cbWest, lsThick);

  MyWorksheet.WriteText(3, 1, '1.0');
  MyWorksheet.WriteNumber(3, 2, 1.0);

  MyWorksheet.WriteText(1, 6, 'Datum:');
  MyWorksheet.WriteDateTime(1, 7, now);

  for i := 0 to 8 do begin
    MyWorksheet.WriteBackgroundColor(i * 2, 1, $FF shl i);
    MyWorksheet.WriteRowHeight(i, i * 0.3 + 1);
  end;


  MyWorksheet := MyWorkbook.AddWorksheet('abc2');

  MyWorkbook.WriteToFile('test.xlsx', sfOOXML, True);
  MyWorkbook.Free;

  OpenDocument('test.xlsx');
  Sleep(1000);
  Close;
end;

end.

