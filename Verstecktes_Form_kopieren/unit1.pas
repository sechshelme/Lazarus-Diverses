unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,Clipbrd, ExtCtrls, unit2;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click({%H-}Sender: TObject);
    procedure Button2Click({%H-}Sender: TObject);
    procedure CheckActive({%H-}Sender: TObject);
    procedure FormClose({%H-}Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate({%H-}Sender: TObject);
    procedure FormPaint({%H-}Sender: TObject);
  private
    aTimer  : TTimer;
    clp     : boolean;
  public
    Form2BMP  : TBitmap;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
 Form2.Show;
 clp := false;
 aTimer.Enabled := true;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
 Form2.Show;
 clp := true;
 aTimer.Enabled := true;
end;

procedure TForm1.CheckActive(Sender: TObject);
var aBmp : TBitmap;
begin
 if Form2.Active then
  begin
   aTimer.Enabled := false;
   if not clp then
    begin
    {$IFDEF WINDOWS}
     Form2BMP.Canvas.CopyRect(rect(0,0,500,250),Form2.Canvas,rect(0,0,500,200));
    {$ENDIF}
    {$IFDEF Linux}
     Form2.PaintTo(Form2Bmp.Canvas,0,0);
    {$ENDIF}
     Form2.Close;
     Form1.Invalidate;
    end;
   if clp then
    begin
     aBmp   := TBitmap.Create;
     try
      aBmp.SetSize(500,200);
     {$IFDEF WINDOWS}
      aBmp.Canvas.CopyRect(rect(0,0,500,250),Form2.Canvas,rect(0,0,500,200));
     {$ENDIF}
    {$IFDEF Linux}
     Form2.PaintTo(aBmp.Canvas,0,0);
    {$ENDIF}
      Clipboard.Assign(aBmp);
      Form2.Close;
     finally
      aBmp.Free;
     end;//try
    end;
   end;//active
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 Form2BMP.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 Form2BMP                    := TBitmap.Create;
 Form2BMP.SetSize(500,200);
 Form2BMP.Canvas.Brush.Color := clWhite;
 Form2BMP.Canvas.FillRect(0,0,500,200);

 aTimer          := TTimer.Create(self);
 aTimer.Enabled  := false;
 aTimer.Interval := 100;
 aTimer.OnTimer  := @CheckActive;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
 canvas.Draw(10,10,Form2BMP);
end;

end.

