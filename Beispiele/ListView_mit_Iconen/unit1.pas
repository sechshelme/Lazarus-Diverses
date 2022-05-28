unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ImageList1: TImageList;
    ListView1: TListView;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListView1AdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
  private
    { private declarations }
  public
    procedure LoadImg(pfad: string);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  //  ListView1.ViewStyle := vsSmallIcon;
  //  ListView1.SmallImages := ImageList1;
  ListView1.ViewStyle := vsIcon;
  ListView1.LargeImages := ImageList1;
end;

procedure TForm1.ListView1AdvancedCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
begin
  Sender.Canvas.Brush.color := Random($FFFFFF);
  Sender.Canvas.Rectangle(5,5,30,30);
  ListView1.Color:=clYellow;
end;

procedure TForm1.LoadImg(pfad: string);
var
  sl: TStringList;
  i: integer;
  bit: TBitmap;
begin
  bit := TBitmap.Create;
  sl := FindAllFiles(pfad, '*.bmp', False);

  ImageList1.BeginUpdate;
  ImageList1.Clear;
  for i := 0 to sl.Count - 1 do begin
    bit.LoadFromFile(sl[i]);
    ImageList1.Add(bit, nil);
  end;
  ImageList1.EndUpdate;

  for i := 0 to ImageList1.Count - 1 do begin
    //ImageList1.Draw(Canvas, i * 16, 0, i);
  end;

  ListView1.Items.Clear;
//  ListView1.Canvas.Brush.Color := clRed;
//  ListView1.Color:=clGreen;
  ListView1.Items.BeginUpdate;
  for i := 0 to sl.Count - 1 do begin
    ListView1.Items.Add;
    ListView1.Items[i].Caption := ExtractFileName(sl[i]);
    ListView1.Items[i].ImageIndex := i;

    //        with ListView1.Items.Add do begin
    //          Caption := ExtractFileName(sl[i]);
    //          ImageIndex := i;
    //        end;
  end;
  Caption := IntToStr(ListView1.Items.Count);

  ListView1.Items.EndUpdate;

  //  Caption := IntToStr(ListView1.Items.Count);
  //  Caption := IntToStr(ImageList1.Count);

  sl.Free;
  bit.Free;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  LoadImg('bmp/pfeile');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  LoadImg('bmp/diverses');
end;

end.
