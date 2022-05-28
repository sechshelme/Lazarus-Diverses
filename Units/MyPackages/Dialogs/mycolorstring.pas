unit MyColorString;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, ExtCtrls;

type
  PColorString = ^TColorString;

  TColorString = record
    s: string;
    col: TColor;
  end;

  { TColorStringList }

  TColorStringList = class(TList)
  private
    FOnChange: TNotifyEvent;
    function GetItem(index: integer): PColorString;
    procedure SetItem(index: integer; AValue: PColorString);
  public
    property Items[index: integer]: PColorString read GetItem write SetItem; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    destructor Destroy; override;

    function Add(s: string; col: TColor): integer;
  end;

  { TColorMemo }

  TColorMemo = class(TCustomPanel)
  private
    FLines: TColorStringList;
    VertScrollBar, HoriScrollBar: TScrollBar;
    procedure LinesChanged(Sender: TObject);
    procedure SchrollBarChanged(Sender: TObject);
    procedure ColorMemoResize(Sender: TObject);
  protected
    procedure Paint; override;
  public
    property Lines: TColorStringList read FLines write FLines;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TColorMemo }

constructor TColorMemo.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Color := clBlack;

  Lines := TColorStringList.Create;
  Lines.OnChange := @LinesChanged;

  OnResize := @ColorMemoResize;

  VertScrollBar := TScrollBar.Create(Self);
  HoriScrollBar := TScrollBar.Create(Self);

  with VertScrollBar do begin
    Align := alRight;
    Kind := sbVertical;
    //    BorderSpacing.Bottom:=0;
    Visible := False;
    LargeChange := 100;
    OnChange := @SchrollBarChanged;
    Parent := Self;
  end;

  with HoriScrollBar do begin
    Align := alBottom;
    Kind := sbHorizontal;
    BorderSpacing.Right := VertScrollBar.Width;
    //    Visible := False;
    Parent := Self;
  end;
end;

destructor TColorMemo.Destroy;
begin
  Lines.Free;
  inherited Destroy;
end;

procedure TColorMemo.LinesChanged(Sender: TObject);
begin
  ColorMemoResize(Sender);
end;

procedure TColorMemo.SchrollBarChanged(Sender: TObject);
begin
  ColorMemoResize(Sender);
end;

procedure TColorMemo.Paint;
var
  h, i, a, e: integer;
begin
  inherited Paint;

  h := Canvas.TextHeight('@');

  a := VertScrollBar.Position div h;
  e := a + Height div h - 1;
  if e > Lines.Count - 1 then begin
    e := Lines.Count - 1;
  end;

  for i := a to e do begin
    Canvas.Font.Color := Lines[i]^.col;
    Canvas.TextOut(0, h * i - VertScrollBar.Position, Lines[i]^.s);
  end;
end;

procedure TColorMemo.ColorMemoResize(Sender: TObject);
var
  h, anzZeilen: integer;
begin
  h := Canvas.TextHeight('@');
  anzZeilen := Height div h;
  with VertScrollBar do begin
    Visible := anzZeilen < Lines.Count;
    //    WriteLn('ps: ', PageSize, '  max: ', max, '  pos: ', Position);
    PageSize := Height;
    Max := h * Lines.Count;
    if Position > Max - PageSize then begin
      Position := Max - PageSize;
    end;
  end;

  Invalidate;
end;

{ TColorStringList }

function TColorStringList.GetItem(index: integer): PColorString;
begin
  Result := PColorString(inherited Get(index));
end;

procedure TColorStringList.SetItem(index: integer; AValue: PColorString);
begin
  inherited Items[index] := AValue;
end;

destructor TColorStringList.Destroy;
var
  cs: PColorString;
  i: integer;
begin
  for i := 0 to Self.Count - 1 do begin
    cs := inherited Items[i];
    Dispose(cs);
  end;

  inherited Destroy;
end;

function TColorStringList.Add(s: string; col: TColor): integer;
var
  cs: PColorString;
begin
  New(cs);
  cs^.s := s;
  cs^.col := col;
  Result := inherited Add(cs);
  if Assigned(FOnChange) then begin
    FOnChange(self);
  end;
end;

end.
