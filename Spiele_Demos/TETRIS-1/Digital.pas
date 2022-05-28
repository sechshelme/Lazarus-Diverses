unit Digital;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TSiebensegment = class(TGraphicControl)
  private
    FColor : Integer;
    FBkColor : Integer;
    FSize : Byte;
    procedure SetColor(col : Integer);
    procedure SetBkColor(col : Integer);
    procedure SetSize(s : Byte);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner : TComponent); override;
  published
    property Color : Integer read FColor write SetColor;
    property BkColor : Integer read FBkColor write SetBkColor;
    property Size : Byte read FSize write SetSize;
  end;

procedure Register;

implementation

type
  TSegment = Array[0..9] of Byte;

const
  Segment : TSegment = ($77, $24, $5D, $6D, $2E, $6B, $FB, $25, $7F, $6F);

constructor TSiebensegment.Create;
begin
  inherited;
  FSize := 0;
  FColor := $FFFF;
  FBkColor := 0;
end;

procedure TSiebensegment.Paint;
var
  w, h : Integer;
begin
  with Canvas do begin
    Pen.Color := FColor;
    Brush.Color := FBkColor;
    FillRect(Rect(0, 0, Width, Height));
    w := Width div 4;
    h := Height div 8;

    Brush.Color := FColor;
{    Rectangle(0, 0, Width, Height);}
    if Segment[FSize] and $01 = $01 then FillRect(Rect(0, 0, Width, h));
    if Segment[FSize] and $02 = $02 then FillRect(Rect(0, 0, w, Height div 2));
    if Segment[FSize] and $04 = $04 then FillRect(Rect(Width, 0, Width - w, Height div 2));
    if Segment[FSize] and $08 = $08 then FillRect(Rect(0, Height div 2, Width, Height div 2 + h));
    if Segment[FSize] and $10 = $10 then FillRect(Rect(0, Height div 2, w, Height));
    if Segment[FSize] and $20 = $20 then FillRect(Rect(Width, Height div 2, Width - w, Height));
    if Segment[FSize] and $40 = $40 then FillRect(Rect(0, Height - h, Width, Height));
  end;
end;

procedure TSiebensegment.SetColor;
begin
  FColor := col;
  Paint;
end;

procedure TSiebensegment.SetBkColor;
begin
  FBkColor := col;
  Paint;
end;

procedure TSiebensegment.SetSize;
begin
  FSize := s;
  Paint;
end;

procedure Register;
begin
  RegisterComponents('Buch', [TSiebensegment]);
end;

end.
