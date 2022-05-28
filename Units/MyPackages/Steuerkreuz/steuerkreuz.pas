unit SteuerKreuz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons;

type
  TVector3f = array[0..2] of single;

  TKreuzClickEvent = procedure(Sender: TObject; Vector: TVector3f) of object;
  TButtonCaption = (abc, xyz);
  TButtonStr = array[0..5] of string;

  TAxis = set of (x, y, z);

  { TSteuerKreuz }

  TSteuerKreuz = class(TPanel)
  private
    ButtonStr: TButtonStr;
    BitBtn: array of TBitBtn;
    FAxis: TAxis;
    FButtonCaption: TButtonCaption;
    FButtonDistance: integer;
    FOnKreuzClick: TKreuzClickEvent;
    procedure KreuzClick(Sender: TObject);
    procedure SetAxis(AValue: TAxis);
    procedure SetButtonCaption(AValue: TButtonCaption);
    procedure SetButtonDistance(AValue: integer);
    procedure _Draw;
  protected
    procedure DoOnResize; override;
  published
    property ButtonCaption: TButtonCaption read FButtonCaption write SetButtonCaption;
    property OnKreuzClick: TKreuzClickEvent read FOnKreuzClick write FOnKreuzClick;
    property Axis: TAxis read FAxis write SetAxis;
    property ButtonDistance: integer read FButtonDistance write SetButtonDistance;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Eigene', [TSteuerKreuz]);
end;


function vec3(x, y, z: single): TVector3f; inline;
begin
  Result[0] := x;
  Result[1] := y;
  Result[2] := z;
end;

const
  abcStr: TButtonStr = ('A-', 'A+', 'B-', 'B+', 'C-', 'C+');
  xyzStr: TButtonStr = ('X-', 'X+', 'Y-', 'Y+', 'Z-', 'Z+');

{ TSteuerKreuz }

constructor TSteuerKreuz.Create(TheOwner: TComponent);
var
  i: integer;
begin
  inherited Create(TheOwner);
  SetLength(BitBtn, 6);

  for i := 0 to Length(BitBtn) - 1 do begin
    BitBtn[i] := TBitBtn.Create(Self);
    BitBtn[i].Parent := Self;
    BitBtn[i].Tag := i;
    BitBtn[i].OnClick := @KreuzClick;
  end;

  ButtonCaption := xyz;
  ButtonStr := xyzStr;
  Width := 75;
  Height := 75;
  FButtonDistance := 4;
  FAxis := [x, y, z];
end;

destructor TSteuerKreuz.Destroy;
var
  i: integer;
begin
  for i := 0 to Length(BitBtn) - 1 do begin
    BitBtn[i].Free;
  end;
  inherited Destroy;
end;

procedure TSteuerKreuz.KreuzClick(Sender: TObject);
var
  Vec: TVector3f = (0.0, 0.0, 0.0);
  Schritt: single = 1.0;
begin
  if Assigned(OnKreuzClick) then begin
    case TButton(Sender).Tag of
      0: begin
        Vec := vec3(-Schritt, 0.0, 0.0);
      end;
      1: begin
        Vec := vec3(Schritt, 0.0, 0.0);
      end;
      2: begin
        Vec := vec3(0.0, -Schritt, 0.0);
      end;
      3: begin
        Vec := vec3(0.0, Schritt, 0.0);
      end;
      4: begin
        Vec := vec3(0.0, 0.0, -Schritt);
      end;
      5: begin
        Vec := vec3(0.0, 0.0, Schritt);
      end;
    end;
    OnKreuzClick(Sender, Vec);
  end;
end;

procedure TSteuerKreuz.SetAxis(AValue: TAxis);
begin
  if FAxis = AValue then begin
    Exit;
  end;
  FAxis := AValue;
  if x in FAxis then begin
    BitBtn[0].Visible := True;
    BitBtn[1].Visible := True;
  end else begin
    BitBtn[0].Visible := False;
    BitBtn[1].Visible := False;
  end;
  if y in FAxis then begin
    BitBtn[2].Visible := True;
    BitBtn[3].Visible := True;
  end else begin
    BitBtn[2].Visible := False;
    BitBtn[3].Visible := False;
  end;
  if z in FAxis then begin
    BitBtn[4].Visible := True;
    BitBtn[5].Visible := True;
  end else begin
    BitBtn[4].Visible := False;
    BitBtn[5].Visible := False;
  end;
end;

procedure TSteuerKreuz.SetButtonCaption(AValue: TButtonCaption);
begin
  if FButtonCaption = AValue then begin
    Exit;
  end;
  FButtonCaption := AValue;
  if FButtonCaption = abc then begin
    ButtonStr := abcStr;
  end else begin
    ButtonStr := xyzStr;
  end;
  _Draw;
end;

procedure TSteuerKreuz.SetButtonDistance(AValue: integer);
begin
  if FButtonDistance = AValue then begin
    Exit;
  end;
  FButtonDistance := AValue;
  _Draw;
end;


procedure TSteuerKreuz._Draw;
var
  i, w: integer;

  procedure But(const ABitBtn: TBitBtn; PosX, PosY: integer);
  begin
    ABitBtn.Left := PosX * Width div 3 + FButtonDistance div 2;
    ABitBtn.Top := PosY * Height div 3 + FButtonDistance div 2;
  end;

begin
  for i := 0 to Length(BitBtn) - 1 do begin
    w := Height div 3 - FButtonDistance;
    if w < 0 then begin
      w := 0;
    end;
    BitBtn[i].Height := w;
    w := Width div 3 - FButtonDistance;
    if w < 0 then begin
      w := 0;
    end;
    BitBtn[i].Width := w;
    BitBtn[i].Caption:=ButtonStr[i];
  end;

  But(BitBtn[0], 0, 1);
  But(BitBtn[1], 2, 1);
  But(BitBtn[2], 1, 2);
  But(BitBtn[3], 1, 0);
  But(BitBtn[4], 0, 2);
  But(BitBtn[5], 2, 0);
end;

procedure TSteuerKreuz.DoOnResize;

begin
  inherited DoOnResize;
  _Draw;
end;


end.
