unit EingabeForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons, EingabeControl;

type

  { TEingabeForm1 }

  TEingabeForm1 = class(TForm)
  private
    procedure EingabeControlChange(Sender: TObject);
    procedure SetWert(AValue: string);
    function Getwert: string;
  published
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    property Wert: string read GetWert write SetWert;
    procedure FormCreate(Sender: TObject);
  private
    EingabeControl: TEingabeControl;
  public

  end;

var
  EingabeForm1: TEingabeForm1;

implementation

{$R *.lfm}

{ TEingabeForm1 }

procedure TEingabeForm1.EingabeControlChange(Sender: TObject);
begin
  Caption:=EingabeControl.Wert;
end;

procedure TEingabeForm1.SetWert(AValue: string);
begin
  EingabeControl.Wert := AValue;
end;

function TEingabeForm1.Getwert: string;
begin
  Result := EingabeControl.Wert;
end;

procedure TEingabeForm1.FormCreate(Sender: TObject);
begin
  EingabeControl := TEingabeControl.Create(self, 6);
  with EingabeControl do begin
    Left := 5;
    Top := 5;
    OnChange:=@EingabeControlChange;
    Parent := Self;
  end;
end;

end.

