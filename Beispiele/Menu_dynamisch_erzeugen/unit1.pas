unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    MenuItem1: TMenuItem;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    MainMenu: TMainMenu;
    MenuItemArray: array of TMenuItem;
    SubMenuItemArray: array of TMenuItem;
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  l: integer;
begin
  l := Length(SubMenuItemArray);
  SetLength(SubMenuItemArray, l + 1);
  SubMenuItemArray[l] := TMenuItem.Create(self);
  SubMenuItemArray[l].Caption := 'Menu ' + IntToStr(l);

  MenuItemArray[0].Insert(0, SubMenuItemArray[l]);  // Wird vorn angefügt
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  l: integer;
begin
  l := Length(MenuItemArray);
  SetLength(MenuItemArray, l + 1);
  MenuItemArray[l] := TMenuItem.Create(self);
  MenuItemArray[l].Caption := 'Sub-Menu ' + IntToStr(l);

  MainMenu.Items.Add(MenuItemArray[l]);  // Wird hinten angefügt
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  MainMenu := TMainMenu.Create(self);

  self.Menu := MainMenu;
end;

end.
procedure MenuCheckAll(Item: TMenuItem);
var
P: TMenuItem;
i: integer;
begin
Item.Checked := True;
P := Item.Parent;
if P = nil then begin
Exit;
end;
for i := 0 to P.Count - 1 do begin
if P.Items[i] <> Item then begin
P.Items[i].Checked := False;
end;
end;


end;
