unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    SubMenuButton: TButton;
    MainMenuButton: TButton;
    MenuItem1: TMenuItem;
    procedure SubMenuButtonClick(Sender: TObject);
    procedure MainMenuButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    MainMenu: TMainMenu;
    MainMenuItemArray: array of TMenuItem;
    SubMenuItemArray: array of TMenuItem;
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.MainMenuButtonClick(Sender: TObject);
var
  l: integer;
begin
  l := Length(MainMenuItemArray);
  SetLength(MainMenuItemArray, l + 1);
  MainMenuItemArray[l] := TMenuItem.Create(self);
  MainMenuItemArray[l].Caption := 'Main-Menu ' + IntToStr(l);

  MainMenu.Items.Add(MainMenuItemArray[l]);  // Wird hinten angefügt
end;

procedure TForm1.SubMenuButtonClick(Sender: TObject);
var
  l: integer;
begin
  l := Length(SubMenuItemArray);
  SetLength(SubMenuItemArray, l + 1);
  SubMenuItemArray[l] := TMenuItem.Create(self);
  SubMenuItemArray[l].Caption := 'Menu ' + IntToStr(l);

  MainMenuItemArray[0].Insert(0, SubMenuItemArray[l]);  // Wird vorn angefügt
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
