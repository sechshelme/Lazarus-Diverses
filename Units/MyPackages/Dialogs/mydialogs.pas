{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MyDialogs;

interface

uses
  MyLogForms, MyMessages, Tools_Info, MyColorString, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('MyDialogs', @Register);
end.
