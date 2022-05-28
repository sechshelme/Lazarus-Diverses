{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MeinePackage;

interface

uses
  SteuerKreuz, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('SteuerKreuz', @SteuerKreuz.Register);
end;

initialization
  RegisterPackage('MeinePackage', @Register);
end.
