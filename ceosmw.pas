{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CeosMW;

interface

uses
  ceosclient, ceosserver, ceostypes, ceosservermethods, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ceosclient', @ceosclient.Register);
  RegisterUnit('ceosserver', @ceosserver.Register);
end;

initialization
  RegisterPackage('CeosMW', @Register);
end.
