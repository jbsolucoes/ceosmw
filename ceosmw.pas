{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CeosMW;

interface

uses
  CeosClient, ceosserver, ceostypes, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('CeosClient', @CeosClient.Register);
  RegisterUnit('ceosserver', @ceosserver.Register);
end;

initialization
  RegisterPackage('CeosMW', @Register);
end.
