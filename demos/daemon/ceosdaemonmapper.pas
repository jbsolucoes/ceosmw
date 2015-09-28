unit ceosdaemonmapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DaemonApp;

type
  Tceosmapper = class(TDaemonMapper)
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  ceosmapper: Tceosmapper;

implementation

procedure RegisterMapper;
begin
  RegisterDaemonMapper(Tceosmapper)
end;

{$R *.lfm}


initialization
  RegisterMapper;
end.

