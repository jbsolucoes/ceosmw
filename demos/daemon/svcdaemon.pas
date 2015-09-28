unit svcdaemon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, ceosserver, DaemonApp, ceostypes, appmethods;

type

  { TCeosDaemon }

  TCeosDaemon = class(TDaemon)
    CeosServer1: TCeosServer;
    procedure CeosServer1Request(Sender: TObject;
      const ARequest: TCeosRequestContent; var AResponse: TCeosResponseContent);
    procedure CeosServer1Start(Sender: TObject);
    procedure CeosServer1Stop(Sender: TObject);
    procedure DataModuleStart(Sender: TCustomDaemon; var OK: Boolean);
    procedure DataModuleStop(Sender: TCustomDaemon; var OK: Boolean);
  private
    { private declarations }
  public
    { public declarations }
    ServerMethods: TCeosMethods;
  end;

var
  CeosDaemon: TCeosDaemon;

implementation

procedure RegisterDaemon;
begin
  RegisterDaemonClass(TCeosDaemon)
end;

{$R *.lfm}

{ TCeosDaemon }

procedure TCeosDaemon.DataModuleStart(Sender: TCustomDaemon; var OK: Boolean);
begin
  CeosServer1.Start;
end;

procedure TCeosDaemon.CeosServer1Request(Sender: TObject;
  const ARequest: TCeosRequestContent; var AResponse: TCeosResponseContent);
begin
  ServerMethods.ProcessRequest(ARequest, AResponse);
end;

procedure TCeosDaemon.CeosServer1Start(Sender: TObject);
begin
  ServerMethods := TCeosMethods.Create;
end;

procedure TCeosDaemon.CeosServer1Stop(Sender: TObject);
begin
  ServerMethods.free;
end;

procedure TCeosDaemon.DataModuleStop(Sender: TCustomDaemon; var OK: Boolean);
begin
  CeosServer1.Stop;
end;


initialization
  RegisterDaemon;
end.

