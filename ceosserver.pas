{*****************************************************************}
{ ceosserver is part of Ceos middleware/n-tier JSONRPC components }
{                                                                 }
{ Beta version                                                    }
{                                                                 }
{ This library is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of  }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.            }
{                                                                 }
{ by Jose Benedito - josebenedito@gmail.com                       }
{ www.jbsolucoes.net                                              }
{*****************************************************************}

unit ceosserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, fpjson,
  variants, jsonparser, fphttpserver, ceostypes, ceosconsts;

type
  THTTPServerThread = class;

  TOnCeosGetRequest = procedure(Sender: TObject; const ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse) of Object;
  TOnCeosRequest = procedure(Sender: TObject; const ARequest: TCeosRequestContent; var AResponse: TCeosResponseContent) of Object;
  TOnCeosRequestError = procedure(Sender: TObject; const E: exception; var AResponse: TCeosResponseContent) of Object;
  TOnCeosException = procedure(Sender: TObject; const E: exception) of Object;

  { TCeosServer }

  TCeosServer = class(TComponent)
  private
    FActive: boolean;
    FOnGetRequest: TOnCeosGetRequest;
    FPort: integer;
    FOnRequestError: TOnCeosRequestError;
    FOnException: TOnCeosException;
    FOnRequest: TOnCeosRequest;
    FOnStart: TNotifyEvent;
    FOnStop: TNotifyEvent;
    FThrdHTTPServer: THTTPServerThread;
    FThreaded: boolean;
    function IsPortStored: boolean;
    function IsThreadedStored: boolean;
    procedure SetPort(AValue: integer);
    procedure SetHeader(var AResponse: TFPHTTPConnectionResponse);
    procedure DoOnGetRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
        var AResponse: TFPHTTPConnectionResponse);
    procedure DoOnRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
        var AResponse: TFPHTTPConnectionResponse);
    procedure SetThreaded(AValue: boolean);
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    property Active: boolean read FActive;
  published
    { Published declarations }
    property Port: integer read FPort write SetPort stored IsPortStored default 8080;
    property Threaded: boolean read FThreaded write SetThreaded stored IsThreadedStored  default true;

    property OnRequestError: TOnCeosRequestError read FOnRequestError write FOnRequestError;
    property OnException: TOnCeosException read FOnException write FOnException;
    property OnGetRequest: TOnCeosGetRequest read FOnGetRequest write FOnGetRequest;
    property OnRequest: TOnCeosRequest read FOnRequest write FOnRequest;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;

  end;

  { THTTPServerThread }

  THTTPServerThread = class(TThread)
  private
    FServer: TFPHTTPServer;
    FOnException: TOnCeosException;
    FActive: boolean;
  public
    constructor Create(APort: word; const AThreaded: boolean;
        const AOnRequest: THTTPServerRequestHandler; const AOnException: TOnCeosException);
    destructor Destroy; override;
    procedure Execute; override;
    procedure DoTerminate; override;
    property Active: boolean read FActive write FActive;
    property Server: TFPHTTPServer read FServer;
    property OnException: TOnCeosException read FOnException write FOnException;
  end;

  //Do Parse of Request content
  procedure DoParseRequest(var ARequest: TCeosRequestContent; AJSONString: TJSONStringType);

var
  UsingServerMethods: boolean = false;

procedure Register;

implementation

uses ceosmessages;

procedure DoParseRequest(var ARequest: TCeosRequestContent;
  AJSONString: TJSONStringType);
var
  parser: TJSONParser;
begin
  parser := TJSONParser.Create(AJSONString);
  try
    try
      ARequest := TCeosRequestContent(parser.Parse as TJSONObject);
    except on e:exception do
      begin
        ARequest := nil;
        raise exception.create(e.message);
      end;
    end;
  finally
    parser.free;
    parser := nil;
  end;
end;

procedure Register;
begin
  {$I ceosserver_icon.lrs}
  RegisterComponents('Ceos',[TCeosServer]);
end;

{ TCeosServer }

function TCeosServer.IsPortStored: boolean;
begin
  result := true;
end;

function TCeosServer.IsThreadedStored: boolean;
begin
  result := true;
end;


procedure TCeosServer.SetPort(AValue: integer);
begin
  FPort := AValue;
end;

procedure TCeosServer.SetHeader(var AResponse: TFPHTTPConnectionResponse);
begin
  AResponse.CustomHeaders.Clear;
  AResponse.SetCustomHeader('Access-Control-Allow-Origin','*');
  AResponse.SetCustomHeader('Access-Control-Allow-Credentials','true');
  AResponse.CustomHeaders.Add(JSON_HEADER_CONTENT_TYPE);
end;

procedure TCeosServer.DoOnGetRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
begin
  try
    if assigned(OnGetRequest) then
    begin
      AResponse.Code := 200;
      OnGetRequest(Sender, ARequest, AResponse) ;
      exit;
    end
    else
      AResponse.Code    := 404;

  except on e:exception do
    begin
      if Assigned(OnException) then
        OnException(Sender,e);

      AResponse.Code := 500;
    end;
  end;
end;

procedure TCeosServer.DoOnRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  ceosRequest: TCeosRequestContent;
  ceosResponse: TCeosResponseContent;
  iID: integer;
begin
  ceosRequest   := nil;
  ceosResponse  := nil;

  if ARequest.Method = 'GET' then
  begin
    DoOnGetRequest(Sender, ARequest, AResponse);

    exit;
  end;

  SetHeader(AResponse);

  try
    try
      DoParseRequest(ceosRequest, ARequest.Content);

      if not UsingServerMethods then
      begin
        ceosResponse := TCeosResponseContent.create;
        ceosResponse.ID := ceosRequest.ID;
      end;

      if assigned(OnRequest) then
        OnRequest(Sender,ceosRequest,ceosResponse);

      if not assigned(ceosResponse) then
        ceosResponse := JSONRPCResult(TJSONString.create(MSG_NO_RESPONSE),ceosRequest.ID);

    except on e:exception do
      if Assigned(OnException) then
        OnRequestError(Sender,e,ceosResponse)
      else
      begin
        if ceosRequest <> nil then
          iID := ceosRequest.ID
        else
          iID := -1;

        ceosResponse := JSONRPCError(ERR_REQUEST_ERROR,ERROR_REQUEST_ERROR,iID);
      end;
    end;

  finally
    AResponse.content := ceosResponse.AsJSON;

    ceosRequest.free;
    ceosRequest := nil;

    ceosResponse.free;
    ceosResponse := nil;
  end;
end;

procedure TCeosServer.SetThreaded(AValue: boolean);
begin
  FThreaded := AValue;
end;

constructor TCeosServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPort     := CEOS_DEFAULT_PORT;
  FThreaded := true;
end;

destructor TCeosServer.Destroy;
begin
  if assigned(FThrdHTTPServer) then
  begin
    FThrdHTTPServer.Terminate;
    FThrdHTTPServer.free;
    FThrdHTTPServer := nil;
  end;

  inherited Destroy;
end;

procedure TCeosServer.Start;
begin
  try
    if Assigned(FThrdHTTPServer) then
      FThrdHTTPServer.free;

    FThrdHTTPServer := THTTPServerThread.Create(Port, Threaded, @DoOnRequest, OnException);

    FActive := FThrdHTTPServer.Active;

    if FActive and Assigned(OnStart) then
      OnStart(Self);

  except on e:exception do
    if Assigned(OnException) then
      OnException(Self,e)
    else
      raise exception.create(e.message);
  end;
end;

procedure TCeosServer.Stop;
begin
  try
    FThrdHTTPServer.Terminate;
    FThrdHTTPServer.DoTerminate;

    if Assigned(OnStop) then
      OnStop(Self);

    FActive := false;
  except on e:exception do
    if Assigned(OnException) then
      OnException(Self,e)
    else
      raise exception.create(e.message);
  end;
end;

{ THTTPServerThread }

constructor THTTPServerThread.Create(APort: word; const AThreaded: boolean;
  const AOnRequest: THTTPServerRequestHandler; const AOnException: TOnCeosException);
begin
  FServer := TFPHTTPServer.Create(nil);
  FServer.Active    := false;
  FServer.Port      := APort;
  FServer.Threaded  := AThreaded;
  FServer.OnRequest := AOnRequest;

  FActive := true;
  OnException       := AOnException;

  inherited Create(False);
end;

destructor THTTPServerThread.Destroy;
begin
  if assigned(FServer) then
  begin
    FServer.free;
    FServer := nil;
  end;

  inherited Destroy;

end;

procedure THTTPServerThread.Execute;
begin
  try
    try
      FServer.Active := True;
    except on e:exception do
      begin
        FActive := false;

        FServer.Active := false;

        if assigned(OnException) then
          OnException(Self,e);
      end;
    end;
  finally
    FreeAndNil(FServer);
  end;
end;

procedure THTTPServerThread.DoTerminate;
begin
  inherited DoTerminate;

  if assigned(FServer) then
    FServer.Active := False;
end;


end.
