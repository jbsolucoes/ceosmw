{*****************************************************************}
{ ceosserver is part of Ceos middleware/n-tier JSONRPC components }
{                                                                 }
{ Version beta 0.0.1                                              }
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
    procedure SetPort(AValue: integer);
    procedure SetHeader(var AResponse: TFPHTTPConnectionResponse);
    procedure DoOnGetRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
        var AResponse: TFPHTTPConnectionResponse);
    procedure DoOnRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
        var AResponse: TFPHTTPConnectionResponse);
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
    property Threaded: boolean read FThreaded write FThreaded default true;

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
  public
    constructor Create(APort: word; const AThreaded: boolean; const OnRequest: THTTPServerRequestHandler);
    procedure Execute; override;
    procedure DoTerminate; override;
    property Server: TFPHTTPServer read FServer;
  end;

  //Do Parse of Request content
  procedure DoParseRequest(var ARequest: TCeosRequestContent; AJSONString: TJSONStringType);
  //default jsonrpc result response
  function JSONRPCResult(const AResult: TJSONData; const AID: integer = -1): TCeosResponseContent;
  //default jsonrpc error response
  function JSONRPCError(const ACode: integer; const AMessage: string; AID: integer = -1): TCeosResponseContent;

procedure Register;

implementation

uses ceosmessages;

procedure DoParseRequest(var ARequest: TCeosRequestContent;
  AJSONString: TJSONStringType);
var
  parser: TJSONParser;
  parsed: TJSONData;
begin
  parser := TJSONParser.Create(AJSONString);
  try
    parsed := parser.Parse;

    try
      ARequest := TCeosRequestContent(parsed as TJSONObject);
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

function JSONRPCResult(const AResult: TJSONData; const AID: integer = -1): TCeosResponseContent;
var
  joResult: TCeosResponseContent;
begin
  joResult := TCeosResponseContent.create;

  joResult.Add('jsonrpc',JSONRPC_VERSION);
  joResult.Add('result',AResult);
  joResult.Add('id',AID);

  result := joResult;
end;

function JSONRPCError(const ACode: integer; const AMessage: string; AID: integer = -1): TCeosResponseContent;
var
  jsonerror: TJSONObject;
  joResult: TCeosResponseContent;
begin
  jsonerror := TJSONObject.Create();
  (jsonerror as TJSONObject).Add('code',ACode);
  (jsonerror as TJSONObject).Add('message',AMessage);

  joResult := TCeosResponseContent.create;
  joResult.Add('jsonrpc',JSONRPC_VERSION);
  joResult.Add('error',jsonerror);
  joResult.Add('id',AID);

  result := joResult;
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

constructor TCeosServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPort := CEOS_DEFAULT_PORT;
end;

destructor TCeosServer.Destroy;
begin
  FThrdHTTPServer.free;
  FThrdHTTPServer := nil;

  inherited Destroy;
end;

procedure TCeosServer.Start;
begin
  try
    FThrdHTTPServer := THTTPServerThread.Create(Port, Threaded, @DoOnRequest);

    if Assigned(OnStart) then
      OnStart(Self);

    { TODO -ojbsolucoes -cimprovements : Improve Active state by FThrdHTTPServer.FServer state }
    FActive := true;
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
  const OnRequest: THTTPServerRequestHandler);
begin
  FServer := TFPHTTPServer.Create(nil);
  FServer.Port      := APort;
  FServer.Threaded  := AThreaded;
  FServer.OnRequest := OnRequest;

  inherited Create(False);
end;

procedure THTTPServerThread.Execute;
begin
  try
    FServer.Active := True;
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
