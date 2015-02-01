{*****************************************************************}
{ ceosclient is part of Ceos middleware/n-tier JSONRPC components }
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

unit ceosclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics,
  fpjson, jsonparser, fphttpclient, ceostypes;

type

  { TCeosClient }

  TCeosClient = class(TComponent)
  private
    FHost: string;
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Call(AMethod: string; Args: array of variant; AID: integer): string; overload;
    function Call(ARequest: TCeosRequestContent): string; overload;
    function JSONCall(AMethod: string; Args: array of variant; AID: integer): TJSONObject;
    function JSONCall(ARequest: TCeosRequestContent): TJSONObject; overload;
  published
    property Host: string read FHost write FHost;
    { Published declarations }
  end;



procedure Register;

implementation

uses ceosconsts;

procedure Register;
begin
  {$I ceosclient_icon.lrs}
  RegisterComponents('Ceos',[TCeosClient]);
end;

{ TCeosClient }

constructor TCeosClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCeosClient.Destroy;
begin
  inherited Destroy;
end;

function TCeosClient.Call(AMethod: string; Args: array of variant; AID: integer): string;
var
  response: TMemoryStream;
  joContent: TJSONObject;
  joArgs: TJSONArray;
  slRes: TStringList;
  httpcli: TFPHTTPClient;
begin
  try
    httpcli := TFPHTTPClient.Create(self);

    slRes    := TStringList.Create;
    response := TMemoryStream.Create;

    joContent := TJSONObject.Create;

    joContent.Add('jsonrpc', JSONRPC_VERSION);
    joContent.Add('method', AMethod);

    joArgs := GetJSONArray(Args);

    joContent.Add('params', joArgs);

    joContent.Add('id',AID);

    httpcli.FormPost(Host,joContent.AsJSON, response);

    if (response <> nil) then
    begin
      response.Seek(0, soFromBeginning);

      slRes.LoadFromStream(response);

      Result := slRes.text;
    end;
  finally
    joContent.free;
    freeandnil(response);
    freeandnil(slRes);
    freeandnil(httpcli);
  end;
end;

function TCeosClient.Call(ARequest: TCeosRequestContent): string;
var
  response: TMemoryStream;
  slRes: TStringList;
  httpcli: TFPHTTPClient;
begin
  try
    httpcli := TFPHTTPClient.Create(self);

    slRes    := TStringList.Create;
    response := TMemoryStream.Create;

    httpcli.FormPost(Host,ARequest.AsJSON, response);

    if (response <> nil) then
    begin
      response.Seek(0, soFromBeginning);

      slRes.LoadFromStream(response);

      Result := slRes.text;
    end;
  finally
    freeandnil(response);
    freeandnil(slRes);
    freeandnil(httpcli);
  end;

end;

function TCeosClient.JSONCall(AMethod: string; Args: array of variant; AID: integer): TJSONObject;
begin
  Result := (TJSONParser.Create(Call(AMethod,Args,AID)).Parse as TJSONObject);
end;

function TCeosClient.JSONCall(ARequest: TCeosRequestContent): TJSONObject;
begin
  Result := (TJSONParser.Create(Call(ARequest)).Parse as TJSONObject);
end;



end.
