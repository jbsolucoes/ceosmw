{*****************************************************************}
{                                                                 }
{ by Jose Benedito - josebenedito@gmail.com                       }
{ www.jbsolucoes.net                                              }
{*****************************************************************}

unit ceosservermethods;

{$mode objfpc}{$H+}

interface
{$M+}

uses
  Classes, SysUtils, variants, ceostypes, fpjson, jsonparser, ceosserver;

type

  TFuncType = function(Request: TCeosRequestContent): TJSONStringType of object;

  { TCeosServerMethods }

  TCeosServerMethods = class
  private
    procedure Call(Request: TCeosRequestContent; var Response: TCeosResponseContent);

  public
    constructor Create;
    destructor Destroy; override;

    procedure ProcessRequest(ARequest: TCeosRequestContent; var Response: TCeosResponseContent);
  published

  end;

  function IsSelect(ASQL: string): boolean;

implementation

uses
  ceosjson, ceosconsts, ceosmessages;

{ TCeosServerMethods }

constructor TCeosServerMethods.Create;
begin

end;

destructor TCeosServerMethods.Destroy;
begin

  inherited Destroy;
end;

procedure TCeosServerMethods.Call(Request: TCeosRequestContent;
    var Response: TCeosResponseContent);
var
  m: TMethod;
  sResult: string;
  parser: tjsonparser;
begin
  try
    m.Code := Self.MethodAddress(Request.Method); //find method code

    if assigned(m.Code) then
    begin
      m.Data := pointer(Self); //store pointer to object instance

      sResult := TFuncType(m)(Request);

      //addlog(sResult);

      if IsJSON(sResult) then
      begin
        parser := TJSONParser.Create(sResult);

        try
          Response := TCeosResponseContent.Create;
          Response.ID := Request.ID;
          Response.ResultContent := parser.Parse;

          //JSONRPCResult(parser.parse,Request.ID);
        finally
          parser.free;
          parser := nil;
        end;
      end
      else
      begin
        //addlog('result '+ sResult);
        //Response := JSONRPCResult(TJSONString.create(sResult),Request.ID);
        Response := TCeosResponseContent.Create;
        Response.ID := Request.ID;
        Response.ResultContent := TJSONString.create(sResult);
      end;
    end
    else
    begin
      if assigned(Response) then
      begin
        Response.free;
        Response := nil;
      end;

      //addlog('call '+ sResult);
      Response := JSONRPCError(ERR_UNKNOW_FUNCTION, ERROR_UNKNOW_FUNCTION);
    end;

  except
    on e: Exception do
    begin
      if assigned(Response) then
      begin
        Response.free;
        Response := nil;
      end;

      Response := JSONRPCError(ERR_INTERNAL_ERROR, e.message, Request.ID);
    end;
  end;
end;


procedure TCeosServerMethods.ProcessRequest(ARequest: TCeosRequestContent; var Response: TCeosResponseContent);
begin
  try
    if Response <> nil then
    begin
      Response.free;
      response := nil;
    end;

    if ARequest <> nil then
      Call(ARequest, Response)
    else
    begin
      if assigned(Response) then
      begin
        Response.free;
        Response := nil;
      end;

      Response := JSONRPCError(ERR_INVALID_CONTENT, ERROR_INVALID_CONTENT, -1);
    end;

  except
    on e: Exception do
    begin
      if assigned(Response) then
      begin
        Response.free;
        Response := nil;
      end;

      Response := JSONRPCError(ERR_INTERNAL_ERROR, ERROR_INTERNAL_ERROR, -1);
    end;
  end;

end;


function IsSelect(ASQL: string): boolean;
begin
  result := (pos('SELECT',UpperCase(ASQL)) > 0);
end;

initialization
  UsingServerMethods := true;

end.

