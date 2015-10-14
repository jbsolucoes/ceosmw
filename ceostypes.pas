{*****************************************************************}
{ ceostypes is part of Ceos middleware/n-tier JSONRPC components  }
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

unit ceostypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, variants, fpjson, jsonparser;

type
  TCeosArgsType = array of variant;

  { TCeosRequestContent }

  TCeosRequestContent = class(TJSONObject)
  private
    FParams: TJSONArray;
    function GetArgs: TJSONArray;
    function GetID: integer;
    function GetMethod: string;
    procedure SetID(AValue: integer);
    procedure SetMethod(AValue: string);
  public
    constructor create;
    destructor destroy; override;

    property ID: integer read GetID write SetID;
    property Method: string read GetMethod write SetMethod;
    property Args: TJSONArray read GetArgs;
  end;

  { TCeosResponseContent }

  TCeosResponseContent = class(TJSONObject)
    private
      function GetID: integer;
      function GetResult: TJSONData;
      function GetVersion: string;
      procedure SetID(AValue: integer);
      procedure SetResult(AValue: TJSONData);
      procedure SetVersion(AValue: string);
    public
      procedure SetResultContent(const AResult: TJSONData; const AID: integer);

      constructor Create;
      destructor Destroy; override;

      property ID: integer read GetID write SetID;
      property ResultContent: TJSONData read GetResult write SetResult;
      property Version: string read GetVersion write SetVersion;
  end;

  function GetVariantType(const v: variant): string;

  function GetJSONArray(AArray: array of variant): TJSONArray;

  //default jsonrpc result response
  function JSONRPCResult(const AResult: TJSONData; const AID: integer = -1): TCeosResponseContent;
  //default jsonrpc error response
  function JSONRPCError(const ACode: integer; const AMessage: string; AID: integer = -1): TCeosResponseContent;


implementation

uses ceosconsts;

function JSONRPCResult(const AResult: TJSONData; const AID: integer = -1): TCeosResponseContent;
var
  joResult: TCeosResponseContent;
  //s: string;
begin
  joResult := TCeosResponseContent.create;

  joResult.Version := JSONRPC_VERSION;
  joResult.ResultContent := AResult;
  joResult.ID := AID;

  result := joResult;

  //s := joResult.AsJSON;

  //s := '';
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
  //joResult.Add('jsonrpc',JSONRPC_VERSION);
  joResult.Add('error',jsonerror);
  joResult.ID := AID;

  result := joResult;
end;


function GetVariantType(const v: variant): string;
begin
  case TVarData(v).vType of
    varEmpty: result := 'Empty';
    varNull: result := 'Null';
    varSmallInt: result := 'SmallInt';
    varInteger: result := 'Integer';
    varSingle: result := 'Single';
    varDouble: result := 'Double';
    varCurrency: result := 'Currency';
    varDate: result := 'Date';
    varOleStr: result := 'OleStr';
    varDispatch: result := 'Dispatch';
    varError: result := 'Error';
    varBoolean: result := 'Boolean';
    varVariant: result := 'Variant';
    varUnknown: result := 'Unknown';
    varByte: result := 'Byte';
    varString: result := 'String';
    varTypeMask: result := 'TypeMask';
    varArray: result := 'Array';
    varByRef: result := 'ByRef';
  end; // case
end;

function GetJSONArray(AArray: array of variant): TJSONArray;
var
  joArray: TJSONArray;
  i, iLen: integer;
  vtype: tvartype;
begin
  joArray := TJSONArray.create;

  iLen := High(AArray);

  for i := 0 to iLen do
  begin
    vtype := TVarData(AArray[i]).vType;

    case vtype of
      varEmpty: joArray.Add('');
      varNull: joArray.Add;
      varSmallInt: joArray.Add(integer(AArray[i]));
      varshortint: joArray.Add(integer(AArray[i]));
      varInteger: joArray.Add(integer(AArray[i]));
      varSingle: joArray.Add(integer(AArray[i]));
      varDouble: joArray.Add(TJSONFloat(AArray[i]));
      varCurrency: joArray.Add(TJSONFloat(AArray[i]));
      varDate: joArray.Add(datetostr(AArray[i]));
      varOleStr: joArray.Add(string(AArray[i]));
      varDispatch: joArray.Add(string(AArray[i]));
      varError: joArray.Add('error');
      varBoolean: joArray.Add(boolean(AArray[i]));
      varVariant: joArray.Add(string(AArray[i]));
      varUnknown: joArray.Add(string(AArray[i]));
      varByte: joArray.Add(integer(AArray[i]));
      varString: joArray.Add(string(AArray[i]));
      varTypeMask: joArray.Add(string(AArray[i]));
      varArray: joArray.Add(GetJSONArray(AArray[i]));
      varByRef: joArray.Add(integer(AArray[i]));
    end;
  end;

  result := joArray;
end;


{ TCeosResponseContent }

function TCeosResponseContent.GetID: integer;
begin
  {$WARNINGS OFF}
  if TJSONObject(Self).Find('id') <> nil then
    result := TJSONObject(Self).Find('id').AsInteger
  else
    result := -1;
  {$WARNINGS ON}
end;

function TCeosResponseContent.GetResult: TJSONData;
begin
  {$WARNINGS OFF}
  if TJSONObject(Self).Find('result') <> nil then
    result := TJSONObject(Self).Find('result')
  else
    result := nil;
  {$WARNINGS ON}
end;

function TCeosResponseContent.GetVersion: string;
begin
  {$WARNINGS OFF}
  if TJSONObject(Self).Find('jsonrpc') <> nil then
    result := TJSONObject(Self).Find('jsonrpc').AsString
  else
    result := JSONRPC_VERSION;
  {$WARNINGS ON}

end;

procedure TCeosResponseContent.SetID(AValue: integer);
begin
  {$WARNINGS OFF}
  if TJSONObject(Self).Find('id') <> nil then
    TJSONObject(Self).Find('id').AsInteger := AValue
  else
    TJSONObject(Self).Add('id',AValue);
  {$WARNINGS ON}
end;

procedure TCeosResponseContent.SetResult(AValue: TJSONData);
begin
  {$WARNINGS OFF}
  if TJSONObject(Self).Find('result') <> nil then
    TJSONObject(Self).Delete('result');

  TJSONObject(Self).Add('result',AValue);
  //TJSONObject(Self).Add('result',AValue as TJSONObject);
  {$WARNINGS ON}
end;

procedure TCeosResponseContent.SetVersion(AValue: string);
begin
  {$WARNINGS OFF}
  if TJSONObject(Self).Find('jsonrpc') <> nil then
    TJSONObject(Self).Find('jsonrpc').AsString := AValue
  else
    TJSONObject(Self).Add('jsonrpc',AValue);
  {$WARNINGS ON}

end;

procedure TCeosResponseContent.SetResultContent(const AResult: TJSONData;
  const AID: integer);
begin
  Self.Clear;

  Self.Version := JSONRPC_VERSION;
  Self.ResultContent := AResult;
  Self.ID := AID;
end;

constructor TCeosResponseContent.Create;
begin
  inherited Create;

  Self.Version := JSONRPC_VERSION;
  Self.ID := 0;
end;

destructor TCeosResponseContent.Destroy;
begin

  inherited Destroy;
end;

{ TCeosRequestContent }

function TCeosRequestContent.GetArgs: TJSONArray;
begin
  if Find('params') <> nil then
    result := (Find('params') as TJSONArray)
  else
  begin
    FParams := TJSONArray.Create;

    self.Add('params',FParams);
    result := (Find('params') as TJSONArray);
  end;
end;

function TCeosRequestContent.GetID: integer;
begin
  if Self.Find('id') <> nil then
    result := Self.Find('id').AsInteger
  else
  begin
    Self.Add('id',0);

    result := 0;
  end;
end;

function TCeosRequestContent.GetMethod: string;
begin
  if Self.Find('method') <> nil then
    result := Self.Find('method').AsString
  else
  begin
    Self.Add('method','');

    result := '';
  end;
end;


procedure TCeosRequestContent.SetID(AValue: integer);
begin
  if Self.Find('id') <> nil then
    Self.Find('id').AsInteger := AValue
  else
    Self.Add('id',AValue);
end;

procedure TCeosRequestContent.SetMethod(AValue: string);
begin
  if Self.Find('method') <> nil then
    Self.Find('method').AsString := AValue
  else
    Self.Add('method',AValue);
end;

constructor TCeosRequestContent.create;
begin
  inherited create;

  FParams := TJSONArray.create;

  Self.Add('jsonrpc',JSONRPC_VERSION);
  Self.Add('method','');
  Self.Add('params',FParams);
  Self.Add('id',0);
end;

destructor TCeosRequestContent.destroy;
begin

  inherited destroy;
end;


end.

