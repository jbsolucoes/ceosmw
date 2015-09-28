{*****************************************************************}
{                                                                 }
{ by Jose Benedito - josebenedito@gmail.com                       }
{ www.jbsolucoes.net                                              }
{*****************************************************************}

unit appmethods;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ceostypes, fpjson, jsonparser, ceosservermethods, ceosjson, db,
  BufDataSet, sqldb, mssqlconn, fgl;

type

  { TCeosMethods }

  TCeosMethods = class(TCeosServerMethods)

  private


  published
    //exemplo metodos
    function Test(Request: TCeosRequestContent): TJSONStringType;
    function Test2(Request: TCeosRequestContent): TJSONStringType;
    function TestValmadson(Request: TCeosRequestContent): TJSONStringType;

    //exemplo de trafego de objetos json
    function GetObj(Request: TCeosRequestContent): TJSONStringType;
    function SetObj(Request: TCeosRequestContent): TJSONStringType;

    //json list serialization
    function GetObjList(Request: TCeosRequestContent): TJSONStringType;
    function SetObjList(Request: TCeosRequestContent): TJSONStringType;

    //exemplo de transferencia de datasets
    function DatasetJSON(Request: TCeosRequestContent): TJSONStringType;
    function ReadDataset(Request: TCeosRequestContent): TJSONStringType;

    //executa instruções SQL
    function QuerySQL(Request: TCeosRequestContent): TJSONStringType;
  end;

  TPerson = class(TObject)
  private
    FId: Int64;
    FName: string;
  published
    property Id: Int64 read FId write FId;
    property Name: string read FName write FName;
  end;

  { TList }

  generic TList<T> = class
    Items: array of T;
    procedure Add(Value: T);
  end;

   { TPessoa }

   TPessoa = class(TCollectionItem)
   private
     FIdade: integer;
     FNome: string;
   public
     constructor Create(ACollection: TCollection); override;
   published
     property Idade: integer read FIdade write FIdade;
     property Nome: string read FNome write FNome;
   end;

   { TListaPessoas }

   TListaPessoas = class(TCollection)
   private
     function GetItems(Index: integer): TPessoa;
     procedure SetItems(Index: integer; AValue: TPessoa);
   public
     constructor Create;
   public
     function Add: TPessoa;
     property Items[Index: integer]: TPessoa read GetItems write SetItems; default;
   end;


implementation

{ TListaPessoas }

function TListaPessoas.GetItems(Index: integer): TPessoa;
begin
  Result := TPessoa(inherited Items[Index]);
end;

procedure TListaPessoas.SetItems(Index: integer; AValue: TPessoa);
begin
  Items[Index].Assign(AValue);
end;

constructor TListaPessoas.Create;
begin
  inherited Create(TPessoa);
end;

function TListaPessoas.Add: TPessoa;
begin
  Result := inherited Add as TPessoa;
end;

{ TPessoa }

constructor TPessoa.Create(ACollection: TCollection);
begin
  if Assigned(ACollection) then
    inherited Create(ACollection);
end;

{ TList }

procedure TList.Add(Value: T);
begin
  SetLength(Items, Length(Items) + 1);
  Items[Length(Items) - 1] := Value;
end;


{ TCeosMethods }

function TCeosMethods.Test(Request: TCeosRequestContent): TJSONStringType;
begin
  result := 'TESTE CEOSMW';
end;

function TCeosMethods.Test2(Request: TCeosRequestContent): TJSONStringType;
begin
  result := 'TESTE 2';
end;

//object to json
function TCeosMethods.GetObj(Request: TCeosRequestContent): TJSONStringType;
var
  pessoa: TPerson;
begin
  pessoa := TPerson.Create;

  try
    pessoa.Id := 10;
    pessoa.Name := Request.Args[0].AsString;

    result := ObjToJSON(pessoa);
  finally
    pessoa.free;
  end;

end;


//json to object
function TCeosMethods.SetObj(Request: TCeosRequestContent): TJSONStringType;
var
  pessoa: TPerson;
  s: string;
begin
  pessoa := TPerson.Create;

  try
    JSONToObject(Request.Args[0].AsJSON, tobject(pessoa));

    try
      if pessoa.id = 10 then
        raise exception.Create('Invalid ID!'); //exception sample

      s := format('id %d nome %s',[pessoa.Id, pessoa.Name]);

      result := s;
    finally
      freeandnil(pessoa);
    end;
  except on e:exception do
    raise exception.create(e.message);
  end;
end;

function TCeosMethods.GetObjList(Request: TCeosRequestContent): TJSONStringType;
var
  Pessoas: TListaPessoas;
  Pessoa: TPessoa;
begin
  try
    Pessoas := TListaPessoas.Create;

    Pessoa := Pessoas.Add;
    Pessoa.Idade := 20;
    Pessoa.Nome := 'JB';

    Pessoa := Pessoas.Add;
    Pessoa.Idade := 20;
    Pessoa.Nome := 'FERNANDO';

    result := ObjToJSON(Pessoas);

  finally
    Pessoas.free;
  end;
end;

function TCeosMethods.SetObjList(Request: TCeosRequestContent): TJSONStringType;
var
  listapessoas: TListaPessoas;
  s: string;
  i: integer;
begin
  listapessoas := TListaPessoas.Create;

  try
    JSONToObject(Request.Args[0].AsJSON, tobject(listapessoas));

    try
      s := '';

      for i := 0 to listapessoas.Count -1 do
        s := s + format('nome %s id %d ,',[listapessoas.Items[i].Nome, listapessoas.Items[i].Idade]);

      result := s;
    finally
      freeandnil(listapessoas);
    end;
  except on e:exception do
    raise exception.create(e.message);
  end;
end;

function TCeosMethods.TestValmadson(Request: TCeosRequestContent): TJSONStringType;
begin
  result := 'EXEMPLO';
end;

function TCeosMethods.DatasetJSON(Request: TCeosRequestContent): TJSONStringType;
var
  Dset: TBufDataSet;
  i: integer;
begin
  {$WARNINGS OFF}
  Dset := TBufDataSet.Create(nil);
  {$WARNINGS ON}
  try
    DSet.FieldDefs.Add('ID',ftInteger);
    DSet.FieldDefs.Add('NAME',ftString,20);
    Dset.CreateDataset;
    DSet.Open;

    for i := 0 to Request.Args.Count -1 do
    begin
      DSet.Append;
      DSet.FieldByName('ID').Value:= i+1;
      DSet.FieldByName('NAME').Value:= Request.Args[i].AsString;
      DSet.Post;
    end;

    result := DatasetToJSON(Dset);
  finally
    Dset.free;
  end;

end;

function TCeosMethods.ReadDataset(Request: TCeosRequestContent): TJSONStringType;
var
  Dset: TBufDataSet;
  jo: tjsonobject;
begin
  jo := tjsonparser.create(Request.Args[0].AsJSON).parse as tjsonobject;
  {$WARNINGS OFF}
  Dset := TBufDataSet.create(nil);
  {$WARNINGS ON}
  try
    if JSONToDataset(tdataset(Dset),jo) then
    begin
      Dset.first;
      result := 'OK '+ Dset.FieldByName('NAME').asstring
    end
    else
      result := 'NOK';
  finally
    Dset.free;
  end;
end;

function TCeosMethods.QuerySQL(Request: TCeosRequestContent): TJSONStringType;
var
  conn: TMSSQLConnection;
  tra: TSQLTransaction;
  qry: TSQLQuery;
  sSQL: string;
  bReturnFields: boolean;
begin
  conn := TMSSQLConnection.Create(nil);
  tra  := TSQLTransaction.Create(nil);
  qry  := TSQLQuery.Create(nil);

  try
    conn.HostName     := 'ABEL\SQLEXPRESS';
    conn.DatabaseName := 'jb';
    conn.UserName     := 'sa';
    conn.Password     := 'jbs123';
    conn.Transaction  := tra;

    qry.DataBase := conn;

    try
      conn.open;

      conn.StartTransaction;

      sSQL := Request.Args[0].AsString;
      qry.SQL.Text := sSQL;

      bReturnFields := (Request.Args[1].AsInteger = 1);

      if IsSelect(sSQL) then
      begin
        qry.Open;

        result := DataSetToJSON(qry,bReturnFields);
      end
      else
      begin
        qry.ExecSQL;

        result := 'OK';
      end;

      tra.Commit;
    except on e:exception do
      begin
        tra.Rollback;

        raise exception.Create(e.Message);
      end;
    end;
  finally
    conn.close;
    qry.free;
    tra.free;
    conn.free;
  end;

end;

end.

