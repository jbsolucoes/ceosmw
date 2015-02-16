unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, BufDataset, FileUtil, Forms, Controls, Graphics,
  Dialogs, StdCtrls, DBGrids, ceosclient;

type

  { TForm1 }

  TForm1 = class(TForm)
    BufDataset1: TBufDataset;
    Button1: TButton;
    CeosClient1: TCeosClient;
    Datasource1: TDatasource;
    DBGrid2: TDBGrid;
    edFunc: TEdit;
    edParam: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
    procedure DoJSONToDataset;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses jsonlib, fpjson, JSONParser;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  memo1.text := CeosClient1.JSONCall(edFunc.text,[edParam.Text],1).Find('result').AsString;

  DoJSONToDataSet;
end;

procedure TForm1.DoJSONToDataset;
var
  JSON: TJSONObject;
  P : TJSONParser;
  sJSON: string;
begin
  try
    sJSON := memo1.text;

    BufDataset1.close;

    BufDataset1.Fields.Clear;

    P := TJSONParser.Create(sJSON);

    JSON := (P.Parse as TJsonObject);

    JSONToDataset(tdataset(BufDataset1),JSON);

    BufDataset1.Open;
    BufDataset1.first;

  finally
    P.Free;
    JSON.Free;
  end;
end;

end.

