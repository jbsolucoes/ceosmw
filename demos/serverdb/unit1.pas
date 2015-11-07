unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ComCtrls, ceosserver, fpjson, jsonparser, ceostypes,
  fphttpserver, db, BufDataset;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnStart: TButton;
    btnStop: TButton;
    btnClear: TButton;
    DatasetDemo: TBufDataset;
    cbxRequestsCount: TCheckBox;
    CeosServer1: TCeosServer;
    cbxVerbose: TCheckBox;
    DatasetDemoCODIGO: TLongintField;
    DatasetDemoIDADE: TLongintField;
    DatasetDemoNOME: TStringField;
    Label1: TLabel;
    Memo1: TMemo;
    sePort: TSpinEdit;
    StatusBar1: TStatusBar;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure CeosServer1Exception(Sender: TObject; const E: exception);
    procedure CeosServer1GetRequest(Sender: TObject;
      const ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure CeosServer1Request(Sender: TObject;
      const ARequest: TCeosRequestContent; var AResponse: TCeosResponseContent);
    procedure CeosServer1RequestError(Sender: TObject; const E: exception;
      var AResponse: TCeosResponseContent);
    procedure CeosServer1Start(Sender: TObject);
    procedure CeosServer1Stop(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    RequestsCount: integer;
    procedure CreateDatasetDemo;
  public
    { public declarations }

    procedure Log(msg: string);
  end;

var
  Form1: TForm1;

implementation

uses ceosconsts, ceosmessages, ceosjson;

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnStartClick(Sender: TObject);
begin
  ceosserver1.port := sePort.value;

  ceosserver1.start;

  CreateDatasetDemo;

  btnStart.enabled := not ceosserver1.Active;
  btnStop.enabled := ceosserver1.Active;
end;

procedure TForm1.btnStopClick(Sender: TObject);
begin
  ceosserver1.stop;

  btnStart.enabled := not ceosserver1.Active;
  btnStop.enabled := ceosserver1.Active;
end;

procedure TForm1.btnClearClick(Sender: TObject);
begin
  memo1.Clear;
  RequestsCount := 0;

  if cbxRequestsCount.Checked then
    StatusBar1.Panels[0].Text := 'Requests: 0'
  else
    StatusBar1.Panels[0].Text := '';

  if not CeosServer1.Active then
    StatusBar1.Panels[2].text := '';
end;

procedure TForm1.CeosServer1Exception(Sender: TObject; const E: exception);
begin
  Log(E.message);
end;

procedure TForm1.CeosServer1GetRequest(Sender: TObject;
  const ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
begin
  Log(format('Get Request on %s',[ARequest.URI]));

  AResponse.Content := format('CeosMW %s - CeosServer Demo (URI: %s)',[CEOS_VERSION, ARequest.URI]);
end;

procedure TForm1.CeosServer1Request(Sender: TObject;
  const ARequest: TCeosRequestContent; var AResponse: TCeosResponseContent);
var
  jo: TJSONData;
begin
  if cbxVerbose.checked then
    Log(ARequest.AsJSON);

  AResponse.ResultContent := DataSetToJSONData(DatasetDemo);

  if cbxRequestsCount.checked then
  begin
    inc(RequestsCount);

    StatusBar1.Panels[0].text := format('Requests: %d',[RequestsCount]);
  end;
end;

procedure TForm1.CeosServer1RequestError(Sender: TObject; const E: exception;
  var AResponse: TCeosResponseContent);
begin
  Log(E.message);

  AResponse := JSONRPCError(ERR_REQUEST_ERROR,ERROR_REQUEST_ERROR);
end;

procedure TForm1.CeosServer1Start(Sender: TObject);
begin
  Log('Start...');

  StatusBar1.Panels[2].text := format('Start: %s',[datetimetostr(now)]);

  RequestsCount := 0;
  StatusBar1.Panels[0].text := format('Requests: %d',[RequestsCount]);
end;

procedure TForm1.CeosServer1Stop(Sender: TObject);
begin
  Log('Stop...');

  StatusBar1.Panels[2].text := StatusBar1.Panels[2].text + '       ' + format('Stop: %s',[datetimetostr(now)]);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  btnStop.Click;;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.CreateDatasetDemo;
var
  i: integer;
begin
  DatasetDemo.close;
  DatasetDemo.CreateDataset;
  DatasetDemo.open;

  for i := 0 to 30 do
  begin
    randomize;

    DatasetDemo.Append;
    DatasetDemoCODIGO.value := i;
    DatasetDemoNOME.value   := format('CLIENTE %d',[i]);
    DatasetDemoIDADE.value  := random(50);
    DatasetDemo.post;
  end;
end;

procedure TForm1.Log(msg: string);
begin
  if cbxVerbose.checked then
    memo1.lines.add(formatdatetime('hh:nn:ss:zzz',now) + #9 + msg);
end;


end.

