unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ceosserver, fpjson, jsonparser, ceostypes, fphttpserver;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CeosServer1: TCeosServer;
    cbxVerbose: TCheckBox;
    Label1: TLabel;
    Memo1: TMemo;
    sePort: TSpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
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
  private
    { private declarations }
  public
    { public declarations }
    procedure Log(msg: string);
  end;

var
  Form1: TForm1;

implementation

uses ceosconsts, ceosmessages;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  ceosserver1.port := sePort.value;

  ceosserver1.start;

  button1.enabled := not ceosserver1.Active;
  button2.enabled := ceosserver1.Active;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ceosserver1.stop;

  button1.enabled := not ceosserver1.Active;
  button2.enabled := ceosserver1.Active;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  memo1.Clear;
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
  joStr: TJSONData;
  str: string;
begin
  Log(ARequest.AsJSON);

  str := 'JBS';

  joStr := TJSONString.Create(str);

  AResponse := JSONRPCResult(joStr, ARequest.ID);
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
end;

procedure TForm1.CeosServer1Stop(Sender: TObject);
begin
  Log('Stop...');
end;

procedure TForm1.Log(msg: string);
begin
  if cbxVerbose.checked then
    memo1.lines.add(formatdatetime('hh:nn:ss:zzz',now) + #9 + msg);
end;


end.

