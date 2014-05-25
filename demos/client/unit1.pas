unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ceosclient;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    CeosClient1: TCeosClient;
    CheckBox1: TCheckBox;
    edHost: TEdit;
    edID: TEdit;
    edMethod: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses ceostypes, fpjson;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  req: TCeosRequestContent;
  ja: TJSONArray;
begin
  ceosclient1.Host := edHost.Text;

  if CheckBox1.Checked then
  begin
    req := TCeosRequestContent.Create;

    try
      req.ID:=1;
      req.Method:=edMethod.text;
      req.Args.Add('JBS');
      req.Args.Add(2014);
      ja := TJSONArray.create(['CeosMW',1]);
      req.Args.Add(ja);

      memo1.text := CeosClient1.Call(req);
    finally
      req.free;
    end;
  end
  else
    memo1.text := CeosClient1.Call(edMethod.text,[],strtoint(edID.text));
end;

end.

