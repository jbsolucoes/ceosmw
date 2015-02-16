unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, LclIntf;

type

  { Tfrmmain }

  Tfrmmain = class(TForm)
    Button1: TButton;
    Button2: TButton;
    cbSynchronize: TCheckBox;
    edHost: TEdit;
    edid: TEdit;
    edContent: TEdit;
    edMethod: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lblstart: TLabel;
    lblstop: TLabel;
    lbltime: TLabel;
    lblOK: TLabel;
    lblNOK: TLabel;
    lblCount: TLabel;
    seQtde: TSpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmmain: Tfrmmain;
  bRunning: boolean;

implementation

uses ustressthread;

{$R *.lfm}

{ Tfrmmain }

procedure Tfrmmain.Button1Click(Sender: TObject);
var
  st: TStressThread;
  dtini, dtfim: tdatetime;
begin
  try
    bRunning := true;

    st      := TStressThread.Create;
    st.host := edHost.text;
    st.id   := strtoint(edId.text);
    st.content := edContent.text;
    st.Method  := edMethod.text;
    st.Total := seQtde.value;
    st.Syncronization := cbSynchronize.Checked;

    lblstart.caption := formatdatetime('dd/mm/yyyy hh:nn:ss:zzz',now);

    dtini := now;

    while (st.Count < st.Total) and bRunning do
      st.Execute;

    dtfim := now;

    lbltime.caption := formatdatetime('hh:nn:ss:zzz',dtfim - dtini);

    lblstop.caption := formatdatetime('dd/mm/yyyy hh:nn:ss:zzz',dtfim);
  finally
    freeandnil(st);
  end;
end;

procedure Tfrmmain.Button2Click(Sender: TObject);
begin
  bRunning := false;
end;

end.

