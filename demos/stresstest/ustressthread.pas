unit ustressthread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CeosClient, forms;

type

{ StressThread }

{ TStressThread }

TStressThread = class(TThread)
private
  FContent: string;
  FHost: string;
  FID: integer;
  FMethod: string;
  FOK: integer;
  FNOK: integer;
  FCount: integer;
  FSyncronization: boolean;
  FTotal: integer;
  FCeosClient: TCeosClient;
  procedure DoTerminate; override;
  procedure Synchronize();
public
  constructor Create;
  destructor Destroy; override;

  procedure Execute; override;

  property Count: integer read FCount;
  property Total: integer read FTotal write FTotal;
  property Ceos: TCeosClient read FCeosClient write FCeosClient;
  property ID: integer read FID write FID;
  property Host: string read FHost write FHost;
  property Method: string read FMethod write FMethod;
  property Content: string read FContent write FContent;
  property Syncronization: boolean read FSyncronization write FSyncronization;
end;

implementation

uses umain;

{ StressThread }

procedure TStressThread.DoTerminate;
begin
  inherited DoTerminate;
end;

procedure TStressThread.Execute;
begin
  Ceos.Host := Host;

  try
  if (Ceos.JSONCall(method,[content],ID).Find('result') <> nil) then
    inc(FOK)
  else
    inc(FNOK);

  except
    inc(FNOK);
  end;

  inc(FCount);

  if Syncronization then
    Synchronize
  else
    application.processmessages;
end;

procedure TStressThread.Synchronize;
begin
  frmmain.lblok.caption := inttostr(FOK);
  frmmain.lblnok.caption := inttostr(FNOK);
  frmmain.lblcount.caption := inttostr(FCount);
  application.processmessages;
end;

constructor TStressThread.Create;
begin
  FOK := 0;
  FNOK := 0;
  FCount := 0;
  FTotal := 0;
  Syncronization := true;

  Ceos := TCeosClient.Create(nil);
  Ceos.host := 'http://localhost:88';
end;

destructor TStressThread.Destroy;
begin
  Ceos.Free;
  Ceos := nil;

  inherited Destroy;
end;

end.

