{********************************************************}
{ JBS JSON Library                                       }

{ Copyright (c) 2013 JBS Soluções.                       }

{********************************************************}

unit jsonlib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, fpjson, jsonparser, typinfo, zstream, base64;

{Converte de dataset para formato JSON}
function DataSetToJSON(DataSet: TDataSet; ReturnFields: boolean = True;
  ReturnData: boolean = True): string;

{converte de formato JSON para dataset}
function JSONToDataset(var dataset: TDataSet; JsonObject: TJSONObject): boolean;





//EncodeString : Encode a String
function EncodeString(const Input: string): string;

//DecodeString : Decode a String
function DecodeString(const Input: string): string;

//BoolStr : boolean to string with numbers options
function BoolStr(Value: boolean; ToNumberString: boolean = False): string;




procedure JSONDataToDataset(AJSON: TJSONData; var ADataSet: TDataSet;
  const ADateAsString: boolean = True); overload;

function CreateFieldsByJson(var dataset: TDataset; ColsJson: TJSONdata): boolean;

procedure GetFieldTypeInfo(Field: TField; var FieldType, JsonTyp: string);
function CreateJsonValueByField(JsonObject: TJSONObject; Field: TField): boolean;
function ImportDataFromJSon(DataSet: TDataSet; DataJson: TJSONData): integer;
function GetValue2Field(Field: TField; JsonValue: TJSONOBject): variant;

function FieldsToJSON(AFields: TFields; const ADateAsString: boolean = True): string;

procedure JSONToFields(AJSON: TJSONObject; var ADataset: TDataset;
  const ADateAsString: boolean = True);

function GetJSONType(const AFieldType: TFieldType): ShortString;



const
  JSON_NULL    = 'null';
  JSON_STRING  = 'string';
  JSON_BOOLEAN = 'boolean';
  JSON_DATE    = 'date';
  JSON_FLOAT   = 'float';
  JSON_INT     = 'int';
  cstFieldType    = 'FieldType'; //< label of json tag FieldType.
  cstFieldName    = 'FieldName'; //< label of json tag FieldName.
  cstDisplayLabel = 'DisplayLabel'; //< label of json tag Displaylabel.
  cstFieldSize    = 'FieldSize'; //< label of json tag FieldSize.
  cstJsonType     = 'JsonType'; //< label of json tag JsonType.
  cstRequired     = 'Required'; //< label of json tag Required.
  cstFieldIndex   = 'FieldIndex'; //< label of json tag FieldIndex.
  cstCols         = 'Fields'; //< label of json tag Fields.
  cstData         = 'Data'; //< label of json tag Data.


implementation

const
  {Constant array of encode chars.}
  EncodeTable: array[0..63] of char =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZ' + 'abcdefghijklmnopqrstuvwxyz' + '0123456789+/';

  {Constant array of encode Bytes.}
  DecodeTable: array[#0..#127] of integer = (
    byte('='), 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
    64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
    64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 62, 64, 64, 64, 63,
    52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 64, 64, 64, 64, 64, 64,
    64, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 64, 64, 64, 64, 64,
    64, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
    41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 64, 64, 64, 64, 64);

type
  TPacket = packed record
    case integer of
      0: (b0, b1, b2, b3: byte);
      1: (i: integer);
      2: (a: array[0..3] of byte);
      3: (c: array[0..3] of char);
  end;


procedure EncodePacket(const Packet: TPacket; NumChars: integer; OutBuf: PChar);
begin
  OutBuf[0] := EnCodeTable[Packet.a[0] shr 2];
  OutBuf[1] := EnCodeTable[((Packet.a[0] shl 4) or (Packet.a[1] shr 4)) and $0000003f];
  if NumChars < 2 then
    OutBuf[2] := '='
  else
    OutBuf[2] := EnCodeTable[((Packet.a[1] shl 2) or (Packet.a[2] shr 6)) and $0000003f];
  if NumChars < 3 then
    OutBuf[3] := '='
  else
    OutBuf[3] := EnCodeTable[Packet.a[2] and $0000003f];
end;

function DecodePacket(InBuf: PChar; var nChars: integer): TPacket;
begin
  Result.a[0] := (DecodeTable[InBuf[0]] shl 2) or (DecodeTable[InBuf[1]] shr 4);
  NChars := 1;
  if InBuf[2] <> '=' then
  begin
    Inc(NChars);
    Result.a[1] := byte((DecodeTable[InBuf[1]] shl 4) or (DecodeTable[InBuf[2]] shr 2));
  end;
  if InBuf[3] <> '=' then
  begin
    Inc(NChars);
    Result.a[2] := byte((DecodeTable[InBuf[2]] shl 6) or DecodeTable[InBuf[3]]);
  end;
end;

procedure EncodeStream(Input, Output: TStream);
var
  InBuf: array[0..509] of byte;
  OutBuf: array[0..1023] of char;
  BufPtr: PChar;
  I, J, K, BytesRead: integer;
  Packet: TPacket;
begin
  InBuf[0] := 0;
  OutBuf[0] := #0;

  K := 0;
  repeat
    BytesRead := Input.Read(InBuf, SizeOf(InBuf));
    I := 0;
    BufPtr := OutBuf;
    while I < BytesRead do
    begin
      if BytesRead - I < 3 then
        J := BytesRead - I
      else
        J := 3;
      Packet.i := 0;
      Packet.b0 := InBuf[I];
      if J > 1 then
        Packet.b1 := InBuf[I + 1];
      if J > 2 then
        Packet.b2 := InBuf[I + 2];
      EncodePacket(Packet, J, BufPtr);
      Inc(I, 3);
      Inc(BufPtr, 4);
      Inc(K, 4);
      if K > 75 then
      begin
        BufPtr[0] := #$0D;
        BufPtr[1] := #$0A;
        Inc(BufPtr, 2);
        K := 0;
      end;
    end;
    Output.Write(Outbuf, BufPtr - PChar(@OutBuf));
  until BytesRead = 0;
end;

procedure DecodeStream(Input, Output: TStream);
var
  InBuf: array[0..75] of char;
  OutBuf: array[0..60] of byte;
  InBufPtr, OutBufPtr: PChar;
  I, J, K, BytesRead: integer;
  Packet: TPacket;

  procedure SkipWhite;
  var
    C: char;
    NumRead: integer;
  begin
    C := #0;

    while True do
    begin
      NumRead := Input.Read(C, 1);
      if NumRead = 1 then
      begin
        if C in ['0'..'9', 'A'..'Z', 'a'..'z', '+', '/', '='] then
        begin
          Input.Position := Input.Position - 1;
          Break;
        end;
      end
      else
        Break;
    end;
  end;

  function ReadInput: integer;
  var
    WhiteFound, EndReached: boolean;
    CntRead, Idx, IdxEnd: integer;
  begin
    IdxEnd := 0;
    repeat
      WhiteFound := False;
      CntRead := Input.Read(InBuf[IdxEnd], (SizeOf(InBuf) - IdxEnd));
      EndReached := CntRead < (SizeOf(InBuf) - IdxEnd);
      Idx := IdxEnd;
      IdxEnd := CntRead + IdxEnd;
      while (Idx < IdxEnd) do
      begin
        if not (InBuf[Idx] in ['0'..'9', 'A'..'Z', 'a'..'z', '+', '/', '=']) then
        begin
          Dec(IdxEnd);
          if Idx < IdxEnd then
            Move(InBuf[Idx + 1], InBuf[Idx], IdxEnd - Idx);
          WhiteFound := True;
        end
        else
          Inc(Idx);
      end;
    until (not WhiteFound) or (EndReached);
    Result := IdxEnd;
  end;

begin
  J := 0;

  repeat
    SkipWhite;
    {
    BytesRead := Input.Read(InBuf, SizeOf(InBuf));
    }
    BytesRead := ReadInput;
    InBufPtr := InBuf;
    OutBufPtr := @OutBuf;
    I := 0;
    while I < BytesRead do
    begin
      Packet := DecodePacket(InBufPtr, J);
      K := 0;
      while J > 0 do
      begin
        OutBufPtr^ := char(Packet.a[K]);
        Inc(OutBufPtr);
        Dec(J);
        Inc(K);
      end;
      Inc(InBufPtr, 4);
      Inc(I, 4);
    end;
    Output.Write(OutBuf, OutBufPtr - PChar(@OutBuf));
  until BytesRead = 0;
end;

function iif(condicao: boolean; verdadeiro,falso: string): string;
begin
  if condicao then
    result := verdadeiro
  else
    result := falso;
end;


function CreateFieldsByJson(var dataset: TDataset; ColsJson: TJSONData): boolean;
var
  ft: TFieldType;
  i: integer;
  fieldName, fieldLabel: string;
  fieldSize: integer;
  fieldRequired: boolean;
begin
  try
    DataSet.Close;
    dataset.FieldDefs.Clear;

    for i := 0 to ColsJson.Count - 1 do
    begin
      ft := TFieldType(GetEnumValue(typeinfo(TFieldType), 'ft' +
        (ColsJson.Items[i] as TJSONObject).Strings[cstFieldType]));

      if ft = ftAutoInc then
        ft := ftInteger;

      fieldName := (ColsJson.Items[i] as TJSONObject).Strings[cstFieldName];
      fieldSize := (ColsJson.Items[i] as TJSONObject).Integers[cstFieldSize];
      fieldRequired := (ColsJson.Items[i] as TJSONObject).Booleans[cstRequired];
      fieldLabel := (ColsJson.Items[i] as TJSONObject).Strings[cstDisplayLabel];

      dataset.FieldDefs.Add(fieldName, ft, fieldSize, fieldRequired);
      dataset.FieldDefs.Find(fieldName).CreateField(dataset).DisplayLabel := fieldLabel;
    end;

    Result := True;
  except
    Result := False;
  end;
end;

procedure GetFieldTypeInfo(Field: TField; var FieldType, JsonTyp: string);
begin
  FieldType := GetEnumName(typeinfo(tfieldtype), Ord(Field.DataType));

  Delete(FieldType, 1, 2);

  JsonTyp := GetJSONType(Field.DataType);
end;

function CreateJsonValueByField(JsonObject: TJSONObject; Field: TField): boolean;
begin
  if Field is TDateTimeField then
    JsonObject.Objects[Field.FieldName].Value := Field.AsDateTime
  else if Field is TBlobField then
    JsonObject.Objects[Field.FieldName].Value := EncodeString(Field.AsString)
  else
    JsonObject.Objects[Field.FieldName].Value := Field.Value;

  Result := True;
end;

function ImportDataFromJSon(DataSet: TDataSet; DataJson: TJSONData): integer;
var
  i: integer;
  FieldName: string;
  FieldValue: variant;
begin
  if not DataSet.Active then
    DataSet.Open;

  DataSet.DisableControls;

  try
    for i := 0 to DataJson.Count - 1 do
    begin
      DataSet.Append;

      FieldName := (DataJson.Items[i] as TJSONObject).AsString;
      FieldValue := DataJson.Items[i].Value;

      if DataSet.FindField(FieldName) <> nil then
      begin
        DataSet.FindField(FieldName).Value := FieldValue;
      end;

      DataSet.Post;
    end;

  finally
    DataSet.First;
    DataSet.EnableControls;
  end;

  Result := 1;
end;

function GetValue2Field(Field: TField; JsonValue: TJSONObject): variant;
begin
  if JsonValue.Types[Field.Name] = jtNull then
    Result := Null
  else if Field is TDateTimeField then
    Result := TDateTime(JsonValue.AsInteger)
  else if (Field is TIntegerField) or (Field is TLargeintField) then
    Result := JsonValue.AsInteger
  else if Field is TNumericField then
    Result := JsonValue.AsFloat
  else if Field is TBooleanField then
    Result := JsonValue.AsBoolean
  else if Field is TStringField then
    Result := JsonValue.AsString
  else if Field is TBlobField then
    Result := DecodeString(JsonValue.AsString);
end;

{Converte de dataset para formato JSON}
function DataSetToJSON(DataSet: TDataSet; ReturnFields: boolean = True;
  ReturnData: boolean = True): string;
var
  i: integer;
  sFields, sData, FieldType, JsonTyp: string;
  List: TStringList;
begin
  List := TStringList.Create;

  JsonTyp := '';
  FieldType := '';
  sData := '';
  sFields := '';

  try
    List.Sorted := True;

    if ReturnFields then
    begin
      sFields := '"' + cstCols + '":[';

      for i := 0 to DataSet.FieldCount - 1 do
      begin
        GetFieldTypeInfo(DataSet.Fields[i], FieldType, JsonTyp);

        sFields := sFields + format(
          '{"%s":"%s","%s":%s,"%s":"%s","%s":%s,"%s":"%s","%s":%s,"%s":"%s"}',
          [cstJsonType, JsonTyp, cstFieldIndex,
          IntToStr(DataSet.Fields[i].Index), cstFieldType,
          FieldType, cstFieldSize, iif(FieldType = 'Integer',
          '0', IntToStr(DataSet.Fields[i].Size)), cstFieldName,
          Dataset.Fields[i].FieldName, cstRequired,
          BoolStr(DataSet.Fields[i].Required), cstDisplayLabel,
          Dataset.Fields[i].DisplayLabel]);

        if i < (dataset.FieldCount - 1) then
          sFields := sFields + ',';

        List.Add(DataSet.Fields[i].FieldName + '=' + JsonTyp);
      end;

      sFields := sFields + ']';
    end;

    if ReturnData then
    begin
      DataSet.DisableControls;
      DataSet.First;

      sData := '"' + cstData + '":[';

      while not dataset.EOF do
      begin
        sData := sData + FieldsToJSON(Dataset.Fields);

        dataset.Next;

        if not dataset.EOF then
          sData := sData + ',';
      end;

      sData := sData + ']';
    end;

    if returnFields then
      Result := sFields;

    if returndata then
    begin
      if ReturnFields then
        Result := sFields + ', ' + sData
      else
        Result := sData;
    end;

    Result := format('{%s}', [Result]);
  finally
    List.Free;
    DataSet.First;
    DataSet.EnableControls;
  end;
end;

{converte de formato JSON para dataset}
function JSONToDataset(var dataset: TDataSet; JsonObject: TJSONObject): boolean;
begin
  if JsonObject = nil then
  begin
    Result := False;
    Exit;
  end;

  CreateFieldsByJson(dataset, JsonObject[cstCols]);

  if dataset.FieldDefs.Count > 0 then
    dataset.Open
  else
  begin
    Result := False;
    exit;
  end;

  JSONDataToDataset(JsonObject[cstData], dataset);

  Result := True;
end;


procedure JSONDataToDataset(AJSON: TJSONData; var ADataSet: TDataSet;
  const ADateAsString: boolean = True); overload;
var
  I, iCount: integer;
  P: TJSONParser;
  O: TJsonArray;
  json: string;
begin
  try
    json := AJSON.AsJSON;
    P := TJSONParser.Create(json);
    O := (P.Parse as TJSONArray);

    iCount := Pred(O.Count);

    for I := 0 to iCount do
    begin
      ADataSet.Append;
      JSONToFields(O.Objects[I], ADataSet, ADateAsString);
      ADataSet.Post;
    end;
  finally
    P.Free;
    O.Free;
  end;
end;

function FieldsToJSON(AFields: TFields; const ADateAsString: boolean = True): string;
var
  I: integer;
  VField: TField;
  VFieldType, VFieldName: ShortString;
  VJSON: TJSONObject;
begin
  try
    VJSON := TJSONOBject.Create;

    for I := 0 to Pred(AFields.Count) do
    begin
      VField := AFields[I];
      VFieldType := GetJSONType(VField.DataType);
      VFieldName := VField.FieldName;
      if (VFieldType = JSON_NULL) or VField.IsNull then
      begin
        VJSON.Add(VFieldName);
        Continue;
      end;
      if VFieldType = JSON_STRING then
        VJSON.Add(VFieldName, VField.AsString)
      else
      if VFieldType = JSON_BOOLEAN then
        VJSON.Add(VFieldName, VField.AsBoolean)
      else
      if VFieldType = JSON_DATE then
      begin
        if ADateAsString then
          VJSON.Add(VFieldName, VField.AsString)
        else
          VJSON.Add(VFieldName, VField.AsFloat);
      end
      else
      if (VFieldType = JSON_FLOAT) and (VField.Size > 0) then
        VJSON.Add(VFieldName, VField.AsFloat)
      else
      if (VFieldType = JSON_FLOAT) and (VField.Size = 0) then
        VJSON.Add(VFieldName, VField.AsInteger)
      else
      if VFieldType = JSON_INT then
        VJSON.Add(VFieldName, VField.AsInteger)
      else
        VJSON.Add(VFieldName, VField.AsString);
    end;

    Result := VJSON.AsJSON;
  finally
    VJSON.Free;
  end;
end;

procedure JSONToFields(AJSON: TJSONObject; var ADataset: TDataset;
  const ADateAsString: boolean = True);
var
  I: integer;
  VName: string;
  VField: TField;
  VData: TJSONData;
begin
  for I := 0 to Pred(AJSON.Count) do
  begin
    VName := (AJSON as tjsonobject).Names[I];
    VField := ADataset.Fields.FindField(VName);

    if not Assigned(VField) then
      Continue;

    VData := AJSON.Items[I];

    ADataset.FieldByName(VName).Clear;

    if VData.IsNull then
      Exit;

    if (VField is TStringField) or (VField is TBinaryField) or
      (VField is TBlobField) or (VField is TVariantField) then
      ADataset.FieldByName(VName).AsString := VData.AsString
    else
    if (VField is TLongintField) or (VField is TLargeintField) or
      (VField is TAutoIncField) then
      ADataset.FieldByName(VName).Value := VData.AsInteger
    else
    if (VField is TFloatField) or (VField is TBCDField) or
      (VField is TFMTBCDField) then
      ADataset.FieldByName(VName).AsFloat := VData.AsFloat
    else
    if VField is TBooleanField then
      ADataset.FieldByName(VName).AsBoolean := VData.AsBoolean
    else
    if VField is TDateTimeField then
    begin
      if ADateAsString then
        ADataset.FieldByName(VName).AsDateTime := StrToDateTime(VData.AsString)
      else
        ADataset.FieldByName(VName).AsDateTime := VData.AsFloat;
    end
    else
      ADataset.FieldByName(VName).AsString := VData.AsString;
  end;
end;

function GetJSONType(const AFieldType: TFieldType): ShortString;
begin
  Result := JSON_NULL;

  case AFieldType of
    ftUnknown: Result := JSON_STRING;
    ftString: Result := JSON_STRING;
    ftSmallint: Result := JSON_INT;
    ftInteger: Result := JSON_INT;
    ftWord: Result := JSON_INT;
    ftBoolean: Result := JSON_BOOLEAN;
    ftFloat: Result := JSON_FLOAT;
    ftCurrency: Result := JSON_FLOAT;
    ftBCD: Result := JSON_FLOAT;
    ftDate: Result := JSON_DATE;
    ftTime: Result := JSON_STRING;
    ftDateTime: Result := JSON_DATE;
    ftAutoInc: Result := JSON_INT;
    ftBlob: Result := JSON_STRING;
    ftMemo: Result := JSON_STRING;
    ftFmtMemo: Result := JSON_STRING;
    ftFixedChar: Result := JSON_STRING;
    ftWideString: Result := JSON_STRING;
    ftLargeint: Result := JSON_INT;
    ftVariant: Result := JSON_STRING;
    ftTimeStamp: Result := JSON_DATE;
    ftFMTBcd: Result := JSON_FLOAT;
    ftFixedWideChar: Result := JSON_STRING;
    ftWideMemo: Result := JSON_STRING;
    else
      Result := JSON_STRING;
  end;
end;


function EncodeString(const Input: string): string;

var
  InStr, OutStr: TStringStream;
begin
  InStr := TStringStream.Create(Input);
  try
    OutStr := TStringStream.Create('');
    try
      EncodeStream(InStr, OutStr);
      Result := OutStr.DataString;
    finally
      OutStr.Free;
    end;
  finally
    InStr.Free;
  end;
end;

function DecodeString(const Input: string): string;

var
  InStr, OutStr: TStringStream;
begin
  InStr := TStringStream.Create(Input);
  try
    OutStr := TStringStream.Create('');
    try
      DecodeStream(InStr, OutStr);
      Result := OutStr.DataString;
    finally
      OutStr.Free;
    end;
  finally
    InStr.Free;
  end;
end;


function BoolStr(Value: boolean; ToNumberString: boolean = False): string;
begin
  if Value then
    Result := BoolToStr(ToNumberString, '1', 'true')
  else
    Result := BoolToStr(ToNumberString, '0', 'false');
end;


end.
