{
 dbComposerConfigParser:
   JSON-parser for dbComposer Config files

   Part of dbComposer project

   Copyright (c) 2021-2022 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{$mode objfpc}
{$h+}
unit dbComposerConfigParser;

interface

uses
  SysUtils, fpJSON, jsonreader,
  ECommonObjs,
  ExtSqliteTokens, ExtSqliteUtils;

Type

  TSqliteExprState = (exstOk,
                exstNeedRebuild,
                exstNeedRepaint,
                exstCreateTable,
                exstTableNotExists,
                exstTableNotEqual,
                exstMalformed);

  TSqliteExprStatus = set of TSqliteExprState;

  { TThreadExprStatus }

  TThreadExprStatus = class(TThreadSafeObject)
  private
    FStatus : TSqliteExprStatus;
    function GetStatus : TSqliteExprStatus;
    procedure SetStatus(AValue : TSqliteExprStatus);
  public
    procedure Exclude(FState : TSqliteExprState);
    procedure Include(FState : TSqliteExprState);
    function Check(FState : TSqliteExprState) : Boolean;
    property Status : TSqliteExprStatus read GetStatus write SetStatus;
  end;

  { TJSONSqliteExpr }

  TJSONSqliteExpr = Class(TJSONString)
  private
    FExpr : TSqliteExpr;
    FStatus : TThreadExprStatus;
    FParseError, FTableName : TThreadUtf8String;
    function GetIsCreateTable : Boolean;
    function GetIsMalformed : Boolean;
    function GetIsOk : Boolean;
    function GetNeedRebuild : Boolean;
    function GetNeedRepaint : Boolean;
    function GetParseError : String;
    function GetTableName : String;
    procedure SetIsCreateTable(AValue : Boolean);
    procedure SetIsMalformed(AValue : Boolean);
    procedure SetIsOk(AValue : Boolean);
    procedure SetNeedRebuild(AValue : Boolean);
    procedure SetNeedRepaint(AValue : Boolean);
    procedure SetParseError(AValue : String);
    procedure SetTableName(AValue : String);
  protected
    procedure SetAsString(const AValue: TJSONStringType); override;
  public
    Constructor Create(const AValue : TJSONStringType); reintroduce;
    destructor Destroy; override;

    procedure RebuildExpr;

    procedure Lock;
    procedure UnLock;

    property Expr : TSqliteExpr read FExpr;
    procedure ExcludeState(FState : TSqliteExprState);
    procedure IncludeState(FState : TSqliteExprState);
    function CheckState(FState : TSqliteExprState) : Boolean;

    property NeedRepaint : Boolean read GetNeedRepaint write SetNeedRepaint;
    property NeedRebuild : Boolean read GetNeedRebuild write SetNeedRebuild;
    property IsCreateTable : Boolean read GetIsCreateTable write SetIsCreateTable;
    property IsOk : Boolean read GetIsOk write SetIsOk;
    property IsMalformed : Boolean read GetIsMalformed write SetIsMalformed;
    property ParseError : String read GetParseError write SetParseError;
    property TableName : String read GetTableName write SetTableName;
  end;

  { TDBJSONConfig }

  TDBJSONConfig = class(TJSONObject)
  private
    FLocker : TThreadSafeObject;
    FStruct : TJSONArray;
    function GetStruct(Index : Integer) : TJSONSqliteExpr;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure AddStructIfNeeded;
    function AddNewObj(const Sec : String) : TJSONObject;
    function AddExpr(const aExpr : String) : TJSONSqliteExpr; overload;
    function AddExpr(aExpr : TSqliteExpr) : TJSONSqliteExpr; overload;
    function Section(const S : String) : TJSONArray;

    function GetUniqID(const S : String) : integer;

    procedure Lock;
    procedure UnLock;

    property StructExpr[Index : Integer] : TJSONSqliteExpr read GetStruct;
    property Struct : TJSONArray read FStruct;
  end;

  { TDBJSONParser }

  TDBJSONParser = Class(TBaseJSONReader)
  private
    FStack : Array of TJSONData;
    FStackPos : integer;
    FStruct : TJSONData;
    FStructArray : TJSONArray;
    FValue : TJSONData;
    FKey: TJSONStringType;
    procedure Pop(aType: TJSONType);
    Procedure Push(AValue : TJSONData);
    Function NewValue(AValue : TJSONData) : TJSONData;
  Protected
    Procedure KeyValue(Const AKey : TJSONStringType); override;
    Procedure StringValue(Const AValue : TJSONStringType);override;
    Procedure NullValue; override;
    Procedure FloatValue(Const AValue : Double); override;
    Procedure BooleanValue(Const AValue : Boolean); override;
    Procedure NumberValue(Const AValue : TJSONStringType); override;
    Procedure IntegerValue(Const AValue : integer); override;
    Procedure Int64Value(Const AValue : int64); override;
    Procedure QWordValue(Const AValue : QWord); override;
    Procedure StartArray; override;
    Procedure StartObject; override;
    Procedure EndArray; override;
    Procedure EndObject; override;
  Public
    function Parse: TDBJSONConfig;
  end;

  EJSONParser = jsonReader.EJSONParser;

const
  JSON_CFG_INDXS = 'indxs';
  JSON_CFG_FORINDXS = 'foreignidxs';
  JSON_CFG_EXTBLOBS = 'extblobs';
  JSON_CFG_STRUCTURE = 'structure';
  JSON_CFG_DATABASE  = 'database';

  JSON_CFG_KIND = 'kind';
  JSON_CFG_PATH = 'path';
  JSON_CFG_TABLE = 'table';
  JSON_CFG_FIELD = 'field';
  JSON_CFG_KFIELD = 'kfield';
  JSON_CFG_VFIELD = 'vfield';
  JSON_CFG_SEFIELD = 'sefield';
  JSON_CFG_FUNCS = 'funcs';
  JSON_CFG_UID = 'uid';

implementation

Resourcestring
  SErrStructure = 'Structural error';

{ TDBJSONConfig }

function TDBJSONConfig.GetStruct(Index : Integer) : TJSONSqliteExpr;
begin
  Result := TJSONSqliteExpr(FStruct[Index]);
end;

constructor TDBJSONConfig.Create;
begin
  inherited Create;
  FLocker := TThreadSafeObject.Create;
end;

destructor TDBJSONConfig.Destroy;
begin
  FLocker.Free;
  inherited Destroy;
end;

procedure TDBJSONConfig.AddStructIfNeeded;
begin
  Lock;
  try
    if not Assigned(Struct) then
    begin
      FStruct := TJSONArray.Create;
      Self.Add(JSON_CFG_STRUCTURE, FStruct);
    end;
  finally
    UnLock;
  end;
end;

function TDBJSONConfig.AddNewObj(const Sec : String) : TJSONObject;
var jArr : TJSONArray;
begin
  Lock;
  try
    jArr := Section(Sec);
    Result := TJSONObject.Create();
    jArr.Add(Result);
  finally
    UnLock;
  end;
end;

function TDBJSONConfig.AddExpr(const aExpr : String) : TJSONSqliteExpr;
begin
  Lock;
  try
    AddStructIfNeeded;
    Result := TJSONSqliteExpr.Create(aExpr);
    FStruct.Add(Result);
  finally
    UnLock;
  end;
end;

function TDBJSONConfig.AddExpr(aExpr : TSqliteExpr) : TJSONSqliteExpr;
begin
  Lock;
  try
    AddStructIfNeeded;
    Result := TJSONSqliteExpr.Create(aExpr.FormatedStr(skfoOriginal));
    FStruct.Add(Result);
  finally
    UnLock;
  end;
end;

function TDBJSONConfig.Section(const S : String) : TJSONArray;
begin
  Lock;
  try
    if not Find(S, Result) then
    begin
      Result := TJSONArray.Create();
      Add(S, Result);
      if SameText(S, JSON_CFG_STRUCTURE) then
        FStruct := Result;
    end;
  finally
    UnLock;
  end;
end;

function TDBJSONConfig.GetUniqID(const S : String) : integer;
var jArr : TJSONArray;
    i, k : integer;
    uid : TJSONNumber;
begin
  Result := 1;
  jArr := Section(S);
  while true do
  begin
    k := -1;
    for i := 0 to jArr.Count-1 do
    begin
      if jArr[i] Is TJSONObject then
      begin
        if Find(JSON_CFG_UID, uid) then
        begin
          if Result = uid.AsInteger then
          begin
            k := i;
            break;
          end;
        end;
      end;
    end;
    if k < 0 then Exit;
    Inc(Result);
  end;
end;

procedure TDBJSONConfig.Lock;
begin
  FLocker.Lock;
end;

procedure TDBJSONConfig.UnLock;
begin
  FLocker.UnLock;
end;

{ TThreadExprStatus }

function TThreadExprStatus.GetStatus : TSqliteExprStatus;
begin
  Lock;
  try
    Result := FStatus;
  finally
    UnLock;
  end;
end;

procedure TThreadExprStatus.SetStatus(AValue : TSqliteExprStatus);
begin
  Lock;
  try
    FStatus := AValue;
  finally
    UnLock;
  end;
end;

procedure TThreadExprStatus.Exclude(FState : TSqliteExprState);
begin
  Lock;
  try
    FStatus := FStatus - [FState];
  finally
    UnLock;
  end;
end;

procedure TThreadExprStatus.Include(FState : TSqliteExprState);
begin
  Lock;
  try
    FStatus := FStatus + [FState];
  finally
    UnLock;
  end;
end;

function TThreadExprStatus.Check(FState : TSqliteExprState) : Boolean;
begin
  Lock;
  try
    Result := FState in FStatus;
  finally
    UnLock;
  end;
end;

{ TJSONSqliteExpr }

function TJSONSqliteExpr.GetNeedRebuild : Boolean;
begin
  Result := (exstNeedRebuild in FStatus.Status);
end;

function TJSONSqliteExpr.GetNeedRepaint : Boolean;
begin
  Result := (exstNeedRepaint in FStatus.Status);
end;

function TJSONSqliteExpr.GetParseError : String;
begin
  Result := FParseError.Value;
end;

function TJSONSqliteExpr.GetTableName : String;
begin
  Result := FTableName.Value;
end;

function TJSONSqliteExpr.GetIsCreateTable : Boolean;
begin
  Result := (exstCreateTable in FStatus.Status);
end;

function TJSONSqliteExpr.GetIsMalformed : Boolean;
begin
  Result := (exstMalformed in FStatus.Status);
end;

function TJSONSqliteExpr.GetIsOk : Boolean;
begin
  Result := (exstOk in FStatus.Status);
end;

procedure TJSONSqliteExpr.SetIsCreateTable(AValue : Boolean);
begin
  if AValue then
    FStatus.Include(exstCreateTable)
  else
    FStatus.Exclude(exstCreateTable);
  FStatus.Include(exstNeedRepaint);
end;

procedure TJSONSqliteExpr.SetIsMalformed(AValue : Boolean);
begin
  if AValue then begin
    FStatus.Include(exstMalformed);
    FStatus.Exclude(exstOk)
  end
  else
  begin
    FStatus.Exclude(exstMalformed);
    FStatus.Include(exstOk);
  end;
  FStatus.Include(exstNeedRepaint);
end;

procedure TJSONSqliteExpr.SetIsOk(AValue : Boolean);
begin
  if AValue then begin
    FStatus.Include(exstOk);
    FStatus.Exclude(exstMalformed);
  end
  else begin
    FStatus.Exclude(exstOk);
    FStatus.Include(exstMalformed);
  end;
  FStatus.Include(exstNeedRepaint);
end;

procedure TJSONSqliteExpr.SetNeedRebuild(AValue : Boolean);
begin
  if AValue then
    FStatus.Include(exstNeedRebuild)
  else
    FStatus.Exclude(exstNeedRebuild);
  FStatus.Include(exstNeedRepaint);
end;

procedure TJSONSqliteExpr.SetNeedRepaint(AValue : Boolean);
begin
  if AValue then
    FStatus.Include(exstNeedRepaint)
  else
    FStatus.Exclude(exstNeedRepaint);
end;

procedure TJSONSqliteExpr.SetParseError(AValue : String);
begin
  FParseError.Value := AValue;
end;

procedure TJSONSqliteExpr.SetTableName(AValue : String);
begin
  FTableName.Value := AValue;
end;

procedure TJSONSqliteExpr.SetAsString(const AValue : TJSONStringType);
begin
  inherited SetAsString(AValue);
  NeedRebuild := True;
end;

constructor TJSONSqliteExpr.Create(const AValue : TJSONStringType);
begin
  inherited Create(AValue);
  FExpr := nil;
  FStatus := TThreadExprStatus.Create;
  FStatus.Status := [exstNeedRebuild];
  FParseError := TThreadUtf8String.Create('');
  FTableName := TThreadUtf8String.Create('');
end;

destructor TJSONSqliteExpr.Destroy;
begin
  if Assigned(FExpr) then FExpr.Free;
  FStatus.Free;
  FParseError.Free;
  FTableName.Free;
  inherited Destroy;
end;

procedure TJSONSqliteExpr.RebuildExpr;
begin
  Lock;
  try
    if Assigned(FExpr) then FExpr.Free;
    FExpr := TSqliteExpr.Create(AsString);
  finally
    UnLock;
  end;
end;

procedure TJSONSqliteExpr.Lock;
begin
  FStatus.Lock;
end;

procedure TJSONSqliteExpr.UnLock;
begin
  FStatus.UnLock;
end;

procedure TJSONSqliteExpr.ExcludeState(FState : TSqliteExprState);
begin
  FStatus.Exclude(FState);
  if FState <> exstNeedRepaint then
    FStatus.Include(exstNeedRepaint);
end;

procedure TJSONSqliteExpr.IncludeState(FState : TSqliteExprState);
begin
  FStatus.Include(FState);
  FStatus.Include(exstNeedRepaint);
end;

function TJSONSqliteExpr.CheckState(FState : TSqliteExprState) : Boolean;
begin
  Result := FStatus.Check(FState);
end;

{ TDBJSONParser }

procedure TDBJSONParser.Pop(aType: TJSONType);
begin
  if (FStackPos=0) then
    DoError(SErrStructure);
  If (FStruct.JSONType<>aType) then
    DoError(SErrStructure);
  Dec(FStackPos);
  FStruct:=FStack[FStackPos];
end;

procedure TDBJSONParser.Push(AValue: TJSONData);
begin
  if (FStackPos=Length(FStack)) then
    SetLength(FStack,FStackPos+10);
  FStack[FStackPos]:=FStruct;
  Inc(FStackPos);
  FStruct:=AValue;
end;

function TDBJSONParser.NewValue(AValue: TJSONData): TJSONData;
begin
  Result:=AValue;
  // Add to existing structural type
  if (FStruct is TJSONObject) then
  begin
   try
     if SameText(FKey, JSON_CFG_STRUCTURE) then
       if (AValue is TJSONArray) and
          (FStruct is TDBJSONConfig) then
     begin
       TDBJSONConfig(FStruct).FStruct := TJSONArray(AValue);
       FStructArray := TJSONArray(AValue);
     end;
     TJSONObject(FStruct).Add(FKey,AValue);
   except
     AValue.Free;
     Raise;
   end;
   FKey:='';
  end
  else if (FStruct is TJSONArray) then
    TJSONArray(FStruct).Add(AValue);
  // The first actual value is our result
  if (FValue=Nil) then
    FValue:=AValue;
end;

procedure TDBJSONParser.KeyValue(const AKey: TJSONStringType);
begin
  if (FStruct is TJSONObject) and (FKey='') then
    FKey:=Akey
  else
    DoError('Duplicatekey or no object');
end;

procedure TDBJSONParser.StringValue(const AValue: TJSONStringType);
begin
  if (FStructArray = FStruct) and Assigned(FStruct) then
    NewValue(TJSONSqliteExpr.Create(AValue)) else
    NewValue(CreateJSON(AValue));
end;

procedure TDBJSONParser.NullValue;
begin
  NewValue(CreateJSON);
end;

procedure TDBJSONParser.FloatValue(const AValue: Double);
begin
  NewValue(CreateJSON(AValue));
end;

procedure TDBJSONParser.BooleanValue(const AValue: Boolean);
begin
  NewValue(CreateJSON(AValue));
end;

procedure TDBJSONParser.NumberValue(const AValue: TJSONStringType);
begin
  // Do nothing
  if AValue='' then ;
end;

procedure TDBJSONParser.IntegerValue(const AValue: integer);
begin
  NewValue(CreateJSON(AValue));
end;

procedure TDBJSONParser.Int64Value(const AValue: int64);
begin
  NewValue(CreateJSON(AValue));
end;

procedure TDBJSONParser.QWordValue(const AValue: QWord);
begin
  NewValue(CreateJSON(AValue));
end;

procedure TDBJSONParser.StartArray;
begin
  Push(NewValue(CreateJSONArray([])))
end;

procedure TDBJSONParser.StartObject;
begin
  if FStackPos = 0 then
  begin
    Push(NewValue(TDBJSONConfig.Create()));
  end else
    Push(NewValue(CreateJSONObject([])));
end;

procedure TDBJSONParser.EndArray;
begin
  Pop(jtArray);
end;

procedure TDBJSONParser.EndObject;
begin
  Pop(jtObject);
end;

function TDBJSONParser.Parse: TDBJSONConfig;
begin
  SetLength(FStack,0);
  FStackPos:=0;
  FValue:=Nil;
  FStruct:=Nil;
  FStructArray := nil;
  try
    DoExecute;
    Result:=TDBJSONConfig(FValue);
  except
    On E : exception do
      begin
      FreeAndNil(FValue);
      FStackPos:=0;
      SetLength(FStack,0);
      Raise;
      end;
  end;
end;

end.

