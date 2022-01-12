unit dbtestingmodule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtSqlite3DS,
  OGLFastList, ECommonObjs,
  Variants,
  fpjson, OGLFastNumList;

type
  TDBTestClient = class;
  TDBSimpleSession = class;

  { TDBClientsPool }

  TDBClientsPool = class(specialize TThreadSafeFastBaseSeq<TDBTestClient>)
  private
    function DetouchAllCriteria({%H-}aClient : TObject;
                                             {%H-}data : Pointer) : Boolean;
    procedure AfterClientDetouched(aClient : TObject);
  private
    function IsSessionName(obj : TObject; data : pointer) : Boolean;
    function IsSessionID(obj : TObject; data : pointer) : Boolean;
    function IsUserID(obj : TObject; data : pointer) : Boolean;
  public
    destructor Destroy; override;

    function GetBySessionID(aSessionID : Cardinal) : TDBTestClient;
    function GetByUserID(aUserID : Cardinal) : TDBTestClient;
    function GetBySessionName(const aSessionName : String) : TDBTestClient;

    procedure RemoveBySessionID(aSessionID : Cardinal);
    procedure RemoveByUserID(aUserID : Cardinal);
    procedure UnRegisterClient(aClient : TDBTestClient);
    procedure UnRegisterAll;

    procedure RegisterClient(aClient : TDBTestClient);
  end;

  { TDBConnection }

  TDBConnection = class
  private
    FClients : TDBClientsPool;
    FDB : TExtSqlite3Dataset;
    PREP_GetSessionId : TSqlite3Prepared;
  private
    function RegisterClient(aClient : TDBTestClient; const aSessionName : String
      ) : TDBSimpleSession;
    procedure UnRegisterClient(aClient : TDBTestClient);
  public
    constructor Create(aDB : TExtSqlite3Dataset);
    destructor Destroy; override;

    property ClientsPool : TDBClientsPool read FClients;
  end;

  { TDBSimpleSession }

  TDBSimpleSession = class
  private
    FSessionID : Cardinal;
    FSessionName : String;
    FUserID : Cardinal;
  public
    constructor Create(aSessionID : Cardinal; const aName : String);
    destructor Destroy; override;

    procedure Authorize(aUserID : Cardinal);
    function IsAuthorized : Boolean;

    property ID : Cardinal read FSessionID;
    property UserID : Cardinal read FUserID;
    property Name : String read FSessionName;
  end;

  { TDBChange }

  TDBChange = class
  private
    FID : Cardinal;
    FData : TObject;
  public
    constructor Create(aID : Cardinal; aData : TObject);
    destructor Destroy; override;
    property ID : Cardinal read FID;
    property Data : TObject read FData;
  end;

  { TDBChangesSeq }

  TDBChangesSeq = class(TThreadSafeFastSeq)
  public
    procedure PushChange(aID : Cardinal; aData : TObject = nil);
    function PopChange : TDBChange;
  end;

  TDBFilterTree = class;

  TDBFilterBranchType = (filbRoot,
                         filbCatName, filbSubCatName,
                         filbItemOptionName,
                         filbCountryName,
                         filbProducerName,
                         filbAllValues, filbMoreValues,
                         filbIntegerField, filbFloatField,
                         filbOptionValue);

  { TDBFilterBranch }

  TDBFilterBranch = class(TFastList)
  private
    FKind : TDBFilterBranchType;
    FUID  : TKey;
    FDBID : Cardinal;
    FData : TJSONObject;
    FEnabled : Boolean;
    FTree : TDBFilterTree;
    FOnChanged : TNotifyEvent;
    function GetBranch(Index : integer) : TDBFilterBranch;
    function GetDataField(const aField : String) : Variant;
    procedure SetDataField(const aField : String; aValue : Variant);
    procedure SetEnabled(AValue : Boolean);
    procedure UpdateData(const aData : String);
  public
    constructor Create(aKind : TDBFilterBranchType;
                             aTree : TDBFilterTree;
                             const aData : String;
                             aUID : TKey; aDBID : Cardinal); overload;
    destructor Destroy; override;
    procedure SaveToJson(aRoot : TJSONArray);
    function DataToString : String;
    property OnChanged : TNotifyEvent read FOnChanged write FOnChanged;
    property Data : TJSONObject read FData;
    property Kind : TDBFilterBranchType read FKind;
    property UID : TKey read FUID;
    property DBID : Cardinal read FDBID;
    property Enabled : Boolean read FEnabled write SetEnabled;
    property Branch[Index : integer] : TDBFilterBranch read GetBranch;
    property DataField[const aField : String] : Variant read GetDataField
                                                        write SetDataField;
  end;

  TDBFilterTableEnum = (fteCategs,
                        fteCountries,
                        fteProducers,
                        fteOptions,
                        // parametrized tables
                        fteOptionsVars);

  TDBFilterTableIndexRec = packed record
    aEnum : Cardinal;
    aID   : Cardinal;
  end;

  TDBFilterTableIndex = TKey;

  { TDBFilterTable }

  TDBFilterTable = class(TFastMapObj)
  private
    FUIndex : TFastMapUInt;
    FParam : Cardinal;
    FKind  : TDBFilterTableEnum;
  public
    constructor Create(aKind : TDBFilterTableEnum; aParam : Cardinal); overload;
    destructor Destroy; override;
    property Param : Cardinal read FParam;
    property Kind :  TDBFilterTableEnum read FKind;
    function BranchByUID(const aUID : TKey) : TDBFilterBranch;
    function BranchByDBID(aDBID : Cardinal) : TDBFilterBranch;
    procedure AddBranch(br : TDBFilterBranch);
    procedure RemoveBranch(br : TDBFilterBranch);
  end;

  { TDBFilterTables }

  TDBFilterTables = class(TFastMapObj)
  private
    function GetTable(Index : TDBFilterTableIndex) : TDBFilterTable;
  public
    constructor Create; overload;
    function BranchByUID(const aUID: TKey): TDBFilterBranch;
    function BranchByDBID(aKind : TDBFilterTableEnum;
                             aDBID : Cardinal) : TDBFilterBranch;
    property Table[Index : TDBFilterTableIndex] : TDBFilterTable read GetTable; default;
  end;

  { TDBFilterTree }

  TDBFilterTree = class
  private
    FUIDGenerator : TThreadSafeAutoIncrementCardinal;
    FRoot : TDBFilterBranch;
    FTables : TDBFilterTables;
    FClient : TDBTestClient;
    function GetNextUID : Cardinal;
    procedure OnBranchDeleted(B : TDBFilterBranch);
    procedure OnBranchChanged(aCID : Cardinal; B : TDBFilterBranch);
  public
    constructor Create(aClient : TDBTestClient);
    destructor Destroy; override;
    property NextUID : Cardinal read GetNextUID;
    property Root : TDBFilterBranch read FRoot;
  end;

  { TDBTestClient }

  TDBTestClient = class
  private
    FConnect : TDBConnection;
    FSession : TDBSimpleSession;
    FChanges : TDBChangesSeq;
    FRegistered : TThreadBoolean;
    procedure DoDetouch;
    function GetRegistered : Boolean;
  public
    constructor Create(aConnect : TDBConnection; const aSessionName : String);
    destructor Destroy; override;

    function StartNewSession : TDBSimpleSession;
    function Authorize(const aUser, aPsw : String) : Boolean;
    procedure ClearCart;

    property Session : TDBSimpleSession read FSession;
    property Registered : Boolean read GetRegistered;

    procedure PushChange(aID : Cardinal; const aData : array of Variant);
  end;

const FB_DELETED = Cardinal($1001);
      FB_ENABLED = Cardinal($1002);
      FB_DISABLED = Cardinal($1003);
      FB_DATA     = Cardinal($1004);

      PARAM_BRANCH_UID = QWORD(1);

      pstr_Kind        = 'kind';
      pstr_UID         = 'uid';
      pstr_DBID        = 'dbid';
      pstr_Enabled     = 'enabled';
      pstr_Data        = 'data';
      pstr_Branches    = 'branches';

      cBranchTypeStr : Array [TDBFilterBranchType] of ansistring =
        ('Root',
         'CatName', 'SubCatName',
         'ItemOptionName',
         'CountryName',
         'ProducerName',
         'AllValues', 'MoreValues',
         'IntegerField', 'FloatField',
         'OptionValue');

implementation

uses LazUtf8;

{ TDBFilterTable }

constructor TDBFilterTable.Create(aKind: TDBFilterTableEnum; aParam: Cardinal);
begin
  inherited Create(true);
  FKind := aKind;
  FParam := aParam;
  FUIndex := TFastMapUInt.Create;
  Sort;
  FUIndex.Sort;
end;

destructor TDBFilterTable.Destroy;
begin
  FUIndex.Free;
  inherited Destroy;
end;

function TDBFilterTable.BranchByUID(const aUID: QWORD): TDBFilterBranch;
begin
  Result := TDBFilterBranch(Value[aUID]);
end;

function TDBFilterTable.BranchByDBID(aDBID: Cardinal): TDBFilterBranch;
var auid : QWord;
begin
  auid := FUIndex.Value[aDBID];
  if auid > 0 then begin
    Result := TDBFilterBranch(Value[auid]);
  end else Result := nil;
end;

procedure TDBFilterTable.AddBranch(br: TDBFilterBranch);
begin
  AddObjSorted(br.UID, br);
  FUIndex.AddUintSorted(br.DBID, br.UID);
end;

procedure TDBFilterTable.RemoveBranch(br: TDBFilterBranch);
var index : Integer;
    KV, DV : TKey;
begin
  KV := br.UID;
  DV := br.DBID;
  index := IndexOfKey(KV);
  if index >= 0 then
  begin
    Delete(Index);
    index := FUIndex.IndexOfKey(DV);
    if index >= 0 then
      FUIndex.Delete(index);
  end;
end;

{ TDBFilterTables }

function TDBFilterTables.GetTable(Index: TDBFilterTableIndex): TDBFilterTable;
begin
  Result := TDBFilterTable(Value[ Index ]);
end;

constructor TDBFilterTables.Create;
begin
  inherited Create(true);
end;

function TDBFilterTables.BranchByUID(const aUID: TKey): TDBFilterBranch;
var i : integer;
begin
  for i := 0 to Count-1 do
  begin
    Result := TDBFilterTable(Items[i].Value).BranchByUID(aUID);
    if assigned(Result) then Exit;
  end;
  Result := nil;
end;

function TDBFilterTables.BranchByDBID(aKind: TDBFilterTableEnum; aDBID: Cardinal
  ): TDBFilterBranch;
var i : integer;
begin
  for i := 0 to Count-1 do
  if TDBFilterTable(Items[i].Value).Kind = aKind then
  begin
    Result := TDBFilterTable(Items[i].Value).BranchByDBID(aDBID);
    if assigned(Result) then Exit;
  end;
  Result := nil;
end;

{ TDBSimpleSession }

constructor TDBSimpleSession.Create(aSessionID : Cardinal; const aName : String
  );
begin
  FSessionID := aSessionID;
  FSessionName := aName;
  FUserID := 0;
  // check is session connected to userid
end;

destructor TDBSimpleSession.Destroy;
begin
  // delete/invalidate session from/in database
  inherited Destroy;
end;

procedure TDBSimpleSession.Authorize(aUserID : Cardinal);
begin
  // connect sessionid to userid
  FUserID := aUserID;
end;

function TDBSimpleSession.IsAuthorized : Boolean;
begin
  Result := FUserID > 0;
end;

{ TDBClientsPool }

function TDBClientsPool.DetouchAllCriteria({%H-}aClient : TObject;
                         {%H-}data : Pointer) : Boolean;
begin
  Result := True;
end;

procedure TDBClientsPool.AfterClientDetouched(aClient : TObject);
begin
  TDBTestClient(aClient).DoDetouch;
end;

function TDBClientsPool.IsSessionName(obj : TObject; data : pointer) : Boolean;
begin
  if Assigned(TDBTestClient(obj).Session) then
    Result := TDBTestClient(obj).Session.Name = PString(data)^ else
    Result := false;
end;

function TDBClientsPool.IsSessionID(obj : TObject; data : pointer) : Boolean;
begin
  if Assigned(TDBTestClient(obj).Session) then
    Result := TDBTestClient(obj).Session.ID = PCardinal(data)^ else
    Result := false;
end;

function TDBClientsPool.IsUserID(obj : TObject; data : pointer) : Boolean;
begin
  if Assigned(TDBTestClient(obj).Session) then
    Result := TDBTestClient(obj).Session.UserID = PCardinal(data)^ else
    Result := false;
end;

destructor TDBClientsPool.Destroy;
begin
  UnRegisterAll;
  inherited Destroy;
end;

function TDBClientsPool.GetBySessionID(aSessionID : Cardinal) : TDBTestClient;
begin
  Result := FindValue(@IsSessionID, @aSessionID);
end;

function TDBClientsPool.GetByUserID(aUserID : Cardinal) : TDBTestClient;
begin
  Result := FindValue(@IsUserID, @aUserID);
end;

function TDBClientsPool.GetBySessionName(const aSessionName : String
  ) : TDBTestClient;
var pstr : PString;
begin
  pstr := NewStr(aSessionName);
  try
    Result := FindValue(@IsSessionName, pstr);
  finally
    DisposeStr(pstr);
  end;
end;

procedure TDBClientsPool.RemoveBySessionID(aSessionID : Cardinal);
begin
  ExtractObjectsByCriteria(@IsSessionID, @AfterClientDetouched, @aSessionID);
end;

procedure TDBClientsPool.RemoveByUserID(aUserID : Cardinal);
begin
  ExtractObjectsByCriteria(@IsUserID, @AfterClientDetouched, @aUserID);
end;

procedure TDBClientsPool.UnRegisterClient(aClient : TDBTestClient);
begin
  ExtractObject(aClient);
  AfterClientDetouched(aClient);
end;

procedure TDBClientsPool.UnRegisterAll;
begin
  ExtractObjectsByCriteria(@DetouchAllCriteria, @AfterClientDetouched, nil);
end;

procedure TDBClientsPool.RegisterClient(aClient : TDBTestClient);
begin
  Push_back(aClient)
end;

{ TDBConnection }

function TDBConnection.RegisterClient(aClient : TDBTestClient;
                         const aSessionName : String) : TDBSimpleSession;
var aSessionID : Int32;
begin
  FClients.RegisterClient(aClient);
  // send request, get last id
  if PREP_GetSessionId.ExecToValue([aSessionName], aSessionID) then
    Result := TDBSimpleSession.Create(aSessionId, aSessionName) else
    Result := nil;
end;

procedure TDBConnection.UnRegisterClient(aClient : TDBTestClient);
begin
  //send request, remove session by id
  //aClient.Session.Id
  FClients.UnRegisterClient(aClient);
end;

constructor TDBConnection.Create(aDB : TExtSqlite3Dataset);
begin
  FClients := TDBClientsPool.Create;
  FDB := aDB;
end;

destructor TDBConnection.Destroy;
begin
  FClients.Free;
  inherited Destroy;
end;

{ TDBChange }

constructor TDBChange.Create(aID : Cardinal; aData : TObject);
begin
  FID := aID;
  FData := aData;
end;

destructor TDBChange.Destroy;
begin
  if assigned(FData) then FData.Free;
  inherited Destroy;
end;

{ TDBChangesSeq }

procedure TDBChangesSeq.PushChange(aID : Cardinal; aData : TObject);
begin
  Push_back(TDBChange.Create(aID, aData));
end;

function TDBChangesSeq.PopChange : TDBChange;
begin
  Result := TDBChange(PopValue);
end;

{ TDBFilterTree }

procedure TDBFilterTree.OnBranchDeleted(B : TDBFilterBranch);
begin
  FClient.PushChange(FB_DELETED, [PARAM_BRANCH_UID, B.UID]);
end;

function TDBFilterTree.GetNextUID : Cardinal;
begin
  Result := FUIDGenerator.ID;
end;

procedure TDBFilterTree.OnBranchChanged(aCID : Cardinal; B : TDBFilterBranch);
begin
  FClient.PushChange(aCID, [PARAM_BRANCH_UID, B.UID]);
end;

constructor TDBFilterTree.Create(aClient : TDBTestClient);
begin
  FUIDGenerator := TThreadSafeAutoIncrementCardinal.Create;
  FTables := TDBFilterTables.Create;
  FClient := aClient;
  FRoot := TDBFilterBranch.Create(filbRoot, Self, '', NextUID, 0);
end;

destructor TDBFilterTree.Destroy;
begin
  FTables.Free;
  FUIDGenerator.Free;
  FRoot.Free;
  inherited Destroy;
end;

{ TDBTestClient }

procedure TDBTestClient.DoDetouch;
begin
  FRegistered.Value := false
end;

function TDBTestClient.GetRegistered : Boolean;
begin
  Result := FRegistered.Value;
end;

constructor TDBTestClient.Create(aConnect : TDBConnection;
  const aSessionName : String);
begin
  FChanges := TDBChangesSeq.Create;
  FConnect := aConnect;
  FSession := nil;
  FRegistered := TThreadBoolean.Create(True);

  FSession :=  FConnect.RegisterClient(Self, aSessionName);
end;

destructor TDBTestClient.Destroy;
begin
  if FRegistered.Value then
    FConnect.UnRegisterClient(Self);

  if assigned(FSession) then FreeAndNil(FSession);
  FChanges.Free;
  FRegistered.Free;
  inherited Destroy;
end;

function TDBTestClient.StartNewSession : TDBSimpleSession;
var aSessionName : String;
begin
  if Assigned(FSession) then begin
    aSessionName := FSession.Name;
    FSession.Free;
  end else
    aSessionName := '';
  FConnect.UnRegisterClient(Self);
  // get from database next session id
  FSession := FConnect.RegisterClient(Self, aSessionName);
  Result := FSession;
end;

function TDBTestClient.Authorize(const aUser, aPsw : String) : Boolean;
begin

end;

procedure TDBTestClient.ClearCart;
begin

end;

procedure TDBTestClient.PushChange(aID : Cardinal;
  const aData : array of Variant);
var i : integer;
    Data : TFastMapVar;
begin
  if Length(aData) > 0 then
  begin
    Data := TFastMapVar.Create;
    Data.Sort;
    i := 0;
    while i < High(aData) do
    begin
      Data.AddKeySorted(aData[i], aData[i+1]);
      inc(i, 2);
    end;
  end else Data := nil;
  FChanges.PushChange(aID, Data);
end;

{ TDBFilterBranch }

function TDBFilterBranch.GetBranch(Index : integer) : TDBFilterBranch;
begin
  Result := TDBFilterBranch(Item[Index]);
end;

function TDBFilterBranch.GetDataField(const aField : String) : Variant;
var
  f : TJSONData;
begin
  if Assigned(FData) then
  begin
    f := FData.Find(aField);
    if assigned(f) then
    begin
      if f is TJSONIntegerNumber then
        Result := f.AsInteger else
      if f is TJSONInt64Number then
        Result := f.AsInt64 else
      if f is TJSONQWordNumber then
        Result := f.AsQWord else
      if f is TJSONFloatNumber then
        Result := f.AsFloat else
      if f is TJSONString then
        Result := f.AsString else
      if f is TJSONBoolean then
        Result := f.AsBoolean
      else
        Result := Null;
    end else
        Result := Null;
  end else
    Result := Null;
end;

procedure TDBFilterBranch.SetDataField(const aField : String; aValue : Variant);
var
  f : TJSONData;
begin
  if Assigned(FData) then
  begin
    f := FData.Find(aField);
    if assigned(f) then
    begin
      if f is TJSONIntegerNumber then begin
        if VarIsOrdinal(aValue) then
          f.AsInteger := aValue;
      end else
      if f is TJSONInt64Number then begin
        if VarIsOrdinal(aValue) then
          f.AsInt64 := aValue;
      end else
      if f is TJSONQWordNumber then begin
        if VarIsOrdinal(aValue) then
          f.AsQWord := aValue;
      end else
      if f is TJSONFloatNumber then begin
        if VarIsFloat(aValue) then
          f.AsFloat := aValue;
      end else
      if f is TJSONString then begin
        if VarIsStr(aValue) then
          f.AsString := aValue;
      end else
      if f is TJSONBoolean then begin
        if VarIsBool(aValue) then
          f.AsBoolean := aValue;
      end;
    end;
  end;
end;

procedure TDBFilterBranch.SetEnabled(AValue : Boolean);
begin
  if FEnabled = AValue then Exit;
  FEnabled := AValue;
  if aValue then
    FTree.OnBranchChanged(FB_ENABLED, Self) else
    FTree.OnBranchChanged(FB_DISABLED, Self);
end;

procedure TDBFilterBranch.UpdateData(const aData: String);

function CompareJSON(Data1, Data2 : TJSONData) : Boolean;
var i : integer;
    f1, f2 : TJSONData;
begin
  if not (Assigned(Data1) and Assigned(Data2)) then Exit(false);
  if Data1.Count <> Data2.Count then Exit(false);
  if (Data1 is TJSONObject) and (Data2 is TJSONObject) then
  begin
    for i := 0 to Data1.Count-1 do
    begin
      f1 := TJSONObject(Data1).Items[i];
      f2 := TJSONObject(Data2).Find(TJSONObject(Data1).Names[i]);
      if not CompareJSON(f1, f2) then Exit(false);
    end;
    Result := true;
  end else
  if (Data1 is TJSONArray) and (Data2 is TJSONArray) then
  begin
    for i := 0 to Data1.Count-1 do
      if not CompareJSON(Data1.Items[i], Data2.Items[i]) then Exit(false);
    Result := true;
  end else
  if ((Data1 is TJSONNumber) and (Data2 is TJSONNumber)) or
      (Data1.ClassType = Data2.ClassType) then
  begin
    Result := UTF8CompareStr(Data1.AsString, Data2.AsString) = 0;
  end else
  begin
    Result := false;
  end;
end;

var NData : TJSONData;
begin
  NData := nil;
  try
    if Length(aData) = 0 then
      NData := TJSONObject.Create else
    begin
      NData := GetJSON(aData);
      if not (NData is TJSONObject) then begin
        if assigned(NData) then NData.Free;
        NData := TJSONObject.Create;
      end;
    end;
  except
    if assigned(NData) then NData.Free;
    NData := TJSONObject.Create;
  end;

  if not CompareJSON(FData, NData) then
  begin
    if assigned(FData) then FreeAndNil(FData);
    FData := TJSONObject(NData);

    FData.CompressedJSON := true;
    FTree.OnBranchChanged(FB_DATA, Self);
  end else
    NData.Free;
end;

constructor TDBFilterBranch.Create(aKind : TDBFilterBranchType;
  aTree : TDBFilterTree; const aData : String; aUID : QWORD; aDBID : Cardinal);
begin
  inherited Create;
  FKind := aKind;
  FTree := aTree;

  FUID := aUID;
  FDBID := aDBID;
  FEnabled := true;

  UpdateData(aData);
end;

destructor TDBFilterBranch.Destroy;
begin
  FTree.OnBranchDeleted(Self);
  if assigned(FData) then FreeAndNil(FData);
  inherited Destroy;
end;

procedure TDBFilterBranch.SaveToJson(aRoot : TJSONArray);
var o : TJSONObject;
    i : integer;
    ar : TJSONArray;
begin
  o := TJSONObject.Create([pstr_Kind, cBranchTypeStr[FKind],
                           pstr_UID, FUID,
                           pstr_DBID, FDBID,
                           pstr_Enabled, FEnabled]);
  o.Add(pstr_Data, FData.Clone);
  ar := TJSONArray.Create;
  o.Add(pstr_Branches, ar);
  for i := 0 to Count-1 do
    SaveToJson(ar);

  aRoot.Add(o);
end;

function TDBFilterBranch.DataToString : String;
begin
  if Assigned(FData) then
    Result := FData.AsJSON else
    Result := '';
end;

end.

