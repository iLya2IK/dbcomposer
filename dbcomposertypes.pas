{
 dbcTypes:
   Common types, classes and components for working with hierarchical tables

   Part of dbComposer project

   Copyright (c) 2021-2022 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit dbComposerTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  OGLFastList, ECommonObjs, kcThreadPool,
  Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, SpinEx, EditBtn,
  dbctypes,
  LCLType, LCLIntf,
  DB, Buttons, ExtSqlite3DS, ExtSqliteUtils,
  dbComposerStruct, dbComposerUtils;

type

 { TLimitedText }

 TLimitedText = class(TSqlite3Function)
 public
   constructor Create; overload;
   procedure ScalarFunc(argc : integer); override;
 end;

 TDBControls = class;
 TDBControlStack = class;

 { TDBControl }

 TDBControl = class
 private
   //Thread objects
   FForeignKeyLoading, FDestroying : TThreadBoolean;
   FForeignKeyValue : TThreadInteger;
   FJobs : TThreadSafeFastSeq;
   FTmpObjects : TNestedExprsList;
   FObjects : TFastCollection;

   FField : TDBField;
   FValue : String;
   FSelectRequestForForeignKey : TSQLRequest;
   FExtBlob : TDBExtBlob;
   FExtBlobValue : TDBExtBlobValue;
   FExtBlobValueUpdated : TThreadBoolean;

   FIsForeignString : TDBForIndxStringTable;
   FForeignStringValue : string;

   FChanged : Boolean;
   FOwnField : Boolean;
   FOnChanged : TNotifyEvent;
   FOwner : TDBControls;
   FGroup : TGroupBox;
   FControls : TStringList;
   FId : Integer;
   FVisible : Boolean;
   FInitilizing : Boolean;

   procedure AddValuedControl(const aName : String; C : TControl);
   class function BuildRequestForForeignKey(ST : TDBTable; const SF : String;
     ExcludeFields : TStringList;
     Options : TSQLNestGenOptions) : TSQLRequest;
   procedure FillFKData(const S: String);
   function GenNestedSQLRequest(Req : TSQLRequest; aTable : TDBTable;
     const Key : String) : String;
   function GetDataSet : TExtSqlite3Dataset;
   function GetFieldDefault : String;
   function GetFieldName : String;
   function GetFieldType : TFieldType;
   function GetForeignKeyTable : TDBTable;
   function GetForeignKeyTo : String;
   function GetIsForeignKey : Boolean;
   function GetIsPrimaryKey : Boolean;
   function GetQuotedFieldName : String;
   function GetQuotedValue : String;
   function GetSelectRequestForForeignKey : TSQLRequest;
   function GetStack : TDBControlStack;
   procedure OnValueChanged(Sender : TObject);
   procedure OnExtBlobChanged(Sender : TObject);
   function  OnExtBlobCheckActual(aId : TDBExtBlobValue) : TDBExtActualResult;
   procedure OnChooseTable(Sender : TObject);
   procedure OnAddNewForeignId(Sender : TObject);
   procedure OnTimerTick(Sender : TObject; Data : Pointer);
   procedure DoOnChanged;
   procedure SetValue(AValue : String);
   procedure UpdateExtBlob;
   procedure UpdateFKObjects(AValue : Integer);
   function  FillListItems(Str : TStrings) : Integer;
 protected
   procedure DoAddControls; virtual;
   procedure DoBeginFillControls; virtual;
   procedure DoEndFillControls; virtual;
 public
   constructor Create(aOwner : TDBControls; aField : TDBField;
     aOwnField : Boolean; aVisible : Boolean); virtual;
   destructor Destroy; override;

   procedure AddControls;
   function GetControl(const CN : String) : TControl;
   procedure Reset;
   procedure SetCurAsDefaultAndReset;
   procedure Show;
   procedure Hide;

   property DataSet : TExtSqlite3Dataset read GetDataSet;

   property FieldName : String read GetFieldName;
   property QuotedFieldName : String read GetQuotedFieldName;
   property FieldType : TFieldType read GetFieldType;
   property FieldDefault : String read GetFieldDefault;
   property IsPrimaryKey : Boolean read GetIsPrimaryKey;

   property IsForeignKey : Boolean read GetIsForeignKey;
   property ForeignKeyTable : TDBTable read GetForeignKeyTable;
   property ForeignKeyTo : String read GetForeignKeyTo;
   procedure SetForeignKeyValue(AValue : Integer); virtual;

   property Stack : TDBControlStack read GetStack;

   property Value : String read FValue write SetValue;
   property ForeignStringValue : String read FForeignStringValue write
                                             FForeignStringValue;
   property QuotedValue : String read GetQuotedValue;

   property Visible : Boolean read FVisible;

   property Changed : Boolean read FChanged write FChanged;
   property OnChanged : TNotifyEvent read FOnChanged write FOnChanged;
 end;

 { TDBConnectControl }

 TDBConnectControl = class(TDBControl)
 private
   FList : TListBox;
   FTBar : TToolBar;
   FRefTable : TDBTable;
   FRefId : Integer;
   FSelectRequestForRefKey : TSQLRequest;
   function GetSelectRequestForRefKey(FESL : TStringList) : TSQLRequest;
   procedure OnAddNewConnection(Sender : TObject);
   procedure OnEditConnection(Sender : TObject);
   procedure OnDelConnection(Sender : TObject);
 protected
   procedure DoAddControls; override;
   procedure DoBeginFillControls; override;
   procedure DoEndFillControls; override;
 public
   constructor Create(aOwner : TDBControls; aId : Integer;
     aRefField : TDBField); overload;
   procedure SetForeignKeyValue(AValue : Integer); override;
   destructor Destroy; override;
 end;

 TDBControlMode = (mtmEdit, mtmAddNew);

 { TDBControls }

 TDBControls = class(TFastCollection)
 private
   FRecordChanged : Boolean;
   FRecordTable : String;
   FIdField : TDBField;
   FTable   : TDBTable;
   FWaitingKey : TDBControl;
   FStack : TDBControlStack;
   FMode : TDBControlMode;
   FEditPKId, FId : Integer;
   function GetDataSet : TExtSqlite3Dataset;
   function GetDBControl(index : integer) : TDBControl;
   function GetWaitingKeyName : String;
   procedure OnDBContolChanged(Sender : TObject);
   procedure SetRecordTable(AValue : String);
 public
   constructor Create(aStack : TDBControlStack;
                             const aTableName : String;
                             aMode : TDBControlMode); overload;
   procedure Clear; override;
   procedure Show;
   procedure Hide;
   procedure Reset;
   procedure SetCurAsDefaultAndReset;
   function Apply(var SelExpr, ErrExpr : String) : Boolean;
   function ByName(const S : String) : TDBControl;
   property DBControl[index : integer] : TDBControl read GetDBControl; default;
   property RecordTable : String read FRecordTable write SetRecordTable;
   property Table : TDBTable read FTable;
   property RecordChanged : Boolean read fRecordChanged;
   property Stack : TDBControlStack read FStack;
   property DataSet : TExtSqlite3Dataset read GetDataSet;
   property Id : Integer read FId;
   property Mode : TDBControlMode read FMode write FMode;
   property EditingKeyId : Integer read FEditPKId write FEditPKId;
   property WaitingKeyName : String read GetWaitingKeyName;
 end;

 TNotifyObjCallBack = procedure(Obj : TObject; Data : Pointer) of object;

 TDBListenerKind = (lkChanged, lkTimer);

 { TDBControlListener }

 TDBControlListener = class
 private
   FEar    : TObject;
   FKind   : TDBListenerKind;
   FNotify : TNotifyObjCallBack;
 public
   constructor Create(aKind : TDBListenerKind; aCallBack : TNotifyObjCallBack;
                             aEar : TObject);
   property  Kind : TDBListenerKind read FKind;
   property  Ear : TObject read FEar;
   procedure Notify(aData: Pointer);

   property OnNotify : TNotifyObjCallBack read FNotify write FNotify;
 end;

 { TDBControlStack }

 TDBControlStack = class(TFastCollection)
 private
   FOnUpdateState : TNotifyEvent;
   FOwner : TWinControl;
   FOwnerImages : TImageList;
   FStructure : TDBStructure;
   FListeners : TFastCollection;

   function GetTable(index : integer) : TDBControls;
   procedure DoUpdateState;
   procedure RemoveListener(aEar : TObject);
   procedure NotifyListeners(aKind: TDBListenerKind; aData: Pointer);
 public
   constructor Create(aOwner : TWinControl;
                             aOwnerImages : TImageList;
                             aStructure : TDBStructure); overload;
   destructor Destroy; override;
   function ProceedTable(const TN : String; IdValue : Integer;
               addNext : Boolean; doReset : Boolean) : boolean;
   property Table[index : integer] : TDBControls read GetTable; default;
   procedure Reset;
   procedure Back(addedId : integer);
   procedure Delete(Ind : integer); override;
   procedure AddNextTable(C : TDBControls);

   procedure AddTimerListener(aCallback : TNotifyObjCallBack; aEar : TObject);
   procedure DoTimerTick;

   function FirstTable : TDBControls;
   function CurTable : TDBControls;
   property Owner : TWinControl read FOwner;
   property OwnerImages : TImageList read FOwnerImages;
   property Structure : TDBStructure read FStructure;

   property OnUpdateState : TNotifyEvent read FOnUpdateState write FOnUpdateState;
 end;

 { TDBControlJob }

 TDBControlJob = class(TJob)
 private
   FControl : TDBControl;
 public
   constructor Create(AControl : TDBControl);
   procedure Execute; override;
 end;

 { TDBControlFillJob }

 TDBControlFillJob = class(TDBControlJob)
 public
   procedure Execute; override;
 end;

implementation


uses dbchooseid, LazUTF8, DateUtils;

const ChooseBitBtn   = 'ChooseId';
      ChooseComboBox = 'ChooseCBId';
      ForeignString  = 'ForeignStringEdit';
      ChangeFloat    = 'FloatEdit';
      ChangeInteger  = 'IntEdit';
      ChangeText     = 'TextEdit';
      TimeStampField = 'TimeStampField';
      ExtImageField  = 'ExtImageField';
      ChooseImageField = 'ChooseImageField';

{ TLimitedText }

constructor TLimitedText.Create;
begin
  inherited Create('limtext', 1, sqlteUtf8, sqlfScalar);
end;

procedure TLimitedText.ScalarFunc(argc: integer);
var S : String; L : Integer;
begin
  if argc = 1 then
  begin
    S := AsString(0);
    L := UTF8Length(S);
    if (L > 20) then
    begin
      S := UTF8Copy(S, 1, 17) + '...';
    end;
    S := UTF8StringReplace(S, '''', '''''', [rfReplaceAll]);
    SetResult(S);
  end else
    SetResultNil;
end;

{ TDBControlFillJob }

procedure TDBControlFillJob.Execute;
begin
  if not FControl.FDestroying.Value then
    FControl.DoBeginFillControls;
  if not FControl.FDestroying.Value then
    TThread.Synchronize(nil, @(FControl.DoEndFillControls));

  inherited Execute;
end;

{ TDBControlJob }

constructor TDBControlJob.Create(AControl: TDBControl);
begin
  FControl := AControl;
end;

procedure TDBControlJob.Execute;
begin
  FControl.FJobs.ExtractObject(Self);
end;

{ TDBControlListener }

constructor TDBControlListener.Create(aKind: TDBListenerKind;
  aCallBack: TNotifyObjCallBack; aEar: TObject);
begin
  FKind:= aKind;
  FNotify:= aCallBack;
  FEar := aEar;
end;

procedure TDBControlListener.Notify(aData : Pointer);
begin
  if assigned(FNotify) then
    FNotify(FEar, aData);
end;

{ TDBConnectControl }

function TDBConnectControl.GetSelectRequestForRefKey(FESL: TStringList
  ): TSQLRequest;
begin
  if ((not Assigned(FSelectRequestForRefKey)) or
                    FSelectRequestForRefKey.IsEmpty) then
  begin
    if Assigned(FSelectRequestForRefKey) then
      FSelectRequestForRefKey.Free;
    FSelectRequestForRefKey := BuildRequestForForeignKey(FRefTable,
                                                         '', FESL,
                                                         [sngoCollapseForeignString,
                                                          sngoExcludePrimaryKeys]);
    FSelectRequestForRefKey.AddNewCompValue(FRefTable.QuotedName + '.' + QuotedFieldName,
                                                           '=', inttostr(FRefId));
  end;
  Result := FSelectRequestForRefKey;
end;

procedure TDBConnectControl.DoAddControls;
var B : TToolButton;
begin
  FTBar := TToolBar.Create(FGroup);
  FTBar.Parent := FGroup;
  FTBar.Align := alTop;
  FTBar.Height := 23;
  FTBar.Images := Stack.OwnerImages;

  B := TToolButton.Create(FTBar);
  B.Parent := FTBar;
  B.ImageIndex := 2;
  B.OnClick := @OnEditConnection;
  B.BringToFront;

  B := TToolButton.Create(FTBar);
  B.Parent := FTBar;
  B.ImageIndex := 4;
  B.OnClick := @OnAddNewConnection;
  B.BringToFront;

  B := TToolButton.Create(FTBar);
  B.Parent := FTBar;
  B.ImageIndex := 11;
  B.OnClick := @OnDelConnection;
  B.BringToFront;

  FList := TListBox.Create(FGroup);
  FList.Top := 22;
  FList.Parent := FGroup;
  FList.Align := alClient;
  FList.Font.Style := [];
  FList.Style := lbOwnerDrawFixed;
  FList.OnDrawItem := @(DBHelper.DrawListItem);
  FList.Enabled := false;

  FGroup.Height := 128;
end;

procedure TDBConnectControl.DoBeginFillControls;
var ESL : TStringList;
begin
  ESL := TStringList.Create;
  try
    ESL.Add(FRefTable.Name + '.' + FieldName);
    FillFKData(GenNestedSQLRequest(GetSelectRequestForRefKey(ESL),
                                   FRefTable,
                                   FRefTable.PrimaryKeyField.FieldName));
  finally
    ESL.Free;
  end;
end;

procedure TDBConnectControl.DoEndFillControls;
begin
  if not FDestroying.Value then
  begin
    FList.Enabled := true;

    FList.ItemIndex := FillListItems(FList.Items);
  end;

  inherited DoEndFillControls;
end;

procedure TDBConnectControl.OnAddNewConnection(Sender : TObject);
var C : TDBControl;
begin
  if FOwner.Stack.ProceedTable(FRefTable.Name, -1, true, false) then
  begin
    FOwner.FWaitingKey := Self;
    if assigned(FOwner.Stack.CurTable) then
    begin
      C := FOwner.Stack.CurTable.ByName(FieldName);
      if assigned(C) then
        C.SetForeignKeyValue(FRefId);
    end;
  end;
end;

procedure TDBConnectControl.OnEditConnection(Sender : TObject);
begin
  if FList.ItemIndex >= 0 then begin
    if FOwner.Stack.ProceedTable(FRefTable.Name,
                                 TNestedExpr(FObjects[FList.ItemIndex]).Tag,
                                 true, false) then
      FOwner.FWaitingKey := Self;
  end;
end;

procedure TDBConnectControl.OnDelConnection(Sender : TObject);
begin

end;

constructor TDBConnectControl.Create(aOwner : TDBControls; aId : Integer;
  aRefField : TDBField);
begin
  inherited Create(aOwner, aRefField, false, false);

  FRefId := aId;
  FRefTable := aRefField.Table;
  FSelectRequestForRefKey := nil;
  FGroup.Caption := FRefTable.Name + '.' + FieldName;
end;

procedure TDBConnectControl.SetForeignKeyValue(AValue : Integer);
begin
  UpdateFKObjects(AValue);
end;

destructor TDBConnectControl.Destroy;
begin
  if assigned(FSelectRequestForRefKey) then
    FreeAndNil(FSelectRequestForRefKey);
  inherited Destroy;
end;

{ TDBControlStack }

function TDBControlStack.GetTable(index : integer) : TDBControls;
begin
  Result := TDBControls(Item[index]);
end;

procedure TDBControlStack.DoUpdateState;
begin
  if assigned(FOnUpdateState) then
    FOnUpdateState(Self);
end;

procedure TDBControlStack.RemoveListener(aEar: TObject);
var i : integer;
begin
  for i := FListeners.Count-1 downto 0 do
  begin
    if TDBControlListener(FListeners[i]).Ear = aEar then
      FListeners.Delete(i);
  end;
end;

procedure TDBControlStack.NotifyListeners(aKind: TDBListenerKind; aData : Pointer);
var i : integer;
begin
  for i := 0 to FListeners.Count-1 do
  begin
    if TDBControlListener(FListeners[i]).Kind = aKind then
      TDBControlListener(FListeners[i]).Notify(aData);
  end;
end;

constructor TDBControlStack.Create(aOwner : TWinControl;
  aOwnerImages : TImageList; aStructure : TDBStructure);
begin
  inherited Create;
  FListeners := TFastCollection.Create;
  FOnUpdateState := nil;
  FOwner := aOwner;
  FOwnerImages := aOwnerImages;
  FStructure := aStructure;
end;

destructor TDBControlStack.Destroy;
begin
  FListeners.Free;
  inherited Destroy;
end;

function TDBControlStack.ProceedTable(const TN : String; IdValue : Integer;
  addNext : Boolean; doReset : Boolean) : boolean;
var i, j : integer;
    C : TDBControl;
    aTable : TDBTable;
    aRecords : TDBControls;
    SL : TStringList;
    m : TDBControlMode;
    F : TField;
begin
  FOwner.DisableAutoSizing;
  try
    if (not addNext) or doReset then
    begin
      Reset;
    end;

    if addNext then
    if Assigned(CurTable) then
    begin
      if SameText(TN, CurTable.RecordTable) then
        if doReset then begin
          CurTable.Reset;
          Exit(false);
        end;
      CurTable.Hide;
    end;

    aTable := FStructure.ByName(TN);
    if Assigned(aTable) then
    begin
      if IdValue >= 0 then
        m := mtmEdit else
        m := mtmAddNew;

      aRecords := TDBControls.Create(Self, TN, m);

      if m = mtmEdit then
        SL := TStringList.Create;
      try
        if m = mtmEdit then
        begin
          aRecords.EditingKeyId := IdValue;
          Structure.DataSet.Lock;
          try
            Structure.DataSet.SQL := 'select * from ' + aTable.QuotedName +
                                     ' where '+aTable.PrimaryKeyField.QuotedName +
                                               ' = ' + inttostr(IdValue);
            try
              Structure.DataSet.Open(eomUniDirectional);
              if not Structure.DataSet.EOF then
              for i := 0 to aTable.Count-1 do
              begin
                F := Structure.DataSet.FieldByName(aTable[i].FieldName);
                if assigned(F) then
                  SL.Add(F.AsString) else
                  SL.Add('');
              end;
            finally
              Structure.DataSet.Close;
            end;
          finally
            Structure.DataSet.UnLock;
          end;
          if SL.Count = 0 then Exit(false);
        end;

        if m = mtmEdit then begin
          C := TDBControl.Create(aRecords,
                       TDBField.Create(aTable, aTable.PrimaryKeyField.FieldName,
                                               aTable.PrimaryKeyField.FieldStrType,
                                               inttostr(IdValue), true),
                       true, true);
          C.AddControls;
        end;
        for i := 0 to aTable.Count-1 do
        if not aTable[i].IsPrimaryKey then
        begin
          if m = mtmEdit then
            C := TDBControl.Create(aRecords,
                TDBField.Create(aTable[i], SL[i], false),
                true,  true)
          else
            C := TDBControl.Create(aRecords, aTable[i], false, true);
          C.OnChanged := @(aRecords.OnDBContolChanged);
          C.AddControls;
        end;
        if m = mtmEdit then
        begin
          if not Assigned(FStructure.IsForeignIndxStringTable(aTable)) then
          for i := 0 to FStructure.Count-1 do
          begin
            if FStructure[i] <> aTable then
            begin
              for j := 0 to FStructure[i].Count-1 do
              begin
                if FStructure[i][j].isForeignKey and
                   (FStructure[i][j].ForeignKeyTable = aTable) then
                begin
                  C := TDBConnectControl.Create(aRecords,
                                                IdValue,
                                                FStructure[i][j]);
                  C.AddControls;
                end;
              end;
            end;
          end;
        end;

        AddNextTable(aRecords);
      finally
        if m = mtmEdit then
          SL.Free;
      end;
      Result := true;
    end else
      Result := false;
  finally
    FOwner.EnableAutoSizing;
    if Result and Assigned(aRecords) then
    begin
      for i := 0 to aRecords.Count-1 do
      begin
        aRecords[i].FInitilizing := false;
      end;
    end;
  end;
end;

procedure TDBControlStack.Reset;
begin
  Clear;
  DoUpdateState;
end;

procedure TDBControlStack.Back(addedId : integer);
begin
  if Assigned(CurTable) then
  begin
    Delete(Count - 1);
    if Assigned(CurTable) then begin
      if addedId >= 0 then
      begin
        CurTable.FWaitingKey.SetForeignKeyValue(addedId);
      end;
      CurTable.Show;
    end;
  end;
end;

procedure TDBControlStack.Delete(Ind : integer);
begin
  TDBControls(Item[ind]).Hide;
  inherited Delete(Ind);
  DoUpdateState;
end;

procedure TDBControlStack.AddNextTable(C : TDBControls);
begin
  Add(C);
  DoUpdateState;
end;

procedure TDBControlStack.AddTimerListener(aCallback: TNotifyObjCallBack;
  aEar: TObject);
begin
  FListeners.Add(TDBControlListener.Create(lkTimer, aCallback, aEar));
end;

procedure TDBControlStack.DoTimerTick;
begin
  NotifyListeners(lkTimer, nil);
end;

function TDBControlStack.FirstTable : TDBControls;
begin
  if Count = 0 then Exit(nil);
  Result := Table[0];
end;

function TDBControlStack.CurTable : TDBControls;
begin
  if Count = 0 then Exit(nil);
  Result := Table[Count-1];
end;

{ TDBControls }

function TDBControls.GetDBControl(index : integer) : TDBControl;
begin
  Result := TDBControl(Item[index]);
end;

function TDBControls.GetWaitingKeyName : String;
begin
  if Assigned(FWaitingKey) then
  begin
    Result := FWaitingKey.FieldName;
  end else Result := 'id';
end;

function TDBControls.GetDataSet : TExtSqlite3Dataset;
begin
  Result := FStack.Structure.DataSet;
end;

procedure TDBControls.OnDBContolChanged(Sender : TObject);
begin
  FRecordChanged := true;
end;

procedure TDBControls.SetRecordTable(AValue : String);
begin
  if FRecordTable = AValue then Exit;
  FRecordTable := AValue;
end;

constructor TDBControls.Create(aStack : TDBControlStack;
  const aTableName : String; aMode : TDBControlMode);
begin
  inherited Create;
  FIdField := nil;
  FStack := aStack;
  FMode := aMode;
  FRecordTable := aTableName;
  FRecordChanged := false;
  FTable := FStack.Structure.ByName(FRecordTable);
  FId := aStack.Count + 1;
end;

procedure TDBControls.Clear;
begin
  inherited Clear;
  if assigned(FIdField) then FreeAndNil(FIdField);
end;

procedure TDBControls.Show;
var i : integer;
begin
  Stack.FOwner.DisableAutoSizing;
  try
    for i := 0 to Count-1 do
      Self[i].Show;
  finally
    Stack.FOwner.EnableAutoSizing;
  end;
end;

procedure TDBControls.Hide;
var i : integer;
begin
  Stack.FOwner.DisableAutoSizing;
  try
    for i := Count-1 downto 0 do
      Self[i].Hide;
  finally
    Stack.FOwner.EnableAutoSizing;
  end;
end;

procedure TDBControls.Reset;
var i : integer;
begin
  for i := 0 to Count-1 do
  begin
    Self[i].Reset;
  end;
  FRecordChanged := false;
end;

procedure TDBControls.SetCurAsDefaultAndReset;
var i : integer;
begin
  for i := 0 to Count-1 do
  begin
    Self[i].SetCurAsDefaultAndReset;
  end;
  FRecordChanged := false;
end;

function TDBControls.Apply(var SelExpr, ErrExpr : String) : Boolean;
var i : integer;
    S, S1, S2, S3 : String;
    HasPK, Added : Boolean;
begin
  Result := true;
  HasPK := false;
  Added := false;
  ErrExpr := '';
  S1 := '';
  S := ''; S2:=''; S3 := '';

  // first pass
  for i := 0 to Count-1 do
  begin
    if Self[i].Visible then
    begin
      if Assigned(Self[i].FIsForeignString) then
      begin
        S2 := Self[i].QuotedValue;
        if (Length(Self[i].Value) = 0) or
           (Length(S2) > 2) then
        begin
          S := 'select ' + sqluQuotedIdIfNeeded(Self[i].FIsForeignString.Indx.KeyField) + ' from ' +
               sqluQuotedIdIfNeeded(Self[i].FIsForeignString.Indx.TblName) + ' where ' +
               sqluQuotedIdIfNeeded(Self[i].FIsForeignString.Indx.ValField) + ' == ' + S2 +
               ' limit 1';
          try
           S1 := DataSet.QuickQuery(S, nil, false);
           if Length(S1) > 0 then begin
             Self[i].FForeignKeyValue.Value := StrToInt(S1);
             Self[i].DoEndFillControls;
           end else
           begin
             S3 := 'insert into ' + sqluQuotedIdIfNeeded(Self[i].FIsForeignString.Indx.TblName) + ' (' +
                                    sqluQuotedIdIfNeeded(Self[i].FIsForeignString.Indx.ValField) + ') ' +
                                    'values (' + S2 + ')';
             DataSet.ExecuteDirect(S3);
             S1 := DataSet.QuickQuery(S, nil, false);
             if Length(S1) > 0 then begin
               Self[i].FForeignKeyValue.Value := StrToInt(S1);
               Self[i].DoEndFillControls;
             end;
           end;
          except
            on e : Exception do
            begin
              Result := false;
              Exit
            end;
          end;
        end;
      end;
    end;
  end;

  // second(final) pass
  S1 := '';
  S := ''; S2:=''; S3 := '';

  for i := 0 to Count-1 do
  begin
    if Self[i].Visible then
    begin
      if Length(Self[i].Value) = 0 then
      begin
        Self[i].FGroup.Font.Color := clRed;
        if Length(ErrExpr) > 0 then ErrExpr := ErrExpr + ', ';
        ErrExpr := ErrExpr + Self[i].QuotedFieldName;
        Result := false;
      end else
      begin
        if (Self[i].Value <> Self[i].FieldDefault) or
           (Self[i].IsPrimaryKey) then
        begin
          if Added then begin
            S := S + ',';
            S1 := S1 + ',';
            S2 := S2 + ',';
          end;
          if Self[i].IsPrimaryKey then begin
            HasPK := true;
            S3 := Self[i].QuotedFieldName + ' = ' + Self[i].Value;
          end else
          begin
            S := S + Self[i].QuotedFieldName;
            if Self[i].FieldType in [ftInteger, ftFloat] then
            begin
              S1 := S1 + Self[i].Value;
              S2 := S2 + Self[i].QuotedFieldName + ' = ' + Self[i].Value;
            end else begin
              S1 := S1 + Self[i].QuotedValue;
              S2 := S2 + Self[i].QuotedFieldName + ' = ' + Self[i].QuotedValue;
            end;
            Added := true;
          end;
        end;
        Self[i].FGroup.Font.Color := clBlack;
      end;
    end;
  end;
  if HasPK then begin
    if Length(S2) > 0 then
      SelExpr := 'update ' + FTable.QuotedName + ' set ' + S2 + ' where ' + S3 else
      SelExpr := '';
  end else
  begin
    SelExpr := 'insert into ' + FTable.QuotedName + ' (' + S + ') values (' + S1 + ')';
  end;
end;

function TDBControls.ByName(const S : String) : TDBControl;
var i : integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
  begin
    if SameText(Self[i].FieldName, S) then
    begin
      Result := Self[i];
      Exit;
    end;
  end;
end;

{ TDBControl }

function TDBControl.GenNestedSQLRequest(Req : TSQLRequest; aTable : TDBTable;
  const Key : String) : String;

procedure GetNestedExpr(lst : TSelectNestedList; var S : String);
const aStrString = '''''''''||limtext(%s)||''''''''';
var
  i : integer;
  FFN : String;
  Added : Boolean;
begin
  Added := false;
  for i := 0 to lst.Count-1 do
  begin
    if Added then S := S + ';';
    S := S + lst[i].AsFieldName + ':';
    Added := true;
    if lst[i].Count > 0 then
    begin
      S := S + '(' + lst[i][0].AsFieldTable + '::';
      GetNestedExpr(lst[i], S);
      S := S + ')';
    end else
    begin
      FFN := lst[i].Table + '.' + lst[i].Field;
      S := S + '''||';
      if lst[i].FieldType in [ftString, ftMemo] then
        S := S + Format(aStrString, [FFN]) else
        S := S + FFN;
      S := S + '||''';
    end;
  end;
end;

var S, S2 : String;
begin
  S := '';
  GetNestedExpr(Req.SelectExprLst, S);
  Result := 'select ' + aTable.QuotedName + '.' + sqluQuotedIdIfNeeded(Key) + ', ''' +
            aTable.QuotedName + '::' + S + ''' from ';
  Result := Result + Req.NestedTablesListed;
  S2 := Req.WhereExpr;
  if length(S2) > 0 then
     Result := Result + ' where ' + S2 + ' group by ' +
               aTable.QuotedName + '.' + sqluQuotedIdIfNeeded(Key);
end;

function TDBControl.GetDataSet : TExtSqlite3Dataset;
begin
  Result := FOwner.DataSet;
end;

procedure TDBControl.AddValuedControl(const aName : String; C : TControl);
begin
  FControls.AddObject(aName, C);
end;

procedure TDBControl.DoAddControls;
var C : TControl;
    CH : Double;
    S : String;
    r : TRect;
    DC : THandle;
begin
  FExtBlob := nil;
  FExtBlobValue := nil;

  DC := GetDC(FGroup.Handle);
  if DC=0 then exit;
  r := Rect(0,0,1,1);
  S := 'A';
  DrawText(DC, pchar(s), Length(S), r, DT_LEFT or DT_CALCRECT);

  CH := Double(r.Height + 2);
  case FieldType of
    ftString, ftMemo : begin
      FExtBlob := FOwner.Stack.Structure.GetExtBlob(FField);
      if Assigned(FExtBlob) then
      begin
        FGroup.Height := Round(CH * 2.375);
        if FExtBlob is TDBExtImageBlob then
        begin
          C := TImage.Create(FGroup);
          C.Parent := FGroup;
          C.Height := 64;
          C.Width := 64;
          C.Align := alLeft;
          //\\
          TImage(C).Stretch := true;
          TImage(C).Proportional := true;
          TImage(C).Center := true;
          TImage(C).BorderSpacing.Around := 4;
          //\\
          AddValuedControl(ChooseImageField, C);

          FGroup.Height := Round(CH * 1.75 + 64.0);
        end;
        C := TFilenameEdit.Create(FGroup);
        C.Parent := FGroup;

        S := FExtBlob.GetAbsolutePath(Value);

        TFilenameEdit(C).Text := S;
        TFilenameEdit(C).BorderSpacing.Around := 3;
        TFilenameEdit(C).InitialDir := FExtBlob.RootPath;
        TFilenameEdit(C).OnChange := @OnValueChanged;
        C.Align := alClient;

        if FExtBlob is TDBExtImageBlob then begin
          TFilenameEdit(C).DialogKind := dkPictureOpen;
          TFilenameEdit(C).Filter := 'Pictures|*.jpg;*.jpeg;*.png';
          if FileExists(S) then
          begin
            C := GetControl(ChooseImageField);
            TImage(C).Picture.LoadFromFile(S);
          end;
        end;
      end else
      if SameText(Value, 'current_timestamp') then
      begin
        C := TLabel.Create(FGroup);
        C.Parent := FGroup;
        TLabel(C).Caption := Value;
        TLabel(C).BorderSpacing.Around := 3;
        FGroup.Height := Round(CH * 2.375);
        C.Align := alClient;
        AddValuedControl(TimeStampField, C);
        FOwner.Stack.AddTimerListener(@OnTimerTick, C);
      end else
      begin
        C := TMemo.Create(FGroup);
        C.Parent := FGroup;
        TMemo(C).Text := Value;
        AddValuedControl(ChangeText, C);
        C.Font.Style := [];
        TMemo(C).OnChange := @OnValueChanged;
        FGroup.Height := 128;
        C.Align := alClient;
      end;
    end;
    ftFloat : begin
      C := TFloatSpinEditEx.Create(FGroup);
      C.Parent := FGroup;
      TFloatSpinEditEx(C).Value := StrToFloatDef( Value, 0.0);
      with TFloatSpinEditEx(C) do
      begin
        UpDownVisible := false;
        MaxValue := -1;
        MinValue := 0;
        Alignment := taLeftJustify;
        Font.Color := DBHelper.TokenVisStyles[ntkNumber].Color;
        Font.Style := DBHelper.TokenVisStyles[ntkNumber].Style;
        OnChange := @OnValueChanged;
      end;
      AddValuedControl(ChangeFloat, C);
      FGroup.Height := Round(CH * 2.75);
      C.Align := alClient;
    end;
    ftInteger : begin
      if IsPrimaryKey then
      begin
        C := TLabel.Create(FGroup);
        C.Parent := FGroup;
        TLabel(C).Caption := Value;
        TLabel(C).BorderSpacing.Around := 3;
        FGroup.Height := Round(CH * 2.375);
        C.Align := alClient;
      end else
      if IsForeignKey then
      begin
        FIsForeignString := FOwner.Stack.Structure.IsForeignIndxStringTable(ForeignKeyTable);

        FGroup.Height := Round(CH * 2.75);

        if (Assigned(FIsForeignString) and
             not (FIsForeignString.Style = issConstList)) or
             (not Assigned(FIsForeignString)) then
        begin
          C := TBitBtn.Create(FGroup);
          C.Parent := FGroup;
          TBitBtn(C).Images := FOwner.Stack.OwnerImages;
          TBitBtn(C).ImageIndex := 4;
          TBitBtn(C).Align := alRight;
          TBitBtn(C).Width := 32;
          TBitBtn(C).Height := 32;
          TBitBtn(C).Caption := '';
          TBitBtn(C).OnClick := @OnAddNewForeignId;
        end;

        C := TBitBtn.Create(FGroup);
        C.Parent := FGroup;
        TBitBtn(C).Images := FOwner.Stack.OwnerImages;
        TBitBtn(C).ImageIndex := 9;
        TBitBtn(C).Align := alRight;
        TBitBtn(C).Width := 128;
        TBitBtn(C).Height := 32;
        TBitBtn(C).Caption := FieldDefault;
        TBitBtn(C).OnClick := @OnChooseTable;

        AddValuedControl(ChooseBitBtn, C);

        if Assigned(ForeignKeyTable) then
        begin
          if ForeignKeyTable.Count = 1 then
          begin
            FExtBlob := FOwner.Stack.Structure.GetExtBlob(ForeignKeyTable[0]);
            if assigned(FExtBlob) then
            begin
              if FExtBlob is TDBExtImageBlob then
              begin
                C := TImage.Create(FGroup);
                C.Parent := FGroup;
                C.Height := 64;
                C.Width := 64;
                C.Align := alLeft;
                //\\
                TImage(C).Stretch := true;
                TImage(C).Proportional := true;
                TImage(C).Center := true;
                TImage(C).BorderSpacing.Around := 4;
                //\\
                AddValuedControl(ExtImageField, C);
                FOwner.Stack.AddTimerListener(@OnTimerTick, C);

                FGroup.Height := Round(CH * 1.75 + 64.0);
              end;
            end;
          end;
        end;

        if Assigned(FIsForeignString) and
           (FIsForeignString.Style = issStrings) then
        begin
          C := TMemo.Create(FGroup);
          C.Parent := FGroup;

          AddValuedControl(ForeignString, C);

          C.Font.Style := [];
          //TMemo(C).ReadOnly := true;
          TMemo(C).OnChange := @OnValueChanged;
          FGroup.Height := 128;
          C.Align := alClient;
        end else
        begin
          C := TComboBox.Create(FGroup);
          C.Parent := FGroup;
          if Assigned(FExtBlob) then
            C.Left := 128;
          C.Align := alClient;
          TComboBox(C).Style := csDropDownList;
          C.Font.Style := [];
          C.Enabled := false;

          AddValuedControl(ChooseComboBox, C);

          TComboBox(C).Style := csOwnerDrawFixed;
          TComboBox(C).OnDrawItem := @(DBHelper.DrawListItem);
          TComboBox(C).OnChange := @OnValueChanged;
        end;
      end else
      begin
        FGroup.Height := Round(CH * 2.75);
        C := TSpinEditEx.Create(FGroup);
        C.Parent := FGroup;
        TSpinEditEx(C).Value := StrToIntDef( FieldDefault, 0 );
        with TSpinEditEx(C) do
        begin
          MaxValue := 10000000;
          MinValue := -10000000;
          Alignment := taLeftJustify;
          Font.Color := DBHelper.TokenVisStyles[ntkNumber].Color;
          Font.Style := DBHelper.TokenVisStyles[ntkNumber].Style;
          OnChange := @OnValueChanged;
        end;
        AddValuedControl(ChangeInteger, C);
        C.Align := alClient;
      end;
    end;
  end;
end;

procedure TDBControl.DoBeginFillControls;
var C : TControl;
    S : String;
begin
  C := GetControl(ChooseComboBox);
  if assigned(C) then
  begin
    FillFKData(GenNestedSQLRequest(GetSelectRequestForForeignKey,
                                           ForeignKeyTable, ForeignKeyTo));
  end else begin
    C := GetControl(ForeignString);
    if Assigned(C) then
    begin
      FForeignStringValue := '';
      S := Inttostr(FForeignKeyValue.Value);
      S := 'select ' + sqluQuotedIdIfNeeded(FIsForeignString.Indx.ValField) + ' from ' +
            ForeignKeyTable.QuotedName + ' where ' +
            sqluQuotedIdIfNeeded(FIsForeignString.Indx.KeyField) + ' = ' + S + ' limit 1;';
      FForeignStringValue := DataSet.QuickQuery(S, nil, false);
      FTmpObjects.Clear;
    end;
  end;
end;

procedure TDBControl.DoEndFillControls;
var
  C : TComboBox;
  M : TMemo;
  B : TBitBtn;
begin
  if not FDestroying.Value then
  begin
    B := TBitBtn(GetControl( ChooseBitBtn ));
    if Assigned(B) then
    begin
      if FForeignKeyValue.Value < 0 then begin
        Value := '';
        B.Caption := Value;
      end else begin
        B.Caption := inttostr(FForeignKeyValue.Value);
        Value := B.Caption;
      end;
    end;

    UpdateExtBlob;

    C := TComboBox(GetControl(ChooseComboBox));
    if assigned(c) then begin
      C.ItemIndex := FillListItems(C.Items);
      C.Enabled := true;
    end;
    M := TMemo(GetControl(ForeignString));
    if Assigned(M) then
    begin
      M.Text := FForeignStringValue;
    end;
  end;
  FForeignKeyLoading.Value := False;
end;

function TDBControl.GetFieldDefault : String;
begin
  Result := FField.FieldDefault;
end;

function TDBControl.GetFieldName : String;
begin
  Result := FField.FieldName;
end;

function TDBControl.GetFieldType : TFieldType;
begin
  Result := FField.FieldType;
end;

function TDBControl.GetForeignKeyTable : TDBTable;
begin
  Result := FField.ForeignKeyTable;
end;

function TDBControl.GetForeignKeyTo : String;
begin
  Result := FField.ForeignKeyTo;
end;

function TDBControl.GetIsForeignKey : Boolean;
begin
  Result := FField.IsForeignKey;
end;

function TDBControl.GetIsPrimaryKey : Boolean;
begin
  Result := FField.IsPrimaryKey;
end;

function TDBControl.GetQuotedFieldName : String;
begin
  Result := FField.QuotedName;
end;

function TDBControl.GetQuotedValue : String;
begin
  if Assigned(FIsForeignString) then
     Result := ForeignStringValue
  else
     Result := Value;

  Result := sqluQuotedStr(UTF8Trim(Result));
end;

class function TDBControl.BuildRequestForForeignKey(ST : TDBTable;
  const SF : String;
  ExcludeFields : TStringList;
  Options : TSQLNestGenOptions) : TSQLRequest;

procedure GetNestedExpr(aNestedTable : TNestedTable; lTable : TDBTable;
                        Req : TSQLRequest;
                        Lst : TSelectNestedList);
var
  i : integer;
  subLst : TSelectNestedList;
  fT : TDBForIndxStringTable;
  fld : TDBField;
  TN : TNestedTable;
begin
  for i := 0 to lTable.Count-1 do
  begin
    if Assigned(ExcludeFields) then
    begin
      if ExcludeFields.IndexOf(lTable.Name + '.' + lTable[i].FieldName) >= 0 then
        Continue;
    end;
    if (sngoExcludePrimaryKeys in Options) and lTable[i].IsPrimaryKey then
      Continue;

    if (sngoCollapseForeignString in Options) and lTable[i].IsForeignKey then
    begin
      ft := lTable.Structure.IsForeignIndxStringTable(lTable[i].ForeignKeyTable);
      if Assigned(ft) then
      begin
        if Assigned(ExcludeFields) and
           (ExcludeFields.IndexOf(lTable[i].ForeignKeyTable.Name + '.' + ft.Indx.ValField) >= 0) then
          Continue;

        fld := lTable[i].ForeignKeyTable.ByName(ft.Indx.ValField);

        if assigned(fld) then
        begin
          TN := Req.AddNewNestedTable(lTable[i].ForeignKeyTable.Name);

          subLst := Lst.AddNewNestedField(TN,
                                          fld.QuotedName, fld.FieldType,
                                          Lst.GenOrigFieldName(lTable.QuotedName,
                                                               lTable[i].QuotedName));

          Req.AddNewCompValue(lTable[i].FullFieldName, '=',
                              TN.QuotedIdName + '.' + sqluQuotedIdIfNeeded(lTable[i].ForeignKeyTo));

        end;

        Continue;
      end;
    end;

    subLst := Lst.AddNewNestedField(aNestedTable, lTable[i].QuotedName,
                                                       lTable[i].FieldType);

    if lTable[i].IsForeignKey then
    begin
      TN := Req.AddNewNestedTable(lTable[i].ForeignKeyTable.Name);

      GetNestedExpr(TN, lTable[i].ForeignKeyTable, Req, subLst);
      Req.AddNewCompValue(lTable[i].FullFieldName, '=',
                          TN.QuotedIdName + '.' + sqluQuotedIdIfNeeded(lTable[i].ForeignKeyTo));
    end;
  end;
end;

var TN : TNestedTable;
begin
  Result := TSQLRequest.Create(ST.Name, SF);
  TN := Result.AddNewNestedTable(ST.Name);

  GetNestedExpr(TN, ST, Result, Result.SelectExprLst);
end;

procedure TDBControl.FillFKData(const S: String);
var ind  : integer;
    S2 : String;
begin
  ind := 0;
  DataSet.Lock;
  try
    DataSet.SQL := S;
    DataSet.Open(eomUniDirectional);
    FTmpObjects.Lock;
    try
      FTmpObjects.Clear;
      while not DataSet.EOF do
      begin
        S2 := DataSet.Fields[0].AsString + ': ' +
                     DataSet.Fields[1].AsUTF8String;
        FTmpObjects.Add( TNestedExpr.Create(Stack.Structure, S2,
                                            DataSet.Fields[0].AsInteger,
                                            DBHelper.TokenVisStyles) );
        inc(ind);
        DataSet.Next;
      end;
    finally
      FTmpObjects.UnLock;
    end;
  finally
    DataSet.Close;
    DataSet.UnLock;
  end;
end;

function TDBControl.GetSelectRequestForForeignKey : TSQLRequest;
var FE : TStringList;
begin
  if ((not Assigned(FSelectRequestForForeignKey)) or
                    FSelectRequestForForeignKey.IsEmpty) and
     (IsForeignKey) and assigned(ForeignKeyTable) then
  begin
    if Assigned(FSelectRequestForForeignKey) then
      FSelectRequestForForeignKey.Free;
    FE := TStringList.Create;
    try
      FE.Add(ForeignKeyTable.QuotedName + '.' + sqluQuotedIdIfNeeded(ForeignKeyTo));
      FSelectRequestForForeignKey := BuildRequestForForeignKey(ForeignKeyTable,
                                                               ForeignKeyTo, FE,
                                                               [sngoCollapseForeignString,
                                                                sngoExcludePrimaryKeys]);
    finally
      FE.Free;
    end;
  end;
  Result := FSelectRequestForForeignKey;
end;

function TDBControl.GetStack : TDBControlStack;
begin
  Result := FOwner.Stack;
end;

procedure TDBControl.OnValueChanged(Sender : TObject);
var C: TControl;
begin
  if Sender is TComboBox then
  begin
    C := GetControl(ChooseBitBtn);
    if assigned(C) then begin
      if (TComboBox(Sender).ItemIndex >= 0) and
         (TComboBox(Sender).ItemIndex < FObjects.count)  then
      begin
        TBitBtn(c).Caption := inttostr(TNestedExpr(FObjects[TComboBox(Sender).ItemIndex]).Tag);
        Value := TBitBtn(c).Caption;
      end;
      UpdateExtBlob;
    end;
  end else
  if Sender is TFilenameEdit then
  begin
    Value := FExtBlob.GetRelativePath(TFilenameEdit(Sender).Text);
    if FExtBlob is TDBExtImageBlob then
    begin
      C := GetControl(ChooseImageField);
      if assigned(C) then
      if FileExists(TFilenameEdit(Sender).Text) then
      begin
        TImage(C).Picture.LoadFromFile(TFilenameEdit(Sender).Text);
      end;
    end;
  end
  else
  if Sender is TFloatSpinEditEx then
    Value := TFloatSpinEditEx(Sender).Text
  else
  if Sender is TSpinEditEx then
    Value := TSpinEditEx(Sender).Text
  else
  if Sender is TMemo then
  begin
    if Assigned(FIsForeignString) then
    begin
      ForeignStringValue := TMemo(Sender).Text;
      Changed := true;
    end else
      Value := TMemo(Sender).Text;
  end;
end;

procedure TDBControl.OnExtBlobChanged(Sender: TObject);
begin
  if Sender = FExtBlobValue then
    FExtBlobValueUpdated.Value := true;
end;

function TDBControl.OnExtBlobCheckActual(aId: TDBExtBlobValue
  ): TDBExtActualResult;
begin
  if aId = FExtBlobValue then
  begin
    Result := earActual;
  end else
    Result := earNoInformation;
end;

procedure TDBControl.OnChooseTable(Sender : TObject);
var C : TControl;
    aId, i : integer;
begin
  aId := ShowChooseIdDlg(GetSelectRequestForForeignKey);
  if aId >= 0 then
  begin
    C := GetControl(ChooseComboBox);
    if assigned(C) then begin
      TBitBtn(Sender).Caption := inttostr(aId);
      Value := TBitBtn(Sender).Caption;
      for i := 0 to FObjects.Count-1 do
      if TNestedExpr(FObjects[i]).tag = aId then
      begin
        TComboBox(C).ItemIndex := i;
        Exit;
      end;
    end else
    begin
      if Assigned(FIsForeignString) then
        SetForeignKeyValue(aId);
    end;
  end;
end;

procedure TDBControl.OnAddNewForeignId(Sender : TObject);
begin
  if FOwner.Stack.ProceedTable(Self.ForeignKeyTable.Name,-1, true, false) then
    FOwner.FWaitingKey := Self;
end;

procedure TDBControl.OnTimerTick(Sender: TObject; Data: Pointer);
begin
  if Sender is TLabel then
  begin
    TLabel(Sender).Caption := DateTimeToStr(Now);
  end else
  if FExtBlobValueUpdated.Value then
  begin
    FExtBlobValueUpdated.Value := false;
    if Sender is TImage then
    begin
      if assigned(FExtBlobValue) and
         (FExtBlobValue is TDBExtBlobImageValue)  then
      begin
        TImage(Sender).Picture := TDBExtBlobImageValue(FExtBlobValue).Bitmap;
      end;
    end;
  end;
end;

procedure TDBControl.DoOnChanged;
begin
  if not FInitilizing then
  begin
    FChanged := true;
    if assigned(FOnChanged) then
      FOnChanged(Self);
  end;
end;

procedure TDBControl.SetValue(AValue : String);
begin
  if FValue = AValue then Exit;
  FValue := AValue;
  DoOnChanged;
end;

procedure TDBControl.UpdateExtBlob;
var
  aIndex: integer;
  forId : Integer;
  S : String;
  C : TControl;
begin
  if Assigned(FExtBlob) then
  begin
    C := GetControl(ChooseComboBox);
    if not assigned(C) then Exit;

    aIndex := TComboBox(C).ItemIndex;

    FExtBlob.RemoveListener(@OnExtBlobChanged);
    if (aIndex < 0) or (aIndex >= FObjects.Count) then
    begin
      forId := -1;
      FExtBlobValue := nil;
    end else
    begin
      forId := TNestedExpr(FObjects[aIndex]).Tag;

      FExtBlobValue := FExtBlob.ValueById[forId];
      if not Assigned(FExtBlobValue) then
      begin
        S := TNestedExpr(FObjects[aIndex]).GetTokenByPath(ForeignKeyTable[0].FullFieldName);
        if Length(S) > 0 then
          FExtBlobValue := FExtBlob.AddValue(forId, S);
      end;
    end;

    if FExtBlob is TDBExtImageBlob then
    begin
      C := GetControl(ExtImageField);
      if assigned(C) then
      begin
        if forId < 0 then
        begin
          TImage(C).Picture := nil;
        end else begin
          if FExtBlobValue.Loaded then
              TImage(C).Picture := TDBExtBlobImageValue(FExtBlobValue).Bitmap;
        end;
      end;
    end;

    if assigned(FExtBlobValue) then
    begin
      FExtBlobValue.Actual := true;
      FExtBlob.AddListener(@OnExtBlobChanged, @OnExtBlobCheckActual);
    end;
  end;
end;

procedure TDBControl.UpdateFKObjects(AValue: Integer);
var aJob : TJob;
begin
  if not FForeignKeyLoading.Value then
  begin
    FForeignKeyLoading.Value := true;
    FForeignKeyValue.Value := AValue;
    if DBHelper.Threaded then
    begin
      aJob := TDBControlFillJob.Create(Self);
      FJobs.Push_back(aJob);
      DBHelper.ThreadPool.Add(aJob);
    end else
    begin
      DoBeginFillControls;
      DoEndFillControls;
    end;
  end;
end;

function TDBControl.FillListItems(Str: TStrings): Integer;
var i : integer;
  N : TNestedExpr;
begin
  Result := -1;
  Str.BeginUpdate;
  try
    Str.Clear;
    FTmpObjects.Lock;
    try
      FObjects.Clear;
      for i := 0 to FTmpObjects.Count-1 do
      begin
        N := FTmpObjects[i];
        FObjects.Add(N);
        Str.AddObject('', N);
        if N.Tag = FForeignKeyValue.Value then
          Result := i;
      end;
      FTmpObjects.Clear;
    finally
      FTmpObjects.UnLock;
    end;
  finally
    Str.EndUpdate;
  end;
end;

constructor TDBControl.Create(aOwner : TDBControls; aField : TDBField;
  aOwnField : Boolean; aVisible : Boolean);
begin
  FInitilizing := true;
  FChanged := false;
  FVisible := aVisible;
  FField := aField;
  FOwner := aOwner;
  FValue := FieldDefault;
  FOwnField := aOwnField;
  FSelectRequestForForeignKey := nil;
  FExtBlobValueUpdated := TThreadBoolean.Create(false);
  FForeignKeyLoading := TThreadBoolean.Create(false);
  FDestroying := TThreadBoolean.Create(false);
  FTmpObjects := TNestedExprsList.Create;
  FObjects := TFastCollection.Create;
  FForeignKeyValue := TThreadInteger.Create(-1);
  FJobs := TThreadSafeFastSeq.Create;

  FControls := TStringList.Create;

  FGroup := TGroupBox.Create(nil);
  FGroup.Height := 128;
  FGroup.Caption := FieldName;
  FGroup.Align := alTop;
  FGroup.Parent := FOwner.Stack.Owner;
  FGroup.Font.Style := [fsBold];
  FGroup.Font.Color := $000000;
  FGroup.BorderSpacing.Around := 2;

  FId := FOwner.Count;
  FGroup.Top := FId * 128 + 5;
  FOwner.Add(Self);

  FForeignStringValue := '';
end;

destructor TDBControl.Destroy;
var C  : TControl;
    It : TIteratorObject;
    J : TJob;
begin
  FDestroying.Value := True;

  if DBHelper.Threaded then
  begin
    It := FJobs.ListBegin;
    while Assigned(It) do
    begin
      J := TJob(It.Value);
      DBHelper.ThreadPool.WaitForJob(J);
      It := FJobs.ListBegin;
    end;
  end;

  FJobs.Free;

  C := GetControl(TimeStampField);
  if assigned(C) then
    FOwner.Stack.RemoveListener(C);
  C := GetControl(ExtImageField);
  if assigned(C) then
    FOwner.Stack.RemoveListener(C);

  if assigned(FExtBlob) then
    FExtBlob.RemoveListener(@OnExtBlobChanged);

  FGroup.Free;
  if Assigned(FSelectRequestForForeignKey) then
    FreeAndNil(FSelectRequestForForeignKey);
  if Assigned(FField) then
    if FOwnField then
      FreeAndNil(FField);
  FControls.Free;
  FExtBlobValueUpdated.Free;
  FForeignKeyLoading.Free;
  FForeignKeyValue.Free;
  FObjects.Free;
  FTmpObjects.Free;
  FDestroying.Free;
  inherited Destroy;
end;

procedure TDBControl.AddControls;
var k : integer;
begin
  DoAddControls;
  if (FieldType = ftInteger) and IsForeignKey then
  begin
    if not TryStrToInt(Value, k) then k := -1;
    UpdateFKObjects( k );
  end;
end;

function TDBControl.GetControl(const CN : String) : TControl;
var k : integer;
begin
  k := FControls.IndexOf(CN);
  if k >= 0 then
    Result := TControl(FControls.Objects[k]) else
    Result := nil;
end;

procedure TDBControl.Reset;
var C : TControl;
begin
  //to default value
  Value := FieldDefault;

  case FieldType of
    ftString : begin
      C := GetControl(ChangeText);
      if assigned(C) then
      begin
        TMemo(C).OnChange := nil;
        TMemo(C).Text := Value;
        TMemo(C).OnChange := @OnValueChanged;
      end;
    end;
    ftFloat : begin
      C := GetControl(ChangeFloat);
      if assigned(C) then
      begin
        TFloatSpinEditEx(C).OnChange := nil;
        TFloatSpinEditEx(C).Value := StrToFloatDef( Value, 0.0);
        TFloatSpinEditEx(C).OnChange := @OnValueChanged;
      end;
    end;
    ftInteger : begin
      if not IsPrimaryKey then
      begin
        if IsForeignKey then
        begin
          if Length(FieldDefault) > 0 then
            SetForeignKeyValue(StrToInt(FieldDefault)) else
          begin
            C := GetControl(ChooseBitBtn);
            if assigned(C) then TBitBtn(C).Caption := '';
            C := GetControl(ChooseComboBox);
            if assigned(C) then TComboBox(C).ItemIndex := -1;
            C := GetControl(ForeignString);
            if assigned(C) then TMemo(C).Text := '';
          end;
        end else
        begin
          C := GetControl(ChangeInteger);
          if assigned(C) then
          begin
            TSpinEditEx(C).OnChange := nil;
            TSpinEditEx(C).Value := StrToIntDef( FieldDefault, 0 );
            TSpinEditEx(C).OnChange := @OnValueChanged;
          end;
        end;
      end;
    end;
  end;
  FGroup.Font.Color := clBlack;
end;

procedure TDBControl.SetCurAsDefaultAndReset;
var aField : TDBField;
begin
  if FOwnField then
  begin
    aField := TDBField.Create(FField, Value, FField.IsPrimaryKey);
    FField.Free;
    FField := aField;
  end;
  FGroup.Font.Color := clBlack;
end;

procedure TDBControl.Show;
begin
  FGroup.Visible := true;
  FGroup.Top := 128 * FId + 5;
end;

procedure TDBControl.Hide;
begin
  FGroup.Visible := false;
end;

procedure TDBControl.SetForeignKeyValue(AValue : Integer);
var C : TComboBox;
begin
  if FForeignKeyLoading.Value then
    FForeignKeyValue.Value := AValue
  else
  begin
    if Assigned(FIsForeignString) then
      UpdateFKObjects(AValue) else
    begin
      DoEndFillControls;
      C := TComboBox(GetControl( ChooseComboBox ));
      if Assigned(C) and (C.ItemIndex < 0) then
        UpdateFKObjects(AValue);
    end;
  end;
end;

end.

