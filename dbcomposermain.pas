{
 dbComposerMain:
   Main Form

   Part of dbComposer project

   Copyright (c) 2021-2022 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit dbcomposermain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Types, Controls, Graphics, Dialogs,
  OGLFastList, ECommonObjs, Variants,
  FileUtil, DateUtils,
  LCLType, LCLIntf,
  ExtCtrls, StdCtrls, ComCtrls, Buttons, Grids, ImgList,

  JSONPropStorage, Menus, fpjson, jsonscanner,
  dbComposerConfigParser,
  kcThreadPool,

  DB, DBComposerGrid, ExtSqlite3DS, ExprSqlite3Funcs,
  ExtSqliteUtils, ExtSqliteTokens, ExtSqliteSynCheck,
  SynEdit,
  SynHighlighterSQLite3, SynEditHighlighter, SynCompletion,
  SynExportWordWrap,
  dbComposerTreeView,
  dbcomposertypes, dbComposerStruct, dbComposerUtils, dbComposerConsts,
  dbComposerCompleteHint,
  SynEditKeyCmds, SynEditTypes, SynHighlighterHTML,

  dbComposerWizards;

type

  { TIndxsCollection }

  TIndxsCollection = class(TFastCollection)
  private
    function GetIndx(aUID : Integer): TBaseSinExprs;
  public
    function IndexOf(aUID : Integer) : Integer;
    property Indx[aUID : Integer] : TBaseSinExprs read GetIndx;
  end;

  TConfigRecType = (crtRoot, crtDataBaseName, crtArray,
                    crtObject, crtProperty, crtStructElement);
  TConfigRecField = (crfUnknown, crfStructure, crfIndx,
                     crfForeignIndx, crfExtBlobs, crfAttachedFuncs);
  TConfigRec = class;

  TOnExprVarClick = procedure (Expr : TJSONData) of object;

  { TStructVarLink }

  TStructVarLink = class(TLabel)
  private
    FExpr : TJSONData;
    FOnExprVarClick : TOnExprVarClick;
    procedure Initialize;
    procedure DoOnClick(Sender : TObject);
  public
    constructor Create(AOwner: TComponent; expr : TJSONData;
                                  const aCaption : String;
                                  aOnClick : TOnExprVarClick); overload;
    constructor Create(AOwner: TComponent;
                                  const aCaption : String;
                                  aOnClick : TNotifyEvent); overload;
  end;

  { TMain }

  TMain = class(TForm)
    ApplicationProperties1 : TApplicationProperties;
    Bevel1: TBevel;
    DBNameLabel: TLabel;
    LogEnabledCB: TCheckBox;
    HeaderControl1 : THeaderControl;
    ImageList1 : TImageList;
    AppConfig : TJSONPropStorage;
    ChooseTable : TListBox;
    LogMemo : TMemo;
    WizardsMenu : TPopupMenu;
    StructInfo : TMemo;
    ModifToolBar : TToolBar;
    ModifyCfgKind: TPopupMenu;
    OpenDBDialog : TOpenDialog;
    MainPages : TPageControl;
    Panel1 : TPanel;
    Panel2 : TPanel;
    ModifyCfgElement: TPopupMenu;
    StructPanel : TPanel;
    SaveDBDialog : TSaveDialog;
    ScrollBox1 : TScrollBox;
    Splitter1 : TSplitter;
    Splitter2 : TSplitter;
    Splitter3 : TSplitter;
    Splitter4 : TSplitter;
    SQLEditor : TSynEdit;
    SQLLog: TSynEdit;
    SynCompletion1: TSynCompletion;
    SQLSheet : TTabSheet;
    ModSheet : TTabSheet;
    LogSheet: TTabSheet;
    ConfigSheet: TTabSheet;
    Timer1: TTimer;
    ToolBar2 : TToolBar;
    ToolBar3: TToolBar;
    ExecButton : TToolButton;
    DataSet : TExtSqlite3Dataset;
    ConfirmButton : TToolButton;
    RefreshCfg: TToolButton;
    SaveCfgAs: TToolButton;
    NewCfg : TToolButton;
    OpenCfg : TToolButton;
    ComposeRequest: TToolButton;
    EditExprRecord : TToolButton;
    LaunchWizard : TToolButton;
    ToolButton2 : TToolButton;
    SaveCfg : TToolButton;
    Sep2 : TToolButton;
    ToolButton4 : TToolButton;
    ToolButton5 : TToolButton;
    ToolButton6 : TToolButton;
    AddAndEditButton : TToolButton;
    CfgToolBarSep: TToolButton;
    procedure ApplicationProperties1Exception(Sender : TObject; E : Exception);
    procedure ChooseTableDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ChooseTableMeasureItem(Control: TWinControl; Index: Integer;
      var AHeight: Integer);
    procedure EditExprRecordClick(Sender : TObject);
    procedure LogEnabledCBChange(Sender: TObject);
    procedure ChooseTableDblClick(Sender : TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender : TObject);
    procedure NewCfgClick(Sender : TObject);
    procedure OpenCfgClick(Sender : TObject);
    procedure OpenDBClick(Sender : TObject);
    procedure MainPagesChange(Sender : TObject);
    procedure RefreshCfgClick(Sender : TObject);
    procedure ExecButtonClick(Sender : TObject);
    procedure ConfirmButtonClick(Sender : TObject);
    procedure SQLEditorChange(Sender : TObject);
    procedure SQLEditorCommandProcessed(Sender : TObject;
      var Command : TSynEditorCommand; var AChar : TUTF8Char; Data : pointer);
    procedure SQLEditorProcessCommand(Sender : TObject;
      var Command : TSynEditorCommand; var AChar : TUTF8Char; Data : pointer);
    procedure SQLEditorStatusChange(Sender : TObject;
      Changes : TSynStatusChanges);
    procedure SynCompletion1Execute(Sender: TObject);
    function SynCompletion1MeasureItem(const AKey: string; ACanvas: TCanvas;
      Selected: boolean; Index: integer): TPoint;
    function SynCompletion1PaintItem(const AKey: string; ACanvas: TCanvas; X,
      Y: integer; Selected: boolean; Index: integer): boolean;
    procedure Timer1Timer(Sender: TObject);
    procedure SaveCfgAsClick(Sender : TObject);
    procedure SaveCfgClick(Sender : TObject);
    procedure ComposeRequestClick(Sender: TObject);
    procedure ToolButton4Click(Sender : TObject);
    procedure ToolButton5Click(Sender : TObject);
    procedure AddAndEditButtonClick(Sender : TObject);
  private
    FSynSQLSyn : TSynSQLite3Syn;

    FModTables : TDBControlStack;
    FStructureChanged : Boolean;
    FStructure : TDBStructure;
    FTableList : TStringList;
    FLogChanged, FLogEnabled : TThreadBoolean;
    FCompletionKeys : TCompletionCollection;
    FLogPull : TThreadStringList;
    FConfig  : TDBJSONConfig;
    FCfgStructNode : TTreeNode;
    FConfigFileName : String;
    FConfigFileLoaded : TDateTime;
    FConfigChanged : Boolean;
    CompleteHint : TCustomCompleteHint;
    ConfigTree: TDBComposerTreeView;
    ConfigItem : TTreeNode;

    NormalTB3ButtonsCnt : Integer;

    StringsIdxs : TTokenedSinExprs;
    Indxs : TIndxsCollection;
    DBGrid     : TDBExtGrid;
    LoggerExport : TSynExporterWordWrap;

    procedure SetDBFileName(const FN : String);
    procedure ReloadConfig(const FN: String);
    procedure SaveConfig;
    procedure ReloadConfigStructure;
    procedure DBGridDblClick(Sender : TObject);
    procedure InitializeDb;
    procedure RefreshDbFromConfig;
    procedure SetConfigChanged(AValue: Boolean);
    procedure SetConfigFileName(const FN : String);
    procedure RefreshDatabaseAndStruct;
    procedure GenerateStructure;
    procedure RebuildConfigTree;
    procedure UpdateCfgStruct(fromind : integer);
    procedure UpdateStructInfoFromExpr(expr : TJSONSqliteExpr);
    function  FindConfigNodeFromJson(jsonData : TJSONData) : TTreeNode;
    procedure ClearStructVarLinks;
    procedure AddVarLink(const aCaption : String; aOnClick : TNotifyEvent);
    procedure AddStructVarLink(const aCaption : String;
                                     expr : TJSONData;
                                     aOnClick : TOnExprVarClick);
    procedure AddConfigNodes(parentNode : TTreeNode;
                              data : TJSONData;
                              const id : String;
                              aType : TConfigRecType);

    procedure UpdateModTablesStack(O : TObject);
    procedure ReadFilters(const Sec: String; SL : TStrings);
    procedure WriteFilters(const Sec: String; SL : TStrings);
    procedure EnableToolbar(TB : TToolBar; en : Boolean);
    procedure SqlRequestPrepared(const aRequest : String; aResult : Integer);
    function StartOpenEditSQLDialog(cfg : TJSONData) : Boolean;

    procedure OnAddToArray(Sender: TObject);
    procedure OnAddTableToStruct(Sender: TObject);
    procedure AddArray(Sender: TObject; const Str: String);
    procedure OnAddArray1(Sender: TObject);
    procedure OnAddArray2(Sender: TObject);
    procedure OnAddArray3(Sender: TObject);
    procedure OnAddArray4(Sender: TObject);
    procedure OnRemoveFromArray(Sender: TObject);
    procedure OnRemoveProperty(Sender: TObject);
    procedure OnMoveUpInArray(Sender: TObject);
    procedure OnMoveDownInArray(Sender: TObject);
    procedure OnAddField(Sender: TObject);
    procedure OnExecuteExpr(Sender: TObject);
    procedure OnEditExpr(Sender: TObject);

    procedure OnWizard(Sender: TObject);

    procedure OnEditExprLink(Expr : TJSONData);
    procedure OnRemoveExpr(Expr : TJSONData);
    procedure OnSetExprFromDatabase(Expr : TJSONData);
    procedure OnModifyDatabaseExpr(Expr : TJSONData);
    procedure OnExecuteExprLink(Expr : TJSONData);

    function ExtractSendersData(Sender : TObject; out cfgRec : TConfigRec;
                                                   out cfgItem : TTreeNode) : Boolean; overload;
    function ExtractSendersData(Sender : TObject; out cfgItem : TTreeNode) : Boolean; overload;

    procedure ConfigTreeAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure ConfigTreeDeletion(Sender: TObject; Node: TTreeNode);
    procedure ConfigTreeEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure ConfigTreeEditingEnd(Sender: TObject; Node: TTreeNode;
      Cancel: Boolean);
    procedure ConfigTreeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ConfigTreeSelectionChanged(Sender: TObject);
  public
    procedure SetFontParamsFromCompletionObj(O : TCompletionObj; F : TFont; out
      imi : integer);
    property ConfigChanged : Boolean read FConfigChanged write SetConfigChanged;
    property CompletionKeys : TCompletionCollection read FCompletionKeys;
    property SynSQLSyn : TSynSQLite3Syn read FSynSQLSyn;
  end;

  { TConfigRec }

  TConfigRec = class
  private
    FType : TConfigRecType;
    FField : TConfigRecField;
    FData :  TJSONData;
    FName :  String;
    function GetDisplayStr: String;
  public
    constructor Create(const aName : String;
                             aType : TConfigRecType;
                             aField : TConfigRecField;
                             aData : TJSONData); virtual;
    constructor Create(aCfgRec : TConfigRec);
    procedure Assign(aCfgRec : TConfigRec); virtual;
    property CfgType : TConfigRecType read FType;
    property CfgField : TConfigRecField read FField;
    property JsonData : TJSONData read FData;
    property Name : String read FName;
    property DisplayStr : String read GetDisplayStr;
  end;

  { TScissorConfigRec }

  TScissorConfigRec = class(TConfigRec)
  public
    destructor Destroy; override;
    procedure Assign(aCfgRec : TConfigRec); override;
    procedure SetData(const S : String); overload;
    procedure SetData(Id : Integer); overload;
  end;

  { TCfgMenuItem }

  TCfgMenuItem = class(TMenuItem)
  private
    FCfgRec: TConfigRec;
    FCfgItem : TTreeNode;
  public
    destructor Destroy; override;
    property CfgRec : TConfigRec read FCfgRec write FCfgRec;
    property CfgItem : TTreeNode read FCfgItem write FCfgItem;
  end;

  { TCfgToolButton }

  TCfgToolButton = class(TToolButton)
  private
    FCfgRec: TConfigRec;
    FCfgItem : TTreeNode;
  public
    destructor Destroy; override;
    property CfgRec : TConfigRec read FCfgRec write FCfgRec;
    property CfgItem : TTreeNode read FCfgItem write FCfgItem;
  end;

  { TReloadStructJob }

  TReloadStructJob = class(TJob)
  private
    FCfg : TDBJSONConfig;
    FCmpStruct : TDBStructure;
  public
    constructor Create(aCfg : TDBJSONConfig; aCmpStruct : TDBStructure);
    procedure Execute; override;
  end;

var
  Main : TMain;

implementation

uses dbchooseid, dbComposerSQLEditor,
     dbcomposersynanalize,
     LazUTF8, Themes;

const
cObjPostfix = '_element';

cSelectedItemBackGround = $885555;

{$R *.lfm}

{ TStructVarLink }

procedure TStructVarLink.Initialize;
var C : TColor;
begin
  AutoSize := true;
  C := $FFFFFF - (clInfoBk and $FFFFFF);
  if (C and $00FFFF) > $8080 then
     C := C - $8080 else
  if (C and $FF0000) < $800000 then
     C := C + $800000;
  Font.Color := C;
  Font.Style := [fsUnderline];
  Cursor := crHandPoint;
  Color := clInfoBk;
  BorderSpacing.Around := 2;
end;

procedure TStructVarLink.DoOnClick(Sender : TObject);
begin
  If Assigned(FOnExprVarClick) then
  begin
    FOnExprVarClick(FExpr);
  end;
end;

constructor TStructVarLink.Create(AOwner : TComponent; expr : TJSONData;
  const aCaption : String; aOnClick : TOnExprVarClick);
begin
  inherited Create(AOwner);
  Caption := aCaption;
  FExpr   := expr;
  FOnExprVarClick := aOnClick;
  OnClick := @DoOnClick;
  Initialize;
end;

constructor TStructVarLink.Create(AOwner : TComponent; const aCaption : String;
  aOnClick : TNotifyEvent);
begin
  inherited Create(AOwner);
  Caption := aCaption;
  OnClick := aOnClick;
  Initialize;
end;

{ TReloadStructJob }

constructor TReloadStructJob.Create(aCfg : TDBJSONConfig;
                                         aCmpStruct : TDBStructure);
begin
  inherited Create;
  FCfg :=  aCfg;
  FCmpStruct := aCmpStruct;
end;

procedure TReloadStructJob.Execute;
var
  i, k : integer;
  analiz : TDBSqliteSynAnalizer;
  cTable, cTable2 : TDBTable;
  cfgStruct : TDBStructure;
begin
  FCfg.Lock;
  try
    if assigned(FCfg.Struct) then
    begin
      k := -1;
      for i := 0 to FCfg.Struct.Count-1 do
      begin
        if FCfg.StructExpr[i].NeedRebuild then begin
          FCfg.StructExpr[i].RebuildExpr;
          FCfg.StructExpr[i].NeedRebuild := false;
          if TDBSqliteSynAnalizer.CheckIsCreateTable(FCfg.StructExpr[i].Expr) then
          begin
            FCfg.StructExpr[i].IsCreateTable := True;
            k := i;
          end else
            FCfg.StructExpr[i].IsCreateTable := False;
        end;
      end;

      if k >= 0 then
      begin
        cfgStruct := TDBStructure.Create(nil);
        try
          for i := 0 to FCfg.Struct.Count-1 do
          begin
            if FCfg.StructExpr[i].IsCreateTable then
            begin
              FCmpStruct.Lock;
              analiz := TDBSqliteSynAnalizer.Create(FCfg.StructExpr[i].Expr);
              try
                cTable := analiz.CreateTable(cfgStruct,
                                             [ctoAddToStruct,
                                              ctoCheckExists,
                                              ctoCheckForeignKeys]);
                if Assigned(cTable) then
                begin
                  FCfg.StructExpr[i].IsOk := True;
                  FCfg.StructExpr[i].TableName := cTable.QuotedName;
                  cTable2 := FCmpStruct.ByName(cTable.Name);
                  if Assigned(cTable2) then
                  begin
                    FCfg.StructExpr[i].ExcludeState(exstTableNotExists);
                    if cTable.Compare(cTable2) then
                      FCfg.StructExpr[i].ExcludeState(exstTableNotEqual) else
                      FCfg.StructExpr[i].IncludeState(exstTableNotEqual);
                  end else begin
                    FCfg.StructExpr[i].IncludeState(exstTableNotExists);
                  end;
                end else
                begin
                  FCfg.StructExpr[i].IsMalformed := True;
                  FCfg.StructExpr[i].ParseError:= analiz.ErrorStr;
                  FCfg.StructExpr[i].TableName := '';
                end;
              finally
                analiz.Free;
                FCmpStruct.UnLock;
              end;
            end else
              FCfg.StructExpr[i].IsOk := True;
          end;
        finally
          cfgStruct.Free;
        end;
      end;
    end;
  finally
    FCfg.UnLock;
  end;
end;

{ TCfgMenuItem }

destructor TCfgMenuItem.Destroy;
begin
  if Assigned(FCfgRec) then FCfgRec.Free;
  inherited Destroy;
end;

{ TCfgToolButton }

destructor TCfgToolButton.Destroy;
begin
  if Assigned(FCfgRec) then FCfgRec.Free;
  inherited Destroy;
end;

{ TScissorConfigRec }

destructor TScissorConfigRec.Destroy;
begin
  if assigned(FData) then FreeAndNil(FData);
  inherited Destroy;
end;

procedure TScissorConfigRec.Assign(aCfgRec: TConfigRec);
begin
  inherited Assign(aCfgRec);
  if aCfgRec.JsonData is TJSONString then
    FData := TJSONString.Create(aCfgRec.JsonData.AsString) else
  if aCfgRec.JsonData is TJSONIntegerNumber then
    FData := TJSONIntegerNumber.Create(aCfgRec.JsonData.AsInteger);
end;

procedure TScissorConfigRec.SetData(const S: String);
begin
  FData := TJSONString.Create(S);
end;

procedure TScissorConfigRec.SetData(Id: Integer);
begin
  FData := TJSONIntegerNumber.Create(Id);
end;

{ TConfigRec }

function TConfigRec.GetDisplayStr: String;
begin
  if FType in [crtDataBaseName, crtProperty, crtStructElement] then
  begin
    Result := FName + ': ' + FData.AsString;
  end else
    Result := FName;
end;

constructor TConfigRec.Create(const aName: String; aType: TConfigRecType;
  aField: TConfigRecField; aData: TJSONData);
begin
  FType := aType;
  FField := aField;
  FData := aData;
  FName := aName;
end;

constructor TConfigRec.Create(aCfgRec: TConfigRec);
begin
  Assign(aCfgRec);
end;

procedure TConfigRec.Assign(aCfgRec: TConfigRec);
begin
  FType := aCfgRec.CfgType;
  FField := aCfgRec.CfgField;
  FData := aCfgRec.JsonData;
  FName := aCfgRec.FName;
end;

{ TIndxsCollection }

function TIndxsCollection.GetIndx(aUID: Integer): TBaseSinExprs;
var k : integer;
begin
  k := IndexOf(aUID);
  if k >= 0 then Result := TBaseSinExprs(Item[k]) else Result := nil;
end;

function TIndxsCollection.IndexOf(aUID: Integer): Integer;
var i : integer;
begin
  for i := 0 to Count-1 do
  begin
    if TBaseSinExprs(Item[i]).UID = aUID then Exit(i);
  end;
  Result := -1;
end;

{ TMain }

procedure TMain.OpenCfgClick(Sender : TObject);
begin
  if OpenDBDialog.Execute then
  begin
    ReloadConfig(OpenDBDialog.FileName);
  end;
end;

procedure TMain.OpenDBClick(Sender : TObject);
begin
  if OpenDBDialog.Execute then
  begin
    ReloadConfig(OpenDBDialog.FileName);
  end;
end;

procedure TMain.MainPagesChange(Sender : TObject);
begin
  if MainPages.ActivePage = SQLSheet then
  begin
    ChooseTable.ItemIndex := ChooseTable.Items.IndexOf(DBGrid.CurSelectedTable);
  end;
end;

procedure TMain.RefreshCfgClick(Sender : TObject);
begin
  if FileExists(FConfigFileName) then
  begin
    ReloadConfig(FConfigFileName);
  end;
end;

procedure TMain.FormCreate(Sender : TObject);
var SL : TStringList;
    i : integer;
begin
  DBHelper.InitThreadPool(4);

  StringsIdxs := nil;
  Indxs := TIndxsCollection.Create;

  DBHelper.AppPath := ExtractFilePath(Application.ExeName);
  SaveDBDialog.InitialDir := DBHelper.AppPath;
  OpenDBDialog.InitialDir := DBHelper.AppPath;
  DataSet := TExtSqlite3Dataset.Create(nil);
  DataSet.DefaultStringSize := 1024;
  DataSet.OnPrepared := @SqlRequestPrepared;
  FTableList := TStringList.Create;
  FStructure := TDBStructure.Create(DataSet);
  FModTables := TDBControlStack.Create(ScrollBox1, ImageList1, FStructure);
  FModTables.OnUpdateState := @UpdateModTablesStack;
  FStructureChanged := true;
  FCompletionKeys := TCompletionCollection.Create;

  FLogChanged := TThreadBoolean.Create(false);
  FLogEnabled := TThreadBoolean.Create(true);
  FLogPull := TThreadStringList.Create;

  FSynSQLSyn := TSynSQLite3Syn.Create(Self);
  FSynSQLSyn.CommentAttri.Foreground := clGreen;
  FSynSQLSyn.CommentAttri.Style := [fsItalic];
  FSynSQLSyn.NumberAttri.Foreground := clNavy;
  FSynSQLSyn.NumberAttri.Style := [fsBold];
  FSynSQLSyn.SymbolAttri.Foreground := clRed;
  FSynSQLSyn.SymbolAttri.Style := [];
  FSynSQLSyn.TableNameAttri.Foreground := clPurple;
  FSynSQLSyn.TableNameAttri.Style := [];
  FSynSQLSyn.FieldNameAttri.Foreground := clFuchsia;
  FSynSQLSyn.FieldNameAttri.Style := [fsItalic];
  FSynSQLSyn.StringAttri.Foreground := clGreen;
  FSynSQLSyn.StringAttri.Style := [fsBold];
  FSynSQLSyn.Enabled := true;

  SQLEditor.Highlighter := FSynSQLSyn;
  SQLLog.Highlighter := FSynSQLSyn;

  SL := TStringList.Create;
  try
    SL.Delimiter := ',';
    for i := 1 to sqluGetKeyWordCount do
      FCompletionKeys.AddObj(sqluGetKeyWordName(i), sckKeyword);
    FCompletionKeys.AddObj(sqluGetIndexedKeyWord(kwROWID), sckKeyword);
    SL.DelimitedText := sqluGetDataTypesList();
    for i := 0 to SL.Count-1 do
      FCompletionKeys.AddObj(SL[i], sckType);
    SL.DelimitedText := sqluGetFunctionsList();
    for i := 0 to SL.Count-1 do
      FCompletionKeys.AddObj(SL[i], sckFunction);
  finally
    SL.Free;
  end;

  ConfigTree := TDBComposerTreeView.Create(ConfigSheet);
  ConfigTree.Top := 102;
  ConfigTree.Left := 2;
  ConfigTree.Width := 100;
  ConfigTree.Height := 100;
  ConfigTree.Align := alClient;
  ConfigTree.StateImages := ImageList1;
  ConfigTree.OnAdvancedCustomDrawItem := @ConfigTreeAdvancedCustomDrawItem;
  ConfigTree.OnDeletion := @ConfigTreeDeletion;
  ConfigTree.OnEditing := @ConfigTreeEditing;
  ConfigTree.OnEditingEnd := @ConfigTreeEditingEnd;
  ConfigTree.OnMouseDown := @ConfigTreeMouseDown;
  ConfigTree.OnSelectionChanged := @ConfigTreeSelectionChanged;
  ConfigTree.RightClickSelect := true;
  ConfigTree.Parent := ConfigSheet;
  {$ifdef Linux}
  ConfigTree.Font.Name := 'Monospace';
  {$else}
  {$ifdef Windows}
  ConfigTree.Font.Name := 'Courier New';
  {$else}
  ConfigTree.Font.Name := 'monospace';
  {$endif}
  {$endif}
  ConfigTree.Font.Size := 10;
  SaveCfg.Enabled := false;

  NormalTB3ButtonsCnt := ToolBar3.ControlCount;

  ReloadConfig(DBHelper.AppPath + AppConfig.ReadString(cfldDBName,
                                                     '..' + cSysDelimiter +
                                                            cDefaultDBName));
  vDefaultTokenVisStyles := TNestedTokenStyles.Create;
  vDefaultTokenVisStyles[ntkNone]      := NestedTokenVisStyle(FSynSQLSyn.IdentifierAttri.Foreground,
                                                              FSynSQLSyn.IdentifierAttri.Style);
  vDefaultTokenVisStyles[ntkSymbol]    := NestedTokenVisStyle(FSynSQLSyn.SymbolAttri.Foreground,
                                                              FSynSQLSyn.SymbolAttri.Style);
  vDefaultTokenVisStyles[ntkTableName] := NestedTokenVisStyle(FSynSQLSyn.TableNameAttri.Foreground,
                                                              FSynSQLSyn.TableNameAttri.Style);
  vDefaultTokenVisStyles[ntkFieldName] := NestedTokenVisStyle(FSynSQLSyn.FieldNameAttri.Foreground,
                                                              FSynSQLSyn.FieldNameAttri.Style);
  vDefaultTokenVisStyles[ntkNumber]    := NestedTokenVisStyle(FSynSQLSyn.NumberAttri.Foreground,
                                                              FSynSQLSyn.NumberAttri.Style);
  vDefaultTokenVisStyles[ntkString]    := NestedTokenVisStyle(FSynSQLSyn.StringAttri.Foreground,
                                                              FSynSQLSyn.StringAttri.Style);

  DBGrid := TDBExtGrid.Create(SQLSheet);
  DBGrid.Top := Splitter1.Top + Splitter1.Height + 5;
  DBGrid.Parent := SQLSheet;
  DBGrid.DB := FStructure;
  DBGrid.DataSet := DataSet;
  DBGrid.OnDblClick := @DBGridDblClick;
  DBGrid.Align := alClient;

  LoggerExport := TSynExporterWordWrap.Create(nil);
  LoggerExport.Highlighter := FSynSQLSyn;

  CompleteHint := TCustomCompleteHint.Create(SQLSheet);
  CompleteHint.SynEdit := SQLEditor;
  CompleteHint.ImageList := ImageList1;
  CompleteHint.CompletionKeys := FCompletionKeys;
  CompleteHint.OnSetFontParams := @SetFontParamsFromCompletionObj;
  CompleteHint.Parent := SQLSheet;
end;

procedure TMain.ApplicationProperties1Exception(Sender : TObject; E : Exception
  );
begin
  if E is EDatabaseError then
  begin
    LogMemo.Append(E.ToString);
  end else
    raise E;
end;

procedure TMain.ChooseTableDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);

procedure DrawPic(imi, off : integer);
var
  B : TBitmap;
begin
  if imi >= 0 then begin
    B := TBitmap.Create;
    try
      B.Width := 16;
      B.Height := 16;
      ImageList1.Getbitmap(imi, B);
      ChooseTable.Canvas.Draw(aRect.Left + off, aRect.Top, B);
    finally
      B.Free;
    end;
    aRect.Left := aRect.Left + 16 + off;
  end;
end;

var
  ts : TTextStyle;
  S  : String;
  Attr : TSynHighlighterAttributes;
  FS : TDBForIndxStringTable;
  T  : TDBTable;
begin
  if ChooseTable.Items.Count > Index then
  with ChooseTable.Canvas do
  begin
    ts := TextStyle;
    ts.Alignment  := taLeftJustify;
    ts.Layout     := tlCenter;
    ts.Wordbreak  := True;
    ts.SingleLine := false;
    ts.Opaque     := false;

    brush.Color := clWhite;
    Pen.Style   := psSolid;
    Pen.Color   := Brush.Color;

    Attr :=  FSynSQLSyn.TableNameAttri;
    S := ChooseTable.Items[Index];
    if Assigned(Attr) then
    begin
      Font.Color := Attr.Foreground;
      if (Font.Color = clDefault) or (Font.Color = clNone) then
         Font.Color := clBlack;
      Font.Style := Attr.Style;
    end;
    if odSelected in State then
    begin
      brush.Color := cSelectedItemBackGround;
      font.Color := clWhite;
      pen.Color := clYellow;
      pen.Style := psDot;
    end;

    Rectangle(aRect);

    DrawPic(IMG_TABLE, 0);
    if Assigned(FStructure) then
    begin
      T := FStructure.ByName(S);
      if Assigned(T) then
      begin
        if not T.HasForeignKey  then
          DrawPic(IMG_CONNECTION, 0);
        if FStructure.IsExtBlobTable(T, ebkImage) then
          DrawPic(IMG_IMAGE, -4);
        if FStructure.IsExtBlobTable(T, ebkText) then
          DrawPic(IMG_TEXT_DOC, -4);
        FS := FStructure.IsForeignIndxStringTable(T);
        if Assigned(FS) then
        begin
          case FS.Style of
            issStrings   : DrawPic(IMG_DICT, -4);
            issConstList : DrawPic(IMG_LIST, -4);
          end;
        end;
      end;
    end;

    ChooseTable.Canvas.TextRect(aRect, aRect.Left+2, aRect.Top + 2, S, ts);
  end;
end;

procedure TMain.ChooseTableMeasureItem(Control: TWinControl; Index: Integer;
  var AHeight: Integer);
begin
  AHeight := 18;
end;


procedure TMain.ConfigTreeAdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);

var
  VertMid, VertDelta, RealIndent: integer;
  NodeRect : TRect;

procedure DrawBackground(ARect: TRect);
var
  bclr: TColor;
begin
  bclr:=Sender.Canvas.Brush.Color;
  try
    Sender.Canvas.Brush.Color := clWhite;
    Sender.Canvas.FillRect(ARect);
  finally
    Sender.Canvas.Brush.Color := bclr;
  end;
end;

function SColor(C : TColor) : TColor; inline;
begin
  if (C = clDefault) or (C = clNone) then
     Result := clBlack else
     Result := C;
end;

procedure DrawNodeText(IsSelected: Boolean; NodeRect: TRect;
                             const S : String);
var
  Details: TThemedElementDetails;
begin
  if IsSelected then
  begin
    Sender.Canvas.Brush.Color := cSelectedItemBackGround;
    Sender.Canvas.FillRect(NodeRect);
  end;

  Details := ThemeServices.GetElementDetails(ttTreeviewDontCare);

  NodeRect.Right := NodeRect.Left + Sender.Canvas.TextWidth(S);
  if (tvoThemedDraw in Sender.Options) then
    ThemeServices.DrawText(Sender.Canvas, Details, S, NodeRect, DT_LEFT or
                                                          DT_VCENTER or
                                                          DT_SINGLELINE or
                                                          DT_NOPREFIX, 0)
  else
    DrawText(Sender.Canvas.Handle, PChar(S), -1, NodeRect, DT_LEFT or
                                                     DT_VCENTER or
                                                     DT_SINGLELINE or
                                                     DT_NOPREFIX);
end;

procedure DrawNodeTextSimple(IsSelected: Boolean; NodeRect: TRect;
                             const S : String);
begin
  Sender.Canvas.Font.Style := [];
  if IsSelected then
     Sender.Canvas.Font.Color := clWhite else
     Sender.Canvas.Font.Color := clBlack;

  DrawNodeText(IsSelected, NodeRect, S);
end;

procedure DrawNodeTextTableName(IsSelected: Boolean; NodeRect: TRect;
                             const S : String);
begin
  Sender.Canvas.Font.Style := vDefaultTokenVisStyles[ntkTableName].Style;
  if IsSelected then
     Sender.Canvas.Font.Color := clWhite else
     Sender.Canvas.Font.Color := vDefaultTokenVisStyles[ntkTableName].Color;

  DrawNodeText(IsSelected, NodeRect, S);
end;

procedure DrawNodeText(IsSelected: Boolean; NodeRect: TRect; cfgRec: TJSONSqliteExpr);
var
  Details: TThemedElementDetails;
  Expr : TSqliteExpr;
  i : integer;
  S : String;
  Attr : TSynHighlighterAttributes;
begin
  if IsSelected then
  begin
    Sender.Canvas.Brush.Color := cSelectedItemBackGround;
    Sender.Canvas.FillRect(NodeRect);
  end;

  Details := ThemeServices.GetElementDetails(ttTreeviewDontCare);

  Expr := cfgRec.Expr;
  try
    for i := 0 to Expr.Count-1 do
    begin
      case Expr[i].Kind of
        stkKeyWord :
          Attr := FSynSQLSyn.KeyAttri;
        stkIdentifier :
          Attr := FSynSQLSyn.IdentifierAttri;
        stkNumber :
          Attr := FSynSQLSyn.NumberAttri;
        stkString :
          Attr := FSynSQLSyn.StringAttri;
        stkSymbol :
          Attr := FSynSQLSyn.SymbolAttri;
      else
         Attr := nil;
      end;

      if Attr = FSynSQLSyn.IdentifierAttri then
      begin
        if Assigned(FStructure) and
           Assigned(FStructure.ByName(Expr[i].Token)) then
        begin
          Sender.Canvas.Font.Style := vDefaultTokenVisStyles[ntkTableName].Style;
          Sender.Canvas.Font.Color := vDefaultTokenVisStyles[ntkTableName].Color;
        end
        else
        begin
          Sender.Canvas.Font.Style := Attr.Style;
          Sender.Canvas.Font.Color := SColor(Attr.Foreground);
        end;
      end else
      begin
        if Assigned(Attr) then
        begin
          Sender.Canvas.Font.Style := Attr.Style;
          Sender.Canvas.Font.Color := SColor(Attr.Foreground);
        end else begin
          Sender.Canvas.Font.Style := [];
          Sender.Canvas.Font.Color := clBlack;
        end;
      end;

      case Expr[i].Kind of
        stkSpace : S := ' ';
        stkKeyWord : S := Expr[i].Token;
      else
        S := Expr[i].QuotedToken;
      end;
      if IsSelected then Sender.Canvas.Font.Color := clWhite;
      NodeRect.Right := NodeRect.Left + Sender.Canvas.TextWidth(S);
      if (tvoThemedDraw in Sender.Options) then
        ThemeServices.DrawText(Sender.Canvas, Details, S, NodeRect, DT_LEFT or
                                                              DT_VCENTER or
                                                              DT_SINGLELINE or
                                                              DT_NOPREFIX, 0)
      else
        DrawText(Sender.Canvas.Handle, PChar(S), -1, NodeRect, DT_LEFT or
                                                         DT_VCENTER or
                                                         DT_SINGLELINE or
                                                         DT_NOPREFIX);
      NodeRect.Left := NodeRect.Right;
    end;
  finally

  end;
end;

procedure DrawVertLine(X, Y1, Y2: Integer);
begin
  if Y1 > Y2 then
    Exit;
  if Sender.TreeLinePenStyle = psPattern then
  begin
    Y1 := Y1 + VertDelta;
    while Y1 < Y2 do
    begin
      Sender.Canvas.Pixels[X, Y1] := Sender.TreeLineColor;
      inc(Y1, 2);
    end;
  end
  else
  begin
    Sender.Canvas.MoveTo(X, Y1);
    Sender.Canvas.LineTo(X, Y2);
  end;
end;

procedure DrawHorzLine(Y, X1, X2: Integer);
begin
  if X1 > X2 then
    Exit;
  if Sender.TreeLinePenStyle = psPattern then
  begin
    while X1 < X2 do
    begin
      Sender.Canvas.Pixels[X1, Y] := Sender.TreeLineColor;
      inc(X1, 2);
    end;
  end
  else
  begin
    Sender.Canvas.MoveTo(X1, Y);
    Sender.Canvas.LineTo(X2, Y);
  end;
end;

function DrawTreeLines(CurNode: TTreeNode): integer;
// paints tree lines, returns indent
var
  CurMid: integer;
begin
  if (CurNode <> nil) and ((tvoShowRoot in Sender.Options) or (CurNode.Parent<>nil)) then
  begin
    Result := DrawTreeLines(CurNode.Parent);
    CurMid := Result + (RealIndent shr 1);
    if CurNode = Node then
    begin
      // draw horizontal line
      DrawHorzLine(VertMid, CurMid, Result + RealIndent);
    end;

    if (CurNode.GetNextVisibleSibling <> nil) then
    begin
      // draw vertical line to next brother
      if (Node.Parent = nil) and (Node.GetPrevSibling = nil) then
        DrawVertLine(CurMid, VertMid + VertDelta, NodeRect.Bottom)
      else
        DrawVertLine(CurMid, NodeRect.Top, NodeRect.Bottom);
    end else
    if (CurNode = Node) then
    begin
      // draw vertical line from top to horizontal line
      DrawVertLine(CurMid, NodeRect.Top, VertMid);
    end;
    inc(Result, RealIndent);
  end else
  begin
    Result := Sender.BorderWidth - TDBComposerTreeView(Sender).ScrolledLeft;
    if CurNode <> nil then // indent first level of tree with ShowRoot = false a bit
      inc(Result, RealIndent shr 2);
  end;
end;

var cfgRec : TConfigRec;
    StateImageRes: TScaledImageListResolution;
    stim : Integer;
begin
  FConfig.Lock;
  try
    if Assigned(FConfig) and Assigned(Node.Data) then
    begin
      cfgRec := TConfigRec(Node.Data);
      if (cfgRec.CfgType = crtStructElement) and
         (FConfig.Struct.IndexOf(cfgRec.JsonData) >= 0)  then
      begin
        if Stage = cdPrePaint then
        begin
          DefaultDraw := false;

          if TJSONSqliteExpr(cfgRec.JsonData).NeedRebuild then
          begin
            Node.Text := TJSONSqliteExpr(cfgRec.JsonData).AsString;
          end else
          begin
            if TJSONSqliteExpr(cfgRec.JsonData).IsCreateTable and
               TJSONSqliteExpr(cfgRec.JsonData).IsOk then
              Node.Text := TJSONSqliteExpr(cfgRec.JsonData).TableName else
              Node.Text := TJSONSqliteExpr(cfgRec.JsonData).Expr.FormatedStr(skfoOriginal);
          end;

          NodeRect := Node.DisplayRect(false);
          VertMid := NodeRect.Top + (NodeRect.Bottom - NodeRect.Top) div 2;
          VertDelta := Ord(Sender.DefaultItemHeight and 3 = 2);
          RealIndent := TDBComposerTreeView(Sender).Indent;

          DrawBackground(NodeRect);
          // draw tree lines
          Sender.Canvas.Pen.Color := Sender.TreeLineColor;
          Sender.Canvas.Pen.Style := Sender.TreeLinePenStyle;
          NodeRect.Left := DrawTreeLines(Node);
          Sender.Canvas.Pen.Style := psSolid;

          if TJSONSqliteExpr(cfgRec.JsonData).NeedRepaint then
          begin
            TJSONSqliteExpr(cfgRec.JsonData).NeedRepaint := false;
            if Node.Selected then
              UpdateStructInfoFromExpr(TJSONSqliteExpr(cfgRec.JsonData));
          end;

          if TJSONSqliteExpr(cfgRec.JsonData).IsMalformed then
          begin
            stim := IMG_HALT;
          end else
          if TJSONSqliteExpr(cfgRec.JsonData).IsOK then
          begin
            if TJSONSqliteExpr(cfgRec.JsonData).IsCreateTable then
            begin
              if TJSONSqliteExpr(cfgRec.JsonData).CheckState(exstTableNotEqual) then
                stim := IMG_TABLE_NOT_EQUAL
              else
              if TJSONSqliteExpr(cfgRec.JsonData).CheckState(exstTableNotExists) then
                stim := IMG_TABLE_NOT_EXISTS else
                stim := IMG_TABLE;
            end else
              stim := IMG_COMMENT;
          end else
            stim := IMG_STRUCT_ELEMENT;


          if (Sender.StateImages <> nil) then
          begin
            StateImageRes := Sender.StateImages.ResolutionForPPI[Sender.StateImagesWidth, Sender.Font.PixelsPerInch, Sender.GetCanvasScaleFactor];
            if (stim >= 0) and (stim < Sender.StateImages.Count) then
            begin
              if PaintImages then
                StateImageRes.Draw(Sender.Canvas, NodeRect.Left + 1, NodeRect.Top + (NodeRect.Bottom - NodeRect.Top - StateImageRes.Height) div 2,
                  stim, True);
              Inc(NodeRect.Left, StateImageRes.Width + 2);
              Inc(NodeRect.Right, StateImageRes.Width + 2);
            end;
          end;
          Inc(NodeRect.Right, Sender.Canvas.TextWidth(Node.Text));
          case stim of
            IMG_HALT, IMG_COMMENT : begin
              DrawNodeText(Node.Selected, NodeRect, TJSONSqliteExpr(cfgRec.JsonData));
            end;
            IMG_TABLE :
             DrawNodeTextTableName(Node.Selected, NodeRect, Node.Text)
           else
             DrawNodeTextSimple(Node.Selected, NodeRect, Node.Text)
           end;
          Exit;
        end;
      end else
        Node.Text := cfgRec.DisplayStr;
    end;
    DefaultDraw := true;
  finally
    FConfig.UnLock;
  end;
end;

procedure TMain.ConfigTreeDeletion(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(Node.Data) then
      Node.FreeAllNodeData;
end;

procedure TMain.ConfigTreeEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
var cfgRec : TConfigRec;
begin
  if Assigned(Node.Data) then
  begin
    cfgRec := TConfigRec(Node.Data);
    if cfgRec.CfgType in [crtDataBaseName, crtProperty, crtStructElement] then
    begin
      if cfgRec.CfgType = crtStructElement then
      begin
        AllowEdit := false;
        if StartOpenEditSQLDialog(TConfigRec(Node.Data).JsonData) then
        begin
          cfgRec.JsonData.AsString := EditSQLDialog.ResultExpr;
          UpdateCfgStruct(FConfig.Struct.IndexOf(cfgRec.JsonData));
        end;
      end else begin
        Node.Text := cfgRec.JsonData.AsString ;
      end;
    end else AllowEdit := false;
  end;
end;

procedure TMain.ConfigTreeEditingEnd(Sender: TObject; Node: TTreeNode;
  Cancel: Boolean);
var cfgRec : TConfigRec;
    v, c : integer;
begin
  if Assigned(Node) and Assigned(Node.Data) then
  begin
    cfgRec := TConfigRec(Node.Data);
    case cfgRec.CfgType of
      crtStructElement:
      begin
        Node.Text := cfgRec.DisplayStr;
      end;
      crtProperty, crtDataBaseName :
      begin
        if cfgRec.JsonData is TJSONString then
        begin
          cfgRec.JsonData.AsString := Node.Text;
          ConfigChanged := true;
        end else
        if cfgRec.JsonData is TJSONIntegerNumber then
        begin
          Val(Node.Text, v, c);
          if c = 0 then
          begin
            cfgRec.JsonData.AsInteger := v;
            ConfigChanged := true;
          end;
        end;
        Node.Text := cfgRec.DisplayStr;
      end;
    end;
  end;
end;

procedure TMain.ConfigTreeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  //show popup
  if Button = mbRight then
  begin
    if ModifyCfgElement.Items.Count > 0 then
      ModifyCfgElement.PopUp;
  end;
end;

procedure TMain.OnAddToArray(Sender: TObject);
var cfgRec : TConfigRec;
    jStr : TJSONSqliteExpr;
    jObj : TJSONObject;
    cfgItem : TTreeNode;
    S : String;
begin
  if ExtractSendersData(Sender, cfgItem) then
  begin
    cfgRec := TConfigRec(cfgItem.Data);
    if assigned(cfgRec) then
    begin
      jStr := nil; jObj := nil;
      case cfgRec.CfgField of
        crfStructure : begin
          jStr := TJSONSqliteExpr.Create('');
        end;
        crfExtBlobs : begin
          jObj := TJSONObject.Create([JSON_CFG_KIND, DBHelper.JsonBlobKinds[0].Name]);
          S := JSON_CFG_EXTBLOBS+cObjPostfix;
        end;
        crfForeignIndx : begin
          jObj := TJSONObject.Create([JSON_CFG_KIND, DBHelper.JsonIndxStringsStyles[0].Name]);
          S := JSON_CFG_FORINDXS+cObjPostfix;
        end;
        crfIndx : begin
          jObj := TJSONObject.Create([JSON_CFG_KIND, DBHelper.JsonIndxClasses[0].Name]);
          S := JSON_CFG_INDXS+cObjPostfix;
        end;
        crfAttachedFuncs : begin
          jObj := TJSONObject.Create([JSON_CFG_KIND, DBHelper.JsonFuncClasses[0].Name]);
          S := JSON_CFG_FUNCS+cObjPostfix;
        end;
      end;
      if Assigned(jStr) then
      begin
        TJSONArray(cfgRec.JsonData).Add(jStr);
        if cfgRec.CfgField = crfStructure then
        begin
          AddConfigNodes(cfgItem, jStr, '', crtStructElement);
          UpdateCfgStruct(TJSONArray(cfgRec.JsonData).Count-1);
        end
        else
          AddConfigNodes(cfgItem, jStr, '', crtProperty);
        ConfigTreeSelectionChanged(nil);
        ConfigChanged := true;
      end else
      if Assigned(jObj) then
      begin
        TJSONArray(cfgRec.JsonData).Add(jObj);
        AddConfigNodes(cfgItem, jObj, S, crtObject);
        ConfigTreeSelectionChanged(nil);
        ConfigChanged := true;
      end;
    end;
  end;
end;

procedure TMain.OnAddTableToStruct(Sender : TObject);
var cfgRec : TConfigRec;
    jStr : TJSONSqliteExpr;
    cfgItem : TTreeNode;
    S : String;
    Expr : TSqliteExpr;
begin
  S := InputBox(Caption, rsSetNewTableName, '');
  if Length(S) > 0 then
  begin
    if ExtractSendersData(Sender, cfgItem) then
    begin
      cfgRec := TConfigRec(cfgItem.Data);
      if assigned(cfgRec) then
      begin
        Expr := TSqliteExpr.Create('');
        try
          Expr.AddKs([kwCREATE, kwTABLE, kwIF, kwNOT, kwEXISTS], skfoUpperCase);
          Expr.AddId(S);
          Expr.OpenBracket;
          Expr.AddId(cIdField);
          Expr.AddId(sqluAffinityToStr(dtaInteger));
          Expr.CloseBracket;
          jStr := TJSONSqliteExpr.Create(Expr.FormatedStr(skfoOriginal));
        finally
          Expr.Free;
        end;

        if Assigned(jStr) then
        begin
          TJSONArray(cfgRec.JsonData).Add(jStr);
          AddConfigNodes(cfgItem, jStr, '', crtStructElement);
          if StartOpenEditSQLDialog(jStr) then
            jStr.AsString := EditSQLDialog.ResultExpr;
          UpdateCfgStruct(FConfig.Struct.IndexOf(jStr));
          ConfigTreeSelectionChanged(nil);
          ConfigChanged := true;
        end;
      end;
    end;
  end;
end;

procedure TMain.AddArray(Sender: TObject; const Str : String);
var cfgRec : TConfigRec;
    jarr : TJSONArray;
    cfgItem : TTreeNode;
begin
  if ExtractSendersData(Sender, cfgItem) then
  begin
    cfgRec := TConfigRec(cfgItem.Data);
    if assigned(cfgRec) then
    if cfgRec.CfgType = crtRoot then
    begin
      jarr := FConfig.Section(Str);
      AddConfigNodes(cfgItem, jarr, Str, crtArray);
      ConfigTreeSelectionChanged(nil);
      ConfigChanged := true;
    end;
  end;
end;

procedure TMain.OnAddArray1(Sender : TObject);
begin
  AddArray(Sender, JSON_CFG_STRUCTURE);
end;

procedure TMain.OnAddArray2(Sender : TObject);
begin
  AddArray(Sender, JSON_CFG_EXTBLOBS);
end;

procedure TMain.OnAddArray3(Sender : TObject);
begin
  AddArray(Sender, JSON_CFG_INDXS);
end;

procedure TMain.OnAddArray4(Sender : TObject);
begin
  AddArray(Sender, JSON_CFG_FORINDXS);
end;

procedure TMain.OnRemoveFromArray(Sender: TObject);
var cfgRec : TConfigRec;
    cfgParent : TTreeNode;
    ind : integer;
    cfgItem : TTreeNode;
begin
  if ExtractSendersData(Sender, cfgItem) then
  begin
    cfgParent := cfgItem.Parent;
    cfgRec := TConfigRec(cfgItem.Data);
    if assigned(cfgRec) and assigned(cfgParent) then
    begin
      if cfgRec.JsonData is TJSONSqliteExpr then
      begin
        OnRemoveExpr(cfgRec.JsonData);
      end else
      begin
        ind := TJSONArray(TConfigRec(cfgParent.Data).JsonData).IndexOf(cfgRec.JsonData);

        if (ind >= 0) and
           (Application.MessageBox(PChar(Format(rsRemoveElement, [ind])),
                                     PChar(Application.Name),
                                     MB_ICONEXCLAMATION or MB_YESNO) = IDYES) then
        begin
          TJSONArray(TConfigRec(cfgParent.Data).JsonData).Delete(ind);
          cfgItem.Delete;
          ConfigChanged := true;
        end;
      end;
    end;
  end;
end;

procedure TMain.OnRemoveProperty(Sender: TObject);
var cfgRec : TConfigRec;
    cfgParent : TTreeNode;
    cfgItem : TTreeNode;
begin
  if ExtractSendersData(Sender, cfgItem) then
  begin
    cfgParent := cfgItem.Parent;
    cfgRec := TConfigRec(cfgItem.Data);
    if assigned(cfgRec) and assigned(cfgParent) then
    begin
      TJSONObject(TConfigRec(cfgParent.Data).JsonData).Remove(cfgRec.JsonData);
      cfgItem.Delete;
      ConfigChanged := true;
    end;
  end;
end;

procedure TMain.OnMoveUpInArray(Sender: TObject);
var cfgRec : TConfigRec;
    cfgParent : TTreeNode;
    ind : integer;
    cfgItem : TTreeNode;
begin
  if ExtractSendersData(Sender, cfgItem) then
  begin
    cfgParent := cfgItem.Parent;
    cfgRec := TConfigRec(cfgItem.Data);
    if assigned(cfgRec) and assigned(cfgParent) then
    begin
      ind := TJSONArray(TConfigRec(cfgParent.Data).JsonData).IndexOf(cfgRec.JsonData);

      if (ind > 0)  then
      begin
        TJSONArray(TConfigRec(cfgParent.Data).JsonData).Move(ind, ind - 1);
        if cfgRec.JsonData is TJSONSqliteExpr then
          UpdateCfgStruct(ind);
        cfgItem.Index := cfgItem.Index-1;
        ConfigChanged := true;
      end;
    end;
  end;
end;

procedure TMain.OnMoveDownInArray(Sender: TObject);
var cfgRec : TConfigRec;
    cfgParent : TTreeNode;
    ind : integer;
    cfgItem : TTreeNode;
begin
  if ExtractSendersData(Sender, cfgItem) then
  begin
    cfgParent := cfgItem.Parent;
    cfgRec := TConfigRec(cfgItem.Data);
    if assigned(cfgRec) and assigned(cfgParent) then
    begin
      ind := TJSONArray(TConfigRec(cfgParent.Data).JsonData).IndexOf(cfgRec.JsonData);

      if (ind < (TJSONArray(TConfigRec(cfgParent.Data).JsonData).Count-1)) and
         (ind >= 0)  then
      begin
        TJSONArray(TConfigRec(cfgParent.Data).JsonData).Move(ind, ind + 1);
        if cfgRec.JsonData is TJSONSqliteExpr then
          UpdateCfgStruct(ind);
        cfgItem.Index := cfgItem.Index+1;
        ConfigChanged := true;
      end;
    end;
  end;
end;

procedure TMain.OnAddField(Sender: TObject);
var cfgRec : TConfigRec;
    cfg, cfg0 : TConfigRec;
    aNode : TJSONData;
    aTreeNode, cfgItem : TTreeNode;
    i : integer;
begin
  if ExtractSendersData(Sender, cfg, cfgItem) then
  begin
    cfgRec := TConfigRec(cfgItem.Data);
    if assigned(cfgRec) then begin
      if (cfgRec.JsonData is TJSONObject) then
        aNode := TJSONObject(cfgRec.JsonData).Find(cfg.FName) else
        aNode := nil;
      if Assigned(aNode) then
      begin
        aTreeNode := nil;
        for i := 0 to cfgItem.Count-1 do
        begin
          cfg0 := TConfigRec(cfgItem[i].Data);
          if assigned(cfg0) then
            if SameText(cfg0.Name, cfg.Name) then
            begin
              aTreeNode := cfgItem[i];
              Break;
            end;
        end;
        if assigned(aTreeNode) then
        begin
          if cfg0.JsonData is TJSONString then
            TJSONString(cfg0.JsonData).AsString := cfg.JsonData.AsString else
          if cfg0.JsonData is TJSONIntegerNumber then
            TJSONIntegerNumber(cfg0.JsonData).AsInteger := cfg.JsonData.AsInteger;
          aTreeNode.Text := cfg0.DisplayStr;
          ConfigChanged := true;
        end;
      end
      else
      begin
        if cfg.JsonData is TJSONString then
          aNode := TJSONString.Create(cfg.JsonData.AsString) else
        if cfg.JsonData is TJSONIntegerNumber then
          aNode := TJSONIntegerNumber.Create(cfg.JsonData.AsInteger);

        TJSONObject(cfgRec.JsonData).Add(cfg.Name, aNode);
        AddConfigNodes(cfgItem, aNode, cfg.Name, cfg.CfgType);
        ConfigTreeSelectionChanged(nil);
        ConfigChanged := true;
      end;
    end;
  end;
end;

procedure TMain.OnExecuteExpr(Sender : TObject);
var cfgRec : TConfigRec;
    cfgItem : TTreeNode;
begin
  if ExtractSendersData(Sender, cfgItem) then
  begin
    cfgRec := TConfigRec(cfgItem.Data);
    if Assigned(cfgRec) and (cfgRec.JsonData is TJSONSqliteExpr) then
      OnExecuteExprLink(cfgRec.JsonData);
  end;
end;

procedure TMain.OnEditExpr(Sender : TObject);
var cfgRec : TConfigRec;
    cfgItem : TTreeNode;
begin
  if ExtractSendersData(Sender, cfgItem) then
  begin
    cfgRec := TConfigRec(cfgItem.Data);
    if Assigned(cfgRec) and (cfgRec.JsonData is TJSONSqliteExpr) then
      OnEditExprLink(cfgRec.JsonData);
  end;
end;

procedure TMain.OnWizard(Sender : TObject);
var wiz : TDBWizard;
begin
  if Sender is TMenuItem then
  begin
    wiz := GetWizardList[TMenuItem(Sender).Tag];
    if wiz.Launch(DataSet, FConfig, FStructure) then
    begin
      if dbwcConfig in wiz.Changes then
      begin
        ConfigChanged := true;
        RebuildConfigTree;
      end;
      if dbwcDatabase in wiz.Changes then
      begin
        RefreshDatabaseAndStruct;
      end else
      if dbwcStruct in wiz.Changes then
      begin
        ReloadConfigStructure;
      end;
    end;
  end;
end;

procedure TMain.OnEditExprLink(Expr : TJSONData);
begin
  if StartOpenEditSQLDialog(Expr) then
  begin
    Expr.AsString := EditSQLDialog.ResultExpr;
    UpdateCfgStruct(FConfig.Struct.IndexOf(Expr));
  end;
end;

procedure TMain.OnRemoveExpr(Expr : TJSONData);
var ind : integer;
    cfgItem : TTreeNode;
begin
  ind := FConfig.Struct.IndexOf(Expr);

  if (ind >= 0) and Assigned(FCfgStructNode) and
     (Application.MessageBox(PChar(Format(rsRemoveElement, [ind])),
                               PChar(Application.Name),
                               MB_ICONEXCLAMATION or MB_YESNO) = IDYES) then
  begin
    cfgItem := FindConfigNodeFromJson(Expr);
    FConfig.Struct.Delete(ind);
    if Assigned(cfgItem) then
      cfgItem.Delete;
    ConfigChanged := true;
    UpdateCfgStruct(ind);
  end;
end;

procedure TMain.OnSetExprFromDatabase(Expr : TJSONData);
var T : TDBTable;
    S : String;
begin
  if Assigned(FStructure) and (Expr is TJSONSqliteExpr) then
  begin
    T := FStructure.ByName(TJSONSqliteExpr(Expr).TableName);
    if Assigned(T) then
    begin
      S := T.BuildCreateExpression(true, skfoOriginal);
      if Length(S) > 0 then
      begin
        Expr.AsString := S;
        UpdateCfgStruct(FConfig.Struct.IndexOf(Expr));
      end;
    end;
  end;
end;

procedure TMain.OnModifyDatabaseExpr(Expr : TJSONData);
var
  AltTable : TDBTableAlterComp;
  T1, T2 : TDBTable;
  analiz : TDBSqliteSynAnalizer;
  ind : integer;
begin
  ind := FConfig.Struct.IndexOf(Expr);
  if ind >= 0 then
  begin
    analiz := TDBSqliteSynAnalizer.Create(TJSONSqliteExpr(Expr).Expr);
    try
      T2 := analiz.CreateTable(nil, []);
      if Assigned(T2) then
      begin
        try
          T1 := FStructure.ByName(T2.Name);
          if Assigned(T1) then
          begin
            AltTable := TDBTableAlterComp.Create(T2, T1);
            try
              AltTable.Compare;
              SQLEditor.Text := AltTable.AlterExpr;
              MainPages.ActivePage := SQLSheet;
            finally
              AltTable.Free;
            end;
          end;
        finally
          T2.Free;
        end;
      end;
    finally
      analiz.Free;
    end;
  end;
end;

procedure TMain.OnExecuteExprLink(Expr : TJSONData);
begin
  if TJSONSqliteExpr(Expr).IsOk then
  begin
    DataSet.ExecuteDirect(TJSONSqliteExpr(Expr).Expr.FormatedStr(skfoOriginal));
    if TJSONSqliteExpr(Expr).IsCreateTable then
    begin
      FStructureChanged := true;
      GenerateStructure;
      UpdateCfgStruct(FConfig.Struct.IndexOf(Expr));
    end;
  end;
end;

function TMain.ExtractSendersData(Sender : TObject; out cfgRec : TConfigRec;
  out cfgItem : TTreeNode) : Boolean;
begin
  if Sender is TCfgMenuItem then begin
      cfgRec :=  TCfgMenuItem(Sender).CfgRec;
      cfgItem := TCfgMenuItem(Sender).CfgItem;
  end else
  if Sender is TCfgToolButton then begin
      cfgRec := TCfgToolButton(Sender).CfgRec;
      cfgItem := TCfgToolButton(Sender).CfgItem;
  end else
  begin
      cfgRec := nil;
      cfgItem := nil;
  end;
  Result := Assigned(cfgRec) and Assigned(cfgItem);
end;

function TMain.ExtractSendersData(Sender : TObject; out cfgItem : TTreeNode
  ) : Boolean;
begin
  if Sender is TCfgMenuItem then begin
      cfgItem := TCfgMenuItem(Sender).CfgItem;
  end else
  if Sender is TCfgToolButton then begin
      cfgItem := TCfgToolButton(Sender).CfgItem;
  end else
  begin
      cfgItem := nil;
  end;
  Result := Assigned(cfgItem);
end;

procedure TMain.UpdateStructInfoFromExpr(expr : TJSONSqliteExpr);
begin
  ClearStructVarLinks;
  if expr.CheckState(exstCreateTable) then
  begin
    if expr.CheckState(exstMalformed) then
    begin
      StructInfo.Text := Format(rsTableMalformed, [expr.ParseError]);
      AddStructVarLink(rsEdit, expr, @OnEditExprLink);
      AddStructVarLink(rsRemove, expr, @OnRemoveExpr);
    end else
    if expr.CheckState(exstTableNotEqual) then
    begin
      StructInfo.Text := Format(rsTableNotEqual, [expr.TableName]);
      AddStructVarLink(rsSetFromDatabase, expr, @OnSetExprFromDatabase);
      AddStructVarLink(rsModifyDatabase, expr, @OnModifyDatabaseExpr);
    end
    else
    if expr.CheckState(exstTableNotExists) then
    begin
      StructInfo.Text := Format(rsTableNotExists, [expr.TableName]);
      AddVarLink(rsSaveConfig, @SaveCfgClick);
      AddStructVarLink(rsExecute, expr, @OnExecuteExprLink);
      AddStructVarLink(rsRemove, expr, @OnRemoveExpr);
    end
    else
    begin
      StructInfo.Text := '';
    end;
  end else begin
    StructInfo.Text := '';
  end;
end;

function TMain.FindConfigNodeFromJson(jsonData : TJSONData) : TTreeNode;

function FindInTreeNode(tn : TTreeNode) : TTreeNode;
var n : TTreeNode;
begin
  n := tn.GetFirstChild;
  while Assigned(n) do
  begin
    if Assigned(n.Data) and
       (TConfigRec(n.Data).JsonData = jsonData) then
    begin
      Result := n;
      Exit;
    end else
    begin
      Result := FindInTreeNode(n);
      if Assigned(Result) then Exit;
      n := n.GetNextSibling;
    end;
  end;
  Result := nil;
end;

var i : integer;
begin
  for i := 0 to ConfigTree.Items.Count-1 do
  begin
    Result := FindInTreeNode(ConfigTree.Items[i]);
    if Assigned(Result) then Exit;
  end;
  Result := nil;
end;

procedure TMain.ClearStructVarLinks;
var i : integer;
begin
  for i := StructPanel.ControlCount-1 downto 0 do
  if StructPanel.Controls[i] is TStructVarLink then
    StructPanel.RemoveControl(StructPanel.Controls[i]);
end;

procedure TMain.AddVarLink(const aCaption : String; aOnClick : TNotifyEvent);
var
  C : TStructVarLink;
begin
  C := TStructVarLink.Create(StructPanel, aCaption, aOnClick);
  C.Left := 3;
  C.Top := StructPanel.Height;
  C.Align := alBottom;
  C.Parent := StructPanel;
end;

procedure TMain.AddStructVarLink(const aCaption : String;
  expr : TJSONData; aOnClick : TOnExprVarClick);
var
  C : TStructVarLink;
begin
  C := TStructVarLink.Create(StructPanel, expr, aCaption, aOnClick);
  C.Left := 3;
  C.Top := StructPanel.Height;
  C.Align := alBottom;
  C.Parent := StructPanel;
end;

procedure TMain.ConfigTreeSelectionChanged(Sender: TObject);

procedure AddNewRecordOption(const aCaption : String; aImInd : Integer;
  donClick : TNotifyEvent; data : TScissorConfigRec = nil);
var
  It : TCfgMenuItem;
  Btn : TCfgToolButton;
  lastbtnidx : integer;
begin
  It := TCfgMenuItem.Create(nil);
  It.Caption := aCaption;
  It.ImageIndex := aImInd;
  It.OnClick := donClick;
  It.CfgRec := data;
  It.CfgItem := ConfigItem;
  ModifyCfgElement.Items.Add(It);
  Btn := TCfgToolButton.Create(ToolBar3);
  Btn.Hint := aCaption;
  Btn.ImageIndex := aImInd;
  lastbtnidx := ToolBar3.ButtonCount - 1;
  if lastbtnidx > -1 then
    Btn.Left := ToolBar3.Buttons[lastbtnidx].Left + ToolBar3.Buttons[lastbtnidx].Width + 10
  else
    Btn.Left := 0;
  if assigned(data) then begin
    Btn.CfgRec := TScissorConfigRec.Create(data);
    Btn.CfgItem := ConfigItem;
    Btn.AutoSize := true;
    Btn.ShowCaption := true;
    Btn.Caption := aCaption;
  end
  else begin
    Btn.CfgRec := nil;
    Btn.CfgItem := ConfigItem;
  end;
  Btn.Parent := ToolBar3;
  Btn.OnClick := donClick;
end;

var
  cfgRec : TConfigRec;
  jKindStr : TJSONData;

procedure AddNewRecordPropFixed(aData : TCfgField);
var jStr, cData : TJSONData;
    cfgR : TScissorConfigRec;
begin
  jStr := TJSONObject(cfgRec.JsonData).Find(aData.Name);
  if not Assigned(jStr) then
  begin
    if VarIsStr(aData.Value) then
      cData := TJSONString.Create(aData.Value) else
    if VarIsOrdinal(aData.Value) then
      cData := TJSONIntegerNumber.Create(aData.Value) else
      cData := nil;

    cfgR := TScissorConfigRec.Create(aData.Name, crtProperty, crfUnknown, cData);
    AddNewRecordOption(Format(rsMenuAddS, ['"'+aData.Name+'"']),
                                          IMG_STRUCT_ELEMENT, @OnAddField, cfgR);
  end;
end;

procedure AddKindMenu(aEnums : TJsonCfgEnums; CfgItem : TTreeNode);
var k : integer;
    It, It0, It1 : TCfgMenuItem;
    Btn : TCfgToolButton;
    lastbtnidx : integer;
begin
  It0 := TCfgMenuItem.Create(nil);
  It0.Caption := JSON_CFG_KIND;
  ModifyCfgKind.Items.Clear;
  for k := 0 to aEnums.Count-1 do
  begin
    It := TCfgMenuItem.Create(nil);
    It.Caption := aEnums[k].Name;
    It.ImageIndex := IMG_STRUCT_ELEMENT;
    It.OnClick := @OnAddField;
    It.CfgRec := TScissorConfigRec.Create(JSON_CFG_KIND,
                                          crtProperty,
                                          crfUnknown,
                                          TJSONString.Create(aEnums[k].Name));
    It.CfgItem := CfgItem;
    ModifyCfgKind.Items.Add(It);
    It1 := TCfgMenuItem.Create(nil);
    It1.Assign(It);
    It0.Add(It1);
  end;
  ModifyCfgElement.Items.Add(It0);
  Btn := TCfgToolButton.Create(ToolBar3);
  Btn.Caption := JSON_CFG_KIND;
  Btn.Hint := JSON_CFG_KIND;
  Btn.Style := tbsDropDown;
  Btn.DropdownMenu := ModifyCfgKind;
  Btn.ImageIndex := IMG_STRUCT_ELEMENT;
  lastbtnidx := ToolBar3.ButtonCount - 1;
  if lastbtnidx > -1 then
    Btn.Left := ToolBar3.Buttons[lastbtnidx].Left + ToolBar3.Buttons[lastbtnidx].Width + 10
  else
    Btn.Left := 0;
  Btn.CfgRec := nil;
  Btn.Parent := ToolBar3;
end;

procedure AddNewRecordPropListed(aEnums : TJsonCfgEnums);
var k, l : integer;
begin
  if aEnums.Count > 0 then
    AddKindMenu(aEnums, ConfigItem);

  for l := 0 to aEnums.FieldsCnt-1 do
  begin
    AddNewRecordPropFixed(aEnums.Field[l]);
  end;
  // addition fields
  if assigned(jKindStr) then
  begin
    for k := 0 to aEnums.Count-1 do
    if SameText(jKindStr.AsString, aEnums[k].Name) then
      for l := 0 to aEnums[k].AddFieldsCnt-1 do
        AddNewRecordPropFixed(aEnums[k].AddField[l]);
  end;
end;

var i : integer;
  cfgParentNode, cfgParentParentNode : TTreeNode;
  cfgParent, cfgParentParent : TConfigRec;
begin
  ModifyCfgElement.Items.Clear;
  for i := ToolBar3.ControlCount-1 downto NormalTB3ButtonsCnt do
  begin
    ToolBar3.RemoveControl(ToolBar3.Controls[i]);
  end;

  ConfigItem := ConfigTree.Selected;

  ClearStructVarLinks;
  StructInfo.Text := '';
  if Assigned(ConfigItem) then
  begin
    cfgRec := TConfigRec(ConfigItem.Data);
    if assigned(cfgRec) then
    begin
      cfgParentNode := ConfigItem.Parent;
      if Assigned(cfgParentNode) then
      begin
        cfgParent := TConfigRec(cfgParentNode.Data);
      end else cfgParent := nil;
      if cfgRec.CfgType = crtArray then
      begin
        AddNewRecordOption(rsMenuAdd, IMG_PLUS, @OnAddToArray);
        if cfgRec.JsonData = FConfig.Struct then
        begin
          AddNewRecordOption(rsMenuAddT, IMG_TABLEADD, @OnAddTableToStruct);
        end;
      end else
      begin
        if Assigned(cfgParent)then
        begin
          if cfgParent.CfgType = crtArray then
          begin
            if cfgRec.JsonData is TJSONSqliteExpr then
            begin
              if TJSONSqliteExpr(cfgRec.JsonData).IsOk then
                AddNewRecordOption(rsMenuRun, IMG_RUN, @OnExecuteExpr);
              AddNewRecordOption(rsMenuEdit, IMG_EDIT, @OnEditExpr);
              UpdateStructInfoFromExpr(TJSONSqliteExpr(cfgRec.JsonData));
            end;

            AddNewRecordOption(rsMenuDelete, IMG_MINUS, @OnRemoveFromArray);
            AddNewRecordOption(rsMenuMoveUp, IMG_UP_ARROW, @OnMoveUpInArray);
            AddNewRecordOption(rsMenuMoveDown, IMG_DWN_ARROW, @OnMoveDownInArray
              );
          end else
          if cfgRec.CfgType = crtProperty then
          begin
            AddNewRecordOption(rsMenuDelete, IMG_MINUS, @OnRemoveProperty);
            jKindStr := TJSONObject(cfgParent.JsonData).Find(JSON_CFG_KIND);

            cfgParentParentNode := cfgParentNode.Parent;
            if Assigned(cfgParentParentNode) then
            begin
              cfgParentParent := TConfigRec(cfgParentParentNode.Data);
            end else cfgParentParent := nil;

            if assigned(cfgParentParent) then
            begin
              if (jKindStr = cfgRec.JsonData) then
              case cfgParentParent.CfgField of
                crfExtBlobs :
                  AddKindMenu(DBHelper.JsonBlobKinds, cfgParentNode);
                crfForeignIndx:
                  AddKindMenu(DBHelper.JsonIndxStringsStyles, cfgParentNode);
                crfIndx :
                  AddKindMenu(DBHelper.JsonIndxClasses, cfgParentNode);
                crfAttachedFuncs :
                  AddKindMenu(DBHelper.JsonFuncClasses, cfgParentNode);
              end;
            end;
          end;
          if cfgRec.CfgType = crtObject then
          begin
            jKindStr := TJSONObject(cfgRec.JsonData).Find(JSON_CFG_KIND);
            case cfgParent.CfgField of
              crfExtBlobs :
              begin
                AddNewRecordPropListed(DBHelper.JsonBlobKinds);
              end;
              crfForeignIndx:
              begin
                AddNewRecordPropListed(DBHelper.JsonIndxStringsStyles);
              end;
              crfIndx :
              begin
                AddNewRecordPropListed(DBHelper.JsonIndxClasses);
              end;
              crfAttachedFuncs :
              begin
                AddNewRecordPropListed(DBHelper.JsonFuncClasses);
              end;
            end;
          end;
        end else begin
          if cfgRec.CfgType = crtRoot then
          begin
            if FConfigChanged then
            begin
              StructInfo.Text := rsChangedNeedToBeSaved;
              AddVarLink(rsSaveConfig, @SaveCfgClick);
            end;

            if not Assigned(FConfig.Struct) then
            begin
              AddNewRecordOption(Format(rsMenuAddS, [JSON_CFG_STRUCTURE]),
                IMG_PLUS, @OnAddArray1);
            end;
            if not Assigned(FConfig.Find(JSON_CFG_EXTBLOBS)) then
            begin
              AddNewRecordOption(Format(rsMenuAddS, [JSON_CFG_EXTBLOBS]),
                IMG_PLUS, @OnAddArray2);
            end;
            if not Assigned(FConfig.Find(JSON_CFG_INDXS)) then
            begin
              AddNewRecordOption(Format(rsMenuAddS, [JSON_CFG_INDXS]),
                IMG_PLUS, @OnAddArray3);
            end;
            if not Assigned(FConfig.Find(JSON_CFG_FORINDXS)) then
            begin
              AddNewRecordOption(Format(rsMenuAddS, [JSON_CFG_FORINDXS]),
                IMG_PLUS, @OnAddArray4);
            end;
          end;
        end;
      end;
    end;
  end;

end;

procedure TMain.SetFontParamsFromCompletionObj(O : TCompletionObj;
                                                 F : TFont; out imi : integer);
var
  Attr : TSynHighlighterAttributes;
begin
  attr := nil;
  if Assigned(O) then
  begin
    case O.Kind of
      sckKeyword : begin
        Attr :=  FSynSQLSyn.KeyAttri;
        imi := IMG_CONFIG;
      end;
      sckFunction : begin
        Attr :=  FSynSQLSyn.FunctionAttri;
        imi := IMG_STRUCT_ELEMENT;
      end;
      sckType : begin
        Attr :=  FSynSQLSyn.DataTypeAttri;
        imi := IMG_DATA_TYPE;
      end;
      sckTable : begin
        Attr :=  FSynSQLSyn.TableNameAttri;
        imi := IMG_TABLE;
      end;
      sckField : begin
        Attr :=  FSynSQLSyn.FieldNameAttri;
        imi := IMG_STRUCT_ELEMENT;
      end;
    end;
  end;
  if Assigned(Attr) then
  begin
    F.Color := Attr.Foreground;
    if (F.Color = clDefault) or (F.Color = clNone) then
       F.Color := clBlack;
    F.Style := Attr.Style;
  end;
end;

procedure TMain.LogEnabledCBChange(Sender: TObject);
begin
  FLogEnabled.Value := LogEnabledCB.Checked;
end;

procedure TMain.ChooseTableDblClick(Sender : TObject);
var S : String;
begin
  if ChooseTable.ItemIndex < 0 then Exit;
  S := ChooseTable.Items[ChooseTable.ItemIndex];

  if MainPages.ActivePage = SQLSheet then
  begin
    DBGrid.ExecuteTable(S);
  end else
  if MainPages.ActivePage = ModSheet then
  begin
    if Assigned(FModTables.CurTable) then
    begin
      if not sqluCompareNames(FModTables.CurTable.RecordTable, S) then
      begin
        if FModTables.Count > 1 then
        begin
          if Application.MessageBox(PChar(Format(rsTableWasChanged,
                                     [FModTables.FirstTable.RecordTable])),
                                       PChar(Application.Name),
                                       MB_ICONEXCLAMATION or
                                       MB_YESNO) = IDNO then
          begin
            ChooseTable.ItemIndex := ChooseTable.Items.IndexOf(FModTables.CurTable.RecordTable);
            Exit;
          end;
        end else
        if FModTables.CurTable.RecordChanged then
        begin
          if Application.MessageBox(PChar(Format(rsTableWasChanged,
                                     [FModTables.CurTable.RecordTable])),
                                       PChar(Application.Name),
                                       MB_ICONEXCLAMATION or
                                       MB_YESNO) = IDNO then
          begin
            ChooseTable.ItemIndex := ChooseTable.Items.IndexOf(FModTables.CurTable.RecordTable);
            Exit;
          end;
        end;
      end else Exit;
    end;
    FModTables.ProceedTable(S, -1, false, true);
  end;
end;

procedure TMain.DBGridDblClick(Sender : TObject);
var id : integer;
begin
  if (DBGrid.ColCount > 0) and (DBGrid.RowCount > 1) and
     (DBGrid.Row > 0) and DBGrid.IsTableBrowsing then
  begin
    id := DBGrid.IdAtRow[DBGrid.Row - 1];
    MainPages.PageIndex := ModSheet.PageIndex;
    FModTables.ProceedTable(DBGrid.CurSelectedTable, id, false, true);
  end;
end;

procedure TMain.FormHide(Sender: TObject);
begin
  Timer1.Enabled := False;
end;

procedure TMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Timer1.Enabled := false;
end;

procedure TMain.FormDestroy(Sender : TObject);
begin
  DBGrid.DB := nil;
  FModTables.Free;
  DataSet.Free;
  FTableList.Free;
  vDefaultTokenVisStyles.Free;
  FLogChanged.Free;
  FLogEnabled.Free;
  FLogPull.Free;
  LoggerExport.Free;
  FCompletionKeys.Free;
  Indxs.Free;
  if Assigned(FConfig) then FConfig.Free;
  FStructure.Free;
end;

procedure TMain.FormShow(Sender : TObject);
var i : integer;
  MI : TMenuItem;
begin
  ChooseIdDlg.DataSet := DataSet;
  ChooseIdDlg.DB := FStructure;
  ChooseIdDlg.OnLoadTemplates := @ReadFilters;
  ChooseIdDlg.OnWriteTemplate := @WriteFilters;

  WizardsMenu.Items.Clear;
  for i := 0 to GetWizardList.Count-1 do
  begin
    MI := TMenuItem.Create(nil);
    MI.Caption := GetWizardList[i].FriendlyName;
    MI.ImageIndex := IMG_WIZARD;
    MI.OnClick := @OnWizard;
    MI.Tag := i;
    WizardsMenu.Items.Add(MI);
  end;


  Timer1.Enabled := True;
end;

procedure TMain.NewCfgClick(Sender : TObject);
var S : String;
    SL : TStringList;
    aNewCfg : TDBJSONConfig;
    FS : TFileStream;
begin
  if SaveDBDialog.Execute then
  begin
    if FileExists(SaveDBDialog.FileName) then
    begin
      if (Application.MessageBox(PChar(Format(rsDBExistsDelete,
                                 [SaveDBDialog.FileName])),
                                   PChar(Application.Name),
                                   MB_ICONEXCLAMATION or MB_YESNO) = IDNO) then
         Exit else
         DeleteFile(SaveDBDialog.FileName);
    end;
    S := ChangeFileExt(SaveDBDialog.FileName, cDBExt);
    aNewCfg := TDBJSONConfig.Create();
    try
      aNewCfg.Add(JSON_CFG_DATABASE, TJSONString.Create(ExtractFileName(S)));
      aNewCfg.Section(JSON_CFG_STRUCTURE);
      aNewCfg.Section(JSON_CFG_EXTBLOBS);
      aNewCfg.Section(JSON_CFG_INDXS);
      aNewCfg.Section(JSON_CFG_FORINDXS);
      SL := TStringList.Create;
      FS := TFileStream.Create(SaveDBDialog.FileName, fmOpenWrite or fmCreate);
      try
        SL.Text := aNewCfg.AsJSON;
        SL.SaveToStream(FS);
      finally
        FS.Free;
        SL.Free;
      end;
    finally
      aNewCfg.Free;
    end;
    ReloadConfig(SaveDBDialog.FileName);
  end;
end;

type
  TSqliteTransactionKind = (stkBeginCommit, stkSavePointRelease);

  TSqliteTransaction = class
  public
    Kind : TSqliteTransactionKind;
    Id   : TSqliteToken;
  end;

  TSqliteTransactions = class(specialize TThreadSafeFastBaseSeq<TSqliteTransaction>)
  public
    function FindSavepoint(id : TSqliteToken) : TSqliteTransaction;
  end;

{ TSqliteTransactions }

function TSqliteTransactions.FindSavepoint(id : TSqliteToken
  ) : TSqliteTransaction;
var it : TIteratorObject;
    t : TSqliteTransaction;
begin
  it := ListBegin;
  while Assigned(it) do
  begin
    t := TSqliteTransaction(it.Value);
    if Assigned(t.Id) and t.Id.Compare(id) then
    begin
      Exit(t);
    end else
      it := it.Next;
  end;
  Result := nil;
end;

procedure TMain.ExecButtonClick(Sender : TObject);
var
  si, i_torun : integer;
  flag : Boolean;

function DoExec(i : integer; const S : String) : Boolean;
begin
  try
    if DataSet.CheckExprIsReadOnly(S) and (i_torun = i) then
      DBGrid.ExecuteSQL(S)
    else
      DataSet.ExecSQL(S);
  except
    on e : EDatabaseError do
    begin
      LogMemo.Lines.Add(e.ToString);
      si := i;
      flag := false;
    end;
  end;
  Result := flag;
end;

var
  i, k : integer;
  Exprs : TSqliteExprs;
  transactions : TSqliteTransactions;
  transaction : TSqliteTransaction;
  tok : TSqliteToken;
  aStmt : TSqliteStmt;
  needupddb : Boolean;
begin
  needupddb := false;
  Exprs := TSqliteExprs.Create(SQLEditor.Text);
  try
    flag := true;
    if Exprs.Count > 1 then
    begin
      Exprs.Insert(0, TSqliteExpr.Create(sqluGetIndexedKeyWord(kwBEGIN)));
      Exprs.Add(TSqliteExpr.Create(sqluGetIndexedKeyWord(kwCOMMIT)));
      transaction := nil;
      transactions := TSqliteTransactions.Create;
      try
        i_torun := -1;
        for i := 0 to Exprs.Count-1 do
        begin
          aStmt := TSqliteSynAnalizer.FindStmtForExpr(Exprs[i]);
          case aStmt of
          stmtBegin :
              begin
                if assigned(transactions.LastValue) then
                begin
                  flag := false;
                  si := i;
                  LogMemo.Lines.Add(rsTransactionWithinTransaction);
                  Break;
                end;
                transaction  := TSqliteTransaction.Create;
                transaction.Kind := stkBeginCommit;
                transaction.Id   := nil;
                transactions.Push_front(transaction);
              end;
          stmtSavepoint:
              begin
                transaction  := TSqliteTransaction.Create;
                transaction.Kind := stkSavePointRelease;
                k := 0;
                transaction.Id   := Exprs[i].NextToken(k, [stkIdentifier]);
                transactions.Push_front(transaction);
              end;
          stmtRelease:
              begin
                if transactions.Count = 0 then
                begin
                  flag := false;
                  si := i;
                  LogMemo.Lines.Add(rsNoOpenedTransactions);
                  Break;
                end else
                begin
                  k := 0;
                  tok := Exprs[i].NextToken(k, [stkIdentifier]);
                  if assigned(tok) then
                  begin
                    transaction := transactions.FindSavepoint(tok);
                    if not assigned(transaction) then
                    begin
                      flag := false;
                      si := i;
                      LogMemo.Lines.Add(Format(rsNoSuchSavepoint, [tok.Token]));
                      Break;
                    end else
                    begin
                      while Assigned(transactions.ListBegin) do
                      begin
                        if transactions.ListBegin.Value <> transaction then
                          transactions.Erase(transactions.ListBegin) else
                        begin
                          transactions.Erase(transactions.ListBegin);
                          break;
                        end;
                      end;
                    end;
                  end;
                end;
              end;
          stmtCommit:
              transactions.Clean;
          stmtRollback:
              begin
                k := 0;
                tok := Exprs[i].NextToken(k, [stkIdentifier]);
                if assigned(tok) then
                begin
                  transaction := transactions.FindSavepoint(tok);
                  if not assigned(transaction) then
                  begin
                    flag := false;
                    si := i;
                    LogMemo.Lines.Add(Format(rsNoSuchSavepoint, [tok.Token]));
                    Break;
                  end else
                  begin
                    while Assigned(transactions.ListBegin) do
                    begin
                      if transactions.ListBegin.Value <> transaction then
                        transactions.Erase(transactions.ListBegin) else
                        break;
                    end;
                  end;
                end else
                begin
                  transactions.Clean;
                end;
              end;
          stmtSelect:
              i_torun := i;
          stmtCreateTable, stmtAlterTable, stmtDropTable :
              needupddb := true;
          end;
        end;
        if (transactions.Count > 0) and flag then begin
          si := -1;
          LogMemo.Lines.Add(rsNotClosedTransactions);
          flag := false;
        end;
      finally
        transactions.Free;
      end;

      if flag then
      begin
        for i := 0 to Exprs.Count-1 do
        begin
          if not DoExec(i, Exprs[i].OrigExpr) then
          begin
            try
              DataSet.ExecSQL(sqluGetIndexedKeyWord(kwROLLBACK));
            except
              on e : EDatabaseError do ;//nothing
            end;
            break;
          end;
        end;
      end;
    end else
    if Exprs.Count > 0 then
    begin
      if TDBSqliteSynAnalizer.CheckIsBegin(Exprs[0]) then
      begin
        LogMemo.Lines.Add(rsMalformedTransaction);
      end else
        DBGrid.ExecuteSQL(Exprs.OrigExpr);

      aStmt := TSqliteSynAnalizer.FindStmtForExpr(Exprs[0]);
      if aStmt in [stmtCreateTable, stmtAlterTable, stmtDropTable] then
          needupddb := true;
    end;

    if (not flag) and (si >= 0 ) then
    begin
      SQLEditor.SelStart := Exprs[si].Pos;
      SQLEditor.SelEnd := Exprs[si].Pos + Exprs[si].Len;
    end;
  finally
    Exprs.Free;
  end;
  if needupddb then
  begin
    RefreshDatabaseAndStruct;
  end;
end;

procedure TMain.ConfirmButtonClick(Sender : TObject);
var S, ES : String;
begin
  S := ''; ES := '';
  if Assigned(FModTables.CurTable) then
  begin
    if FModTables.CurTable.Apply(S, ES) then
    begin
      if Length(S) > 0 then
        DataSet.ExecuteDirect(S);
      if (FModTables.CurTable.Mode = mtmAddNew) then
      begin
        S := DataSet.QuickQuery('select max('+FModTables.CurTable.Table.PrimaryKeyField.QuotedName+') from ' +
                                        sqluQuotedIdIfNeeded(FModTables.CurTable.RecordTable), nil, false);
        if Length(S) > 0 then
        begin
          LogMemo.Lines.Add(Format(rsRecordSuccessAdded, [FModTables.CurTable.RecordTable, StrToInt(S)]));
          if FModTables.Count > 1 then
            FModTables.Back(StrToInt(S)) else
          if FModTables.Count = 1 then
            FModTables.CurTable.Reset;
        end;
      end else
      if (FModTables.CurTable.Mode = mtmEdit) then
      begin
        if Length(S) > 0 then
        begin
          LogMemo.Lines.Add(Format(rsRecordSuccessUpdated,
                                        [FModTables.CurTable.EditingKeyId,
                                         FModTables.CurTable.RecordTable]));

          FStructure.NotifyTableUpdated(FStructure.ByName(FModTables.CurTable.RecordTable),
                                            FModTables.CurTable.EditingKeyId);
        end;

        if FModTables.Count > 1 then
          FModTables.Back(FModTables.CurTable.EditingKeyId) else
        if FModTables.Count = 1 then
          FModTables.CurTable.SetCurAsDefaultAndReset;
      end;
    end else
      Application.MessageBox(PChar(Format(rsValuesAreEmpty, [ES])),
                                   PChar(Application.Name),
                                   MB_ICONEXCLAMATION or MB_OK);
  end;
end;

procedure TMain.SQLEditorChange(Sender : TObject);
begin
  CompleteHint.EditorChange(Sender);
end;

procedure TMain.SQLEditorCommandProcessed(Sender : TObject;
  var Command : TSynEditorCommand; var AChar : TUTF8Char; Data : pointer);
begin
  CompleteHint.EditorCommandProcessed(Sender, Command, AChar, Data);
end;

procedure TMain.SQLEditorProcessCommand(Sender : TObject;
  var Command : TSynEditorCommand; var AChar : TUTF8Char; Data : pointer);
begin
  CompleteHint.EditorProcessCommand(Sender, Command, AChar, Data);
end;

procedure TMain.SQLEditorStatusChange(Sender : TObject;
  Changes : TSynStatusChanges);
begin
  CompleteHint.EditorStatusChange(Sender, Changes);
end;

procedure TMain.SynCompletion1Execute(Sender: TObject);
var i : integer;
begin
  FCompletionKeys.CompleteFillList(SynCompletion1.ItemList,
                                   SynCompletion1.CurrentString,
                                   i);
  SynCompletion1.Position := i;
end;

function TMain.SynCompletion1MeasureItem(const AKey: string; ACanvas: TCanvas;
  Selected: boolean; Index: integer): TPoint;
begin
  Result := Point(ACanvas.Width, 18);
end;

function TMain.SynCompletion1PaintItem(const AKey: string; ACanvas: TCanvas; X,
  Y: integer; Selected: boolean; Index: integer): boolean;
var
  ts : TTextStyle;
  S  : String;
  aRect  : TRect;
  O  : TCompletionObj;
  B : TBitmap;
  imi : integer;
begin
  if SynCompletion1.ItemList.Count > Index then
  with ACanvas do
  begin
    ts := TextStyle;
    ts.Alignment := taLeftJustify;
    ts.Layout := tlCenter;
    ts.Wordbreak := True;
    ts.SingleLine := false;
    ts.Opaque := false;

    brush.Color := clWhite;
    Pen.Style := psClear;

    imi := -1;
    O := TCompletionObj(SynCompletion1.ItemList.Objects[Index]);
    SetFontParamsFromCompletionObj(O, Font, imi);
    if Assigned(O) then
      S := O.Value
    else
      S := AKey;
    aRect := Rect(X, Y, Width, Y + 16);
    if Selected then
    begin
      brush.Color := cSelectedItemBackGround;
      font.Color := clWhite;
      pen.Color := clYellow;
      pen.Style := psDot;
    end;
    Rectangle(aRect);
    if imi >= 0 then begin
      B := TBitmap.Create;
      try
        B.Width := 16;
        B.Height := 16;
        ImageList1.Getbitmap(imi, B);
        ACanvas.Draw(X, Y, B);
      finally
        B.Free;
      end;
      aRect.Left := aRect.Left + 16;
    end;
    ACanvas.TextRect(aRect, aRect.Left+2, aRect.Top + 2, S, ts);
  end;
  Result := true;
end;

procedure TMain.Timer1Timer(Sender: TObject);
var SL : TStringList;
    SS : TStringStream;
    FDT : TDateTime;
    i : integer;
    flag : Boolean;
begin
  FModTables.DoTimerTick;
  FStructure.DoTimerTick;
  DBGrid.DoIdle;
  ChooseIdDlg.DoIdle;
  if Assigned(FConfig) then
  begin
    if FileAge(FConfigFileName, FDT) then
    begin
      if (Double(FDT) - Double(FConfigFileLoaded)) > 0 then
      begin
        RefreshCfg.Enabled := true;
      end else
        RefreshCfg.Enabled := false;
    end;
    FConfig.Lock;
    try
      flag := false;
      if Assigned(FConfig.Struct) then
      begin
        for i := 0 to FConfig.Struct.Count-1 do
        begin
          if FConfig.StructExpr[i].NeedRepaint then
          begin
            flag := true;
            break;
          end;
        end;
      end;
    finally
      FConfig.UnLock;
    end;
    if flag then begin
      ConfigTree.Invalidate;
      if Assigned(ConfigTree.Selected) and
         Assigned(ConfigTree.Selected.Data) and
         (TConfigRec(ConfigTree.Selected.Data).JsonData is TJSONSqliteExpr) then
        UpdateStructInfoFromExpr(TJSONSqliteExpr(TConfigRec(ConfigTree.Selected.Data).JsonData));
    end;
  end;
  if FLogChanged.Value then begin
    SL := TStringList.Create;
    try
      SL.Text := FLogPull.Text;
      LoggerExport.ExportAll(SL);
      SS := TStringStream.Create('');
      try
        LoggerExport.SaveToStream(SS);
        SL.Text := SS.DataString;
      finally
        SS.Free;
      end;
      SQLLog.Lines.AddStrings(SL);
    finally
      SL.Free;
    end;
    SQLLog.TopLine := SQLLog.Lines.Count;
    FLogPull.Clear;
    FLogChanged.Value := false;
  end;
end;

procedure TMain.SaveCfgAsClick(Sender : TObject);
begin
  if SaveDBDialog.Execute then
  begin
    if not SameFileName(FConfigFileName, SaveDBDialog.FileName) then
    begin
      if FileExists(SaveDBDialog.FileName) then
      begin
        if  (Application.MessageBox(PChar(Format(rsDBExistsDelete,
                                    [SaveDBDialog.FileName])),
                                     PChar(Application.Name),
                                     MB_ICONEXCLAMATION or MB_YESNO) = IDYES) then
        begin
          DeleteFile(SaveDBDialog.FileName);
        end else
          Exit;
      end;
      SetConfigFileName(SaveDBDialog.FileName);
    end;
    SaveConfig;
  end;
end;

procedure TMain.SaveCfgClick(Sender : TObject);
begin
  SaveConfig;
end;

procedure TMain.ComposeRequestClick(Sender: TObject);
begin
  if OpenEditSQLDialog(SQLEditor.Text, [], DataSet) then
    SQLEditor.Text := EditSQLDialog.ResultExpr;
end;

procedure TMain.EditExprRecordClick(Sender : TObject);
var
  S, S0 : String;
  i, p: integer;
  e: string;
  LineEndLen: Integer;
  RowCol : TPoint;
  SynExpr : TSqliteExpr;
  Exprs : TSqliteExprs;
begin
  S0 := UTF8Trim(SQLEditor.Text);
  Exprs := TSqliteExprs.Create(S0);
  try
    p := 0;
    RowCol := SQLEditor.CaretXY;
    RowCol.y := RowCol.y - 1;
    e:= LineEnding;
    LineEndLen:=length(e);
    for i := 0 to RowCol.y - 1 do
      p := p + Length(SQLEditor.Lines[i]) + LineEndLen;
    p := p + RowCol.x;
    synExpr := Exprs.ExprAtPos(p);
    if Assigned(SynExpr) then S := SynExpr.OrigExpr else
                              S := '';
    if OpenEditSQLDialog(S, [seoOneExpression], DataSet) then
    begin
      if Assigned(SynExpr) then
        SQLEditor.Text := Utf8Copy(S0, 1, synExpr.Pos - 1) +
                           EditSQLDialog.ResultExpr +
                           UTF8Copy(S0, synExpr.Pos + synExpr.Len + 1,
                                  UTF8Length(S0))
      else
        SQLEditor.Text := S0 + EditSQLDialog.ResultExpr;
    end;
  finally
    Exprs.Free;
  end;
end;

procedure TMain.ToolButton4Click(Sender : TObject);
begin
  if Assigned(FModTables.CurTable) then
  begin
    if FModTables.CurTable.RecordChanged then
    begin
      if Application.MessageBox(PChar(Format(rsTableWasChanged,
                                  [FModTables.CurTable.RecordTable])),
                                   PChar(Application.Name),
                                   MB_ICONEXCLAMATION or MB_YESNO) = IDNO then
        Exit;
      FModTables.CurTable.Reset;
    end;
  end;
end;

procedure TMain.ToolButton5Click(Sender : TObject);
begin
  if Assigned(FModTables.CurTable) then
  begin
    if FModTables.CurTable.RecordChanged then
    begin
      if Application.MessageBox(PChar(Format(rsTableWasChanged,
                                   [FModTables.CurTable.RecordTable])),
                                   PChar(Application.Name),
                                   MB_ICONEXCLAMATION or MB_YESNO) = IDNO then
        Exit;
    end;
    FModTables.Back(-1);
  end;
end;

procedure TMain.AddAndEditButtonClick(Sender : TObject);
var S, ES, T : String;
begin
  S := ''; ES := '';
  if Assigned(FModTables.CurTable) then
  begin
    if (FModTables.CurTable.Mode = mtmEdit) then Exit; //
    if FModTables.CurTable.Apply(S, ES) then
    begin
      T := FModTables.CurTable.RecordTable;
      DataSet.ExecuteDirect(S);
      if (FModTables.CurTable.Mode = mtmAddNew) then
      begin
        S := DataSet.QuickQuery('select max('+sqluQuotedIdIfNeeded(FModTables.CurTable.WaitingKeyName)+') from ' + T, nil, false);
        if Length(S) > 0 then
        begin
          LogMemo.Lines.Add(Format(rsRecordSuccessAdded, [T, StrToInt(S)]));
          FModTables.ProceedTable(T, StrToInt(S), false, true);
        end;
      end;
    end else
      Application.MessageBox(PChar(Format(rsValuesAreEmpty, [ES])),
                                   PChar(Application.Name),
                                   MB_ICONEXCLAMATION or MB_OK);
  end;
end;

procedure TMain.SetDBFileName(const FN : String);
begin
  if not SameText(FN, DataSet.FileName) then
  begin
    DataSet.FileName := FN;
    DBNameLabel.Caption := ExtractFileName( FN );
    DBHelper.DBPath     := ExtractFilePath( FN );
  end;
end;

procedure TMain.ReloadConfig(const FN : String);
var S : TFileStream;
    JP : TDBJSONParser;
begin

  if assigned(FConfig) then
  begin
    FConfig.Lock;
    try
    finally
      FConfig.UnLock;
    end;
    FreeAndNil(FConfig);
  end;

  try
    try
      SetConfigFileName(FN);

      if FileExists(FConfigFileName) then
      begin
        S := TFileStream.Create(FConfigFileName, fmOpenRead);
        JP := TDBJSONParser.Create(S, [joUTF8]);
        try
          FConfig := JP.Parse;
        finally
          S.Free;
          JP.Free;
        end;
        if not Assigned(FConfig.Find(JSON_CFG_DATABASE)) then
          FConfig.Strings[JSON_CFG_DATABASE] := '';
      end;
    except
      on E : Exception do begin
        if Assigned(FConfig) then
          FreeAndNil(FConfig);
      end;
    end;
  finally
    RebuildConfigTree;
  end;

  if Assigned(FConfig) then
  begin
    FConfigFileLoaded := Now;
    ConfigChanged := false;
    RefreshDatabaseAndStruct;
  end else
  begin
    SaveCfgAs.Enabled  := false;
    RefreshCfg.Enabled := false;

    ConfigChanged := false;

    FStructure.Clear;
    ChooseTable.Items.Clear;
    FSynSQLSyn.TableNames.Clear;
    FTableList.Clear;
    DataSet.Active := false;
    SetDBFileName('');
  end;
end;

procedure TMain.SaveConfig;
var SL : TStringList;
begin
  if not Assigned(FConfig) then Exit;
  SL := TStringList.Create;
  try
    SL.Text := FConfig.FormatJSON();
    SL.SaveToFile(FConfigFileName);
  finally
    SL.Free;
  end;
  FConfigFileLoaded := Now;
  ConfigChanged := false;
  RefreshDatabaseAndStruct;
end;

procedure TMain.ReloadConfigStructure;
var aReloadJob : TReloadStructJob;
begin
  aReloadJob := TReloadStructJob.Create(FConfig, FStructure);
  DBHelper.ThreadPool.Add(aReloadJob);
end;

procedure TMain.InitializeDb;
var Tmp : String;
begin
  if FileExists(DataSet.FileName) then
  begin
    if Application.MessageBox(PChar(Format(rsDBExistsDelete, [DataSet.FileName])),
                                 PChar(Application.Name),
                                  MB_ICONEXCLAMATION or MB_YESNO) = IDNO then
    begin
      if Assigned(FConfig) then
      begin
        //todo : rollback to last database file name
      end;
      Exit;
    end else
    begin
      DataSet.Close;
      Tmp := DataSet.FileName;
      DataSet.FileName := '';
      DeleteFile(Tmp);
      DataSet.FileName := Tmp;
      DataSet.SQL := 'PRAGMA user_version;';
      try
        DataSet.Open;
      finally
        DataSet.Close;
      end;
    end;
  end;

  RefreshDbFromConfig;
end;

procedure TMain.RefreshDbFromConfig;
var i : integer;
begin
  try
    if Assigned(FConfig) then
    begin
      FConfig.Lock;
      try
        if Assigned(FConfig.Struct) then
        begin
          for i := 0 to FConfig.Struct.Count-1 do
          begin
            DataSet.ExecSQL(FConfig.Struct[i].AsString);
            FConfig.StructExpr[i].NeedRebuild := true;
          end;
        end;
      finally
        FConfig.UnLock;
      end;
    end;
  finally
    FStructureChanged := true;
  end;
end;

procedure TMain.SetConfigChanged(AValue: Boolean);
begin
  if FConfigChanged = AValue then Exit;
  FConfigChanged := AValue;
  SaveCfg.Enabled := AValue;

  if Assigned(ConfigTree.TopItem) then
  begin
    if FConfigChanged then
      ConfigTree.TopItem.StateIndex := IMG_WARNING else
      ConfigTree.TopItem.StateIndex := IMG_CONFIG;
  end;
end;

procedure TMain.SetConfigFileName(const FN : String);
var Str : String;
begin
  Str := UTF8StringReplace(FN, cNonSysDelimiter, cSysDelimiter, [rfReplaceAll]);
  FConfigFileName := Str;

  Str := ExtractRelativePath(DBHelper.AppPath, ExtractFilePath(Str)) + ExtractFileName(Str);
  AppConfig.StoredValue[cfldDBName] := Str;
  AppConfig.WriteString(cfldDBName, Str);
end;

procedure TMain.RefreshDatabaseAndStruct;
var act : Boolean;
    S : String;
begin
  if Assigned(FConfig) then
  begin
    S := UTF8StringReplace(DBHelper.AppPath + FConfig.Strings[JSON_CFG_DATABASE],
                                 cNonSysDelimiter, cSysDelimiter,
                                 [rfReplaceAll]);
    act := DataSet.Active;
    DataSet.Active := false;
    SetDBFileName(S);
    DataSet.ClearPrepared;
    DataSet.ClearFunctions;
    if not FileExists(S) then
      InitializeDb else
      RefreshDbFromConfig;
    DataSet.Active := act;

    GenerateStructure;
  end;
end;

procedure TMain.GenerateStructure;
var i, tn, k : integer;
    T: TDBTable;
    Indx : TBaseSinExprs;
    expr : TSqliteExpr;
    analiz : TDBSqliteSynAnalizer;

    jindxs : TJSONArray;
    jindxo : TJSONObject;
    kind : String;
    uid  : integer;
    tblname : String;
    vfield, kfield, sefield : String;

    SL : TStringList;
begin
  if not FStructureChanged then Exit;

  FStructure.Lock;
  try
    FStructureChanged := false;
    FModTables.Reset;
    FStructure.Clear;
    Indxs.Clear;
    FTableList.Clear;
    ChooseTable.Items.Clear;
    FSynSQLSyn.TableNames.Clear;

    DataSet.SQL := 'SELECT * FROM sqlite_master where type == ''table'' and name not like ''sqlite_%'';';

    i := 0;
    while True do
    begin
      k := 0; //errors count
      tn := 0;
      try
        DataSet.Open(eomUniDirectional);
        while not DataSet.EOF do
        begin
          inc(tn);
          expr := TSqliteExpr.Create(DataSet.FieldByName('sql').AsString);
          try
            if FTableList.IndexOf(DataSet.FieldByName('name').AsString) < 0 then
            begin
              analiz := TDBSqliteSynAnalizer.Create(expr);
              try
                T := analiz.CreateTable(FStructure, [ctoAddToStruct,
                                                     ctoCheckForeignKeys,
                                                     ctoCheckExists]);
                if Assigned(T) then
                begin
                  FTableList.Add(DataSet.FieldByName('name').AsString);
                end else
                begin
                  FLogPull.Add(DataSet.FieldByName('sql').AsString);
                  FLogPull.Add('');
                  FLogPull.Add(Format(rsSqliteResultAsComment, [analiz.ErrorStr,
                                                                analiz.ErrorCode]));
                  FLogPull.Add('');
                  inc(k);
                end;
              finally
                analiz.Free;
              end;
            end;
          finally
            expr.Free;
          end;
          DataSet.Next;
        end;
      finally
        DataSet.Close;
      end;
      Inc(i);
      if (i = tn) or (k = 0) then Break;
    end;
  finally
    FStructure.UnLock;
  end;
  ChooseTable.Items.Assign(FTableList);
  FSynSQLSyn.TableNames.Assign(FTableList);

  try
    if Assigned(FConfig) then
    begin
      if FConfig.Find(JSON_CFG_EXTBLOBS, jindxs) then
      begin
        for i := 0 to jindxs.Count-1 do
        begin
          jindxo := TJSONObject(jindxs[i]);
          kind := jindxo.Strings[JSON_CFG_KIND];
          k := -1;
          for tn := 0 to DBHelper.JsonBlobKinds.Count-1 do
          if SameText(DBHelper.JsonBlobKinds[tn].Name, kind) then
            k := tn;

          if (k >= 0) and (TJsonBlobKind(DBHelper.JsonBlobKinds[k]).Kind <> ebkNo) then
          begin
            tblname := jindxo.Strings[JSON_CFG_TABLE];
            vfield := jindxo.Strings[JSON_CFG_FIELD];
            kfield := jindxo.Get(JSON_CFG_PATH, '');

            if (Length(tblname) > 0) and (Length(vfield) > 0) then
            begin
              kfield := UTF8StringReplace(kfield, cCURPATH, DBHelper.AppPath, [rfReplaceAll]);
              kfield := UTF8StringReplace(kfield, cDBPATH,  DBHelper.DBPath, [rfReplaceAll]);
              if Length(kfield) = 0 then kfield := DBHelper.AppPath;

              T := FStructure.ByName(tblname);
              if Assigned(T) and
                 Assigned(T.ByName(vfield)) and Assigned(T.ByName(kfield)) then
              begin
                FStructure.AddNewExtBlob(TJsonBlobKind(DBHelper.JsonBlobKinds[k]).Kind,
                                         kfield, tblname, vfield,
                                         true);
              end;
            end;
          end;
        end;
      end;

      if FConfig.Find(JSON_CFG_INDXS, jindxs) then
      begin
        for i := 0 to jindxs.Count-1 do
        begin
          jindxo := TJSONObject(jindxs[i]);
          kind := jindxo.Strings[JSON_CFG_KIND];
          k := -1;
          for tn := 0 to DBHelper.JsonIndxClasses.Count-1 do
          if SameText(DBHelper.JsonIndxClasses[tn].Name, kind) then
            k := tn;

          if (k >= 0) and Assigned(TJsonIndxClass(DBHelper.JsonIndxClasses[k]).tClass) then
          begin
            uid := jindxo.Integers[JSON_CFG_UID];
            tblname := jindxo.Strings[JSON_CFG_TABLE];
            kfield := jindxo.Strings[JSON_CFG_KFIELD];
            vfield := jindxo.Strings[JSON_CFG_VFIELD];
            sefield := jindxo.Get(JSON_CFG_SEFIELD, '');

            if (Length(tblname) > 0) and (Length(kfield) > 0) and
               (Length(vfield) > 0) then
            begin
              T := FStructure.ByName(tblname);
              if Assigned(T) and
                 Assigned(T.ByName(vfield)) and
                 Assigned(T.ByName(kfield)) and
                 (Assigned(T.ByName(sefield)) or (Length(sefield) = 0)) then
              begin
                Indx := TJsonIndxClass(DBHelper.JsonIndxClasses[k]).tClass.Create(uid, DataSet);
                if TJsonIndxClass(DBHelper.JsonIndxClasses[k]).tClass = TTokenedSinExprs then
                begin
                  if (Length(sefield) > 0) then
                    TTokenedSinExprs(Indx).InitStates(tblname, kfield, vfield, sefield) else
                    Continue;
                end else
                  Indx.InitStates(tblname, kfield, vfield);

                if FConfig.Find(JSON_CFG_FUNCS, jindxo) then
                begin
                  kind := jindxo.Strings[JSON_CFG_KIND];
                  k := -1;
                  for tn := 0 to DBHelper.JsonFuncClasses.Count-1 do
                  if SameText(DBHelper.JsonFuncClasses[tn].Name, kind) then
                    k := tn;

                  if (k >= 0) and Assigned(TJsonFuncClass(DBHelper.JsonFuncClasses[k]).tClass) then
                  begin
                    if (TJsonFuncClass(DBHelper.JsonFuncClasses[k]).tClass = TExprCrossHitContIdxFunction) and
                       (Indx is TBaseSinIndexedExprs) then
                      DataSet.AddFunction(TExprCrossHitContIdxFunction.Create(TBaseSinIndexedExprs(Indx)));
                  end;
                end;

                Indxs.Add(Indx);

              end;
            end;
          end;
        end;
      end;

      if FConfig.Find(JSON_CFG_FORINDXS, jindxs) then
      begin
        for i := 0 to jindxs.Count-1 do
        begin
          jindxo := TJSONObject(jindxs[i]);
          kind := jindxo.Strings[JSON_CFG_KIND];
          k := -1;
          for tn := 0 to DBHelper.JsonIndxStringsStyles.count-1 do
          if SameText(DBHelper.JsonIndxStringsStyles[tn].Name, kind) then
            k := tn;

          if (k >= 0) and (TJsonIndxStringsStyle(DBHelper.JsonIndxStringsStyles[k]).Kind <> issUnknown) then
          begin
            uid := jindxo.Integers[JSON_CFG_UID];
            Indx := Indxs.Indx[uid];
            if Assigned(Indx) then
            begin
              FStructure.AddForeignIndxStringTable(TJsonIndxStringsStyle(DBHelper.JsonIndxStringsStyles[k]).Kind,
                                                   Indx,
                                                   FStructure.ByName(Indx.TblName));
            end;
          end;
        end;
      end;
    end;

    SL := TStringList.Create;
    try
      FCompletionKeys.DeleteMasked([sckTable, sckField]);
      //
      for tn := 0 to FStructure.Count-1 do
      begin
        FCompletionKeys.AddObj(FStructure[tn].Name, sckTable, FStructure[tn]);
        for i := 0 to FStructure[tn].Count-1 do begin
          FCompletionKeys.AddObj(FStructure[tn].DBField[i].FieldName, sckField, FStructure[tn][i]);
          SL.Add(FStructure[tn].DBField[i].FieldName);
        end;
      end;
      FSynSQLSyn.FieldNames := SL;
    finally
      SL.Free;
    end;

  finally
    ReloadConfigStructure;
  end;
end;


procedure TMain.AddConfigNodes(parentNode : TTreeNode;
                              data : TJSONData;
                              const id : String;
                              aType : TConfigRecType);
var
  CNODE : TTreeNode;
  aField : TConfigRecField;
  tp : TConfigRecType;
  i : integer;
  s : String;
begin
  CNODE := ConfigTree.Items.AddChild(parentNode, id);
  case aType of
    crtRoot : begin
      if FConfigChanged then
        CNODE.StateIndex := IMG_WARNING else
        CNODE.StateIndex := IMG_CONFIG;
    end;
    crtArray, crtObject : CNODE.StateIndex := IMG_STRUCT;
    crtDataBaseName,
    crtProperty, crtStructElement : CNODE.StateIndex := IMG_STRUCT_ELEMENT;
  end;
  if SameText(id, JSON_CFG_STRUCTURE) then
  begin
    aField := crfStructure;
    FCfgStructNode := CNODE;
    CNODE.StateIndex := IMG_STRUCT;
  end else
  if SameText(id, JSON_CFG_FORINDXS) then
  begin
    aField := crfForeignIndx;
    CNODE.StateIndex := IMG_CONNECTION;
  end else
  if SameText(id, JSON_CFG_INDXS) then
  begin
    aField := crfIndx;
    CNODE.StateIndex := IMG_CONNECTION;
  end else
  if SameText(id, JSON_CFG_EXTBLOBS) then
  begin
    aField := crfExtBlobs;
    CNODE.StateIndex := IMG_CONNECTION;
  end else
  if SameText(id, JSON_CFG_FUNCS) then
  begin
    aField := crfAttachedFuncs;
    CNODE.StateIndex := IMG_CONNECTION;
  end else
  begin
    aField := crfUnknown;
  end;

  case aType of
    crtStructElement : begin
       CNODE.Data := TConfigRec.Create(id, aType, aField, data);
    end
  else
    CNODE.Data := TConfigRec.Create(id, aType, aField, data);
  end;
  CNODE.Text := TConfigRec(CNODE.Data).DisplayStr;
  for i := 0 to data.Count-1 do
  begin
    if data is TJSONObject then
    begin
      s := TJSONObject(data).Names[i];
    end else
    if data is TJSONArray then
    begin
      if (data.Items[i] is TJSONObject) then
        S := id+cObjPostfix else
        s := '';
    end;
    if data.Items[i] is TJSONObject then
      tp := crtObject else
    if data.Items[i] is TJSONArray then
      tp := crtArray else
    begin
      if aField = crfStructure then
        tp := crtStructElement else
      if (aType = crtRoot) and
         (SameText(s, JSON_CFG_DATABASE))  then
        tp := crtDataBaseName else
        tp := crtProperty;
    end;
    AddConfigNodes(CNODE, data.Items[i], s, tp);
  end;
end;

procedure TMain.RebuildConfigTree;
begin
  FCfgStructNode := nil;
  ConfigTree.BeginUpdate;
  ConfigTree.Items.Clear;
  if Assigned(FConfig) then
  begin
    FConfig.Lock;
    try
      AddConfigNodes(nil, FConfig, ExtractFileName(FConfigFileName), crtRoot);
    finally
      FConfig.UnLock;
    end;
  end;
  ConfigTree.EndUpdate;
end;

procedure TMain.UpdateCfgStruct(fromind : integer);
var i : integer;
begin
  if fromind < 0 then fromind := 0;
  FConfig.Lock;
  try
    for i := fromind to FConfig.Struct.Count-1 do
      FConfig.StructExpr[i].NeedRebuild := true;
  finally
    FConfig.UnLock;
  end;
  ReloadConfigStructure;
  ConfigChanged := true;
end;

procedure TMain.UpdateModTablesStack(O : TObject);
var i : integer;
    HS : THeaderSection;
begin
  HeaderControl1.Sections.Clear;
  for i := 0 to FModTables.Count-1 do
  begin
    HS := HeaderControl1.Sections.Add;
    HS.ImageIndex := IMG_TABLE;
    HS.Text := FModTables[i].RecordTable;
    HS.Width := ImageList1.Width + HeaderControl1.Canvas.GetTextWidth(HS.Text) + 12;
  end;
  if FModTables.Count = 0 then begin
    ChooseTable.ItemIndex := -1;
    EnableToolbar( ModifToolBar, false);
  end else
  begin
    ChooseTable.ItemIndex := ChooseTable.Items.IndexOf(FModTables.CurTable.RecordTable);
    EnableToolbar( ModifToolBar, true);
    AddAndEditButton.Enabled := (FModTables.Count = 1) and (FModTables.CurTable.Mode = mtmAddNew);
    if FModTables.CurTable.Mode = mtmEdit then
      ConfirmButton.ImageIndex := IMG_OK else
      ConfirmButton.ImageIndex := IMG_PLUS;
  end;
end;

procedure TMain.ReadFilters(const Sec : String; SL : TStrings);
begin
  SL.Text := AppConfig.StoredValue[cfldFilters + Sec];
end;

procedure TMain.WriteFilters(const Sec : String; SL : TStrings);
begin
  AppConfig.StoredValue[cfldFilters + Sec] := SL.Text;
  AppConfig.WriteString(cfldFilters + Sec, SL.Text);
end;

procedure TMain.EnableToolbar(TB : TToolBar; en : Boolean);
var i : integer;
begin
  if TB.Enabled <> en then
  begin
    for i := 0 to TB.ButtonCount-1 do
    begin
      TB.Buttons[i].Enabled := en;
    end;
    TB.Enabled := en;
  end;
end;

procedure TMain.SqlRequestPrepared(const aRequest: String; aResult: Integer);
begin
  if FLogEnabled.Value then
  begin
    FLogPull.Lock;
    try
      FLogChanged.Value := true;
      FLogPull.Add(aRequest);
      FLogPull.Add('');
      FLogPull.Add(Format(rsSqliteResultAsComment, [sqluGetLastError(DataSet.SqliteHandle, aResult), aResult]));
      FLogPull.Add('');
    finally
      FLogPull.UnLock;
    end;
  end;
end;

function TMain.StartOpenEditSQLDialog(cfg : TJSONData) : Boolean;
var i, k : integer;
    memdb : Pointer;
begin
  memdb := sqluNewMemoryDB('tempmem');
  try
    FConfig.Lock;
    try
      if Assigned(FConfig.Struct) and Assigned(memdb) then
      begin
        k := FConfig.Struct.IndexOf(cfg);
        for i := 0 to k-1 do
        begin
          if Assigned(FConfig.StructExpr[i].Expr) then
          begin
            if FConfig.StructExpr[i].IsCreateTable and
               FConfig.StructExpr[i].IsOk then
              sqluExecuteInMemory(memdb, FConfig.StructExpr[i].Expr.OrigExpr);
          end;
        end;
      end;
    finally
      FConfig.UnLock;
    end;
    Result := OpenEditSQLDialog(cfg.AsString, [seoOneExpression,
                                               seoAutoFormatExpr], memdb);
  finally
    if Assigned(memdb) then sqluDeleteMemoryDB(memdb)
  end;
end;

end.

