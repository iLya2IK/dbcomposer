{
 dbComposerSQLEditor:
   The Form to create and editing sqlite3 expressions

   Part of dbComposer project

   Copyright (c) 2021-2022 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit dbComposerSQLEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Menus, SynEdit, SynEditHighlighter, SynCompletion,
  LCLType,
  dbComposerStruct, dbcomposersynanalize,
  dbComposerTreeView,
  OGLFastList,
  ExtSqliteUtils,  ExtSqlite3DS, ExtSqliteTokens,
  dbComposerConsts, dbComposerCompleteHint, dbComposerUtils,
  SynEditKeyCmds, SynEditTypes, SynExportWordWrap;

type
  TDBTreeElementKind = (ttekTable,
                           ttekColumnConstrains, ttekColumnConstrain,
                           ttekTableConstrains, ttekTableConstrain,
                           ttekTableOptions, ttekTableOption,
                           ttekColumn,
                           ttekDataType, ttekOption, ttekValue);

  { TDBTreeElement }

  TDBTreeElement = class(TTreeNode)
  private
    FKind  : TDBTreeElementKind;
    FValue : String;
    FData  : TObject;
    function GetElText : String; virtual;
  public
    constructor Create(aOwned : TTreeNodes; aKind : TDBTreeElementKind;
      const aValue : String; aData : TObject); overload;
    property Kind  : TDBTreeElementKind read FKind;
    property Value : String read FValue;
    property Data  : TObject read FData;
  end;

  { TDBTreeElementTable }

  TDBTreeElementTable = class(TDBTreeElement)
  private
    function GetElText : String; override;
    function GetTable : TDBTable;
  public
    constructor Create(aOwned : TTreeNodes; aTable : TDBTable); overload;
    property Table : TDBTable read GetTable;
  end;

  { TDBTreeElementColumn }

  TDBTreeElementColumn = class(TDBTreeElement)
  private
    function GetElText : String; override;
    function GetField : TDBField;
  public
    constructor Create(aOwned : TTreeNodes; aField : TDBField); overload;
    property Field : TDBField read GetField;
  end;

  { TDBTreeElementType }

  TDBTreeElementType = class(TDBTreeElement)
  private
    function GetElText : String; override;
    function GetField : TDBField;
  public
    constructor Create(aOwned : TTreeNodes; aField : TDBField;
                                            const aType : String); overload;
    property Field : TDBField read GetField;
  end;

  { TDBTreeElementOption }

  TDBTreeElementOption = class(TDBTreeElement)
  private
    function GetElText : String; override;
  public
    constructor Create(aOwned : TTreeNodes; const aOption : String); overload;
  end;

  { TDBTreeTableOptions }

  TDBTreeTableOptions = class(TDBTreeElement)
  public
    constructor Create(aOwned : TTreeNodes); override;
  end;

  TDBTreeTableOption = class(TDBTreeElement)
  private
    FOptID : TSqliteTableOption;
    function GetElText : String; override;
  public
    constructor Create(aOwned : TTreeNodes; aOptID : TSqliteTableOption); overload;
    property OptionID : TSqliteTableOption read FOptID;
  end;

  { TDBTreeElementValue }

  TDBTreeElementValue = class(TDBTreeElement)
  private
    find : Integer;
    function GetOwnerConstr: TDBConstraint;
    function GetValueKind : TSqliteValueKind;
  public
    constructor Create(aOwned: TTreeNodes; const aValue: String;
                                            aOwner: TDBConstraint;
                                            aInd: integer); overload;
    property Ind : Integer read find;
    property ValueKind : TSqliteValueKind read GetValueKind;
    property OwnerConstr : TDBConstraint read GetOwnerConstr;
  end;

  { TDBTreeElementConstrain }

  TDBTreeElementConstrain = class(TDBTreeElement)
  private
    function GetConstr : TDBConstraint;
    function GetElText : String; override;
  public
    constructor Create(aOwned : TTreeNodes; aConstr : TDBConstraint;
      IsTable : Boolean); overload;
    property Constr : TDBConstraint read GetConstr;
  end;

  { TDBTreeElementConstrains }

  TDBTreeElementConstrains = class(TDBTreeElement)
  private
    function GetConstrs : TDBConstraints;
  public
    constructor Create(aOwned : TTreeNodes; aConstrs : TDBConstraints;
      IsTable : Boolean); overload;
    property Constrs : TDBConstraints read GetConstrs;
  end;

  TDBTreeMenuItem = class(TMenuItem);

  { TDBTreeConstrMenuItem }

  TDBTreeConstrMenuItem = class(TDBTreeMenuItem)
  private
    FKind : TSqliteConstrKind;
    FIsTable : Boolean;
  public
    constructor Create(aKind : TSqliteConstrKind; aIsTable : Boolean); overload;
    property Kind : TSqliteConstrKind read FKind;
    property IsTable : Boolean read FIsTable;
  end;

  { TDBTreeOptionMenuItem }

  TDBTreeOptionMenuItem = class(TDBTreeMenuItem)
  private
    FKind : Cardinal;
    FConstr : TDBConstraint;
  public
    constructor Create(aConstr : TDBConstraint; aKind : Cardinal); overload;
    property Kind : Cardinal read FKind;
    property Constr : TDBConstraint read FConstr;
  end;

  { TDBTreeTableOptionMenuItem }

  TDBTreeTableOptionMenuItem = class(TDBTreeMenuItem)
  private
    FKind : TSqliteTableOption;
  public
    constructor Create(aKind : TSqliteTableOption); overload;
    property Kind : TSqliteTableOption read FKind;
  end;

  { TDBTreeTypeMenuItem }

  TDBTreeTypeMenuItem = class(TDBTreeMenuItem)
  private
    FKind : TSqliteDataTypeAffinity;
  public
    constructor Create(aKind : TSqliteDataTypeAffinity); overload;
    property Kind : TSqliteDataTypeAffinity read FKind;
  end;

  TEditSQLOption = (seoOneExpression, seoAutoFormatExpr);

  TEditSQLOptions = set of TEditSQLOption;

  { TEditSQLDialog }

  TEditSQLDialog = class(TForm)
    AddButton : TToolButton;
    DeleteButton : TToolButton;
    Memo1 : TMemo;
    ComposersPages : TPageControl;
    Panel1: TPanel;
    AddMenu : TPopupMenu;
    Panel2 : TPanel;
    Panel3 : TPanel;
    CreateTablePanel : TPanel;
    PopupItemMenu: TPopupMenu;
    RefreshButton : TToolButton;
    Splitter1 : TSplitter;
    StaticText1 : TStaticText;
    SynEdit1 : TSynEdit;
    CreateTableComposer : TTabSheet;
    Timer1: TTimer;
    ToolBar1: TToolBar;
    ToolBar2 : TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5 : TToolButton;
    ToolButton6 : TToolButton;
    ToolButton7 : TToolButton;
    ResetButton : TToolButton;
    ToolButton8 : TToolButton;
    procedure DeleteButtonClick(Sender : TObject);
    procedure FormClose(Sender : TObject; var CloseAction : TCloseAction);
    procedure FormCreate(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure RefreshButtonClick(Sender : TObject);
    procedure ResetButtonClick(Sender : TObject);
    procedure SynEdit1Change(Sender : TObject);
    procedure SynEdit1CommandProcessed(Sender : TObject;
      var Command : TSynEditorCommand; var AChar : TUTF8Char; Data : pointer);
    procedure SynEdit1ProcessCommand(Sender : TObject;
      var Command : TSynEditorCommand; var AChar : TUTF8Char; Data : pointer);
    procedure SynEdit1StatusChange(Sender : TObject; Changes : TSynStatusChanges
      );
    procedure Timer1Timer(Sender : TObject);
    procedure ToolButton1Click(Sender : TObject);
    procedure ToolButton2Click(Sender : TObject);
    procedure ToolButton4Click(Sender : TObject);
    procedure ToolButton5Click(Sender : TObject);
    procedure ToolButton7Click(Sender : TObject);
  private
    FNodeEditing : Boolean;
    FCurTableNode : TDBTreeElement;
    procedure SetCurTableNode(AValue : TDBTreeElement);
    procedure SetExpr(AValue : String);
    function CharIndexToRowCol(ind : Integer) : TPoint;
    procedure HelperChanged(aState : TDBHelperState);
  private
    TableTree : TDBComposerTreeView;
    Wrapper : TSynExporterWordWrap;

    Options : TEditSQLOptions;
    InitialExpr,
    Expr    : String;
    ExprChanged, ExprsChanged, ExprChangedLocked : Boolean;
    synExprs : TSqliteExprs;
    synExpr : TSqliteExpr;
    DataSet : TExtSqlite3Dataset;
    sHandle : Pointer;
    cTable : TDBTable;

    CompleteHint : TCustomCompleteHint;

    function Start(const aExpr : String; aOptions : TEditSQLOptions) : Boolean;
    procedure RebuildExpr;
    procedure CheckTableExpr(shwerrors : Boolean);
    procedure CheckExprBySqlite;
    procedure RebuildTableTree;
    procedure RebuildTableTreeAndExpr;
    procedure RefreshExprAtCaretPos;
    procedure UpdateTableMenu;

    procedure OnAddOption(Sender : TObject);
    procedure OnAddColumn(Sender : TObject);
    procedure OnAddConstr(Sender : TObject);
    procedure OnChangeTableOption(Sender : TObject);
    procedure OnSetValue(Sender : TObject);
    procedure OnDeleteOption(Sender : TObject);
    procedure OnTypeChanged(Sender : TObject);

    procedure TableTreeMouseDown(Sender: TObject; Button: TMouseButton;
                                             Shift: TShiftState; X, Y: Integer);
    procedure TableTreeSelectionChanged(Sender: TObject);
    procedure TableTreeEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure TableTreeEditingEnd(Sender: TObject; Node: TTreeNode;
      Cancel: Boolean);
    procedure TableTreeEditorTextChange(Sender : TObject);

    property Expression : String read Expr write SetExpr;
    property CurTableNode : TDBTreeElement read FCurTableNode write SetCurTableNode;
  public
    procedure SetCompletion(SC : TSynCompletion;
                               aCompletionKeys : TCompletionCollection);
    property ResultExpr : String read Expr;
  end;

  { TDBTreeNodeState }

  TDBTreeNodeState = class(TFastCollection)
  public
    Name : String;
    Kind : TDBTreeElementKind;
    Data : TObject;
    Expanded : Boolean;
    NodesCnt : Integer;
    constructor Create(aKind : TDBTreeElementKind; const aName : String;
                                             aData : TObject); overload;
    function Find(aKind : TDBTreeElementKind; const aName : String;
                                             aData : TObject) : TDBTreeNodeState;
  end;

function OpenEditSQLDialog(const aExpr : String;
                                 aOptions : TEditSQLOptions;
                                 aDataSet   : TExtSqlite3Dataset = nil) : Boolean; overload;
function OpenEditSQLDialog(const aExpr : String;
                                 aOptions : TEditSQLOptions;
                                 aSqliteHandle : Pointer) : Boolean; overload;

var
  EditSQLDialog: TEditSQLDialog;

implementation

uses  dbcomposermain, LazUTF8;

{$R *.lfm}

const cDBTreeElementKindToStr : Array [TDBTreeElementKind] of String =
                          ('Table',
                           'Column Constrains', 'Column Constrain',
                           'Table Constrains', 'Table Constrain',
                           'Table Options', 'Table Option',
                           'Column', 'DataType', 'Option', 'Value');
     cDBTreeElementKindToImIdx : Array [TDBTreeElementKind] of Integer =
                               (IMG_TABLE,
                                IMG_CONFIG, IMG_CONFIG_OPT,
                                IMG_CONFIG, IMG_CONFIG_OPT,
                                IMG_CONFIG, IMG_CONFIG,
                                IMG_STRUCT, IMG_DATA_TYPE, IMG_CONFIG,
                                IMG_VARIANT);
     cSqliteValueKindToImgIdx : Array [TSqliteValueKind] of Integer =
                              (IMG_STRUCT_ELEMENT,
                                IMG_VARIANT,
                                IMG_VARIANT,
                                IMG_STRUCT,
                                IMG_STRUCT,
                                IMG_TABLE);

function OpenEditSQLDialog(const aExpr : String; aOptions : TEditSQLOptions;
                                 aDataSet : TExtSqlite3Dataset) : Boolean;
begin
  EditSQLDialog.sHandle := nil;
  EditSQLDialog.DataSet := aDataSet;
  Result := EditSQLDialog.Start(aExpr, aOptions);
end;

function OpenEditSQLDialog(const aExpr : String; aOptions : TEditSQLOptions;
                                 aSqliteHandle : Pointer) : Boolean;
begin
  EditSQLDialog.sHandle := aSqliteHandle;
  EditSQLDialog.DataSet := nil;
  Result := EditSQLDialog.Start(aExpr, aOptions);
end;

{ TDBTreeNodeState }

constructor TDBTreeNodeState.Create(aKind : TDBTreeElementKind;
  const aName : String; aData : TObject);
begin
  inherited Create;
  Kind := aKind;
  Name := aName;
  Data := aData;
  Expanded := false;
  NodesCnt := 0;
end;

function TDBTreeNodeState.Find(aKind : TDBTreeElementKind;
  const aName : String; aData : TObject) : TDBTreeNodeState;
var i : integer;
begin
  for i := 0 to Count-1 do
  begin
    if (TDBTreeNodeState(Self[i]).Kind = aKind) and
       (TDBTreeNodeState(Self[i]).Data = aData) and
       SameStr(TDBTreeNodeState(Self[i]).Name, aName) then
    begin
      Result := TDBTreeNodeState(Self[i]);
      Exit;
    end;
  end;
  Result := nil;
end;

{ TDBTreeTypeMenuItem }

constructor TDBTreeTypeMenuItem.Create(aKind : TSqliteDataTypeAffinity);
begin
  inherited Create(nil);
  FKind := aKind;
  ImageIndex := cDBTreeElementKindToImIdx[ttekDataType];
  Caption := sqluAffinityToStr(aKind);
end;

{ TDBTreeOptionMenuItem }

constructor TDBTreeOptionMenuItem.Create(aConstr: TDBConstraint;
                                                  aKind: Cardinal);
begin
  inherited Create(nil);
  FConstr := aConstr;
  FKind := aKind;
end;

{ TDBTreeTableOptionMenuItem }

constructor TDBTreeTableOptionMenuItem.Create(aKind : TSqliteTableOption);
begin
  inherited Create(nil);
  FKind := aKind;
end;

{ TDBTreeConstrMenuItem }

constructor TDBTreeConstrMenuItem.Create(aKind: TSqliteConstrKind;
  aIsTable: Boolean);
begin
  inherited Create(nil);
  FKind := aKind;
  FIsTable := aIsTable;
end;

{ TDBTreeElementType }

function TDBTreeElementType.GetElText : String;
begin
  Result := FValue;
end;

function TDBTreeElementType.GetField : TDBField;
begin
  Result := TDBField(FData);
end;

constructor TDBTreeElementType.Create(aOwned : TTreeNodes; aField : TDBField;
  const aType : String);
begin
  inherited Create(aOwned, ttekDataType, aType, aField);
end;

{ TDBTreeElementTable }

function TDBTreeElementTable.GetElText : String;
begin
  Result := Table.Name;
end;

function TDBTreeElementTable.GetTable : TDBTable;
begin
  Result := TDBTable(FData);
end;

constructor TDBTreeElementTable.Create(aOwned : TTreeNodes; aTable : TDBTable);
begin
  inherited Create(aOwned, ttekTable, '', aTable);
end;

{ TDBTreeElementColumn }

function TDBTreeElementColumn.GetElText : String;
begin
  Result := Field.FieldName;
end;

function TDBTreeElementColumn.GetField : TDBField;
begin
  Result := TDBField(FData);
end;

constructor TDBTreeElementColumn.Create(aOwned : TTreeNodes; aField : TDBField);
begin
  inherited Create(aOwned, ttekColumn, '', aField);
end;

{ TDBTreeElementOption }

function TDBTreeElementOption.GetElText : String;
begin
  Result := FValue;
end;

constructor TDBTreeElementOption.Create(aOwned : TTreeNodes; const
                                                               aOption : String);
begin
  inherited Create(aOwned, ttekOption, aOption, nil);
end;

{ TDBTreeTableOption }

function TDBTreeTableOption.GetElText : String;
begin
  Result := FValue;
end;

constructor TDBTreeTableOption.Create(aOwned : TTreeNodes;
  aOptID : TSqliteTableOption);
begin
  FOptID := aOptID;
  inherited Create(aOwned, ttekTableOption, sqluTableOptionKindToStr(aOptID), nil);
end;

{ TDBTreeTableOptions }

constructor TDBTreeTableOptions.Create(aOwned : TTreeNodes);
begin
  inherited Create(aOwned, ttekTableOptions, '', nil);
end;

{ TDBTreeElementValue }

function TDBTreeElementValue.GetOwnerConstr: TDBConstraint;
begin
  Result := TDBConstraint(FData);
end;

function TDBTreeElementValue.GetValueKind : TSqliteValueKind;
begin
  if Assigned(FData) then
  begin
    Result := OwnerConstr.ValueExprs[Ind].Kind;
  end else
    Result := dbvkExpression;
end;

constructor TDBTreeElementValue.Create(aOwned: TTreeNodes;
  const aValue: String; aOwner: TDBConstraint; aInd: integer);
begin
  inherited Create(aOwned, ttekValue, aValue, aOwner);
  find := aInd;
end;

{ TDBTreeElementConstrains }

function TDBTreeElementConstrains.GetConstrs : TDBConstraints;
begin
  Result := TDBConstraints(FData);
end;

constructor TDBTreeElementConstrains.Create(aOwned : TTreeNodes;
  aConstrs : TDBConstraints; IsTable : Boolean);
begin
  if IsTable then
  begin
    inherited Create(aOwned, ttekTableConstrains, '', aConstrs);
  end else
    inherited Create(aOwned, ttekColumnConstrains, '', aConstrs);
end;

{ TDBTreeElementConstrain }

function TDBTreeElementConstrain.GetConstr : TDBConstraint;
begin
  Result := TDBConstraint(FData);
end;

function TDBTreeElementConstrain.GetElText : String;
begin
  Result := sqluConstraintKindToStr(Constr.Kind);
end;

constructor TDBTreeElementConstrain.Create(aOwned : TTreeNodes;
  aConstr : TDBConstraint; IsTable : Boolean);
begin
  if IsTable then
  begin
    inherited Create(aOwned, ttekTableConstrain, '', aConstr);
  end else
    inherited Create(aOwned, ttekColumnConstrain, '', aConstr);
  case aConstr.Kind of
    dbckPrimaryKey : StateIndex := IMG_KEY;
    dbckForeignKey : StateIndex := IMG_CONNECTION;
  end;
end;

{ TDBTreeElement }

function TDBTreeElement.GetElText : String;
begin
  if Length(FValue) > 0 then
    Result := FValue else
    Result := cDBTreeElementKindToStr[FKind];
end;

constructor TDBTreeElement.Create(aOwned : TTreeNodes;
  aKind : TDBTreeElementKind; const aValue : String; aData : TObject);
begin
  inherited Create(aOwned);
  FKind := aKind;
  FValue := aValue;
  FData := aData;
  StateIndex := cDBTreeElementKindToImIdx[aKind];
  Text := GetElText;
end;

{ TEditSQLDialog }

procedure TEditSQLDialog.ToolButton4Click(Sender : TObject);
begin
  Memo1.Lines.Clear;
  RebuildExpr;
  CheckExprBySqlite;
end;

procedure TEditSQLDialog.ToolButton5Click(Sender : TObject);
begin
  SynEdit1.Undo;
end;

procedure TEditSQLDialog.ToolButton7Click(Sender : TObject);
begin
  SynEdit1.Redo;
end;

procedure TEditSQLDialog.SetCurTableNode(AValue : TDBTreeElement);
begin
  if FCurTableNode = AValue then Exit;
  FCurTableNode := AValue;
  UpdateTableMenu;
end;

procedure TEditSQLDialog.SetExpr(AValue : String);
begin
  if Expr = AValue then Exit;
  Expr := AValue;
  Timer1.Enabled := false;
  if not ExprChangedLocked then ExprsChanged := true;
  Timer1.Enabled := true;
end;

function TEditSQLDialog.CharIndexToRowCol(ind : Integer) : TPoint;
var
  x, y, Chars: integer;
  e: string;
  LineEndLen: Integer;
begin
  x := 0;
  y := 0;
  e:=LineEnding;
  LineEndLen:=length(e);
  Chars := 0;
  while y < SynEdit1.Lines.Count do begin
    x := Length(SynEdit1.Lines[y]);
    if Chars + x + LineEndLen > Ind then begin
      x := Ind - Chars;
      break;
    end;
    Inc(Chars, x + LineEndLen);
    x := 0;
    Inc(y);
  end;
  Result := Point(x + 1, y + 1);
end;

procedure TEditSQLDialog.HelperChanged(aState : TDBHelperState);
begin
  case aState of
    dbhsEditorFont : begin
      SynEdit1.Font.Name := DBHelper.EditorFontName;
      SynEdit1.Font.Height := DBHelper.EditorFontSize;
    end;
  end;
end;

function TEditSQLDialog.Start(const aExpr : String; aOptions : TEditSQLOptions
  ) : Boolean;
var SL : TStringList;
    SS : TStringStream;
    tmp : TSqliteExprs;
    S, S0 : String;
    i : integer;
    T : TDBTable;
    analiz : TDBSqliteSynAnalizer;
    fmt : TSqliteKwFormatOption;
begin
  Options := aOptions;
  InitialExpr := aExpr;
  ExprChangedLocked := true;
  cTable := nil;
  if Length(aExpr) > 0 then
  begin
    if seoAutoFormatExpr in Options then
    begin
      S := '';
      tmp := TSqliteExprs.Create(aExpr);
      try
        for i := 0 to tmp.Count-1 do
        begin
          if TDBSqliteSynAnalizer.CheckIsCreateTable(tmp[i]) then
          begin
            analiz := TDBSqliteSynAnalizer.Create(tmp[i]);
            try
              T := analiz.CreateTable(nil, []);
              if Assigned(T) then
              begin
                fmt := tmp[i].DetectKeywordFormat;
                S0 := T.BuildCreateExpression(false, fmt);
                T.Free;
              end else
                S0 := tmp[i].FormatedStr(skfoOriginal);
            finally
              analiz.Free;
            end;
          end else
            S0 := tmp[i].FormatedStr(skfoOriginal);
          if i > 0 then
          begin
            if not tmp[i].HasClosingSemiColumn then
              S := S + ';';
            S := S + #10 + S0;
          end else
            S := S0;
        end;
      finally
        tmp.Free;
      end;

      SL := TStringList.Create;
      try
        SL.Text := S;
        Wrapper.ExportAll(SL);
        SS := TStringStream.Create('');
        try
          Wrapper.SaveToStream(SS);
          SynEdit1.Text := SS.DataString;
          Expression := SS.DataString;
        finally
          SS.Free;
        end;
      finally
        SL.Free;
      end;
    end else begin
      Expression := aExpr;
      SynEdit1.Text := aExpr;
    end;
  end else begin
    Expression := aExpr;
    SynEdit1.Text := '';
  end;
  TableTree.Items.Clear;
  Timer1.Enabled := true;
  ExprChangedLocked := false;
  ExprsChanged := true;

  ShowModal;
  if ModalResult = mrOK then
  begin
    if seoOneExpression in Options then
      Expr := TSqliteExprs.FormatSQLExprs(Expr, skfoOriginal, 1)
    else
      Expr := TSqliteExprs.FormatSQLExprs(Expr, skfoOriginal, -1);
    Result := True;
  end else
  begin
    Expr := aExpr;
    Result := false;
  end;
end;

procedure TEditSQLDialog.FormDestroy(Sender : TObject);
begin
  if Assigned(synExprs) then synExprs.Free;
  if Assigned(cTable) then cTable.Free;
  Wrapper.Free;
end;

procedure TEditSQLDialog.RefreshButtonClick(Sender : TObject);
begin
  RebuildTableTreeAndExpr;
end;

procedure TEditSQLDialog.ResetButtonClick(Sender : TObject);
begin
  Timer1.Enabled := false;
  ExprChangedLocked := true;
  Expression := InitialExpr;
  SynEdit1.Text := InitialExpr;
  TableTree.Items.Clear;
  Timer1.Enabled := true;
  ExprChangedLocked := false;
  ExprsChanged := true;
end;

procedure TEditSQLDialog.SynEdit1Change(Sender : TObject);
begin
  if not ExprChangedLocked then begin
    Expression := SynEdit1.Lines.Text;
  end;
  CompleteHint.EditorChange(Sender);
end;

procedure TEditSQLDialog.SynEdit1CommandProcessed(Sender : TObject;
  var Command : TSynEditorCommand; var AChar : TUTF8Char; Data : pointer);
begin
  CompleteHint.EditorCommandProcessed(Sender, Command, AChar, Data);
end;

procedure TEditSQLDialog.SynEdit1ProcessCommand(Sender : TObject;
  var Command : TSynEditorCommand; var AChar : TUTF8Char; Data : pointer);
begin
  CompleteHint.EditorProcessCommand(Sender, Command, AChar, Data);
end;

procedure TEditSQLDialog.SynEdit1StatusChange(Sender : TObject;
  Changes : TSynStatusChanges);
begin
  CompleteHint.EditorStatusChange(Sender, Changes);
  if ([scCaretX, scCaretY] * Changes <> []) then
  begin
    RefreshExprAtCaretPos;
  end;
end;

procedure TEditSQLDialog.FormCreate(Sender : TObject);
begin
  TableTree := TDBComposerTreeView.Create(CreateTablePanel);
  TableTree.Top := 102;
  TableTree.Left := 2;
  TableTree.Width := 100;
  TableTree.Height := 100;
  TableTree.Align := alClient;
  TableTree.StateImages := Main.ImageList1;
  TableTree.OnEditing := @TableTreeEditing;
  TableTree.OnEditingEnd := @TableTreeEditingEnd;
  TableTree.OnMouseDown := @TableTreeMouseDown;
  TableTree.OnSelectionChanged := @TableTreeSelectionChanged;
  TableTree.Editor.OnChange := @TableTreeEditorTextChange;
  TableTree.RightClickSelect := true;
  TableTree.AutoExpand := true;
  TableTree.Parent := CreateTablePanel;
  FNodeEditing := false;

  CompleteHint := TCustomCompleteHint.Create(Panel3);
  CompleteHint.SynEdit := SynEdit1;
  CompleteHint.ImageList := Main.ImageList1;
  CompleteHint.Parent := Panel3;

  Wrapper := TSynExporterWordWrap.Create(nil);

  SynEdit1.Highlighter := DBHelper.Sqlite3Highlighter;
  CompleteHint.OnSetFontParams := @(DBHelper.SetFontParamsFromCompletionObj);
  Wrapper.Highlighter := DBHelper.Sqlite3Highlighter;

  DBHelper.AddStateListener(@HelperChanged);

  SynEdit1.Font.Name := DBHelper.EditorFontName;
  SynEdit1.Font.Height := DBHelper.EditorFontSize;
end;

procedure TEditSQLDialog.FormClose(Sender : TObject;
  var CloseAction : TCloseAction);
begin
  Timer1.Enabled := false;
  if Assigned(cTable) then FreeAndNil(cTable);
end;

procedure TEditSQLDialog.DeleteButtonClick(Sender : TObject);
var flag : Boolean;
    fParent : TDBTreeElement;
begin
  if Assigned(FCurTableNode) then
  begin
    flag := false;
    case FCurTableNode.Kind of
      ttekColumnConstrain, ttekTableConstrain : begin
        fParent := TDBTreeElement(FCurTableNode.Parent);
        TDBTreeElementConstrains(fParent).Constrs.Remove(TDBTreeElementConstrain(FCurTableNode).Constr);
        flag := true;
      end;
      ttekTableOption : begin
        cTable.OptionByName[TDBTreeTableOption(FCurTableNode).Value] := false;
        flag := true;
      end;
      ttekColumn : begin
        cTable.Remove(TDBTreeElementColumn(FCurTableNode).Field);
        flag := true;
      end;
      ttekOption : begin
        fParent := TDBTreeElement(FCurTableNode.Parent);
        TDBTreeElementConstrain(fParent).Constr.DeleteOption(TDBTreeElementOption(FCurTableNode).Value);
        flag := true;
      end;
    end;
    if flag then
      RebuildTableTreeAndExpr;
  end;
end;

procedure TEditSQLDialog.Timer1Timer(Sender : TObject);
begin
  if ExprsChanged then
  begin
    RebuildExpr;
    ExprsChanged := false;
    ExprChanged := true;
  end;
  if ExprChanged then
  begin
    CheckTableExpr(False);
    ExprChanged := false;
  end;
end;

procedure TEditSQLDialog.ToolButton1Click(Sender : TObject);
begin
  ModalResult := mrOK;
end;

procedure TEditSQLDialog.ToolButton2Click(Sender : TObject);
begin
  ModalResult := mrCancel;
end;

procedure TEditSQLDialog.RebuildExpr;
begin
  if Assigned(synExprs) then synExprs.Free;
  synExprs := TSqliteExprs.Create(Expr);
  RefreshExprAtCaretPos;
end;

procedure TEditSQLDialog.CheckTableExpr(shwerrors : Boolean);
var analizer : TDBSqliteSynAnalizer;
    aTable : TDBTable;
begin
  if Assigned(synExpr) and
     TDBSqliteSynAnalizer.CheckIsCreateTable(synExpr) then
  begin

    if assigned(ActivePopupMenu) then
      ActivePopupMenu.Close;

    AddMenu.Items.Clear;
    PopupItemMenu.Items.Clear;

    analizer := TDBSqliteSynAnalizer.Create(synExpr);
    try
      aTable := analizer.CreateTable(nil, []);
      if Assigned(aTable) then
      begin
        if Assigned(cTable) then cTable.Free;
        cTable := aTable;
        RebuildTableTree;
        if shwerrors then
          Memo1.Lines.Add(rsNoErrors);
      end else begin
        if shwerrors then
          Memo1.Lines.Add(analizer.ErrorStr);
      end;
    finally
      analizer.Free;
    end;
  end else
    TableTree.Items.Clear;
end;

procedure TEditSQLDialog.CheckExprBySqlite;
var er, i : integer;
    flag : Boolean;
    p, l : integer;
    analizer : TDBSqliteSynAnalizer;
    aTable   : TDBTable;
begin
  RebuildExpr;
  if Assigned(synExprs) then
  begin
    flag := true;
    for i := 0 to synExprs.Count-1 do
    begin
      if TDBSqliteSynAnalizer.CheckIsCreateTable(synExprs[i]) then
      begin
        analizer := TDBSqliteSynAnalizer.Create(synExprs[i]);
        try
          aTable := analizer.CreateTable(nil, []);
          if Assigned(aTable) then
          begin
            aTable.Free;
          end else begin
            flag := false;
            Memo1.Lines.Add(analizer.ErrorStr);
            p := synExprs[i].Pos + analizer.ErrorPos;
            if Assigned(analizer.ErroredToken) then
              l := analizer.ErroredToken.Len else
              l := synExprs[i].Len - analizer.ErrorPos;
            SynEdit1.SelStart := p - 1;
            SynEdit1.SelEnd := p + l - 1;
            Exit;
          end;
        finally
          analizer.Free;
        end;
      end;
      if Assigned(sHandle) then
        er := sqluCheckExpr(synExprs[i].OrigExpr, sHandle) else
      if Assigned(DataSet) then
        er := DataSet.CheckExpression(synExprs[i].OrigExpr) else
        er := 0;
      if er <> 0 then
      begin
        if Assigned(sHandle) then
          Memo1.Lines.Add(sqluGetLastError(sHandle, er)) else
          Memo1.Lines.Add(DataSet.FormatLastError(er));
        p := synExprs[i].Pos;
        l := synExprs[i].Len;
        flag := false;
        Break;
      end;
    end;
    if flag then
      Memo1.Lines.Add(rsNoErrors) else
    begin
      SynEdit1.SelStart := p;
      SynEdit1.SelEnd := p + l;
    end;
  end;
end;

procedure TEditSQLDialog.RebuildTableTree;

procedure AddNode(Parent, Child : TTreeNode);
begin
  if Assigned(Parent) then
    TableTree.Items.AddNode(Child, Parent, Child.Text, nil, naAddChild) else
    TableTree.Items.AddNode(Child, Parent, Child.Text, nil, naAdd);
end;

procedure FillSavedState(N : TTreeNode; SS : TDBTreeNodeState);
var i : integer;
    SC : TDBTreeNodeState;
begin
  if N is TDBTreeElement then
  begin
    case TDBTreeElement(N).Kind of
      ttekTable, ttekColumn, ttekColumnConstrains, ttekTableConstrains,
      ttekTableConstrain, ttekTableOptions,
      ttekColumnConstrain:
      begin
        SC := TDBTreeNodeState.Create(TDBTreeElement(N).Kind,
                                      TDBTreeElement(N).Text,
                                      TDBTreeElement(N).Data);
        SC.Expanded := N.Expanded;
        SC.NodesCnt := N.Count;
        SS.Add(SC);
        for i := 0 to N.Count-1 do
          FillSavedState(N[i], SC);
      end;
    end;
  end;
end;

procedure RestoreSavedState(N : TDBTreeElement; SS : TDBTreeNodeState);
var i : integer;
    SC : TDBTreeNodeState;
begin
  SC := SS.Find(N.Kind, N.Text, N.Data);
  if Assigned(SC) then
  begin
    N.Expanded := SC.Expanded or (N.Count <> SC.NodesCnt);
    for i := 0 to N.Count-1 do
    begin
      RestoreSavedState(TDBTreeElement(N[i]), SC);
    end;
  end else
  if N.Kind in [ttekColumn, ttekColumnConstrains, ttekTableConstrains] then
    N.Expanded := true; // expand all new nodes
end;

var TableNode : TDBTreeElementTable;
    ColumnNode : TDBTreeElementColumn;
    ConstrsNode : TDBTreeElementConstrains;
    ConstrNode : TDBTreeElementConstrain;
    TOptNode : TDBTreeTableOption;
    TOptsNode : TDBTreeTableOptions;
    Node : TDBTreeElement;
    Nodes : TTreeNodes;
    i, j, k : integer;
    c : TSqliteTableOption;
    //
    SavedState : TDBTreeNodeState;
begin
  if Assigned(cTable) then
  begin
    TableTree.BeginUpdate;
    SavedState := TDBTreeNodeState.Create;
    try
      Nodes := TableTree.Items;

      for i := 0 to Nodes.Count-1 do
        FillSavedState(Nodes[i], SavedState);

      TableTree.Items.Clear;

      TableNode := TDBTreeElementTable.Create(Nodes, cTable);
      AddNode(nil, TableNode);
      for i := 0 to cTable.Count-1 do
      begin
        ColumnNode := TDBTreeElementColumn.Create(Nodes, cTable[i]);
        AddNode(TableNode, ColumnNode);
        Node := TDBTreeElementType.Create(Nodes, cTable[i], cTable[i].FieldStrType);
        AddNode(ColumnNode, Node);
        ConstrsNode := TDBTreeElementConstrains.Create(Nodes, cTable[i].Constraints, false);
        AddNode(ColumnNode, ConstrsNode);
        For j := 0 to cTable[i].Constraints.Count-1 do
        begin
          ConstrNode := TDBTreeElementConstrain.Create(Nodes, cTable[i].Constraints[j], false);
          AddNode(ConstrsNode, ConstrNode);
          for k := 0 to cTable[i].Constraints[j].ValuesCount-1 do
          begin
            Node := TDBTreeElementValue.Create(Nodes,
                                               cTable[i].Constraints[j].Values[k],
                                               cTable[i].Constraints[j], k);
            AddNode(ConstrNode, Node);
          end;
          for k := 0 to cTable[i].Constraints[j].OptionsCount-1 do
          begin
            Node := TDBTreeElementOption.Create(Nodes, cTable[i].Constraints[j].Option[k]);
            AddNode(ConstrNode, Node);
          end;
        end;
      end;
      ConstrsNode := TDBTreeElementConstrains.Create(Nodes, cTable.Constraints, true);
      AddNode(TableNode, ConstrsNode);
      For j := 0 to cTable.Constraints.Count-1 do
      begin
        ConstrNode := TDBTreeElementConstrain.Create(Nodes, cTable.Constraints[j], true);
        AddNode(ConstrsNode, ConstrNode);
        for k := 0 to cTable.Constraints[j].ValuesCount-1 do
        begin
          Node := TDBTreeElementValue.Create(Nodes, cTable.Constraints[j].Values[k],
                                                    cTable.Constraints[j], k);
          AddNode(ConstrNode, Node);
        end;
        for k := 0 to cTable.Constraints[j].OptionsCount-1 do
        begin
          Node := TDBTreeElementOption.Create(Nodes, cTable.Constraints[j].Option[k]);
          AddNode(ConstrNode, Node);
        end;
      end;
      TOptsNode := TDBTreeTableOptions.Create(Nodes);
      AddNode(TableNode, TOptsNode);
      For c := Low(TSqliteTableOption) to High(TSqliteTableOption) do
      if cTable.Options[c] then
      begin
        TOptNode := TDBTreeTableOption.Create(Nodes, c);
        AddNode(TOptsNode, TOptNode);
      end;

      RestoreSavedState(TableNode, SavedState);
    finally
      SavedState.Free;
      TableTree.EndUpdate;
    end;
  end else TableTree.Items.Clear;
end;

procedure TEditSQLDialog.RebuildTableTreeAndExpr;
var aFormat : TSqliteKwFormatOption;
    aTableExpr : String;
    p : TPoint;
begin
  ExprChangedLocked := true;

  RebuildTableTree;

  SynEdit1.BeginUpdate;
  try
    if Assigned(cTable) then
    begin
      if Assigned(synExpr) then
        aFormat := synExpr.DetectKeywordFormat else
        aFormat := DBHelper.KwFormat;
      aTableExpr := cTable.BuildCreateExpression(false, aFormat);
      if Assigned(synExprs) and
         Assigned(synExpr) then
      begin
        Expr := Utf8Copy(synExprs.OrigExpr, 1, synExpr.Pos - 1) +
                      aTableExpr +
                      UTF8Copy(UTF8Trim(synExprs.OrigExpr, [u8tKeepStart]),
                               synExpr.Pos + synExpr.Len + 1,
                               UTF8Length(synExprs.OrigExpr));
      end else
      begin
        Expr := aTableExpr;
      end;
      p := SynEdit1.CaretXY;
      SynEdit1.ClearAll;
      SynEdit1.InsertTextAtCaret(Expr);
      SynEdit1.CaretXY := p;
    end;
  finally
    SynEdit1.EndUpdate;
  end;

  ExprChangedLocked := false;
  ExprsChanged := true;
end;

procedure TEditSQLDialog.RefreshExprAtCaretPos;
var
  i, p: integer;
  e: string;
  LineEndLen: Integer;
  RowCol : TPoint;
  lstSynExpr : TSqliteExpr;
begin
  lstSynExpr := synExpr;
  if Assigned(synExprs) then
  begin
    p := 0;
    RowCol := SynEdit1.CaretXY;
    RowCol.y := RowCol.y - 1;
    e:= LineEnding;
    LineEndLen:=length(e);
    for i := 0 to RowCol.y - 1 do
      p := p + Length(SynEdit1.Lines[i]) + LineEndLen;
    p := p + RowCol.x;
    synExpr := synExprs.ExprAtPos(p);
  end else
    synExpr := nil;
  ExprChanged := synExpr <> lstSynExpr;
end;

procedure TEditSQLDialog.UpdateTableMenu;

procedure SetButtonState(DBE, ABE : Boolean);
begin
  DeleteButton.Enabled := DBE;
  AddButton.Enabled := ABE;
end;

procedure DoAdd(M, aParent : TMenuItem; const aCaption : String;
                                              aImageIndex : Integer;
                                              aOnClick : TNotifyEvent);
begin
  M.Caption := aCaption;
  M.ImageIndex := aImageIndex;
  M.OnClick := aOnClick;

  aParent.Add(M);
end;

procedure AddToMenuIntern(M : TMenuItem; kind : TDBTreeElementKind);
var c : TSqliteConstrKind;
    topt : TSqliteTableOption;
    ii : integer;
begin
  case kind of
    ttekColumn : begin
      DoAdd(TMenuItem.Create(nil), M, cDBTreeElementKindToStr[kind],
                                      cDBTreeElementKindToImIdx[kind],
                                      @OnAddColumn);
    end;
    ttekColumnConstrains, ttekTableConstrains : begin
      for c := Low(TSqliteConstrKind) to High(TSqliteConstrKind) do
      if (kind = ttekColumnConstrains) or
         (c in [dbckPrimaryKey, dbckCheck, dbckUnique, dbckForeignKey]) then
      begin
        case c of
          dbckPrimaryKey : ii := IMG_KEY;
          dbckForeignKey : ii := IMG_CONNECTION;
        else
          ii := cDBTreeElementKindToImIdx[kind];
        end;
        DoAdd(TDBTreeConstrMenuItem.Create(c, (kind = ttekTableConstrains)), M,
                                                sqluConstraintKindToStr(c),
                                                ii, @OnAddConstr);
      end;
    end;
    ttekTableOptions : begin
      for topt := Low(TSqliteTableOption) to High(TSqliteTableOption) do
      begin
        if cTable.Options[topt] then
          ii := IMG_BULB_ON else
          ii := IMG_BULB_OFF;
        DoAdd(TDBTreeTableOptionMenuItem.Create(topt), M,
                                           sqluTableOptionKindToStr(topt),
                                           ii, @OnChangeTableOption);
      end;
    end;
  end;
end;

procedure AddToMenu(kind : TDBTreeElementKind); overload;
begin
  AddToMenuIntern(AddMenu.Items, kind);
end;

procedure AddToMenu(kind : TSqliteValueKind); overload;
var SL : TStringList;
    i : integer;
begin
  SL := TStringList.Create;
  try
    case kind of
      dbvkTable : begin
        CompleteHint.CompletionKeys.CompleteFillList(SL, '', i, [sckTable]);
      end;
      dbvkColumns, dbvkIndexedColumns : begin
        CompleteHint.CompletionKeys.CompleteFillList(SL, '', i, [sckField]);
        if Assigned(cTable) then
        for i := 0 to cTable.Count-1 do
        begin
          if SL.IndexOf(cTable[i].FieldName) < 0 then
            SL.Add(cTable[i].FieldName);
        end;
      end;
    end;
    SL.Sort;
    for i := 0 to SL.Count-1 do
    begin
      DoAdd(TMenuItem.Create(nil), AddMenu.Items, SL[i],
                                      cSqliteValueKindToImgIdx[kind],
                                      @OnSetValue);
    end;
  finally
    SL.Free;
  end;
end;

procedure AddToSubMenu(kind : TDBTreeElementKind);
var
  SubMenu : TMenuItem;
begin
  SubMenu := TMenuItem.Create(nil);
  SubMenu.Caption := cDBTreeElementKindToStr[kind];
  SubMenu.ImageIndex := cDBTreeElementKindToImIdx[kind];
  AddMenu.Items.Add(SubMenu);

  AddToMenuIntern(SubMenu, kind);
end;

procedure AddOption(kw : Cardinal; grpIndx : Integer = 0);
var
  OptMenu : TDBTreeOptionMenuItem;
begin
  OptMenu := TDBTreeOptionMenuItem.Create(TDBTreeElementConstrain(CurTableNode).Constr,
                                          kw);
  OptMenu.Caption := sqluGetIndexedKeyWord(kw);
  if not TDBTreeElementConstrain(CurTableNode).Constr.CheckOption(OptMenu.Caption) then
  begin
    OptMenu.OnClick := @OnAddOption;
    OptMenu.ImageIndex := IMG_BULB_OFF;
  end else begin
    OptMenu.OnClick := @OnDeleteOption;
    OptMenu.Checked := true;
    OptMenu.ImageIndex := IMG_BULB_ON;
  end;

  if grpIndx > 0 then
  begin
    OptMenu.RadioItem := true;
    OptMenu.GroupIndex := grpIndx;
  end;

  AddMenu.Items.Add(OptMenu);
end;

procedure AddSubOption(SubMenu : TMenuItem;
                       constr : TDBConstraint;
                       const aCaption : String);
var
  OptMenu : TDBTreeOptionMenuItem;
  Concat  : String;
begin
  if (Length(SubMenu.Caption) > 0) and
     (Pos(SubMenu.Caption, aCaption) = 0) then
     Concat := SubMenu.Caption + ' ' + aCaption else
     Concat := aCaption;

  OptMenu := TDBTreeOptionMenuItem.Create(constr, 0);
  OptMenu.Caption := aCaption;
  if not constr.CheckOption(Concat) then
  begin
    OptMenu.OnClick := @OnAddOption;
    OptMenu.ImageIndex := IMG_BULB_OFF;
  end else begin
    OptMenu.OnClick := @OnDeleteOption;
    OptMenu.Checked := true;
    OptMenu.ImageIndex := IMG_BULB_ON;
  end;

  SubMenu.Add(OptMenu);
end;

procedure AddDefferable;
var
  SubMenu : TMenuItem;
  constr : TDBConstraint;
begin
  constr := TDBTreeElementConstrain(CurTableNode).Constr;

  SubMenu := TMenuItem.Create(nil);
  SubMenu.Caption := sqluGetIndexedKeyWords([kwDEFERRABLE]);
  SubMenu.ImageIndex := cDBTreeElementKindToImIdx[ttekOption];
  AddMenu.Items.Add(SubMenu);

  AddSubOption(SubMenu, constr, sqluGetIndexedKeyWords([kwNOT, kwDEFERRABLE]));
  AddSubOption(SubMenu, constr, sqluGetIndexedKeyWords([kwDEFERRABLE]));
  AddSubOption(SubMenu, constr, sqluGetIndexedKeyWords([kwNOT, kwDEFERRABLE, kwINITIALLY, kwDEFERRED]));
  AddSubOption(SubMenu, constr, sqluGetIndexedKeyWords([kwDEFERRABLE, kwINITIALLY, kwDEFERRED]));
  AddSubOption(SubMenu, constr, sqluGetIndexedKeyWords([kwNOT, kwDEFERRABLE, kwINITIALLY, kwIMMEDIATE]));
  AddSubOption(SubMenu, constr, sqluGetIndexedKeyWords([kwDEFERRABLE, kwINITIALLY, kwIMMEDIATE]));
end;

procedure AddConflictClause;
var
  SubMenu : TMenuItem;
  constr : TDBConstraint;
begin
  constr := TDBTreeElementConstrain(CurTableNode).Constr;
  SubMenu := TMenuItem.Create(nil);
  SubMenu.Caption := sqluGetIndexedKeyWords([kwON, kwCONFLICT]);
  SubMenu.ImageIndex := cDBTreeElementKindToImIdx[ttekOption];
  AddMenu.Items.Add(SubMenu);

  AddSubOption(SubMenu, constr, sqluGetIndexedKeyWords([kwROLLBACK]));
  AddSubOption(SubMenu, constr, sqluGetIndexedKeyWords([kwABORT]));
  AddSubOption(SubMenu, constr, sqluGetIndexedKeyWords([kwFAIL]));
  AddSubOption(SubMenu, constr, sqluGetIndexedKeyWords([kwIGNORE]));
  AddSubOption(SubMenu, constr, sqluGetIndexedKeyWords([kwREPLACE]));
end;

procedure AddDeleteUpdateOptions(SubMenu : TMenuItem);
var
  constr : TDBConstraint;
begin
  constr := TDBTreeElementConstrain(CurTableNode).Constr;
  AddSubOption(SubMenu, constr, sqluGetIndexedKeyWords([kwSET, kwNULL]));
  AddSubOption(SubMenu, constr, sqluGetIndexedKeyWords([kwSET, kwDEFAULT]));
  AddSubOption(SubMenu, constr, sqluGetIndexedKeyWords([kwCASCADE]));
  AddSubOption(SubMenu, constr, sqluGetIndexedKeyWords([kwRESTRICT]));
  AddSubOption(SubMenu, constr, sqluGetIndexedKeyWords([kwNO, kwACTION]));
end;

procedure AddOnUpdate;
var
  SubMenu : TMenuItem;
begin
  SubMenu := TMenuItem.Create(nil);
  SubMenu.Caption := sqluGetIndexedKeyWords([kwON, kwUPDATE]);
  SubMenu.ImageIndex := cDBTreeElementKindToImIdx[ttekOption];
  AddMenu.Items.Add(SubMenu);

  AddDeleteUpdateOptions(SubMenu);
end;

procedure AddOnDelete;
var
  SubMenu : TMenuItem;
begin
  SubMenu := TMenuItem.Create(nil);
  SubMenu.Caption := sqluGetIndexedKeyWords([kwON, kwDELETE]);
  SubMenu.ImageIndex := cDBTreeElementKindToImIdx[ttekOption];
  AddMenu.Items.Add(SubMenu);

  AddDeleteUpdateOptions(SubMenu);
end;

procedure AddToMenu2(kind : TSqliteConstrKind; isTable : Boolean);
begin
  case kind of
    dbckPrimaryKey : begin
        if not isTable then
        begin
          AddOption(kwASC, 1);
          AddOption(kwDESC, 1);
          AddOption(kwAUTOINCREMENT);
        end;
        AddConflictClause;
      end;
    dbckForeignKey : begin
        AddOnUpdate;
        AddOnDelete;
        AddDefferable;
      end;
    dbckUnique : AddConflictClause;
    dbckNotNull: AddConflictClause;
    dbckGenerated : begin
      AddOption(kwSTORED, 1);
      AddOption(kwVIRTUAL, 1);
    end;
  end;
end;

begin
  AddMenu.Items.Clear;
  if Assigned(FCurTableNode) then
  begin
    case CurTableNode.Kind of
      ttekTable : begin
        SetButtonState(false, true);
        AddToMenu(ttekColumn);
        AddToSubMenu(ttekTableConstrains);
        AddToSubMenu(ttekTableOptions);
      end;
      ttekColumn : begin
        SetButtonState(true, true);
        AddToMenu(ttekColumnConstrains);
      end;
      ttekColumnConstrains, ttekTableConstrains : begin
        SetButtonState(false, true);
        AddToMenu(CurTableNode.Kind);
      end;
      ttekOption, ttekTableOption :
        SetButtonState(true, false);
      ttekColumnConstrain : begin
        SetButtonState(true, true);
        AddToMenu2(TDBTreeElementConstrain(CurTableNode).Constr.Kind, False);
      end;
      ttekTableConstrain : begin
        SetButtonState(true, true);
        AddToMenu2(TDBTreeElementConstrain(CurTableNode).Constr.Kind, True);
      end;
      ttekTableOptions : begin
        SetButtonState(false, true);
        AddToMenu(CurTableNode.Kind);
      end;
      ttekValue : begin
        SetButtonState(false, false);
        AddToMenu(TDBTreeElementValue(CurTableNode).ValueKind);
      end;
    else
      SetButtonState(false, false);
    end;
  end else
  begin
    SetButtonState(false, true);
  end;
end;

procedure TEditSQLDialog.SetCompletion(SC : TSynCompletion;
  aCompletionKeys : TCompletionCollection);
begin
  SC.AddEditor(SynEdit1);
  CompleteHint.CompletionKeys := aCompletionKeys;
end;

procedure TEditSQLDialog.OnAddOption(Sender: TObject);
var i, k : integer;
    aParent : TMenuItem;
    Concat, Concat0 : String;
    flag : boolean;
begin
  flag := false;
  if Sender is TDBTreeOptionMenuItem then
  begin
    with TDBTreeOptionMenuItem(Sender) do
    begin
      aParent := TMenuItem(Sender).Parent;
      if assigned(aParent) then
      begin
        if (aParent.ImageIndex = cDBTreeElementKindToImIdx[ttekOption]) and
           (Length(aParent.Caption) > 0) and
           (Pos(aParent.Caption, TDBTreeOptionMenuItem(Sender).Caption) = 0) then
           Concat0 := aParent.Caption + ' ' + TDBTreeOptionMenuItem(Sender).Caption else
           Concat0 := TDBTreeOptionMenuItem(Sender).Caption;
        k := -1;
        for i := 0 to aParent.Count-1 do
        begin
          if GroupIndex = aParent.Items[i].GroupIndex then
          begin
            if (aParent.ImageIndex = cDBTreeElementKindToImIdx[ttekOption]) and
               (Length(aParent.Caption) > 0) and
               (Pos(aParent.Caption, aParent.Items[i].Caption) = 0) then
               Concat := aParent.Caption + ' ' + aParent.Items[i].Caption else
               Concat := aParent.Items[i].Caption;

            if TDBTreeOptionMenuItem(Sender).Constr.CheckOption(Concat) then
            begin
              TDBTreeOptionMenuItem(Sender).Constr.ReplaceOption(Concat, Concat0);
              flag := true;
              k := i;
              Break;
            end;
          end;
        end;
        if k < 0 then
        begin
          TDBTreeOptionMenuItem(Sender).Constr.AddOption(Concat0);
          flag := true;
        end;
      end;
    end;
    if flag then
      RebuildTableTreeAndExpr;
  end;
end;

procedure TEditSQLDialog.OnAddColumn(Sender: TObject);
const cColNamePrefix = 'col';
var  i : integer;
begin
  i := 1;
  while Assigned(cTable.ByName(cColNamePrefix + inttostr(i))) do
    inc(i);
  cTable.Add(TDBField.Create(cTable, cColNamePrefix + inttostr(i),
                             sqluAffinityToStr(dtaInteger), ''));
  RebuildTableTreeAndExpr;
end;

procedure TEditSQLDialog.OnAddConstr(Sender: TObject);
var i : integer;
    flag, check : boolean;
    constr : TDBConstraints;
    con : TDBConstraint;
begin
  flag := false;
  if Sender is TDBTreeConstrMenuItem then
  begin
    if TableTree.Selected is TDBTreeElementConstrains then
      constr := TDBTreeElementConstrains(TableTree.Selected).Constrs else
    if TableTree.Selected is TDBTreeElementColumn then
      constr := TDBTreeElementColumn(TableTree.Selected).Field.Constraints else
    if TableTree.Selected is TDBTreeElementTable then
      constr := TDBTreeElementTable(TableTree.Selected).Table.Constraints else
      constr := nil;
    if assigned(constr) then
    with TDBTreeConstrMenuItem(Sender) do
    begin
      case Kind of
        dbckPrimaryKey, dbckDefault : check := true;
        dbckForeignKey : check := not IsTable;
      else
        check := false;
      end;
      if check then
      begin
        for i := 0 to constr.Count-1 do
        if constr[i].Kind = Kind then
          Exit;
      end;

      if IsTable then
      begin
        case Kind of
          dbckUnique, dbckPrimaryKey : begin
            con := TDBConstraint.Create(Kind, '');
            con.AddValue('Columns', dbvkIndexedColumns);
          end;
          dbckForeignKey : begin
            con := TDBConstraint.Create(Kind, '');
            con.AddValue('Columns', dbvkIndexedColumns);
            con.AddValue('forTable', dbvkTable);
            con.AddValue('forColumns', dbvkColumns);
          end;
          dbckCheck : begin
            con := TDBConstraint.Create(Kind, '');
            con.AddValue('expr', dbvkExpression);
          end;
          else con := nil;
        end;
      end else begin
        case Kind of
          dbckUnique, dbckPrimaryKey, dbckNotNull : begin
            con := TDBConstraint.Create(Kind, '');
          end;
          dbckForeignKey : begin
            con := TDBConstraint.Create(Kind, '');
            con.AddValue('forTable', dbvkTable);
            con.AddValue('forColumns', dbvkColumns);
          end;
          dbckCheck, dbckDefault, dbckGenerated : begin
            con := TDBConstraint.Create(Kind, '');
            con.AddValue('expr', dbvkExpression);
          end;
          dbckCollate : begin
            con := TDBConstraint.Create(Kind, '');
            con.AddValue('collation', dbvkCollationName);
          end;
          else con := nil;
        end;
      end;
      if assigned(con) then
      begin
        constr.Add(con);
        flag := true;
      end;
    end;
    if flag then
      RebuildTableTreeAndExpr;
  end;
end;

procedure TEditSQLDialog.OnChangeTableOption(Sender : TObject);
begin
  if Sender is TDBTreeTableOptionMenuItem then
  begin
    with TDBTreeTableOptionMenuItem(Sender) do
    begin
      if Assigned(cTable) then begin
         cTable.Options[Kind] := not cTable.Options[Kind];
         RebuildTableTreeAndExpr;
      end;
    end;
  end;
end;

procedure TEditSQLDialog.OnSetValue(Sender : TObject);
begin
  if Sender is TMenuItem then
  begin
    if Assigned(CurTableNode) and
       (CurTableNode is TDBTreeElementValue) then begin
      TDBTreeElementValue(CurTableNode).OwnerConstr.UpdateValue(TDBTreeElementValue(CurTableNode).Ind,
                                                                TMenuItem(Sender).Caption);
      RebuildTableTreeAndExpr;
    end;
  end;
end;

procedure TEditSQLDialog.OnDeleteOption(Sender: TObject);
var aParent : TMenuItem;
    Concat0 : String;
    flag : boolean;
begin
  flag := false;
  if Sender is TDBTreeOptionMenuItem then
  begin
    with TDBTreeOptionMenuItem(Sender) do
    begin
      aParent := TMenuItem(Sender).Parent;
      if assigned(aParent) then
      begin
        if (aParent.ImageIndex = cDBTreeElementKindToImIdx[ttekOption]) and
           (Length(aParent.Caption) > 0) and
           (Pos(aParent.Caption, TDBTreeOptionMenuItem(Sender).Caption) = 0) then
           Concat0 := aParent.Caption + ' ' + TDBTreeOptionMenuItem(Sender).Caption else
           Concat0 := TDBTreeOptionMenuItem(Sender).Caption;
        if TDBTreeOptionMenuItem(Sender).Constr.CheckOption(Concat0) then
        begin
          TDBTreeOptionMenuItem(Sender).Constr.DeleteOption(Concat0);
          flag := true;
        end;
      end;
    end;
    if flag then
      RebuildTableTreeAndExpr;
  end;
end;

procedure TEditSQLDialog.OnTypeChanged(Sender : TObject);
begin
  if assigned(FCurTableNode) and
     (FCurTableNode is TDBTreeElementType) and
     (sender is TDBTreeTypeMenuItem) then
  begin
    TDBTreeElementType(FCurTableNode).Field.SetFieldType(TDBTreeTypeMenuItem(Sender).Caption);
    RebuildTableTreeAndExpr;
  end;
end;

procedure TEditSQLDialog.TableTreeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

procedure AssignTreeMenu(it, src : TMenuItem);
var sit : TMenuItem;
    i : integer;
begin
  it.Caption := src.Caption;
  it.ImageIndex := src.ImageIndex;
  it.GroupIndex := src.GroupIndex;
  it.RadioItem := src.RadioItem;
  it.OnClick := src.OnClick;
  for i := 0 to src.Count-1 do
  begin
    if src[i] is TDBTreeOptionMenuItem then
    begin
      sit := TDBTreeOptionMenuItem.Create(TDBTreeOptionMenuItem(src[i]).Constr,
                                              TDBTreeOptionMenuItem(src[i]).Kind);
    end else
    if src[i] is TDBTreeTableOptionMenuItem then
    begin
      sit := TDBTreeTableOptionMenuItem.Create(TDBTreeTableOptionMenuItem(src[i]).Kind);
    end else
    if src[i] is TDBTreeConstrMenuItem then
    begin
      sit := TDBTreeConstrMenuItem.Create(TDBTreeConstrMenuItem(src[i]).Kind,
                                              TDBTreeConstrMenuItem(src[i]).IsTable);
    end else begin
      sit := TMenuItem.Create(nil);
    end;
    AssignTreeMenu(sit, src[i]);
    it.Add(sit);
  end;
end;

var it : TMenuItem;
    aff : TSqliteDataTypeAffinity;
begin
  //show popup
  if Button = mbRight then
  begin
    if Assigned(FCurTableNode) and
       (FCurTableNode is TDBTreeElementType) then
    begin
      PopupItemMenu.Items.Clear;
      aff := dtaUnknown;;
      while aff < High(TSqliteDataTypeAffinity) do
      begin
        Inc(aff);
        it := TDBTreeTypeMenuItem.Create(aff);
        it.OnClick := @OnTypeChanged;
        PopupItemMenu.Items.Add(it);
      end;
      PopupItemMenu.PopUp;
    end else
    if (AddMenu.Items.Count > 0) or
       (DeleteButton.Enabled) then
    begin
      PopupItemMenu.Items.Clear;

      if DeleteButton.Enabled then
      begin
        it := TMenuItem.Create(nil);
        it.ImageIndex := DeleteButton.ImageIndex;
        it.Caption := DeleteButton.Caption;
        it.OnClick := DeleteButton.OnClick;

        PopupItemMenu.Items.Add(it);

        if (AddMenu.Items.Count > 0) then
        begin
          it := TMenuItem.Create(nil);
          PopupItemMenu.Items.Add(it);
        end;
      end else it := PopupItemMenu.Items;

      if AddMenu.Items.Count > 0 then
      begin
        AssignTreeMenu(it, AddMenu.Items);
        it.ImageIndex := AddButton.ImageIndex;
        it.Caption := AddButton.Caption;
      end;

      PopupItemMenu.PopUp;
    end;
  end;
end;

procedure TEditSQLDialog.TableTreeSelectionChanged(Sender : TObject);
begin
  CurTableNode := TDBTreeElement(TableTree.Selected);
end;

procedure TEditSQLDialog.TableTreeEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
var
  aNode : TDBTreeElement;
begin
  if Node is TDBTreeElement then
  begin
    aNode := TDBTreeElement(Node);
    case aNode.Kind of
      ttekColumnConstrain, ttekTableConstrain,
      ttekColumnConstrains, ttekTableConstrains,
      ttekOption, ttekTableOptions, ttekTableOption :
        AllowEdit := false;
    end;
  end;
end;

procedure TEditSQLDialog.TableTreeEditingEnd(Sender: TObject; Node: TTreeNode;
  Cancel: Boolean);
var aNode : TDBTreeElement;
    ENode : TDBTreeElementValue;
    S : String;
begin
  if (Node is TDBTreeElement) and (not Cancel) then
  begin
    aNode := TDBTreeElement(Node);
    case aNode.Kind of
      ttekDataType : begin
        TDBTreeElementType(Node).Field.SetFieldType(Utf8UpperCase(Node.Text));
        RebuildTableTreeAndExpr;
      end;
      ttekValue : begin
        ENode := TDBTreeElementValue(Node);
        ENode.OwnerConstr.UpdateValue(ENode.Ind, Node.Text);
        RebuildTableTreeAndExpr;
      end;
      ttekColumn :
      begin
        S := Node.Text;
        if Assigned(cTable.ByName(S)) then
        begin
          Application.MessageBox(PChar(Format(rsColumnExists, [S])),
                                   PChar(Application.Name),
                                   MB_ICONEXCLAMATION or MB_OK);
          Node.Text := TDBTreeElementColumn(Node).Field.FieldName;
        end else begin
          TDBTreeElementColumn(Node).Field.SetFieldName(S);
          RebuildTableTreeAndExpr;
        end;
      end;
      ttekTable :
      begin
        S := Node.Text;
        TDBTreeElementTable(Node).Table.SetName(S);
        RebuildTableTreeAndExpr;
      end;
    end;
  end;
end;

procedure TEditSQLDialog.TableTreeEditorTextChange(Sender: TObject);
var ENode : TDBTreeElementValue;
    DBV : TDBValue;
    SL : TStringList;
    S, S0 : String;
    ind, ws, we, i : Integer;
    aExpr : TSqliteExpr;
    p : TPoint;
begin
  if FNodeEditing then Exit;
  FNodeEditing := true;
  try
    if assigned(FCurTableNode) and (FCurTableNode.Kind = ttekValue) then
    begin
      S0 := TEdit(Sender).Text;
      p := TEdit(Sender).CaretPos;
      aExpr := TSqliteExpr.Create(S0);
      try
        ws := 0;
        for ind := 0 to aExpr.Count-1 do
        if (aExpr[ind].Pos <= p.X) and
           ((aExpr[ind].Pos + aExpr[ind].Len) >= p.X) and
           (aExpr[ind].Kind in [stkKeyWord, stkDataType, stkIdentifier,
                               stkTableName, stkFieldName, stkNumber,
                               stkString]) then
        begin
          ws := aExpr[ind].Pos;
          we := aExpr[ind].Pos + aExpr[ind].Len - 1;
          S := UTF8Copy(S0, ws, p.X - aExpr[ind].Pos + 1);
        end;
      finally
        aExpr.Free;
      end;
      if (Length(S) > 0) and (ws > 0) then
      begin
        ENode := TDBTreeElementValue(FCurTableNode);
        DBV := TDBValue(ENode.OwnerConstr.ValueExprs[ENode.Ind]);
        SL := TStringList.Create;
        try
          ind := -1;
          case DBV.Kind of
            dbvkTable :
              CompleteHint.CompletionKeys.CompleteFillList(SL, S, ind, [sckTable]);
            dbvkColumns, dbvkIndexedColumns : begin
              CompleteHint.CompletionKeys.CompleteFillList(SL, S, ind, [sckField]);

              if assigned(cTable) then
              for i := 0 to cTable.Count-1 do
              begin
                if SL.IndexOf(cTable[i].FieldName) < 0 then
                begin
                  if sqluCompareNames(UTF8Copy(cTable[i].FieldName,
                                               1, UTF8Length(S)), S) then begin
                    SL.Add(cTable[i].FieldName);
                    if ind < 0 then ind := SL.Count-1;
                  end;
                end;
              end;
            end;
          end;
          if (SL.Count > 0) and (ind >= 0) then
          begin
            TEdit(Sender).Text := UTF8Copy(S0, 1, ws - 1) +
                                  SL[ind] +
                                  UTF8Copy(S0, we + 1, Length(S0));
            TEdit(Sender).SelLength := UTF8Length(SL[ind]) + ws - p.X - 1;
            TEdit(Sender).SelStart := p.X;
            TEdit(Sender).CaretPos := p;
          end else
          if DBV.Kind in [dbvkTable, dbvkColumns, dbvkIndexedColumns] then
          begin
            TEdit(Sender).Text := UTF8Copy(S0, 1, ws - 1) +
                                  S +
                                  UTF8Copy(S0, we + 1, Length(S0));
            TEdit(Sender).SelLength := 0;
            TEdit(Sender).CaretPos := p;
          end;
        finally
          SL.Free;
        end;
      end;
    end;
  finally
    FNodeEditing := false;
  end;
end;

end.


