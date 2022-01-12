{
 dbComposerStruct:
   Types and classes for working with Sqlite3 database structures

   Part of dbComposer project

   Copyright (c) 2021-2022 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit dbComposerStruct;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  ECommonObjs, AvgLvlTree, Laz_AVL_Tree, OGLFastList,
  kcThreadPool,
  DB, ExtSqlite3DS, OGLB64Utils,
  ExprSqlite3Funcs, ExtSqliteUtils, ExtSqliteTokens,
  ExprComparator;

type
  TDBTable = class;
  TDBStructure = class;
  TDBExtBlob = class;
  TDBExtBlobValue = class;
  TDBForIndxStringTable = class;

  TDBExtActualResult = (earActual, earUnActual, earNoInformation);
  TDBCheckActual = function (aId : TDBExtBlobValue) : TDBExtActualResult of object;

  { TDBValue }

  TDBValue = class(TSqliteExpr)
  private
    FKind : TSqliteValueKind;
  public
    constructor Create(const aExpr : String; aKind : TSqliteValueKind); overload;
    constructor Create(aExpr : TSqliteExpr; aKind : TSqliteValueKind); overload;
    property Kind : TSqliteValueKind read FKind;
  end;

  { TDBValuesList }

  TDBValuesList = class(specialize TFastBaseCollection <TDBValue>)
  public
    function IndexOfValue(v : TDBValue) : Integer;
  end;

  { TDBConstraint }

  TDBConstraint = class
  private
    FKind : TSqliteConstrKind;
    FName : String;
    FValues : TDBValuesList;
    FOptions : TStringList;
    function GetOption(Index : Integer) : String;
    function GetOptionsCount : Integer;
    function GetValue(Index : Integer) : String;
    function GetValue : String;
    function GetValueExpr(Index : Integer): TDBValue;
    function GetValuesCount : Integer;
    procedure Initialize;
  public
    constructor Create(aKind : TSqliteConstrKind; const aName : String); overload;
    constructor Create(aConstr : TDBConstraint); overload;
    destructor Destroy; override;

    procedure Assign(aConstr : TDBConstraint);
    function Compare(aConstr : TDBConstraint) : Boolean;

    procedure AddValue(const aVal : String; valKind : TSqliteValueKind);
    procedure UpdateValue(ind : integer; const newValue : String);

    procedure AddOption(const aOpt : String); overload;
    procedure AddOption(OptInd : Cardinal; kf : TSqliteKwFormatOption); overload;
    procedure AddOption(const OptInd : Array of Cardinal; kf : TSqliteKwFormatOption); overload;
    function CheckOption(const OptName : String) : Boolean; overload;
    function CheckOption(OptInd : Cardinal) : Boolean; overload;
    procedure DeleteOption(const OptName : String);
    procedure ReplaceOption(const OptName, newName : String);

    function ConflictClause : String;
    function OnUpdate : String;
    function OnDelete : String;
    function OptionBegins(const StartWith : String) : String;

    property Name : String read FName;
    property Value : String read GetValue;
    property Kind : TSqliteConstrKind read FKind;
    property Option[Index : Integer] : String read GetOption;
    property OptionsCount : Integer read GetOptionsCount;
    property Values[Index : Integer] : String read GetValue;
    property ValueExprs[Index : Integer] : TDBValue read GetValueExpr;
    property ValuesCount : Integer read GetValuesCount;
  end;

  { TDBConstraints }

  TDBConstraints = class(specialize TFastBaseCollection <TDBConstraint>)
  public
    procedure Add(aKind : TSqliteConstrKind; const aName : String); overload;
    procedure Assign(aConstraints : TDBConstraints);
    function Compare(aConstraints : TDBConstraints) : Boolean;
  end;

  { TDBField }

  TDBField = class
  private
    FTable : TDBTable;
    FConstraints : TDBConstraints;

    FFieldName, FQuotedName : String;
    FFieldType : TFieldType;
    FFieldDefault : String;

    FIsPrimaryKey : Boolean;
    FIsForeignKey : Boolean;
    FForeignKeyTable : TDBTable;
    FForeignKeyTo    : String;

    FIsNameNeedQuoting : Boolean;

    function GetFieldStrType : String;
    function GetForeignKeyTableName : String;
    function GetFullFieldName : String;
    function GetQuotedName : String;
  public
    constructor Create(aOwner : TDBTable;
      const FN, FT, FD : String; IsPK : Boolean = false); overload;
    constructor Create(aField : TDBField; const aValue : String; IsPK : Boolean);
      overload;
    procedure UpdateForeignKeyInfo(FT : TDBTable; const FKTO : String);
    destructor Destroy; override;

    procedure SetFieldName(const aName : String);
    procedure SetFieldType(const aType : String);

    function CompareName(const S : String) : Boolean;
    function Compare(aField : TDBField) : Boolean;

    property IsNameNeedQuoting : Boolean read FIsNameNeedQuoting;
    property IsPrimaryKey : Boolean read FIsPrimaryKey;
    property FieldName : String read FFieldName;
    property QuotedName : String read GetQuotedName;
    property FullFieldName : String read GetFullFieldName;
    property FieldType : TFieldType read FFieldType;
    property FieldStrType : String read GetFieldStrType;
    property FieldDefault : String read FFieldDefault;

    property IsForeignKey : Boolean read FIsForeignKey;
    property ForeignKeyTable : TDBTable read FForeignKeyTable;
    property ForeignKeyTableName : String read GetForeignKeyTableName;
    property ForeignKeyTo : String read FForeignKeyTo;

    function GetFullDefinition(aKwFormat : TSqliteKwFormatOption) : String;

    property Table : TDBTable read FTable;
    property Constraints : TDBConstraints read FConstraints;
  end;

  { TDBTable }

  TDBTable = class(TThreadSafeFastCollection)
  private
    FStruct : TDBStructure;
    FConstraints : TDBConstraints;
    FIsTemp : Boolean;
    FCheckExists : Boolean;
    FWORowID : Boolean;
    FIsNameNeedQuoting : Boolean;
    FName, FSchema, FQuotedName : String;
    function GetDBField(index : integer) : TDBField;
    function GetHasForeignKeys: Boolean;
    function GetHasPrimaryKey : Boolean;
    function GetOption(Index : TSqliteTableOption) : Boolean;
    function GetOptionByName(const Index : String) : Boolean;
    function GetPrimaryKeyField : TDBField;
    function GetQuotedName : String;
    function GetQuotedSchema: String;
    procedure SetOption(Index : TSqliteTableOption; AValue : Boolean);
    procedure SetOptionByName(const Index : String; AValue : Boolean);
    procedure SetStructure(AValue : TDBStructure);
  public
    constructor Create(aOwner : TDBStructure; const aName : String); overload;
    destructor Destroy; override;
    procedure SetName(const S : String);
    procedure SetSchema(const S : String);
    function CompareName(const S : String) : Boolean;
    function CompareSchema(const S : String) : Boolean;
    function Compare(aTable : TDBTable) : Boolean;
    function ByName(const S : String) : TDBField;
    function AddField(const FN, FT, FD : String; IsPK : Boolean = false) : TDBField; overload;
    function AddField(const FN : String; FT : TSqliteDataTypeAffinity; const FD : String; IsPK : Boolean = false) : TDBField; overload;
    function AddField(const FN : String; FT : TSqliteDataTypeAffinity; IsPK : Boolean = false) : TDBField; overload;
    property DBField[index : integer] : TDBField read GetDBField; default;
    property PrimaryKeyField : TDBField read GetPrimaryKeyField;
    property Name : String read FName;
    property Schema : String read FSchema;
    property QuotedName : String read GetQuotedName;
    property Structure : TDBStructure read FStruct write SetStructure;
    property HasForeignKey : Boolean read GetHasForeignKeys;
    property HasPrimaryKey : Boolean read GetHasPrimaryKey;
    property IsNameNeedQuoting : Boolean read FIsNameNeedQuoting;

    property Constraints : TDBConstraints read FConstraints;

    function BuildCreateExpression(OneLined : Boolean;
      aKwFormat : TSqliteKwFormatOption) : String;
    property IsTemp : Boolean read FIsTemp write FIsTemp;
    property CheckExists : Boolean read FCheckExists write FCheckExists;
    property WithoutRowID : Boolean read FWORowID write FWORowID;
    property Options[Index : TSqliteTableOption] : Boolean read GetOption write
                                                                SetOption;
    property OptionByName[const Index : String] : Boolean read GetOptionByName
                                                           write SetOptionByName;
  end;

  TDBAlterTableKind = (atkNotReady, atkAltering, atkRecreate);

  { TDBAlterField }

  TDBAlterField = class
  private
    FField : TDBField;
    FFieldIndex : Integer;
    FRatio : Single;
  public
    constructor Create(aField : TDBField; aIndex : integer);

    function Compare(aAField : TDBAlterField) : Integer;
    //
    property Ratio : Single read FRatio write FRatio;
    property Field : TDBField read FField;
    property FieldIndex : Integer read FFieldIndex;
  end;

  { TDBAlterFields }

  TDBAlterFields = class(specialize TFastBaseCollection <TDBAlterField>)
  private
    FOrigField : TDBField;
  public
    constructor Create(aOrigField : TDBField); overload;
    function Compare(aAFields : TDBAlterFields) : Integer;
    procedure CompareAllAndSort;
    procedure Exclude(aField : TDBField);
    function BetterAltCol : TDBField;
    property OrigField : TDBField read FOrigField;
  end;

  { TDBTableAlterComp }

  TDBTableAlterComp = class(specialize TFastBaseCollection <TDBAlterFields>)
  private
    FOrig, FAlt : TDBTable;
    FAlterTableKind : TDBAlterTableKind;
    FAlterExpr : String;
    function  GetAlterExpr : String;
  public
    constructor Create(aOrig, aAlt : TDBTable); overload;

    procedure Compare;

    property AlterKind : TDBAlterTableKind read FAlterTableKind;
    property AlterExpr : String read GetAlterExpr;
  end;

  { TDBBlobListener }

  TDBBlobListener = class
  private
    FCheckActual: TDBCheckActual;
    FNotify : TNotifyEvent;
  public
    constructor Create(aNotify : TNotifyEvent; aCheckActual : TDBCheckActual);
    property Notify : TNotifyEvent read FNotify;
    property CheckActual : TDBCheckActual read FCheckActual;
  end;

  TDBExtBlobKind = (ebkNo, ebkImage, ebkText, ebkFormattedText);

  { TDBExtBlobValue }

  TDBExtBlobValue = class(TNetCustomLockedObject)
  private
    function GetLoading: Boolean;
    procedure SetLoading(AValue: Boolean);
    procedure SetLoaded(AValue: Boolean);
    procedure SetValid(AValue: Boolean);
  private
    FLoaded  : Boolean;
    FLoading : Boolean;
    FActual  : Boolean;
    FValid   : Boolean;
    FPath   : String;
    FOwner  : TDBExtBlob;
    function GetActual : Boolean;
    function GetFullPath: String;
    function GetLoaded : Boolean;
    function GetValid : Boolean;
    procedure Initialize; virtual; abstract;
    function  DoLoad : Boolean; virtual; abstract;
    procedure DoClear; virtual; abstract;
    procedure SetActual(AValue : Boolean);
    procedure SetPath(AValue: String);
    property Loading : Boolean read GetLoading write SetLoading;
  public
    constructor Create(aOwner : TDBExtBlob; const aPath : String);

    procedure Load;
    procedure Clear;
    procedure CheckValid;
    function CheckActual : Boolean;

    property FullPath : String read GetFullPath;
    property Path : String read FPath write SetPath;
    property Loaded : Boolean read GetLoaded;
    property Actual : Boolean read GetActual write SetActual;
    property Valid  : Boolean read GetValid;
    property Owner  : TDBExtBlob read FOwner;
  end;

  { TDBExtBlobValueHolder }

  TDBExtBlobValueHolder = class
  private
    FValueObj : TDBExtBlobValue;
    FId : Integer;
  public
    constructor Create(aObj : TDBExtBlobValue; aId : Integer);
    property Id : Integer read FId;
    property ValueObj : TDBExtBlobValue read FValueObj write FValueObj;
  end;

  TDBExtBlobValueClass = class of TDBExtBlobValue;

  TDBExtBlobWorking = function (O : TDBExtBlobValue) : Boolean of object;

  { TDBExtBlob }

  TDBExtBlob = class
  private
    FField : TDBField;
    FIsUniqPair: Boolean;
    FPathValues : TStringToPointerTree;
    FIdValues   : TAvgLvlTree;
    FRootPath: String;
    FValuesLocker : TNetCustomLockedObject;
    FListeners : TThreadSafeFastCollection;

    function AllwaysActual(aId : TDBExtBlobValue) : TDBExtActualResult;

    function GetById(Id : integer) : TDBExtBlobValue;
    function GetHolderById(Id : integer) : TDBExtBlobValueHolder;
    function GetByPath(const aPath : String): TDBExtBlobValue;
    function GetTableName: String;
    procedure Initialize(const aRootPath : String; aField : TDBField;
                              aIsUniqPairs : Boolean); virtual;
    procedure LoadAllActualValues;
    procedure UnLoadAllUnActualValues;
    function OnExtBlobValuesCompare({%H-}Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
    procedure ForAllValuesDo(aWork: TDBExtBlobWorking; limit: integer);

    function CheckValueValid(O : TDBExtBlobValue) : Boolean;
    function CheckValueActual(O : TDBExtBlobValue) : Boolean;
    function CheckValueUnActual(O : TDBExtBlobValue) : Boolean;
  protected
    procedure NotifyValueLoaded(O : TDBExtBlobValue); virtual;
    function CheckIsValueActual(aID: TDBExtBlobValue) : Boolean;
  public
    constructor Create(aTable : TDBTable; const aRootPath, FN : String;
                              aIsUniqPairs : Boolean); overload;
    constructor Create(aDB : TDBStructure; const aRootPath, TN, FN : String;
                              aIsUniqPairs : Boolean); overload;

    function CheckField(aField : TDBField) : Boolean;
    function CheckPath(const aTable, aField : String) : Boolean;
    function GetAbsolutePath(const relPath : String) : String;
    function GetRelativePath(const absPath : String) : String;

    function AddValue(Id : Integer; const aPath : String) : TDBExtBlobValue; virtual;
    procedure NotifyValueUpdated(Id : integer);

    property ValueById[Id : integer] : TDBExtBlobValue read GetById;
    property ValueByPath[const aPath : String] : TDBExtBlobValue read GetByPath;

    class function Kind : TDBExtBlobKind; virtual;
    class function ValueClass : TDBExtBlobValueClass; virtual;

    procedure AddListener(L : TNotifyEvent; CA : TDBCheckActual = nil);
    procedure RemoveListener(L : TNotifyEvent);

    property RootPath : String read FRootPath write FRootPath;
    property TableName : String read GetTableName;
    property IsUniqPair : Boolean read FIsUniqPair write FIsUniqPair;

    destructor Destroy; override;
  end;

  { TDBExtBlobImageValue }

  TDBExtBlobImageValue = class(TDBExtBlobValue)
  private
    FBitmap : TPicture;
    procedure Initialize; override;
    function  DoLoad : Boolean; override;
    procedure DoClear; override;
  public
    destructor Destroy; override;
    property Bitmap : TPicture read FBitmap;
  end;

  { TDBExtImageBlob }

  TDBExtImageBlob = class(TDBExtBlob)
  private
    function GetBitmap(Id : integer) : TPicture;
  public
    property MiniImage[Id : integer] : TPicture read GetBitmap;
    class function Kind : TDBExtBlobKind; override;
    class function ValueClass : TDBExtBlobValueClass; override;
  end;

  { TDBExtBlobs }

  TDBExtBlobs = class(TFastCollection)
  private
    function GetExtBlob(index : integer) : TDBExtBlob;
  public
    property ExtBlob[index : integer] : TDBExtBlob read GetExtBlob; default;
  end;

   TDBIndxStringsStyle = (issUnknown, issStrings, issConstList);

  { TDBForIndxStringTable }

  TDBForIndxStringTable = class
  private
    FTable : TDBTable;
    FStyle : TDBIndxStringsStyle;
    FIndx : TBaseSinExprs;
  public
    constructor Create(aStyle : TDBIndxStringsStyle;
                       aIndx : TBaseSinExprs;  aTable : TDBTable);
    property Indx : TBaseSinExprs read FIndx;
    property Table : TDBTable read FTable;
    property Style : TDBIndxStringsStyle read FStyle;
  end;

  TDBForIndxStringTables = class(specialize TFastBaseCollection <TDBForIndxStringTable>);

  { TDBStructure }

  TDBStructure = class(TThreadSafeFastCollection)
  private
    FDataSet : TExtSqlite3Dataset;
    FExtBlobs : TDBExtBlobs;
    FForStrTables : TDBForIndxStringTables;
    function GetDBTable(index : integer) : TDBTable;
  public
    constructor Create(aDataSet : TExtSqlite3Dataset); overload;
    destructor Destroy; override;

    procedure Clear; override;

    procedure AddNewExtBlob(aKind: TDBExtBlobKind; const aRootPath, TN,
      FN: String; aIsUniqPair : Boolean);
    procedure AddNewExtBlob(aKind: TDBExtBlobKind; const aRootPath: String;
      aTable: TDBTable; const FN: String; aIsUniqPair : Boolean);
    function GetExtBlob(aField : TDBField) : TDBExtBlob; overload;
    function GetExtBlob(const aTable, aField : String) : TDBExtBlob; overload;
    function GetExtBlob(const aPath : String) : TDBExtBlob; overload;
    procedure AddListener(L : TNotifyEvent; CA : TDBCheckActual);
    procedure RemoveListener(L : TNotifyEvent);
    procedure NotifyTableUpdated(aTable : TDBTable; aId : integer);
    function IsExtBlobTable(aTable : TDBTable; aKind : TDBExtBlobKind) : Boolean;

    procedure AddForeignIndxStringTable(aIndxStyle : TDBIndxStringsStyle;
                                        aIndx : TBaseSinExprs;
                                        aTable : TDBTable);
    function IsForeignIndxStringTable(aTable : TDBTable) : TDBForIndxStringTable;
    function CheckIsReferenced(aField : TDBField) : Boolean;

    procedure DoTimerTick;

    function ByName(const S : String) : TDBTable;
    function BySchemaAndName(const aSchema, aName : String) : TDBTable;
    property DBTable[index : integer] : TDBTable read GetDBTable; default;
    property DataSet : TExtSqlite3Dataset read FDataSet;
  end;

  { TDBExtBlobJob }

  TDBExtBlobJob = class(TJob)
  private
    FValue : TDBExtBlobValue;
  public
    constructor Create(AValue : TDBExtBlobValue); overload;
  end;

  { TDBLoadBlobValueJob }

  TDBLoadBlobValueJob = class(TDBExtBlobJob)
  public
    procedure Execute; override;
  end;

  { TDBClearBlobValueJob }

  TDBClearBlobValueJob = class(TDBExtBlobJob)
  public
    procedure Execute; override;
  end;

implementation

uses dbComposerUtils, LazUtf8, Math;

const cOpenBracket = '(';
      cCloseBracket = ')';
      cPeriod = ',';
      cSemiColumn = ';';
      cNewLine = #10;
      cTabs = '  ';

function AltFieldCompare(obj1: TObject; obj2 : TObject) : Integer;
begin
  Result := TDBAlterField(obj1).Compare(TDBAlterField(obj2));
end;

function AltFieldsCompare(obj1: TObject; obj2 : TObject) : Integer;
begin
  Result := TDBAlterFields(obj1).Compare(TDBAlterFields(obj2));
end;

{ TDBAlterField }

constructor TDBAlterField.Create(aField : TDBField; aIndex : integer);
begin
  FField := aField;
  FFieldIndex := aIndex;
  FRatio := 1;
end;

function TDBAlterField.Compare(aAField : TDBAlterField) : Integer;
begin
  Result := CompareValue(FRatio, aAField.Ratio);
end;

{ TDBAlterFields }

constructor TDBAlterFields.Create(aOrigField : TDBField);
begin
  inherited Create;
  FOrigField := aOrigField;
end;

function TDBAlterFields.Compare(aAFields : TDBAlterFields) : Integer;
begin
  if (Count = 0) then
  begin
    if (aAFields.Count = 0) then
      Result := 0 else
      Result := -1;
  end else
  if (aAFields.Count = 0) then
     Result := 1
  else
  begin
    Result := CompareValue(Self[0].Ratio, aAFields[0].Ratio);
  end;
end;

procedure TDBAlterFields.CompareAllAndSort;
var i, j, k, n, s : integer;
    used : Array of Boolean;
begin
  for i := 0 to Count-1 do
  begin
    s := 0;
    if OrigField.FieldType = Self[i].Field.FieldType then
      Inc(s);
    if OrigField.IsForeignKey = Self[i].Field.IsForeignKey then
      Inc(s);
    if OrigField.IsPrimaryKey = Self[i].Field.IsPrimaryKey then
      Inc(s);

    if Self[i].Field.Constraints.Count > 0 then
    begin
      SetLength(used, Self[i].Field.Constraints.Count);
      FillByte(used[0], Length(used), $ff);
      for j := 0 to OrigField.Constraints.Count-1 do
      begin
        k := -1;
        for n := 0 to Self[i].Field.Constraints.Count-1 do
        if used[n] then
        begin
          if OrigField.Constraints[j].Compare(Self[i].Field.Constraints[n]) then
          begin
            k := n;
            Break;
          end;
        end;
        if k >= 0 then
        begin
          used[k] := false;
          inc(s);
        end;
      end;
    end;

    Self[i].FRatio := 1.0 - (0.75 * Single(s) / Single(3 + OrigField.Constraints.Count) +
                             0.25 * (1.0 - DLdist(OrigField.FieldName, Self[i].Field.FieldName)));
  end;

  SortList(@AltFieldCompare);
end;

procedure TDBAlterFields.Exclude(aField : TDBField);
var i,k  : integer;
begin
  k := -1;
  for i := 0 to Count-1 do
  begin
    if Self[i].Field = aField then begin
      k := i;
      Break;
    end;
  end;
  if k >= 0 then Delete(k);
end;

function TDBAlterFields.BetterAltCol : TDBField;
begin
  if Count > 0 then
  begin
    Result := Self[0].Field;
  end else
    Result := nil;
end;

{ TDBTableAlterComp }

function TDBTableAlterComp.GetAlterExpr : String;
var
  Exprs : TSqliteExprs;
  Expr, ExprSel  : TSqliteExpr;
  i, j, k : integer;
  fmt : TSqliteKwFormatOption;
  s0, s: string;
  dbS : TDBStructure;
  fkpkch : Boolean;
begin
  if Length(FAlterExpr) > 0 then Exit(FAlterExpr);
  if FAlterTableKind = atkNotReady then
    Compare;
  Randomize;
  FAlterExpr := '';
  fmt := skfoUpperCase;
  // generating alt expression
  Exprs := TSqliteExprs.Create('');
  try
    case FAlterTableKind of
      atkRecreate : begin
        // generate recreate expression

        fkpkch := true; //by default

        //step 3-4
        //Use CREATE TABLE to construct a new table "new_X" that is in the
        // desired revised format of table X. Make sure that the name "new_X"
        // does not collide with any existing table name, of course.
        s0 := FOrig.Name;
        s := 'new_' + EncodeInt64ToB64(Random(High(Int64)), 8) + '_' + s0;
        FOrig.SetName(s);
        Expr := TSqliteExpr.Create(FOrig.BuildCreateExpression(true, fmt));
        FOrig.SetName(s0);
        Exprs.Add(Expr);

        //step 5
        //Transfer content from X into new_X using a statement like:
        // INSERT INTO new_X SELECT ... FROM X.
        k := 0;
        Expr := TSqliteExpr.Create;
        ExprSel := TSqliteExpr.Create;
        try
          ExprSel.AddK(kwSELECT, fmt);
          with Expr do
          begin
            AddKs([kwINSERT, kwINTO], fmt); AddId(s);

            AddS('(');
            for i := 0 to Self.Count-1 do
            begin
              if Assigned(Self[i].BetterAltCol) then
              begin
                if k > 0 then begin
                  AddS(',');
                  ExprSel.AddS(',');
                end;
                AddId(Self[i].OrigField.FieldName);
                ExprSel.AddId(Self[i].BetterAltCol.FieldName);
                Inc(k);
              end;
            end;
            AddS(')');
          end;
          ExprSel.AddK(kwFROM, fmt);
          ExprSel.AddId(s0);
          Expr.AddExpr(ExprSel);
        finally
          ExprSel.Free;
        end;
        if k > 0 then
          Exprs.Add(Expr) else
          Expr.Free;

        //step 6
        //  Drop the old table X: DROP TABLE X.
        Expr := TSqliteExpr.Create;
        with Expr do
        begin
          AddKs([kwDROP, kwTABLE], fmt); AddId(s0);
        end;
        Exprs.Add(Expr);

        //step 7
        //  Change the name of new_X to X using: ALTER TABLE new_X RENAME TO X.
        Expr := TSqliteExpr.Create;
        with Expr do
        begin
          AddKs([kwALTER, kwTABLE], fmt); AddId(s);
          AddKs([kwRENAME, kwTO], fmt); AddId(s0);
        end;
        Exprs.Add(Expr);
      end;
      atkAltering : begin
        // generate altering expressions
        dbS := FAlt.Structure;
        fkpkch := not Assigned(dbS);
        //check renames
        for i := 0 to Count-1 do
        begin
          if Assigned(Self[i].BetterAltCol) then
          begin
            if not Self[i].OrigField.CompareName(Self[i].BetterAltCol.FieldName) then
            begin
              if Assigned(dbS) and
                 dbS.CheckIsReferenced(Self[i].BetterAltCol) then
                 fkpkch := true;

              Expr := TSqliteExpr.Create;
              with Expr do
              begin
                AddKs([kwALTER, kwTABLE], fmt);
                AddId(FOrig.Name);
                AddK(kwRENAME, fmt);
                AddId(Self[i].BetterAltCol.QuotedName);
                AddK(kwTO, fmt);
                AddId(Self[i].OrigField.QuotedName);
              end;
              Exprs.Add(Expr);
            end;
          end;
        end;
        //check additions
        for i := 0 to Count-1 do
        begin
          if not Assigned(Self[i].BetterAltCol) then
          begin
            Expr := TSqliteExpr.Create;
            with Expr do
            begin
              AddKs([kwALTER, kwTABLE], fmt);
              AddId(FOrig.Name);
              AddK(kwADD, fmt);
              AddExpr(Self[i].OrigField.GetFullDefinition(fmt));
            end;
            Exprs.Add(Expr);
          end;
        end;
        //check deletions
        for i := 0 to FAlt.Count-1 do
        begin
          k := -1;
          for j := 0 to Count-1 do
          begin
            if Self[j].BetterAltCol = FAlt[i] then
            begin
              k := j;
              break;
            end;
          end;
          if k < 0 then
          begin
            if Assigned(dbS) and
               dbS.CheckIsReferenced(FAlt[i]) then
               fkpkch := true;

            Expr := TSqliteExpr.Create;
            with Expr do
            begin
              AddKs([kwALTER, kwTABLE], fmt);
              AddId(FOrig.Name);
              AddK(kwDROP, fmt);
              AddId(FAlt[i].QuotedName);
            end;
            Exprs.Add(Expr);
          end;
        end;
      end;
      else
        fkpkch := false;
    end;

    if fkpkch then
    begin
      //step 1
      //If foreign key constraints are enabled, disable them using
      //  PRAGMA foreign_keys=OFF.
      Expr := TSqliteExpr.Create;
      with Expr do
      begin
        AddK(kwPRAGMA, fmt); AddExpr('foreign_keys=OFF');
      end;
      Exprs.Insert(0, Expr);

      //step 2
      //Start a transaction.
      s := 'savepoint_' + EncodeInt64ToB64(Random(High(Int64)), 8);
      Expr := TSqliteExpr.Create;
      with Expr do
      begin
        AddK(kwSAVEPOINT, fmt); AddId(s);
      end;
      Exprs.Insert(1, Expr);

      //step 10
      //  If foreign key constraints were originally enabled then run
      //  PRAGMA foreign_key_check to verify that the schema change did not
      //  break any foreign key constraints.
      Expr := TSqliteExpr.Create;
      with Expr do
      begin
        AddK(kwPRAGMA, fmt); AddExpr('foreign_key_check');
      end;
      Exprs.Add(Expr);

      //step 11
      Expr := TSqliteExpr.Create;
      with Expr do
      begin
        AddK(kwRELEASE, fmt); AddId(s);
      end;
      Exprs.Add(Expr);

      //step 12
      Expr := TSqliteExpr.Create;
      with Expr do
      begin
        AddK(kwPRAGMA, fmt); AddExpr('foreign_keys=ON');
      end;
      Exprs.Add(Expr);
    end;

    FAlterExpr := Exprs.FormatedStr(skfoOriginal, -1, #10);
  finally
    Exprs.Free;
  end;
  Result := FAlterExpr;
end;

constructor TDBTableAlterComp.Create(aOrig, aAlt : TDBTable);
var i : integer;
begin
  inherited Create;
  FOrig := aOrig;
  FAlt := aAlt;
  for i := 0 to FOrig.Count-1 do
  begin
    Add(TDBAlterFields.Create(FOrig.DBField[i]));
  end;
  FAlterTableKind := atkNotReady;
  FAlterExpr := '';
end;

procedure TDBTableAlterComp.Compare;
var i, j  : integer;
    f : TDBField;
begin
  FAlterExpr := '';
  //check if cols constr/datatype changed
  for i := 0 to Count-1 do
  begin
    Self[i].Clear;
    for j := 0 to FAlt.Count-1 do
    begin
      Self[i].Add(TDBAlterField.Create(FAlt.DBField[j], j));
    end;
    Self[i].CompareAllAndSort;
  end;
  SortList(@AltFieldsCompare);
  for i := 0 to Count-1 do
  begin
    f := Self[i].BetterAltCol;
    for j := i + 1 to Count-1 do
      Self[j].Exclude(f);
  end;
  for i := 0 to Count-1 do
  if Assigned(Self[i].BetterAltCol) and
     ((not (Self[i].OrigField.Constraints.Compare(Self[i].BetterAltCol.Constraints)) or
      (Self[i].OrigField.FieldType <> Self[i].BetterAltCol.FieldType))) then
  begin
    FAlterTableKind := atkRecreate;
    Exit;
  end;
  //check if table constr changed
  if not FOrig.Constraints.Compare(FAlt.Constraints) then
  begin
    FAlterTableKind := atkRecreate;
  end else
    //constr/datatype not changed -> only cols added, renamed or removed
    FAlterTableKind := atkAltering;
end;

{ TDBValue }

constructor TDBValue.Create(const aExpr: String; aKind: TSqliteValueKind);
begin
  inherited Create(aExpr);
  FKind := aKind;
end;

constructor TDBValue.Create(aExpr: TSqliteExpr; aKind: TSqliteValueKind);
begin
  inherited Create(aExpr);
  FKind := aKind;
end;

{ TDBValuesList }

function TDBValuesList.IndexOfValue(v : TDBValue) : Integer;
var i : integer;
begin
  for i := 0 to Count-1 do
  begin
    if Self[i].Compare(v) then
      Exit(i);
  end;
  Result := -1;
end;

{ TDBConstraints }

procedure TDBConstraints.Add(aKind : TSqliteConstrKind; const aName : String);
begin
  Add(TDBConstraint.Create(aKind, aName));
end;

procedure TDBConstraints.Assign(aConstraints : TDBConstraints);
var
  i : integer;
begin
  if not Assigned(aConstraints) then Exit;
  Clear;
  for i := 0 to aConstraints.Count-1 do
  begin
    Add(TDBConstraint.Create(aConstraints[i]));
  end;
end;

function TDBConstraints.Compare(aConstraints : TDBConstraints) : Boolean;
var i, j, k : integer;
    used : Array of Boolean;
begin
  if Count <> aConstraints.Count then Exit(false);
  if aConstraints.Count > 0 then
  begin
    SetLength(used, aConstraints.Count);
    FillByte(used[0], Length(used), $ff);
    for i := 0 to Count-1 do
    begin
      k := -1;
      for j := 0 to Count-1 do
      if used[j] then
      begin
        if Self[i].Compare(aConstraints[j]) then
        begin
          k := j;
          Break;
        end;
      end;
      if k < 0 then Exit(false);
      used[k] := false;
    end;
  end;
  Result := true;
end;

{ TDBConstraint }

function TDBConstraint.GetOption(Index : Integer) : String;
begin
  Result := FOptions[index];
end;

function TDBConstraint.GetOptionsCount : Integer;
begin
  Result := FOptions.Count;
end;

function TDBConstraint.GetValue(Index : Integer) : String;
begin
  Result := FValues[index].OrigExpr;
end;

function TDBConstraint.GetValue : String;
begin
  if FValues.Count > 0 then Result := FValues[0].OrigExpr else Result := '';
end;

function TDBConstraint.GetValueExpr(Index : Integer) : TDBValue;
begin
  Result := FValues[index];
end;

function TDBConstraint.GetValuesCount : Integer;
begin
  Result := FValues.Count;
end;

procedure TDBConstraint.Initialize;
begin
  FValues := TDBValuesList.Create;
  FOptions := TStringList.Create;
end;

constructor TDBConstraint.Create(aKind : TSqliteConstrKind;
                                    const aName : String);
begin
  Initialize;
  FKind := aKind;
  FName := aName;
end;

constructor TDBConstraint.Create(aConstr : TDBConstraint);
begin
  Initialize;
  Assign(aConstr);
end;

destructor TDBConstraint.Destroy;
begin
  FValues.Free;
  FOptions.Free;
  inherited Destroy;
end;

procedure TDBConstraint.Assign(aConstr : TDBConstraint);
var i : integer;
begin
  if not Assigned(aConstr) then Exit;
  FValues.Clear;
  for i := 0 to aConstr.FValues.Count-1 do
  begin
    FValues.Add(TDBValue.Create(aConstr.FValues[i], aConstr.FValues[i].Kind));
  end;
  FOptions.Assign(aConstr.FOptions);
  FName := aConstr.Name;
  FKind := aConstr.Kind;
end;

function TDBConstraint.Compare(aConstr : TDBConstraint) : Boolean;
var i : integer;
begin
  if aConstr.Kind = Kind then
  begin
    for i := 0 to FValues.Count-1 do
      if aConstr.FValues.IndexOfValue(FValues[i]) < 0 then
        Exit(false);
    for i := 0 to FOptions.Count-1 do
      if aConstr.FOptions.IndexOf(FOptions[i]) < 0 then
        Exit(false);
    Result := true;
  end else
    Result := false;
end;

procedure TDBConstraint.AddOption(const aOpt : String);
begin
  FOptions.Add(aOpt);
end;

procedure TDBConstraint.AddOption(OptInd : Cardinal; kf : TSqliteKwFormatOption
  );
begin
  if not CheckOption(OptInd) then
  begin
    AddOption(sqluGetIndexedKeyWord(OptInd, kf));
  end;
end;

procedure TDBConstraint.AddOption(const OptInd : array of Cardinal;
  kf : TSqliteKwFormatOption);
var S : String;
begin
  S := sqluGetIndexedKeyWords(OptInd, kf);
  if not CheckOption(S) then
    AddOption(S);
end;

procedure TDBConstraint.AddValue(const aVal: String; valKind: TSqliteValueKind);
begin
  FValues.Add(TDBValue.Create(aVal, valKind));
end;

procedure TDBConstraint.UpdateValue(ind: integer; const newValue: String);
var k : TSqliteValueKind;
begin
  if (ind >= 0) and (ind < FValues.Count) then
  begin
    k := FValues[ind].Kind;
    FValues[ind].Free;
    FValues[ind] := TDBValue.Create( newValue, k );
  end;
end;

function TDBConstraint.CheckOption(const OptName : String) : Boolean;
begin
  Result := FOptions.IndexOf(OptName) >= 0;
end;

function TDBConstraint.CheckOption(OptInd: Cardinal): Boolean;
begin
  Result := CheckOption(sqluGetIndexedKeyWord(OptInd));
end;

procedure TDBConstraint.DeleteOption(const OptName : String);
var k : integer;
begin
  k := FOptions.IndexOf(OptName);
  if k >= 0 then
  begin
    FOptions.Delete(k);
  end;
end;

procedure TDBConstraint.ReplaceOption(const OptName, newName: String);
var i : integer;
begin
  i := FOptions.IndexOf(OptName);
  if i >= 0 then FOptions[i] := newName;
end;

function TDBConstraint.ConflictClause: String;
begin
  Result := OptionBegins(sqluGetIndexedKeyWords([kwON, kwCONFLICT]));
end;

function TDBConstraint.OnUpdate: String;
begin
  Result := OptionBegins(sqluGetIndexedKeyWords([kwON, kwUPDATE]));
end;

function TDBConstraint.OnDelete: String;
begin
  Result := OptionBegins(sqluGetIndexedKeyWords([kwON, kwDELETE]));
end;

function TDBConstraint.OptionBegins(const StartWith: String): String;
var i : integer;
begin
  for i := 0 to FOptions.Count-1 do
  if Pos(StartWith, FOptions[i]) = 1 then
  begin
    Result := FOptions[i];
    Exit;
  end;
  Result := '';
end;

{ TDBForIndxStringTable }

constructor TDBForIndxStringTable.Create(aStyle : TDBIndxStringsStyle;
  aIndx : TBaseSinExprs; aTable : TDBTable);
begin
  FStyle := aStyle;
  FIndx  := aIndx;
  FTable := aTable;
end;

{ TDBExtBlobJob }

constructor TDBExtBlobJob.Create(AValue: TDBExtBlobValue);
begin
  FValue := AValue;
end;

{ TDBExtBlobValueHolder }

constructor TDBExtBlobValueHolder.Create(aObj: TDBExtBlobValue; aId: Integer);
begin
  FValueObj := aObj;
  FId := aId;
end;

{ TDBClearBlobValueJob }

procedure TDBClearBlobValueJob.Execute;
begin
  if assigned(FValue) then
    FValue.Clear;
end;

{ TDBLoadBlobValueJob }

procedure TDBLoadBlobValueJob.Execute;
begin
  if assigned(FValue) then
    FValue.Load;
end;

{ TDBBlobListener }

constructor TDBBlobListener.Create(aNotify: TNotifyEvent;
  aCheckActual: TDBCheckActual);
begin
  FNotify := aNotify;
  FCheckActual := aCheckActual;
end;

{ TDBExtBlobValue }

function TDBExtBlobValue.GetLoading: Boolean;
begin
  Lock;
  try
    Result := FLoading;
  finally
    UnLock;
  end;
end;

procedure TDBExtBlobValue.SetLoading(AValue: Boolean);
begin
  Lock;
  try
    FLoading := AValue;
  finally
    UnLock;
  end;
end;

procedure TDBExtBlobValue.SetLoaded(AValue: Boolean);
begin
  Lock;
  try
    FLoaded := AValue;
  finally
    UnLock;
  end;
end;

procedure TDBExtBlobValue.SetValid(AValue: Boolean);
begin
  Lock;
  try
    FValid := AValue;
  finally
    UnLock;
  end;
end;

function TDBExtBlobValue.GetActual : Boolean;
begin
  Lock;
  try
    Result := FActual;
  finally
    UnLock;
  end;
end;

function TDBExtBlobValue.GetFullPath: String;
begin
  Result := FOwner.GetAbsolutePath(Path);
end;

function TDBExtBlobValue.GetLoaded : Boolean;
begin
  Lock;
  try
    Result := FLoaded;
  finally
    UnLock;
  end;
end;

function TDBExtBlobValue.GetValid : Boolean;
begin
  Lock;
  try
    Result := FValid;
  finally
    UnLock;
  end;
end;

procedure TDBExtBlobValue.SetActual(AValue : Boolean);
begin
  Lock;
  try
    FActual := AValue;
  finally
    UnLock;
  end;
end;

procedure TDBExtBlobValue.SetPath(AValue: String);
begin
  Lock;
  try
    if FPath = AValue then Exit;
    FPath := AValue;
  finally
    UnLock;
  end;

  Clear;
  CheckValid;
end;

constructor TDBExtBlobValue.Create(aOwner : TDBExtBlob; const aPath : String);
begin
  inherited Create;
  FPath   := aPath;
  FActual := false;
  FLoaded := false;
  FValid  := false;
  FLoading:= false;
  FOwner  := aOwner;
  Initialize;
  CheckValid;
end;

procedure TDBExtBlobValue.Load;
begin
  if (not Loaded) and Valid then
  begin
    if not Loading then
    begin
      Loading := true;
      try
        if DoLoad then
          SetLoaded(true);
      finally
        Loading := false;
      end;
    end;
    if Loaded then
      FOwner.NotifyValueLoaded(Self);
  end;
end;

procedure TDBExtBlobValue.Clear;
begin
  if Loaded then
  begin
    SetLoaded(false);
    DoClear;
    FOwner.NotifyValueLoaded(Self);
  end;
end;

procedure TDBExtBlobValue.CheckValid;
begin
  SetValid(FileExists(FullPath));
  if not Valid then Clear;
end;

function TDBExtBlobValue.CheckActual: Boolean;
begin
  Result := FOwner.CheckIsValueActual(Self);
  Actual := Result;
end;

{ TDBExtBlob }

procedure TDBExtBlob.Initialize(const aRootPath: String; aField: TDBField;
  aIsUniqPairs: Boolean);
begin
  FIsUniqPair := aIsUniqPairs;
  FRootPath := aRootPath;
  FField := aField;
  FIdValues := TAvgLvlTree.CreateObjectCompare(@OnExtBlobValuesCompare);
  FIdValues.OwnsObjects := true;
  FPathValues := TStringToPointerTree.Create(false);
  FPathValues.FreeValues := true;
  FValuesLocker := TNetCustomLockedObject.Create;
  FListeners := TThreadSafeFastCollection.Create;
end;

function OnExtBlobValuesKeyCompare(Item1, Item2: Pointer): Integer;
begin
  Result := CompareValue(PInteger(Item1)^, TDBExtBlobValueHolder(Item2).Id);;
end;

function TDBExtBlob.AllwaysActual({%H-}aId: TDBExtBlobValue): TDBExtActualResult;
begin
  if aId.Owner = Self then
    Result := earActual else
    Result := earNoInformation;
end;

function TDBExtBlob.GetById(Id: integer): TDBExtBlobValue;
var R : TAVLTreeNode;
begin
  FValuesLocker.Lock;
  try
    R := FIdValues.FindKey(@Id, @OnExtBlobValuesKeyCompare);
    if not assigned(R) then Exit(nil);
    Result := TDBExtBlobValueHolder(R.Data).ValueObj;
    if Assigned(Result) then
      Result.Actual := true;
  finally
    FValuesLocker.UnLock;
  end;
end;

function TDBExtBlob.GetHolderById(Id: integer): TDBExtBlobValueHolder;
var R : TAVLTreeNode;
begin
  FValuesLocker.Lock;
  try
    R := FIdValues.FindKey(@Id, @OnExtBlobValuesKeyCompare);
    if not assigned(R) then Exit(nil);
    Result := TDBExtBlobValueHolder(R.Data);
  finally
    FValuesLocker.UnLock;
  end;
end;

function TDBExtBlob.GetByPath(const aPath : String): TDBExtBlobValue;
var R : TAVLTreeNode;
begin
  FValuesLocker.Lock;
  try
    R := FPathValues.FindNode(aPath);
    if not assigned(R) then Exit(nil);
    Result := TDBExtBlobValue(R.Data);
    if Assigned(Result) then
      Result.Actual := true;
  finally
    FValuesLocker.UnLock;
  end;
end;

function TDBExtBlob.GetTableName: String;
begin
  Result := FField.FTable.Name;
end;

function TDBExtBlob.OnExtBlobValuesCompare(Tree : TAvgLvlTree; Data1,
  Data2 : Pointer) : integer;
begin
  Result := CompareValue(TDBExtBlobValueHolder(Data1).Id,
                                 TDBExtBlobValueHolder(Data2).Id);
end;

procedure TDBExtBlob.ForAllValuesDo(aWork: TDBExtBlobWorking; limit : integer);

procedure ProceedNode(n : TAVLTreeNode);
begin
  if limit <= 0 then Exit;
  if Assigned(n) then
  begin
    if aWork(TDBExtBlobValueHolder(n.Data).ValueObj) then
      dec(limit);

    ProceedNode(n.Left);
    ProceedNode(n.Right);
  end;
end;

var Node : TAVLTreeNode;
begin
  FValuesLocker.Lock;
  try
    Node:=FIdValues.Root;
    ProceedNode(Node);
  finally
    FValuesLocker.UnLock;
  end;
end;

constructor TDBExtBlob.Create(aTable : TDBTable; const aRootPath, FN : String;
  aIsUniqPairs: Boolean);
var
  aField : TDBField;
begin
  aField := aTable.ByName(FN);
  Initialize(aRootPath,aField,aIsUniqPairs);
end;

constructor TDBExtBlob.Create(aDB : TDBStructure; const aRootPath, TN, FN : String;
  aIsUniqPairs: Boolean);
var
  aTable : TDBTable;
  aField : TDBField;
begin
  aTable := aDB.ByName(TN);
  aField := aTable.ByName(FN);
  Initialize(aRootPath,aField,aIsUniqPairs);
end;

function TDBExtBlob.CheckField(aField : TDBField) : Boolean;
begin
  if FField = aField then Exit(True);
  if Assigned(aField) and Assigned(FField) then
  begin
    if sqluCompareNames(aField.FullFieldName,
                               FField.FullFieldName) then Exit(True);
  end;
  Result := false;
end;

function TDBExtBlob.CheckPath(const aTable, aField : String) : Boolean;
begin
  if Assigned(FField) then
  begin
    if FField.FTable.CompareName(aTable) and FField.CompareName(aField) then
    begin
      Exit(True);
    end;
  end;
  Result := false;
end;

function TDBExtBlob.GetAbsolutePath(const relPath: String): String;
begin
  Result := RootPath + Utf8StringReplace(relPath, cNonSysDelimiter, cSysDelimiter, [rfReplaceAll])
end;

function TDBExtBlob.GetRelativePath(const absPath: String): String;
begin
  Result := Utf8StringReplace(ExtractRelativePath(RootPath, ExtractFilePath(absPath)),
                              cSysDelimiter, '/', [rfReplaceAll]) + ExtractFileName(absPath);
end;

function TDBExtBlob.AddValue(Id: Integer; const aPath: String): TDBExtBlobValue;
var Vid : TDBExtBlobValueHolder;
begin
  FValuesLocker.Lock;
  try
    Result := TDBExtBlobValue(FPathValues.Values[aPath]);
    if Id >= 0 then Vid := GetHolderById(Id);

    if assigned(Result) then begin
       if assigned(Vid) then
       begin
         if Vid.ValueObj <> Result then begin
           if FIsUniqPair then begin
             FPathValues.Remove(Vid.ValueObj.Path);
             Vid.ValueObj.Free;
           end;
           Vid.ValueObj := Result;
         end;
         // else do nothing, cause this id-path pair already exists
         Exit;
       end;
    end else
    begin
      Result := ValueClass.Create(Self, aPath);
      FPathValues.Values[aPath] := Result;

      if assigned(Vid) then
      begin
        if FIsUniqPair then begin
          FPathValues.Remove(Vid.ValueObj.Path);
          Vid.ValueObj.Free;
        end;
        Vid.ValueObj := Result;
      end;
    end;

    if (Id >= 0) and not assigned(Vid) then
      FIdValues.Add(TDBExtBlobValueHolder.Create(Result, Id));
  finally
    FValuesLocker.UnLock;
  end;
end;

function TDBExtBlob.CheckValueValid(O : TDBExtBlobValue) : Boolean;
begin
  O.CheckValid;
  Result := true;
end;

function TDBExtBlob.CheckValueActual(O: TDBExtBlobValue): Boolean;
begin
  if O.Actual and O.Valid then
  if O.CheckActual and (not O.Loaded) then
  begin
    if DBHelper.Threaded then
      DBHelper.ThreadPool.Add(TDBLoadBlobValueJob.Create(O)) else
      O.Load;
    Result := true;
  end else
    Result := false;
end;

function TDBExtBlob.CheckValueUnActual(O: TDBExtBlobValue): Boolean;
begin
  if (not O.Actual) and O.Loaded then
  begin
    if DBHelper.Threaded then
      DBHelper.ThreadPool.Add(TDBClearBlobValueJob.Create(O)) else
      O.Clear;
    Result := true;
  end else
    Result := false;
end;

procedure TDBExtBlob.LoadAllActualValues;
begin
  ForAllValuesDo(@CheckValueActual, 100);
end;

procedure TDBExtBlob.UnLoadAllUnActualValues;
begin
  ForAllValuesDo(@CheckValueUnActual, 100);
end;

procedure TDBExtBlob.NotifyValueUpdated(Id: integer);
var V : TDBExtBlobValue;
begin
  if id < 0 then
  begin
    ForAllValuesDo(@CheckValueValid, High(Integer));
  end else
  begin
    V := GetById(Id);
    if assigned(v) then V.CheckValid;
  end;
end;

procedure TDBExtBlob.NotifyValueLoaded(O: TDBExtBlobValue);
var i : integer;
begin
  FListeners.Lock;
  try
    for i := 0 to FListeners.Count-1 do
      TDBBlobListener(FListeners[i]).Notify(O);
  finally
    FListeners.UnLock;
  end;
end;

function TDBExtBlob.CheckIsValueActual(aID: TDBExtBlobValue): Boolean;
var i : integer;
begin
  FListeners.Lock;
  try
    for i := 0 to FListeners.Count-1 do
    begin
      if TDBBlobListener(FListeners[i]).CheckActual(aID) = earActual then
        Exit(True);
    end;
    Result := false;
  finally
    FListeners.UnLock;
  end;
end;

class function TDBExtBlob.Kind : TDBExtBlobKind;
begin
  Result := ebkNo;
end;

class function TDBExtBlob.ValueClass: TDBExtBlobValueClass;
begin
  Result := TDBExtBlobValue;
end;

procedure TDBExtBlob.AddListener(L: TNotifyEvent; CA: TDBCheckActual);
begin
  if not assigned(CA) then CA := @AllwaysActual;
  FListeners.Add(TDBBlobListener.Create(L, CA));
end;

procedure TDBExtBlob.RemoveListener(L: TNotifyEvent);
var
  i : integer;
begin
  FListeners.Lock;
  try
    for i := FListeners.Count-1 downto 0 do
    if TDBBlobListener(FListeners[i]).Notify = L then
      FListeners.Delete(i);
  finally
    FListeners.UnLock;
  end;
end;

destructor TDBExtBlob.Destroy;
begin
  FIdValues.Free;
  FPathValues.Free;
  FValuesLocker.Free;
  FListeners.Free;
  inherited Destroy;
end;

{ TDBExtBlobImageValue }

procedure TDBExtBlobImageValue.Initialize;
begin
  FBitmap := TPicture.Create;
end;

function TDBExtBlobImageValue.DoLoad : Boolean;
begin
  try
    if FileExists(Path) then
    begin
      FBitmap.LoadFromFile(Path);
      Result := true;
    end else begin
      Result := false;
      SetValid(false);
    end;
  except
    DoClear;
    SetValid(false);
    Result := false;
  end;
end;

procedure TDBExtBlobImageValue.DoClear;
begin
  FBitmap.Clear;
end;

destructor TDBExtBlobImageValue.Destroy;
begin
  FBitmap.Clear;
  FBitmap.Free;
  inherited Destroy;
end;

{ TDBExtImageBlob }

function TDBExtImageBlob.GetBitmap(Id: integer): TPicture;
var
  V : TDBExtBlobImageValue;
begin
  V := TDBExtBlobImageValue(ValueById[Id]);
  if Assigned(V) and V.Loaded then
  begin
    Result := V.Bitmap;
  end else
    Result := nil;
end;

class function TDBExtImageBlob.Kind : TDBExtBlobKind;
begin
  Result := ebkImage;
end;

class function TDBExtImageBlob.ValueClass: TDBExtBlobValueClass;
begin
  Result := TDBExtBlobImageValue;
end;

{ TDBExtBlobs }

function TDBExtBlobs.GetExtBlob(index : integer) : TDBExtBlob;
begin
  Result := TDBExtBlob(Item[index]);
end;

{ TDBStructure }

function TDBStructure.GetDBTable(index : integer) : TDBTable;
begin
  Result := TDBTable(Item[index]);
end;

constructor TDBStructure.Create(aDataSet : TExtSqlite3Dataset);
begin
  inherited Create;
  FDataSet := aDataSet;
  FExtBlobs := TDBExtBlobs.Create;
  FForStrTables := TDBForIndxStringTables.Create;
end;

destructor TDBStructure.Destroy;
begin
  inherited Destroy;
  FExtBlobs.Free;
  FForStrTables.Free;
end;

procedure TDBStructure.Clear;
begin
  inherited Clear;
  FExtBlobs.Clear;
  FForStrTables.Clear;
end;

procedure TDBStructure.AddNewExtBlob(aKind: TDBExtBlobKind;
  const aRootPath : String;
  aTable: TDBTable;
  const FN: String; aIsUniqPair: Boolean);
var V : TDBExtBlob;
begin
  case aKind of
    ebkImage : V := TDBExtImageBlob.Create(aTable, aRootPath, FN, aIsUniqPair);
  else
    V := TDBExtBlob.Create(aTable, aRootPath, FN, aIsUniqPair);
  end;
  FExtBlobs.Add(V);
end;

procedure TDBStructure.AddNewExtBlob(aKind: TDBExtBlobKind; const aRootPath,
  TN, FN: String; aIsUniqPair: Boolean);
var V : TDBExtBlob;
begin
  case aKind of
    ebkImage : V := TDBExtImageBlob.Create(Self, aRootPath, TN, FN, aIsUniqPair);
  else
    V := TDBExtBlob.Create(Self, aRootPath, TN, FN, aIsUniqPair);
  end;
  FExtBlobs.Add(V);
end;

function TDBStructure.GetExtBlob(aField: TDBField): TDBExtBlob;
var i : integer;
begin
  for i := 0 to FExtBlobs.Count-1 do
  begin
    if FExtBlobs[i].CheckField(aField) then
      Exit(FExtBlobs[i]);
  end;
  Result := nil;
end;

function TDBStructure.GetExtBlob(const aTable, aField : String) : TDBExtBlob;
var i : integer;
begin
  for i := 0 to FExtBlobs.Count-1 do
  begin
    if FExtBlobs[i].CheckPath(aTable, aField) then
      Exit(FExtBlobs[i]);
  end;
  Result := nil;
end;

function TDBStructure.GetExtBlob(const aPath : String) : TDBExtBlob;
var TN, FN : String;
begin
  if ExtractTableField(aPath, TN, FN) then
  begin
    Result := GetExtBlob(TN, FN);
  end else
    Result := nil;
end;

procedure TDBStructure.AddListener(L: TNotifyEvent; CA: TDBCheckActual);
var i : integer;
begin
  for i := 0 to FExtBlobs.Count-1 do
    FExtBlobs[i].AddListener(L, CA);
end;

procedure TDBStructure.RemoveListener(L: TNotifyEvent);
var i : integer;
begin
  for i := 0 to FExtBlobs.Count-1 do
    FExtBlobs[i].RemoveListener(L);
end;

procedure TDBStructure.NotifyTableUpdated(aTable: TDBTable; aId: integer);
var i : integer;
    B : TDBExtBlob;
begin
  if assigned(aTable) then
  begin
    for i := 0 to aTable.Count-1 do
    begin
      B := GetExtBlob(aTable[i]);
      if assigned(B) then
        B.NotifyValueUpdated(aId);
    end;
  end;
end;

function TDBStructure.IsExtBlobTable(aTable: TDBTable; aKind: TDBExtBlobKind
  ): Boolean;
var i : integer;
begin
  for i := 0 to FExtBlobs.Count-1 do
  begin
    if (FExtBlobs[i].TableName = aTable.Name) and
       (FExtBlobs[i].Kind = aKind) then
      Exit(true);
  end;
  Result := false;
end;

procedure TDBStructure.AddForeignIndxStringTable(
  aIndxStyle : TDBIndxStringsStyle; aIndx : TBaseSinExprs;
  aTable : TDBTable);
var O : TDBForIndxStringTable;
begin
  O := TDBForIndxStringTable.Create(aIndxStyle, aIndx, aTable);
  FForStrTables.Add(O);
end;

function TDBStructure.IsForeignIndxStringTable(aTable: TDBTable
  ): TDBForIndxStringTable;
var i : integer;
begin
  for i := 0 to FForStrTables.Count-1 do
  begin
    if FForStrTables[i].Table = aTable then
      Exit(FForStrTables[i]);
  end;
  Result := nil;
end;

function TDBStructure.CheckIsReferenced(aField : TDBField) : Boolean;
var i, j : integer;
begin
  for i := 0 to Count-1 do
  begin
    for j := 0 to Self[i].Count-1 do
    if (Self[i].DBField[j].ForeignKeyTable = aField.Table) and
       aField.CompareName(Self[i].DBField[j].ForeignKeyTo) then
    begin
      Exit(true);
    end;
  end;
  Result := false;
end;

procedure TDBStructure.DoTimerTick;
var i : integer;
begin
  for i := 0 to FExtBlobs.Count-1 do
  begin
    FExtBlobs[i].LoadAllActualValues;
    FExtBlobs[i].UnLoadAllUnActualValues;
  end;
  for i := 0 to FForStrTables.Count-1 do
    FForStrTables[i].Indx.Synchronize;
end;

function TDBStructure.ByName(const S : String) : TDBTable;
var i : integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
  begin
    if Self[i].CompareName(S) then
    begin
      Result := Self[i];
      Exit;
    end;
  end;
end;

function TDBStructure.BySchemaAndName(const aSchema, aName: String): TDBTable;
var i : integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
  begin
    if Self[i].CompareName(aName) and
       Self[i].CompareSchema(aSchema) then
    begin
      Result := Self[i];
      Exit;
    end;
  end;
end;

{ TDBTable }

function TDBTable.GetDBField(index : integer) : TDBField;
begin
  Result := TDBField(Item[index]);
end;

function TDBTable.GetHasForeignKeys: Boolean;
var i : Integer;
begin
  for i := 0 to Count-1 do
  begin
    if DBField[i].IsForeignKey then
    begin
      Exit(True);
    end;
  end;
  Result := False;
end;

function TDBTable.GetHasPrimaryKey : Boolean;
var i : Integer;
begin
  for i := 0 to Count-1 do
  begin
    if DBField[i].IsPrimaryKey then
    begin
      Exit(True);
    end;
  end;
  Result := False;
end;

function TDBTable.GetOption(Index : TSqliteTableOption) : Boolean;
begin
  case Index of
    toCheckExists : Result := FCheckExists;
    toIsTemp      : Result := FIsTemp;
    toWORowID     : Result := FWORowID;
  end;
end;

function TDBTable.GetOptionByName(const Index : String) : Boolean;
var c : TSqliteTableOption;
begin
  for c := Low(TSqliteTableOption) to High(TSqliteTableOption) do
  if SameStr(sqluTableOptionKindToStr(c), Index) then
  begin
    Result := Options[c];
    Exit;
  end;
  Result := false;
end;

function TDBTable.GetPrimaryKeyField : TDBField;
var i : Integer;
begin
  for i := 0 to Count-1 do
  begin
    if DBField[i].IsPrimaryKey then
    begin
      Exit(DBField[i]);
    end;
  end;
  Result := nil;
end;

function TDBTable.GetQuotedName : String;
begin
  if FIsNameNeedQuoting then
  begin
    Result := FQuotedName;
  end else
    Result := FName;
end;

function TDBTable.GetQuotedSchema: String;
begin
  if FIsNameNeedQuoting then
  begin
    Result := FQuotedName;
  end else
    Result := FSchema;
end;

procedure TDBTable.SetOption(Index : TSqliteTableOption; AValue : Boolean);
begin
  case Index of
    toCheckExists : FCheckExists := AValue;
    toIsTemp      : FIsTemp := AValue;
    toWORowID     : FWORowID := AValue;
  end;
end;

procedure TDBTable.SetOptionByName(const Index : String; AValue : Boolean);
var c : TSqliteTableOption;
begin
  for c := Low(TSqliteTableOption) to High(TSqliteTableOption) do
  if SameStr(sqluTableOptionKindToStr(c), Index) then
  begin
    Options[c] := AValue;
    Exit;
  end;
end;

procedure TDBTable.SetStructure(AValue : TDBStructure);
begin
  if FStruct = AValue then Exit;
  FStruct := AValue;
end;

constructor TDBTable.Create(aOwner : TDBStructure; const aName : String);
begin
  inherited Create;
  FStruct := aOwner;
  FSchema := '';
  SetName(aName);
  FIsTemp := false;
  FWORowID := false;
  FCheckExists := false;
  FConstraints := TDBConstraints.Create;
end;

destructor TDBTable.Destroy;
begin
  FConstraints.Free;
  inherited Destroy;
end;

procedure TDBTable.SetName(const S: String);
begin
  FName := S;
  FIsNameNeedQuoting := sqluCheckIsNeedQuoted(FName);
  if FIsNameNeedQuoting then
    FQuotedName := sqluQuotedId(FName) else
    FQuotedName := '';
end;

procedure TDBTable.SetSchema(const S: String);
begin
  FSchema := S;
end;

function TDBTable.CompareName(const S : String) : Boolean;
begin
  Result := sqluCompareNames(FName, S);
end;

function TDBTable.CompareSchema(const S: String): Boolean;
begin
  Result := sqluCompareNames(FSchema, S);
end;

function TDBTable.Compare(aTable : TDBTable) : Boolean;
var curField : TDBField;
    i : integer;
begin
  if not Assigned(aTable) then Exit(false);
  if CompareName(aTable.Name) then
  begin
    if FConstraints.Compare(aTable.Constraints) then
    begin
      if Count <> aTable.Count then Exit(False);
      for i := 0 to Count-1 do
      begin
        curField := aTable.ByName(Self[i].FieldName);
        if assigned(curField) then
        begin
          if not Self[i].Compare(curField) then
            Exit(false);
        end else
          Exit(false);
      end;
      Result := True;
    end else Result := false;
  end else
    Result := false;
end;

function TDBTable.ByName(const S : String) : TDBField;
var i : integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
  begin
    if Self[i].CompareName(S) then
    begin
      Result := Self[i];
      Exit;
    end;
  end;
end;

function TDBTable.AddField(const FN, FT, FD : String; IsPK : Boolean
  ) : TDBField;
begin
  Result := TDBField.Create(Self, FN, FT, FD, IsPK);
  Add(Result);
end;

function TDBTable.AddField(const FN : String; FT : TSqliteDataTypeAffinity;
  const FD : String; IsPK : Boolean) : TDBField;
begin
  Result := AddField(FN, sqluAffinityToStr(FT), FD, IsPK);
end;

function TDBTable.AddField(const FN : String; FT : TSqliteDataTypeAffinity;
  IsPK : Boolean) : TDBField;
begin
  Result := AddField(FN, sqluAffinityToStr(FT), '', IsPK);
end;

function TDBTable.BuildCreateExpression(OneLined: Boolean;
                                        aKwFormat : TSqliteKwFormatOption): String;

var str : String;

procedure ConcatStr(const aStr : String);
begin
  if length(aStr) = 0 then exit;
  if length(str) > 0 then Str := Str + ' ';
  Str := Str + aStr;
end;

procedure ConcatStrExpr(const aStr : String);
var
  expr : TSqliteExpr;
begin
  if length(aStr) = 0 then exit;
  if length(str) > 0 then Str := Str + ' ';
  expr := TSqliteExpr.Create(aStr);
  try
    Str := Str + expr.FormatedStr(aKwFormat);
  finally
    expr.Free;
  end;
end;

procedure ConcatStrInBackets(const aStr : String);
begin
  if length(aStr) = 0 then exit;
  Str := Str + ' (';
  Str := Str + aStr + cCloseBracket;
end;

procedure ConcatStrExprInBackets(const aStr : String);
var
  expr : TSqliteExpr;
begin
  if length(aStr) = 0 then exit;
  Str := Str + ' (';
  expr := TSqliteExpr.Create(aStr);
  try
    Str := Str + expr.FormatedStr(aKwFormat) + cCloseBracket;
  finally
    expr.Free;
  end;
end;

procedure ConcatExpr(kw : Cardinal);
begin
  ConcatStr(sqluGetIndexedKeyWord(kw, aKwFormat));
end;

procedure SemiColumn;
begin
  Str := Str + cSemiColumn;
end;

procedure Period;
begin
  Str := Str + cPeriod;
end;

procedure OpenBracket;
begin
  Str := Str + cOpenBracket;
end;

procedure CloseBracket;
begin
  Str := Str + cCloseBracket;
end;

procedure WriteTabs;
begin
  Str := Str + cTabs;
end;

procedure NewLine;
begin
  Str := Str + cNewLine;
end;

procedure ConcatExprs(const kw : array of Cardinal);
begin
  ConcatStr(sqluGetIndexedKeyWords(kw, aKwFormat));
end;

procedure ConcatField(aField : TDBField);
begin
  ConcatStr(aField.GetFullDefinition(aKwFormat));
end;

procedure ConcatTableConstr(constr : TDBConstraint);
var T : TSqliteToken;
begin
  case constr.Kind of
    dbckPrimaryKey : begin
      ConcatExprs([kwPRIMARY, kwKEY]);
      if constr.ValuesCount >= 1 then
      begin
        T := constr.ValueExprs[0].IsIdentifier;
      end else T := nil;
      if Assigned(T) then
        ConcatStrInBackets(T.QuotedToken) else
        ConcatStrExprInBackets(constr.Value);
      ConcatStrExpr(constr.ConflictClause);
    end;
    dbckUnique : begin
      ConcatExpr(kwUNIQUE);
      if constr.ValuesCount >= 1 then
      begin
        T := constr.ValueExprs[0].IsIdentifier;
      end else T := nil;
      if Assigned(T) then
        ConcatStrInBackets(T.QuotedToken) else
        ConcatStrExprInBackets(constr.Value);
      ConcatStrExpr(constr.ConflictClause);
    end;
    dbckCheck : begin
      ConcatExpr(kwCHECK);
      ConcatStrExprInBackets(constr.Value);
    end;
    dbckForeignKey : begin
      ConcatExpr(kwREFERENCES);
      if constr.ValuesCount >= 2 then
      begin
        T := constr.ValueExprs[0].IsIdentifier;
        if Assigned(T) then
          ConcatStrInBackets(T.QuotedToken) else
          ConcatStrExprInBackets(constr.Value);

        ConcatExpr(kwREFERENCES);
        T := constr.ValueExprs[1].IsIdentifier;
        if Assigned(T) then
          ConcatStr(T.QuotedToken) else
          ConcatStrExpr(constr.Values[1]);

        if (constr.ValuesCount >= 3) and
           (Length(constr.Values[2]) > 0) then
        begin
          T := constr.ValueExprs[2].IsIdentifier;
          if Assigned(T) then
            ConcatStrInBackets(T.QuotedToken) else
            ConcatStrInBackets(constr.Values[2]);
        end;
      end  else
        ConcatStr(constr.Value);
      ConcatStrExpr(constr.OnDelete);
      ConcatStrExpr(constr.OnUpdate);
      ConcatStrExpr(constr.OptionBegins(sqluGetIndexedKeyWord(kwMATCH)));
      ConcatStrExpr(constr.OptionBegins(sqluGetIndexedKeyWord(kwDEFERRABLE)));
      ConcatStrExpr(constr.OptionBegins(sqluGetIndexedKeyWords([kwNOT, kwDEFERRABLE])));
    end;
  end;
end;

var i : integer;
begin
  Str := '';
  ConcatExpr(kwCREATE);
  if IsTemp then
    ConcatExpr(kwTEMP);
  ConcatExpr(kwTABLE);
  if CheckExists then
    ConcatExprs([kwIF, kwNOT, kwEXISTS]);
  ConcatStr(QuotedName);
  ConcatStr(cOpenBracket); if not OneLined then NewLine;
  for i := 0 to Count-1 do
  begin
    if not OneLined then WriteTabs;
    ConcatField(Self[i]);
    if (i < (Count - 1)) or (FConstraints.Count > 0) then Period;
    if not OneLined then NewLine;
  end;
  for i := 0 to FConstraints.Count-1 do
  begin
    if not OneLined then WriteTabs;
    ConcatTableConstr(Constraints[i]);
    if i < (FConstraints.Count - 1) then Period;
    if not OneLined then NewLine;
  end;
  CloseBracket;
  if WithoutRowID then
    ConcatExprs([kwWITHOUT, kwROWID]);
  SemiColumn;
  Result := Str;
end;

{ TDBField }

function TDBField.GetFullFieldName : String;
begin
  Result := FTable.QuotedName + '.' + QuotedName;
end;

function TDBField.GetQuotedName : String;
begin
  if FIsNameNeedQuoting then
  begin
    Result := FQuotedName;
  end else
    Result := FFieldName;
end;

function TDBField.GetFieldStrType : String;
begin
  case FFieldType of
    ftString  : Result := sqluAffinityToStr(dtaText);
    ftInteger : Result := sqluAffinityToStr(dtaInteger);
    ftCurrency: Result := sqluAffinityToStr(dtaNumeric);
    ftFloat   : Result := sqluAffinityToStr(dtaReal);
    ftGraphic : Result := sqluAffinityToStr(dtaBlob);
  else
    Result := sqluAffinityToStr(dtaUnknown);
  end;
end;

function TDBField.GetForeignKeyTableName : String;
begin
  if Assigned(FForeignKeyTable) then
  begin
    Result := FForeignKeyTable.Name;
  end else
    Result := '';
end;

constructor TDBField.Create(aOwner : TDBTable; const FN, FT, FD : String;
  IsPK : Boolean);
var C : TDBConstraint;
begin
  FConstraints := TDBConstraints.Create;
  FTable := aOwner;
  FForeignKeyTable := nil;
  FIsForeignKey := false;
  SetFieldName(FN);
  SetFieldType(FT);
  FFieldDefault := FD;
  FIsPrimaryKey := IsPK;
  if IsPK then
    FConstraints.Add(dbckPrimaryKey, '');
  if Length(FFieldDefault) > 0 then
  begin
    C := TDBConstraint.Create(dbckDefault, '');
    C.AddValue(FD, dbvkDefaultValue);
    FConstraints.Add(C);
  end;

  FForeignKeyTo := '';
end;

constructor TDBField.Create(aField : TDBField;
                                   const aValue : String;
                                   IsPK : Boolean);
begin
  FConstraints := TDBConstraints.Create;
  FTable := aField.FTable;
  FIsForeignKey := aField.IsForeignKey;
  FForeignKeyTable := aField.ForeignKeyTable;
  FForeignKeyTo := aField.ForeignKeyTo;
  FFieldName := aField.FieldName;
  FFieldType := aField.FieldType;
  FIsNameNeedQuoting := aField.FIsNameNeedQuoting;
  FQuotedName := aField.FQuotedName;
  FConstraints.Assign(aField.Constraints);
  FFieldDefault := aValue;
  FIsPrimaryKey := IsPK;
end;

procedure TDBField.UpdateForeignKeyInfo(FT : TDBTable; const FKTO : String);
begin
  FIsForeignKey := Assigned(FT);
  FForeignKeyTable := FT;
  FForeignKeyTo := FKTO;
end;

destructor TDBField.Destroy;
begin
  FConstraints.Free;
  inherited Destroy;
end;

procedure TDBField.SetFieldName(const aName: String);
begin
  FFieldName := aName;
  FIsNameNeedQuoting := sqluCheckIsNeedQuoted(aName);
  if FIsNameNeedQuoting then
    FQuotedName  := sqluQuotedId(aName) else
    FQuotedName := '';
end;

procedure TDBField.SetFieldType(const aType : String);
var AFT : TSqliteDataTypeAffinity;
begin
  AFT := sqluGetDataTypeAffinity(aType);
  if (AFT = dtaInteger) then
      FFieldType := ftInteger
  else if (AFT = dtaText) then
      FFieldType := ftString
  else if AFT = dtaReal then
      FFieldType := ftFloat
  else if AFT = dtaNumeric then
      FFieldType := ftCurrency
  else if AFT = dtaBlob then
      FFieldType := ftGraphic;
end;

function TDBField.CompareName(const S : String) : Boolean;
begin
  Result := sqluCompareNames(FFieldName, S);
end;

function TDBField.Compare(aField : TDBField) : Boolean;
begin
  if not Assigned(aField) then Exit(false);
  if CompareName(aField.FieldName) then
  begin
    if (FIsPrimaryKey = aField.IsPrimaryKey) and
       (FIsForeignKey = aField.IsForeignKey) and
       (FForeignKeyTo = aField.FForeignKeyTo) and
       (ForeignKeyTableName = aField.ForeignKeyTableName) and
       (FFieldType = aField.FieldType) and
       (FFieldDefault = aField.FieldDefault) and
       (FConstraints.Compare(aField.Constraints)) then
    begin
      Result := true;
    end else
      Result := false;
  end else
    Result := false;
end;

function TDBField.GetFullDefinition(aKwFormat : TSqliteKwFormatOption) : String;
var str : String;

procedure ConcatStr(const aStr : String);
begin
  if length(aStr) = 0 then exit;
  if length(str) > 0 then Str := Str + ' ';
  Str := Str + aStr;
end;

procedure ConcatStrExpr(const aStr : String);
var
  expr : TSqliteExpr;
begin
  if length(aStr) = 0 then exit;
  if length(str) > 0 then Str := Str + ' ';
  expr := TSqliteExpr.Create(aStr);
  try
    Str := Str + expr.FormatedStr(aKwFormat);
  finally
    expr.Free;
  end;
end;

procedure ConcatStrInBackets(const aStr : String);
begin
  if length(aStr) = 0 then exit;
  Str := Str + ' (';
  Str := Str + aStr + cCloseBracket;
end;

procedure ConcatStrExprInBackets(const aStr : String);
var
  expr : TSqliteExpr;
begin
  if length(aStr) = 0 then exit;
  Str := Str + ' (';
  expr := TSqliteExpr.Create(aStr);
  try
    Str := Str + expr.FormatedStr(aKwFormat) + cCloseBracket;
  finally
    expr.Free;
  end;
end;

procedure ConcatExpr(kw : Cardinal);
begin
  ConcatStr(sqluGetIndexedKeyWord(kw, aKwFormat));
end;

procedure SemiColumn;
begin
  Str := Str + cSemiColumn;
end;

procedure Period;
begin
  Str := Str + cPeriod;
end;

procedure OpenBracket;
begin
  Str := Str + cOpenBracket;
end;

procedure CloseBracket;
begin
  Str := Str + cCloseBracket;
end;

procedure WriteTabs;
begin
  Str := Str + cTabs;
end;

procedure NewLine;
begin
  Str := Str + cNewLine;
end;

procedure ConcatExprs(const kw : array of Cardinal);
begin
  ConcatStr(sqluGetIndexedKeyWords(kw, aKwFormat));
end;

var i : integer;
    constr : TDBConstraint;
    t : TSqliteToken;
begin
  Str := '';
  ConcatStr(QuotedName);
  ConcatStr(sqluFormatKeyWord(FieldStrType, aKwFormat));
  for i := 0 to Constraints.Count-1 do
  begin
    constr := Constraints[i];
    case constr.Kind of
      dbckPrimaryKey : begin
        ConcatExprs([kwPRIMARY, kwKEY]);
        if constr.CheckOption(kwASC) then
        begin
          ConcatExpr(kwASC);
        end else
        if constr.CheckOption(kwDESC) then
        begin
          ConcatExpr(kwDESC);
        end;
        ConcatStrExpr(constr.ConflictClause);
        if constr.CheckOption(kwAUTOINCREMENT) then
          ConcatExpr(kwAUTOINCREMENT);
      end;
      dbckNotNull : begin
        ConcatExprs([kwNOT, kwNULL]);
        ConcatStrExpr(constr.ConflictClause);
      end;
      dbckUnique : begin
        ConcatExpr(kwUNIQUE);
        ConcatStrExpr(constr.ConflictClause);
      end;
      dbckCheck : begin
        ConcatExpr(kwCHECK);
        ConcatStrExprInBackets(constr.Value);
      end;
      dbckDefault : begin
        ConcatExpr(kwDEFAULT);
        if constr.ValuesCount >= 1 then
        begin
          T := constr.ValueExprs[0].IsLiteral;
        end else T := nil;
        if Assigned(T) then
          ConcatStr(T.QuotedToken) else
        begin
          ConcatStrExprInBackets(constr.Value);
        end;
      end;
      dbckCollate : begin
        ConcatExpr(kwCOLLATE);
        ConcatStr(constr.Value);
      end;
      dbckForeignKey : begin
        ConcatExpr(kwREFERENCES);
        if constr.ValuesCount >= 1 then
        begin
          T := constr.ValueExprs[0].IsIdentifier;
          if Assigned(T) then
            ConcatStr(T.QuotedToken);
          if (constr.ValuesCount >= 2) and
             (Length(constr.Values[1]) > 0) then
          begin
            T := constr.ValueExprs[1].IsIdentifier;
            if Assigned(T) then
              ConcatStrInBackets(T.QuotedToken) else
              ConcatStrInBackets(constr.Values[1]);
          end;
        end  else
          ConcatStr(constr.Value);
        ConcatStrExpr(constr.OnDelete);
        ConcatStrExpr(constr.OnUpdate);
        ConcatStrExpr(constr.OptionBegins(sqluGetIndexedKeyWord(kwMATCH)));
        ConcatStrExpr(constr.OptionBegins(sqluGetIndexedKeyWord(kwDEFERRABLE)));
        ConcatStrExpr(constr.OptionBegins(sqluGetIndexedKeyWords([kwNOT, kwDEFERRABLE])));
      end;
      dbckGenerated : begin
        ConcatExpr(kwAS);
        ConcatStrExprInBackets(constr.Value);
        if constr.CheckOption(kwSTORED) then
          ConcatExpr(kwSTORED) else
        if constr.CheckOption(kwVIRTUAL) then
          ConcatExpr(kwVIRTUAL);
      end;
    end;
  end;
  Result := Str;
end;

end.

