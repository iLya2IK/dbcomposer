{
 dbcTypes:
   Classes and types for constructing hierarchical (nested) requests

   Part of dbComposer project

   Copyright (c) 2021-2022 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit dbctypes;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, OGLFastList, DB;

type

  { TRequestCompElement }

  TRequestCompElement = class
  private
    FLeftSide : String;
    FRightSide : String;
    FOp : String;
  public
    constructor Create(const LS, OP, RS : String);  overload;
    constructor Create(O : TRequestCompElement); overload;
    procedure Assign(E : TRequestCompElement);
    property Left  : String read FLeftSide;
    property Right : String read FRightSide;
    property Op    : String read FOp;
  end;

  { TRequestCompList }

  TRequestCompList = class(TFastCollection)
  private
    function GetElement(index : integer) : TRequestCompElement;
  public
    function GetExpr : String;
    procedure Assign(O : TRequestCompList);
    property Element[index : integer] : TRequestCompElement read GetElement; default;
  end;

  { TSelectNestedList }

  TSelectNestedList = class(TFastCollection)
  private
    FTable     : String;
    FOrigTable : String;
    FField     : String;
    FAsField   : String;
    FAsFieldToken,
      FAsTableToken : String;
    FType      : TFieldType;
    procedure SetAsField(const AFN : String);
    function GetElement(index : integer) : TSelectNestedList;
  public
    constructor Create(const aOrigTable, aTable, aField : String;
                aFieldType : TFieldType); overload;
    constructor Create(const aOrigTable, aTable, aField : String;
                aFieldType : TFieldType; const aAsFieldName : String); overload;
    constructor Create(const aTable, aField : String;
                             aFieldType : TFieldType); overload;
    constructor Create(O : TSelectNestedList); overload;
    procedure Assign(O : TSelectNestedList);
    function GetExpr(WithPaths : Boolean) : String;
    function GetFields : String;
    function AddNewNestedField(const aTable, aField : String;
      aFieldType : TFieldType) : TSelectNestedList;  overload;
    function AddNewNestedField(const aOrigTable, aTable, aField : String;
      aFieldType : TFieldType) : TSelectNestedList;  overload;
    function AddNewNestedField(const aOrigTable, aTable, aField : String;
      aFieldType : TFieldType;
      const aAsFieldName : String) : TSelectNestedList;  overload;
    function CheckFieldName(const AFieldName : String) : Boolean;
    function GenOrigFieldName(const ATableName, AFieldName : String) : String;
    property Element[index : integer] : TSelectNestedList read GetElement; default;
    property OrigTable : String read FOrigTable;
    property Table : String read FTable;
    property Field : String read FField;
    property AsFieldName : String read FAsFieldToken;
    property AsFieldTable : String read FAsTableToken;
    property FieldType : TFieldType read FType;
  end;

  { TNestedTable }

  TNestedTable = class
  public
    Table  : String;
    AsName : String;
    function ToSqlExpr : String;
    function IdName : String;
  end;

  { TNestedTablesList }

  TNestedTablesList = class(specialize TFastBaseCollection<TNestedTable>)
  public
    function IndexOf(const Tn, AsExpr : String) : Integer;
    function Add(const Tn, AsExpr : String) : TNestedTable; overload;
  end;

  { TSQLRequest }

  TSQLRequest = class
  private
    FSelectExpr  : TSelectNestedList;
    FNestedTables: TNestedTablesList;
    FWhereExpr   : TRequestCompList;
    FSelectExprPaths : Boolean;
    FSelectTable, FQuotedTable : String;
    FSelectId,    FQuotedId    : String;
    function GetListOfFielda: String;
    procedure Initialize;
    function GetNestedTables : String;
    function GetSelectExpr : String;
    function GetWhereExpr : String;
  public
    constructor Create; overload;
    constructor Create(const ST, SF : String); overload;
    destructor Destroy; override;

    function AddNewNestedField(const aTable, aField : String;
      aFieldType : TFieldType) : TSelectNestedList;
    function AddNewCompValue(const LS, OP, RS : String) : TRequestCompElement;
    function AddNewNestedTable(const TN : String) : TNestedTable;

    procedure Assign(R: TSQLRequest);
    function Compare(R: TSQLRequest) : Boolean;
    function IsEmpty : Boolean;
    procedure Clear;

    property WhereExpr : String read GetWhereExpr;
    property NestedTables : String read GetNestedTables;
    property ListOfFields : String read GetListOfFielda;
    property SelectTable  : String read FSelectTable;
    property SelectId  : String read FSelectId;
    property SelectQuotedTable  : String read FQuotedTable;
    property SelectQuotedId  : String read FQuotedId;
    property SelectExpr : String read GetSelectExpr;
    property SelectExprPaths : Boolean read FSelectExprPaths write FSelectExprPaths;
    property SelectExprLst : TSelectNestedList read FSelectExpr;
  end;

implementation

uses ExtSqliteUtils, dbComposerUtils;

{ TNestedTablesList }

function TNestedTablesList.IndexOf(const Tn, AsExpr : String) : Integer;
var i : integer;
begin
  for i := 0 to Count-1 do
  if SameText(Tn, Self[i].Table) and SameText(AsExpr, Self[i].AsName) then
  begin
    Exit(i);
  end;
  Result := -1;
end;

function TNestedTablesList.Add(const Tn, AsExpr : String) : TNestedTable;
begin
  Result := TNestedTable.Create;
  Result.Table := Tn;
  Result.AsName := AsExpr;
  Add(Result);
end;

{ TNestedTable }

function TNestedTable.ToSqlExpr : String;
begin
  if Length(AsName) > 0 then
    Result := sqluQuotedIdIfNeeded(Table) + ' as ' + sqluQuotedIdIfNeeded(AsName) else
    Result := sqluQuotedIdIfNeeded(Table);
end;

function TNestedTable.IdName : String;
begin
  if Length(AsName) > 0 then Result := AsName else Result := Table;
  Result := sqluQuotedIdIfNeeded(Result);
end;

{ TRequestCompElement }

constructor TRequestCompElement.Create(const LS, OP, RS : String);
begin
  FLeftSide := LS;
  FOp := Op;
  FRightSide := RS;
end;

constructor TRequestCompElement.Create(O : TRequestCompElement);
begin
  Assign(O);
end;

procedure TRequestCompElement.Assign(E : TRequestCompElement);
begin
  if not Assigned(E) then Exit;
  FLeftSide := E.FLeftSide;
  FOp := E.FOp;
  FRightSide := E.FRightSide;
end;

{ TRequestCompList }

function TRequestCompList.GetElement(index : integer) : TRequestCompElement;
begin
  Result := TRequestCompElement(Item[index]);
end;

function TRequestCompList.GetExpr : String;
var i : integer;
begin
  Result := '';
  for i := 0 to Count-1 do
  begin
    if Length(Result) > 0 then Result := Result + ' and ';
    Result := Result + Self[i].Left + Self[i].Op + Self[i].Right;
  end;
end;

procedure TRequestCompList.Assign(O : TRequestCompList);
var i : integer;
begin
  if not Assigned(O) then Exit;

  for i := 0 to O.Count-1 do
  begin
    Add(TRequestCompElement.Create(O[i]));
  end;
end;

{ TSelectNestedList }

function TSelectNestedList.GetElement(index : integer) : TSelectNestedList;
begin
  Result := TSelectNestedList(Item[index]);
end;

procedure TSelectNestedList.SetAsField(const AFN : String);
begin
  FAsField := AFN;

  if not ExtractTableField(AFN, FAsTableToken, FAsFieldToken) then
  begin
    FAsTableToken := FOrigTable;
    FAsFieldToken := FAsField;
  end;
end;

constructor TSelectNestedList.Create(const aOrigTable, aTable, aField : String;
  aFieldType : TFieldType);
begin
  inherited Create;
  FTable := aTable;
  FOrigTable := aOrigTable;
  FField := aField;
  FType := aFieldType;
  SetAsField(FTable + '.' + FField);
end;

constructor TSelectNestedList.Create(const aOrigTable, aTable, aField : String;
  aFieldType : TFieldType; const aAsFieldName : String);
begin
  inherited Create;
  FTable := aTable;
  FOrigTable := aOrigTable;
  FField := aField;
  FType := aFieldType;
  SetAsField(aAsFieldName);
end;

constructor TSelectNestedList.Create(const aTable, aField : String;
  aFieldType : TFieldType);
begin
  inherited Create;
  FTable := aTable;
  FOrigTable := aTable;
  FField := aField;
  FType := aFieldType;
  SetAsField(FTable + '.' + FField);
end;

constructor TSelectNestedList.Create(O : TSelectNestedList);
begin
  inherited Create;
  Assign(O);
end;

procedure TSelectNestedList.Assign(O : TSelectNestedList);
var i : integer;
begin
  if not Assigned(O) then Exit;
  FField := O.FField;
  FTable := O.FTable;
  FType  := O.FType;
  FAsField := O.FAsField;
  FOrigTable := O.FOrigTable;
  FAsFieldToken := O.FAsFieldToken;
  FAsTableToken := O.FAsTableToken;
  for i := 0 to O.Count-1 do
  begin
    Add(TSelectNestedList.Create(O[i]));
  end;
end;

function TSelectNestedList.GetExpr(WithPaths : Boolean) : String;
var i : integer;
begin
  Result := '';
  if Count > 0 then begin
    for i := 0 to Count-1 do
    begin
      if Length(Result) > 0 then Result := Result + ',';
      Result := Result + Self[i].GetExpr(WithPaths);
    end;
  end else
   if (Length(FTable) > 0) and (Length(FField) > 0) then
   begin
    Result := FTable + '.' + FField;
    if WithPaths then
    begin
      Result := Result + ' as [' + FAsField + ']';
    end;
   end else
      Result := '';
end;

function TSelectNestedList.GetFields: String;
var i : integer;
begin
  Result := '';
  if Count > 0 then begin
    for i := 0 to Count-1 do
    begin
      if Length(Result) > 0 then Result := Result + ',';
      Result := Result + Self[i].GetFields;
    end;
  end else
   if (Length(FTable) > 0) and (Length(FField) > 0) then
   begin
    Result := FTable + '.' + FField;
   end else
    Result := '';
end;

function TSelectNestedList.AddNewNestedField(const aTable, aField : String;
  aFieldType : TFieldType) : TSelectNestedList;
begin
  Result := TSelectNestedList.Create(aTable, aField, aFieldType);
  Add(Result);
end;

function TSelectNestedList.AddNewNestedField(const aOrigTable, aTable,
  aField : String; aFieldType : TFieldType) : TSelectNestedList;
begin
  Result := TSelectNestedList.Create(aOrigTable, aTable, aField, aFieldType);
  Add(Result);
end;

function TSelectNestedList.AddNewNestedField(const aOrigTable, aTable,
  aField : String; aFieldType : TFieldType; const aAsFieldName : String
  ) : TSelectNestedList;
begin
  Result := TSelectNestedList.Create(aOrigTable, aTable, aField, aFieldType,
                                                 aAsFieldName);
  Add(Result);
end;

function TSelectNestedList.CheckFieldName(const AFieldName : String) : Boolean;
var i : integer;
begin
  Result := true;
  if Count > 0 then begin
    for i := 0 to Count-1 do
      Result := Result and Self[i].CheckFieldName(AFieldName);
  end else
    Result := not SameText(FAsField, AFieldName);
end;

function TSelectNestedList.GenOrigFieldName(const ATableName,
  AFieldName : String) : String;
var i : integer;
begin
  Result := ATableName+'.'+AFieldName;
  i := 0;
  while not CheckFieldName(Result) do
  begin
    Inc(I);
    Result := ATableName+'.'+AFieldName+'_'+InttoStr(I);
  end;
end;

{ TSQLRequest }

procedure TSQLRequest.Initialize;
begin
  FQuotedTable := sqluQuotedIdIfNeeded(FSelectTable);
  FQuotedId := sqluQuotedIdIfNeeded(FSelectId);
  FNestedTables := TNestedTablesList.Create;
  FWhereExpr := TRequestCompList.Create;
  FSelectExprPaths := false;
  FSelectExpr := TSelectNestedList.Create('', '', ftReference);
end;

function TSQLRequest.GetListOfFielda: String;
begin
  Result := SelectQuotedTable + '.' + SelectQuotedId + ',' + FSelectExpr.GetFields;
end;

function TSQLRequest.GetNestedTables : String;
var i : integer;
begin
  Result := '';
  for i := 0 to FNestedTables.Count-1 do
  begin
    if Length(Result) > 0 then Result := Result + ',';
    Result := Result + FNestedTables[i].ToSqlExpr;
  end;
end;

function TSQLRequest.GetSelectExpr : String;
begin
  Result := FSelectExpr.GetExpr(FSelectExprPaths);
end;

function TSQLRequest.GetWhereExpr : String;
begin
  Result := FWhereExpr.GetExpr;
end;

constructor TSQLRequest.Create;
begin
  FSelectTable := '';
  FSelectId := '';
  Initialize;
end;

constructor TSQLRequest.Create(const ST, SF : String);
begin
  FSelectTable := ST;
  FSelectId := SF;
  Initialize;
end;

destructor TSQLRequest.Destroy;
begin
  FNestedTables.Free;
  FWhereExpr.Free;
  FSelectExpr.Free;
  inherited Destroy;
end;

function TSQLRequest.AddNewNestedField(const aTable, aField : String;
  aFieldType : TFieldType) : TSelectNestedList;
begin
  Result := FSelectExpr.AddNewNestedField(aTable, aField, aFieldType);
end;

function TSQLRequest.AddNewCompValue(const LS, OP, RS : String
  ) : TRequestCompElement;
begin
  Result := TRequestCompElement.Create(LS, OP, RS);
  FWhereExpr.Add(Result);
end;

function TSQLRequest.AddNewNestedTable(const TN : String) : TNestedTable;
var i : Integer;
    S : String;
begin
  S := '';
  i := 1;
  while FNestedTables.IndexOf(TN, S) >= 0 do
  begin
    S:= TN + inttostr(i);
    inc(i);
  end;
  Result := FNestedTables.Add(TN, S);
end;

procedure TSQLRequest.Assign(R : TSQLRequest);
begin
  if not Assigned(R) then Exit;
  FSelectId := R.FSelectId;
  FSelectTable := R.FSelectTable;
  FQuotedId := R.FQuotedId;
  FQuotedTable := R.FQuotedTable;
  FNestedTables.Assign(R.FNestedTables);
  FSelectExpr.Assign(R.FSelectExpr);
  FWhereExpr.Assign(R.FWhereExpr);
end;

function TSQLRequest.Compare(R : TSQLRequest) : Boolean;
begin
  Result := (NestedTables = R.NestedTables) and
            (SelectExpr =   R.SelectExpr) and
            (SelectId =     R.SelectId) and
            (SelectTable =  R.SelectTable) and
            (WhereExpr =    R.WhereExpr);
end;

function TSQLRequest.IsEmpty : Boolean;
begin
  Result := (Length(NestedTables) = 0 ) and
            (Length(SelectExpr  ) = 0 ) and
            (Length(SelectId    ) = 0 ) and
            (Length(SelectTable ) = 0 ) and
            (Length(WhereExpr   ) = 0 );
end;

procedure TSQLRequest.Clear;
begin
  FNestedTables.Clear;
  FSelectExpr.Clear;
  FWhereExpr.Clear;
  FSelectId := '';
  FSelectTable := '';
  FQuotedTable := '';
  FQuotedId := '';
end;

end.

