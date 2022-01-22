{
 dbComposerGrid:
   The Control for working with composed requests

   Part of dbComposer project

   Copyright (c) 2021-2022 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit dbComposerGrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Graphics, LCLType, LCLIntf,
  Grids, DB,
  dbctypes, dbComposerStruct, ExtSqlite3DS, ECommonObjs;

type

  { TDBExtGrid }

  TDBExtGrid = class(TCustomStringGrid)
  private
    FDataSet    : TExtSqlite3Dataset;
    FDB         : TDBStructure;
    FTable      : TDBTable;
    FNeedRedraw : TThreadBoolean;

    FIdTable            : String;
    FCurSelectedTable   : String;
    FSelectedColumns    : TStringList;
    FIDList             : PIntegerArray;
    FIDListCapacity     : Integer;
    FIDListCapacityGrow : Integer;
    FIDListSize         : Integer;

    procedure ClearIdList;
    procedure AddId(id : integer);
    function  GetIdAtRow(index : integer) : integer;
    procedure DBGridOnChanged(O : TObject);
    function  DBGridCheckActual(O : TDBExtBlobValue) : TDBExtActualResult;
    procedure DBGridDrawCell(Sender : TObject; aCol, aRow : Integer;
      aRect : TRect; aState : TGridDrawState);
    function GetIsTableBrowsing: Boolean;
    function GetIsTableGeneric: Boolean;
    procedure SetDataSet(AValue: TExtSqlite3Dataset);
    procedure SetDB(AValue: TDBStructure);
    procedure Execute;
  private
    function GetNeedRedraw: Boolean;
    procedure SetNeedRedraw(AValue: Boolean);
    property NeedRedraw : Boolean read GetNeedRedraw write SetNeedRedraw;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoIdle;

    procedure ExecuteSQL(const S : String);
    procedure ExecuteTable(const TN : String);
    procedure ExecuteSQLExpr(Request: TSQLRequest; const Filter: String;
      GroupBy: Boolean);
    procedure ClearGrid;

    property DataSet : TExtSqlite3Dataset read FDataSet write SetDataSet;
    property DB : TDBStructure read FDB write SetDB;
    property CurSelectedTable : String read FCurSelectedTable;

    property IdAtRow[index : integer] : integer read GetIdAtRow;

    property IsTableBrowsing : Boolean read GetIsTableBrowsing;
    property IsTableGeneric : Boolean read GetIsTableGeneric;
  end;

implementation

const cGenericTable = '<GENERIC>';

{ TDBExtGrid }

procedure TDBExtGrid.ClearIdList;
begin
  if Assigned(FIDList) then
  begin
    FreeMemAndNil(FIDList);
    FIDListCapacity := 0;
    FIDListSize := 0;
  end;
end;

procedure TDBExtGrid.AddId(id: integer);
begin
  if not Assigned(FIDList) then
  begin
    FIDListCapacity := FIDListCapacityGrow;
    FIDList := GetMem(Sizeof(Integer) * FIDListCapacity);
  end;
  if FIDListSize = FIDListCapacity then
  begin
    FIDListCapacityGrow := FIDListCapacityGrow shl 1;
    if FIDListCapacityGrow > 1024 then FIDListCapacityGrow := 1024;
    Inc(FIDListCapacity, FIDListCapacityGrow);
    FIDList := ReAllocMem(FIDList, FIDListCapacity);
  end;
  FIDList^[FIDListSize] := id;
  Inc(FIDListSize);
end;

function TDBExtGrid.GetIdAtRow(index: integer): integer;
begin
  if not Assigned(FIDList) then Exit(-1);
  if index >= FIDListSize then Exit(-1);
  Result := FIDList^[index];
end;

procedure TDBExtGrid.DBGridOnChanged(O: TObject);
var i : integer;
begin
  for i := 0 to FSelectedColumns.Count-1 do
  begin
    if FSelectedColumns.Objects[i] = TDBExtBlobValue(O).Owner then
    begin
      NeedRedraw := true;
      Exit;
    end;
  end;
end;

function TDBExtGrid.DBGridCheckActual(O: TDBExtBlobValue): TDBExtActualResult;
var i : integer;
begin
  for i := 0 to FSelectedColumns.Count-1 do
  begin
    if FSelectedColumns.Objects[i] = O.Owner then
    begin
      Exit(earActual); //!!!
    end;
  end;
  Result := earNoInformation;
end;

procedure TDBExtGrid.DBGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  pen : TPen;
  br  : TBrush;
  fnt : TFont;
  ts : TTextStyle;
  S  : String;
  r  : TRect;
  SG : TDBExtGrid;
  B  : TDBExtBlob;
  BV : TDBExtBlobValue;
  id : integer;
begin
  if FSelectedColumns.Count > aCol then
    B := TDBExtBlob(FSelectedColumns.Objects[aCol])
  else
    B := nil;

  SG := TDBExtGrid(Sender);
  S := SG.Cells[aCol, aRow];

  if Assigned(B) and (aRow > 0) then begin
    if SameText( B.TableName, FIdTable ) then
      id := GetIdAtRow(aRow - 1) else
      id := -1;
    BV := B.AddValue(id, s);
    if BV is TDBExtBlobImageValue then
    begin
      if TDBExtBlobImageValue(BV).Loaded then
        SG.Canvas.StretchDraw(aRect, TDBExtBlobImageValue(BV).Bitmap.Graphic) else
        TDBExtBlobImageValue(BV).Actual := true;
    end;
  end else
  begin
    pen := SG.Canvas.Pen;
    br := SG.Canvas.Brush;
    fnt := SG.Canvas.Font;

    ts := SG.Canvas.TextStyle;
    ts.Alignment := taLeftJustify;
    ts.Layout := tlCenter;
    ts.Wordbreak := True;
    ts.SingleLine := false;
    ts.Opaque := false;

    br.Color := clWhite;
    if gdFixed in aState then br.Color := $DDDDDD;
    Pen.Style := psClear;
    if aRow = 0 then fnt.Color := clBlack else
      fnt.Color := clBlack;
    if gdSelected in aState then
    begin
      br.Color := clBlack;
      fnt.Color := clWhite;
      pen.Color := clYellow;
      pen.Style := psDot;
    end;
    SG.Canvas.Rectangle(aRect);
    fnt.Style := [];
    r := aRect;
    DrawText(SG.Canvas.Handle, pchar(s), Length(S), r, DT_LEFT or DT_WORDBREAK or DT_CALCRECT);
    if r.Height > aRect.Height then ts.Layout := tlTop;
    SG.Canvas.TextRect(aRect, aRect.Left+2, aRect.Top + 2, S, ts);
  end;
end;

function TDBExtGrid.GetIsTableBrowsing: Boolean;
begin
  Result := Length(FCurSelectedTable) > 0;
end;

function TDBExtGrid.GetIsTableGeneric: Boolean;
begin
  Result := SameStr(FCurSelectedTable, cGenericTable);
end;

procedure TDBExtGrid.SetDataSet(AValue: TExtSqlite3Dataset);
begin
  if FDataSet = AValue then Exit;
  FDataSet := AValue;
end;

procedure TDBExtGrid.SetDB(AValue: TDBStructure);
begin
  if FDB = AValue then Exit;
  FDB := AValue;
end;

procedure TDBExtGrid.Execute;
var k, h, i, id : integer;
    S : String;
    R : TRect;
    B : TDBExtBlob;
begin
  if not assigned(FDB) then Exit;
  if not assigned(FDataSet) then Exit;

  FDB.RemoveListener(@DBGridOnChanged);

  FDataSet.Lock;
  try
    ClearIdList;
    FSelectedColumns.Clear;
    FDataSet.Close;

    BeginUpdate;
    try
      RowCount := 1;
      ColCount := 1;

      if IsTableBrowsing then
        FixedCols := 1 else
        FixedCols := 0;

      FDataSet.Open(eomUniDirectional);
      ColCount := DataSet.FieldCount;
      for i := 0 to DataSet.FieldCount- 1 do
      begin
        s := DataSet.Fields[i].FieldName;
        Cells[i, 0] := s;
        case DataSet.Fields[i].DataType of
          ftString : ColWidths[i] := 128;
          ftMemo : ColWidths[i] := 250;
          ftInteger, ftFloat: ColWidths[i] := 32;
        else
          ColWidths[i] := 100;
        end;
        r := Rect(0,0,ColWidths[i],24);
        DrawText(Canvas.Handle, pchar(s), Length(S), r, DT_LEFT or DT_WORDBREAK or DT_CALCRECT);
        if ColWidths[i] < (r.Width + 4) then
          ColWidths[i] := r.Width + 4;

        FSelectedColumns.Add(DataSet.Fields[i].FieldName);
        if IsTableBrowsing then
        begin
          B := FDB.GetExtBlob(DataSet.Fields[i].FieldName);
          if not assigned(B) then
            B := FDB.GetExtBlob(FCurSelectedTable, DataSet.Fields[i].FieldName);
          if assigned(B) then begin
            FSelectedColumns.Objects[i] := B;
            FDB.AddListener(@DBGridOnChanged, @DBGridCheckActual);
            if B is TDBExtImageBlob then ColWidths[i] := 64;
          end;
        end;
      end;
      k := 1;
      while not DataSet.EOF do
      begin
        RowCount := RowCount + 1;
        for i := 0 to DataSet.FieldCount-1 do
        begin
          s := DataSet.Fields[i].AsUTF8String;
          Cells[i, k] := s;
          r := Rect(0,0,ColWidths[i],24);
          DrawText(Canvas.Handle, pchar(s), Length(S), r, DT_LEFT or DT_WORDBREAK or DT_CALCRECT);
          if IsTableBrowsing then
          begin
            if Assigned(FTable) and
               FTable.HasPrimaryKey and
               SameText(DataSet.Fields[i].FieldName,
                        FTable.PrimaryKeyField.FieldName) then
            begin
              id := DataSet.Fields[i].AsInteger;
              AddId(id);
            end else
            if Assigned(FSelectedColumns.Objects[i]) then
            begin
              B := TDBExtBlob(FSelectedColumns.Objects[i]);
              if B is TDBExtImageBlob then
                r.Height := 60;
            end;
          end;

          if DataSet.Fields[i].DataType in [ftInteger, ftFloat] then
          begin
            if ColWidths[i] < (r.Width + 4) then
              ColWidths[i] := r.Width + 4;
          end;

          h := r.Height + 4;
          if h > 100 then h := 100;
          if h > RowHeights[k] then
            RowHeights[k] := h;
        end;
        Inc(k);
        DataSet.Next;
      end;
    finally
      DataSet.Close;
      EndUpdate;
    end;
  finally
    FDataSet.UnLock;
  end;
end;

function TDBExtGrid.GetNeedRedraw: Boolean;
begin
  Result := FNeedRedraw.Value;
end;

procedure TDBExtGrid.SetNeedRedraw(AValue: Boolean);
begin
  FNeedRedraw.Value := AValue;
end;

constructor TDBExtGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Options := [goFixedVertLine,goFixedHorzLine,goVertLine,goHorzLine,goRowSelect,goSmoothScroll];
  TitleStyle := tsNative;

  FIDListSize := 0; FIDListCapacity := 0; FIDListCapacityGrow := 32;
  FIDList := nil;
  FIdTable := '';
  FCurSelectedTable := '';
  FSelectedColumns := TStringList.Create;
  FNeedRedraw := TThreadBoolean.Create(false);

  OnDrawCell := @DBGridDrawCell;
end;

destructor TDBExtGrid.Destroy;
begin
  ClearGrid;
  ClearIdList;
  FSelectedColumns.Free;
  FNeedRedraw.Free;
  inherited Destroy;
end;

procedure TDBExtGrid.DoIdle;
begin
  if NeedRedraw then begin
    Invalidate;
  end;
end;

procedure TDBExtGrid.ExecuteSQL(const S: String);
begin
  if not assigned(FDataSet) then Exit;
  FDataSet.Lock;
  try
    FDataSet.Close;
    FDataSet.SQL := S;
    FCurSelectedTable := '';
    FIdTable := '';
    Execute;
  finally
    FDataSet.UnLock;
  end;
end;

procedure TDBExtGrid.ExecuteTable(const TN: String);
begin
  if not assigned(FDataSet) then Exit;
  if not Assigned(FDB) then Exit;

  FTable := FDB.ByName(TN);

  if not Assigned(FTable) then Exit;

  FDataSet.Lock;
  try
    FDataSet.Close;
    FDataSet.SQL := 'select * from ' + FTable.QuotedName;
    FCurSelectedTable := TN;
    FIdTable := TN;
    Execute;
  finally
    FDataSet.UnLock;
  end;
end;

procedure TDBExtGrid.ExecuteSQLExpr(Request: TSQLRequest;
  const Filter : String;
  GroupBy: Boolean);
var s, s2 : String;
begin
  if not assigned(FDataSet) then Exit;
  FDataSet.Lock;
  try
    Request.SelectExprPaths := true;
    s2 := Request.WhereExpr;

    s := 'select ' + Request.SelectQuotedTable + '.' + Request.SelectQuotedId + ', ' +
                   Request.SelectExpr + ' from ' + Request.NestedTablesListed;

    if (length(Filter) + Length(S2)) > 0 then
    begin
      S := S + ' where ';
      if length(S2) > 0 then
      begin
        S := S + '(' + S2 + ')';
        if length(Filter) > 0 then
          S := S + ' and ';
      end;
      if length(Filter) > 0 then
        S := S + '(' + Filter + ')';
    end;

    if GroupBy then
    begin
      S := S + ' group by ' + Request.SelectQuotedTable + '.' + Request.SelectQuotedId;
      FCurSelectedTable := Request.SelectTable;
      if Assigned(FDB) then
        FTable := FDB.ByName(FCurSelectedTable) else
        FTable := nil;
    end else
      FCurSelectedTable := cGenericTable;

    FDataSet.Close;
    FDataSet.SQL := S;
    FIdTable := Request.SelectTable;
    Execute;
  finally
    FDataSet.UnLock;
  end;
end;

procedure TDBExtGrid.ClearGrid;
begin
  Clear;
  if Assigned(FDB) then
    FDB.RemoveListener(@DBGridOnChanged);
end;

end.

