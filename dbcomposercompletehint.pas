{
 dbComposerCompleteHint:
   The Control for SynEdit Completions

   Part of dbComposer project

   Copyright (c) 2021-2022 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit dbComposerCompleteHint;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics,
  OGLFastList, LCLType,
  SynEdit, SynEditKeyCmds, SynEditTypes,
  ExtSqliteUtils;

type

  TSQLCompletionKind = (sckKeyword, sckType, sckFunction, sckTable, sckField);
  TSQLCompletionKinds = set of TSQLCompletionKind;

const

  cDefCompletionKinds : TSQLCompletionKinds = [sckKeyword, sckType,
                                               sckFunction, sckTable, sckField];
type

  { TCompletionObj }

  TCompletionObj = class
  private
    FValue : String;
    FKind  : TSQLCompletionKind;
    FData  : TObject;
  public
    constructor Create(const aValue : String; aKind : TSQLCompletionKind; aData : TObject = nil);

    property Value : String read FValue write FValue;
    property Data  : TObject read FData write FData;
    property Kind : TSQLCompletionKind read FKind write FKind;
  end;

  { TCompletionCollection }

  TCompletionCollection = class(TFastCollection)
  private
    function GetObj(aIndex : Integer): TCompletionObj;
    procedure SetObj(aIndex : Integer; AValue: TCompletionObj);
  public
    procedure AddObj(const aValue : String; aKind : TSQLCompletionKind; aData : TObject = nil);
    function IndexOf(const S : String) : Integer;
    function Complete(const S : String) : String;
    procedure DeleteMasked(aMask : TSQLCompletionKinds);
    procedure CompleteFillList(SL : TStrings; const S : String;
                               out ind : Integer;
                               aMask : TSQLCompletionKinds = [sckKeyword, sckType,
                                               sckFunction, sckTable, sckField]);
    property Obj[aIndex : Integer] : TCompletionObj read GetObj write SetObj;
  end;

  TSetFontParams = procedure (O : TCompletionObj; F : TFont; out imi : integer) of object;

  { TCustomCompleteHint }

  TCustomCompleteHint = class(TCustomControl)
  private
    FCompletionKeys : TCompletionCollection;
    FIcoBitmap : TBitmap;
    FImageIndex : Integer;
    FValue : String;
    FValueWordStartsAt : TPoint;
    FImageList : TImageList;
    FSynEdit : TSynEdit;
    FOnSetFontParams : TSetFontParams;
    procedure SetImageIndex(AValue : Integer);
    procedure SetSynEdit(AValue : TSynEdit);
    procedure SetValue(AValue : String);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;

    procedure EditorChange(Sender : TObject);
    procedure EditorProcessCommand(Sender : TObject;
      var Command : TSynEditorCommand; var AChar : TUTF8Char; Data : pointer);
    procedure EditorCommandProcessed(Sender : TObject;
      var Command : TSynEditorCommand; var AChar : TUTF8Char; Data : pointer);
    procedure EditorStatusChange(Sender : TObject;
      Changes : TSynStatusChanges);

    property Value : String read FValue write SetValue;
    property ImageList : TImageList read FImageList write FImageList;
    property ImageIndex : Integer read FImageIndex write SetImageIndex;
    property SynEdit : TSynEdit read FSynEdit write SetSynEdit;
    property CompletionKeys :TCompletionCollection read FCompletionKeys write
                                                        FCompletionKeys;
    property OnSetFontParams : TSetFontParams read FOnSetFontParams
                                              write FOnSetFontParams;
  end;

implementation

uses LazUTF8;

{ TCustomPlotter }

constructor TCustomCompleteHint.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FImageList := nil;
  FImageIndex := -1;
  FIcoBitmap := TBitmap.Create;
  FIcoBitmap.Width := 16;
  FIcoBitmap.Height := 16;
  Height := 24;
  Width := 24;
  FOnSetFontParams := nil;
  FCompletionKeys := nil;
  Visible := false;
end;

procedure TCustomCompleteHint.SetImageIndex(AValue : Integer);
begin
  if FImageIndex = AValue then Exit;
  FImageIndex := AValue;
  if Assigned(FImageList) and (FImageIndex >= 0) then
  begin
    FImageList.Getbitmap(FImageIndex, FIcoBitmap);
  end;
end;

procedure TCustomCompleteHint.SetSynEdit(AValue : TSynEdit);
begin
  if FSynEdit = AValue then Exit;
  FSynEdit := AValue;
  Canvas.Font.Assign(FSynEdit.Font);
end;

procedure TCustomCompleteHint.SetValue(AValue : String);
begin
  if FValue = AValue then Exit;
  FValue := AValue;
  Width := Canvas.TextWidth(FValue) + 28;
end;

destructor TCustomCompleteHint.Destroy;
begin
  FIcoBitmap.Free;
  inherited Destroy;
end;

procedure TCustomCompleteHint.Paint;
var ts : TTextStyle;
    aRect : TRect;
begin
  Canvas.Brush.Color := clWhite;
  Canvas.Pen.Color := clBlack;
  Canvas.Rectangle(0, 0, Width, Height);
  if FImageIndex > 0 then
  begin
   Canvas.Draw(4, 4, FIcoBitmap);
  end;
  ts := Canvas.TextStyle;
  ts.Alignment  := taLeftJustify;
  ts.Layout     := tlCenter;
  ts.Wordbreak  := True;
  ts.SingleLine := false;
  ts.Opaque     := false;
  aRect := Rect(24, 0, Width, Height);
  Canvas.TextRect(aRect, 24, Height div 2, FValue, ts);
  inherited Paint;
end;

procedure TCustomCompleteHint.EditorChange(Sender : TObject);
var s : String;
    SL : TStringList;
    i : integer;
    p : TPoint;
begin
  if not (Assigned(FSynEdit) and Assigned(FCompletionKeys)) then Exit;

  S := FSynEdit.GetWordAtRowCol(FSynEdit.LogicalCaretXY);
  if Length(S) > 0 then
  begin
    SL := TStringList.Create;
    try
      FCompletionKeys.CompleteFillList(SL, S, i);
      if SL.Count = 1 then
      begin
        FSynEdit.GetWordBoundsAtRowCol(FSynEdit.LogicalCaretXY, p.x, p.y);
        FValueWordStartsAt.x := p.x;
        FValueWordStartsAt.y := FSynEdit.LogicalCaretXY.y;
        p := FSynEdit.ClientToParent(FSynEdit.RowColumnToPixels(Point(p.x, FSynEdit.CaretY) ));
        Left := p.x;
        Top := p.y + FSynEdit.Canvas.TextHeight('|');
        Value := SL[0];

        i := -1;
        if Assigned(OnSetFontParams) then
          OnSetFontParams(TCompletionObj(SL.Objects[0]), Canvas.Font, i);

        ImageIndex := i;

        Show;
      end else
        Hide;
    finally
      SL.Free;
    end;
  end else
  begin
    Hide;
  end;
end;

procedure TCustomCompleteHint.EditorProcessCommand(Sender : TObject;
  var Command : TSynEditorCommand; var AChar : TUTF8Char; Data : pointer);
var S : String;
    st, en : integer;
begin
  if not (Assigned(FSynEdit)) then Exit;
  case Command of
    ecLineBreak :
    begin
      if Visible then
      begin
        S := FSynEdit.GetWordAtRowCol(FSynEdit.LogicalCaretXY);
        FSynEdit.GetWordBoundsAtRowCol(FSynEdit.LogicalCaretXY, st, en);
        FSynEdit.CaretX := en;
        FSynEdit.InsertTextAtCaret(Utf8Copy(Value, Utf8Length(S)+1,
                                                   Utf8Length(Value)));
        Hide;
        Command := ecNone;
      end;
    end;
    ecChar :
    begin
      if (Ord(AChar[1]) = VK_ESCAPE) and Visible then
      begin
         Hide;
      end;
    end;
  end;
end;

procedure TCustomCompleteHint.EditorCommandProcessed(Sender : TObject;
  var Command : TSynEditorCommand; var AChar : TUTF8Char; Data : pointer);
begin
  // do nothing
end;

procedure TCustomCompleteHint.EditorStatusChange(Sender : TObject;
  Changes : TSynStatusChanges);
var S : String;
    p : TPoint;
begin
  if not (Assigned(FSynEdit)) then Exit;
  if (scFocus in Changes) then
  begin
    if (not FSynEdit.Focused) and (Visible) then
      Hide;
  end else
  if ([scCaretX, scCaretY] * Changes <> []) then
  begin
    if Visible then
    begin
      S := FSynEdit.GetWordAtRowCol(FSynEdit.LogicalCaretXY);
      FSynEdit.GetWordBoundsAtRowCol(FSynEdit.LogicalCaretXY, p.x, p.y);
      p.y := FSynEdit.LogicalCaretXY.y;
      if (p.x <> FValueWordStartsAt.x) or
         (p.y <> FValueWordStartsAt.y) or
         (not (UTF8Pos(S, Value) = 1)) then
        Hide;
    end;
  end;
end;

{ TCompletionCollection }

function TCompletionCollection.GetObj(aIndex : Integer): TCompletionObj;
begin
  Result := TCompletionObj(Item[aIndex]);
end;

procedure TCompletionCollection.SetObj(aIndex : Integer; AValue: TCompletionObj
  );
begin
  Item[aIndex] := AValue;
end;

procedure TCompletionCollection.AddObj(const aValue: String;
  aKind: TSQLCompletionKind; aData: TObject);
begin
  if aKind = sckField then
  begin
    if IndexOf(aValue) >= 0 then
      Exit;
  end;
  Add(TCompletionObj.Create(aValue, aKind, aData));
end;

function TCompletionCollection.IndexOf(const S: String): Integer;
var i : integer;
begin
  for i := 0 to Count-1 do
  begin
    if sqluCompareNames( Obj[i].Value, S ) then Exit(i);
  end;
  Result := -1;
end;

function TCompletionCollection.Complete(const S: String): String;
var i, L, v : integer;
    k : TSqliteKwFormatOption;
begin
  L := Length(S);
  v := -1;
  for i := 0 to Count-1 do
  begin
    if sqluCompareNames(Copy(Obj[i].Value, 1, L), S) then
    begin
      v := i;
      Break;
    end;
  end;
  if v >= 0 then
  begin
    if Obj[v].Kind = sckKeyword then
    begin
      k := sqluDetectFormat(S);
      Result := sqluFormatKeyWord(Obj[v].Value, k);
    end
    else
      Result := Obj[v].Value;
  end else
    Result := '';
end;

procedure TCompletionCollection.DeleteMasked(aMask: TSQLCompletionKinds);
var i : integer;
begin
  for i := Count-1 downto 0 do
  begin
    if Obj[i].Kind in aMask then Delete(i)
  end;
end;

procedure TCompletionCollection.CompleteFillList(SL : TStrings;
  const S : String; out ind : Integer; aMask : TSQLCompletionKinds);
var i, L : integer;
    k : TSqliteKwFormatOption;
    st : String;
begin
  ind := -1;
  if not assigned(SL) then Exit;

  SL.Clear;

  L := Length(S);
  for i := 0 to Count-1 do
  if Obj[i].Kind in aMask then
  begin
    if sqluCompareNames(Copy(Obj[i].Value, 1, L), S) then begin
      if ind < 0 then ind := SL.Count;

      if Obj[i].Kind = sckKeyword then
      begin
        k := sqluDetectFormat(S);
        st := sqluFormatKeyWord(Obj[i].Value, k);
      end
      else
        st := Obj[i].Value;

      SL.AddObject(st, Obj[i]);
    end;
  end;
end;

{ TCompletionObj }

constructor TCompletionObj.Create(const aValue: String;
  aKind: TSQLCompletionKind; aData: TObject);
begin
  FValue := aValue;
  FKind := aKind;
  FData := aData;
end;

end.

