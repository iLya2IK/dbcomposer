{
 dbComposerUtils:
   Some helpful utils for interunit connecting in project

   Part of dbComposer project

   Copyright (c) 2021-2022 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit dbComposerUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, kcThreadPool,
  Forms, Controls, Graphics,
  ExtCtrls, StdCtrls,
  LCLType, LCLIntf, LazMethodList, OGLFastNumList,
  JSONPropStorage,
  SynHighlighterSQLite3, SynEditHighlighter,
  ECommonObjs, OGLFastList,
  dbComposerStruct, dbComposerCompleteHint,
  ExtSqlite3DS, ExtSqliteUtils, ExprSqlite3Funcs;

type
  { TCfgField }

  TCfgField = class
    Name : String;
    Value : Variant;
    constructor Create(const aName : String; const aDefValue : Variant);
  end;

  TCfgFields = class (specialize TFastBaseCollection <TCfgField>);

  { TJsonCfgEnum }

  TJsonCfgEnum = class
  private
    FName : String;
    FAddFields : TCfgFields;
    function GetAddField(Index : Integer): TCfgField;
    function GetAddFieldsCnt: integer;
  public
    constructor Create(const aname : String);
    destructor Destroy; override;
    procedure AddNewAdvField(const aName: String; const aDefValue: Variant);
    property Name : String read FName;
    property AddFieldsCnt : integer read GetAddFieldsCnt;
    property AddField[Index : Integer] : TCfgField read GetAddField;
  end;

  { TJsonIndxClass }

  TJsonIndxClass = class(TJsonCfgEnum)
  private
    ftClass : TBaseSinExprsClass;
  public
    constructor Create(const aname : String; aClass : TBaseSinExprsClass);
    property tClass : TBaseSinExprsClass read FTClass;
  end;

  { TJsonFuncClass }

  TJsonFuncClass = class(TJsonCfgEnum)
  private
    ftClass : TSqlite3FunctionClass;
  public
    constructor Create(const aname : String; aClass : TSqlite3FunctionClass);
    property tClass : TSqlite3FunctionClass read FTClass;
  end;

  { TJsonBlobKind }

  TJsonBlobKind = class(TJsonCfgEnum)
  private
    FKind : TDBExtBlobKind;
  public
    constructor Create(const aname : String; aKind : TDBExtBlobKind);
    property Kind : TDBExtBlobKind read FKind;
  end;

  { TJsonIndxStringsStyle }

  TJsonIndxStringsStyle = class(TJsonCfgEnum)
  private
    FKind : TDBIndxStringsStyle;
  public
    constructor Create(const aname : String; aKind : TDBIndxStringsStyle);
    property Kind : TDBIndxStringsStyle read FKind;
  end;

  { TJsonCfgEnums }

  TJsonCfgEnums = class (specialize TFastBaseCollection <TJsonCfgEnum>)
  private
    FFields : TCfgFields;
    function GetField(Index : Integer): TCfgField;
    function GetFieldsCnt: integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure AddNewField(const aName : String; const aDefValue : Variant);
    property FieldsCnt : integer read GetFieldsCnt;
    property Field[Index : Integer] : TCfgField read GetField;
  end;


  TNestedExpr = class;

  TSQLNestGenOption = (sngoCollapseForeignString);
  TSQLNestGenOptions = set of TSQLNestGenOption;

  TNestedExprsList = class(specialize TThreadSafeFastBaseList<TNestedExpr>);

  TNestedTokenKind = (ntkNone,      ntkSymbol, ntkTableName,
                      ntkFieldName, ntkNumber, ntkString);

  { TNestedToken }

  TNestedToken = class
  private
    FKind : TNestedTokenKind;
    FText : String;
    FCuLocPos : Integer;
    function GetLen : Integer;
    function GetUnquotedText: String;
  public
    constructor Create(const Token : String; aKind : TNestedTokenKind;
                             aCuLocPos : Integer);
    property Kind : TNestedTokenKind read FKind;
    property Text : string read FText;
    property UnQuotedText : String read GetUnquotedText;
    //
    property StrLen : Integer read GetLen;
    property LocPos : Integer read FCuLocPos;
  end;

  TNestedTokenVisStyle = record
    Color : TColor;
    Style : TFontStyles;
  end;

  { TNestedTokenStyles }

  TNestedTokenStyles = class
  private
    FStyle : Array [TNestedTokenKind] of TNestedTokenVisStyle;
    function GetStyle(index : TNestedTokenKind) : TNestedTokenVisStyle;
    procedure SetStyle(index : TNestedTokenKind; AValue : TNestedTokenVisStyle);
  public
    property Style[index : TNestedTokenKind] : TNestedTokenVisStyle
                         read GetStyle write SetStyle; default;
  end;


  { TNestedExpr }

  TNestedExpr = class(TFastCollection)
  private
    FDB : TDBStructure;
    FExpr : String;
    FTagValue : LongInt;
    FTokenVisStyles : TNestedTokenStyles;
    function GetToken(index : integer) : TNestedToken;
  public
    constructor Create(aDB : TDBStructure; const Expr : String; aTag : LongInt;
                             Styles : TNestedTokenStyles); overload;
    procedure DrawText(C : TCanvas; R: TRect);
    function Parse(const Expr : String) : Boolean;
    function GetTokenByPath(const aPath : String ) : String;
    procedure AddToken(const Token : String; aKind : TNestedTokenKind;
                             aCuLocPos : Integer);
    function GetTokenAtPos(AChar : Integer) : TNestedToken;
    property Tag : LongInt read FTagValue write FTagValue;
    property FullExpr : String read FExpr;
    property Token[index : integer] : TNestedToken read GetToken; default;
  end;

  TDBHelperState = (dbhsEditorFont, dbhsFormatSettings);

  TDBHelperStateChanged = procedure (aState : TDBHelperState) of object;
  PDBHelperStateChanged = ^TDBHelperStateChanged;

  { TStateChanges }

  TStateChanges = class(specialize TFastBaseNumericList <TDBHelperState>)
  public
    function  DoCompare(Item1, Item2 : Pointer) : Integer; override;
    procedure Cummulate(St : TDBHelperState);
  end;

  { TSynAttrsDefs }

  TSynAttrsDefs = class(TStringList)
  public
    procedure AddAttr(aAttr : TSynHighlighterAttributes);

    function AttrByName(const aIndex : String) : TSynHighlighterAttributes;
    function AttrByIndex(aIndex : Integer) : TSynHighlighterAttributes;
  end;

  { TDBHelper }

  TDBHelper = class
  private
    FThreadPool : TThreadPool;
    FDBPath : TThreadUtf8String;
    FSynSQL : TSynSQLite3Syn;
    FAttrsDefs : TSynAttrsDefs;
    FCummulation : TThreadInteger;
    FCummulatedStates : TStateChanges;
    fDefaultTokenVisStyles, fTokenVisStyles : TNestedTokenStyles;

    FKwFormat : TSqliteKwFormatOption;

    FStateListeners : TMethodList;

    FEditorFontName : String;
    FEditorFontSize : Integer;

    function GetDBPath : String;
    function GetThreaded: Boolean;
    procedure SetDBPath(AValue : String);
    procedure SetEditorFontName(AValue : String);
    procedure SetEditorFontSize(AValue : Integer);
    procedure SendStateChanged(aState : TDBHelperState);
    procedure SetKwFormat(AValue: TSqliteKwFormatOption);
  public
    JsonIndxClasses : TJsonCfgEnums;
    JsonIndxStringsStyles : TJsonCfgEnums;
    JsonFuncClasses : TJsonCfgEnums;
    JsonBlobKinds : TJsonCfgEnums;
    function GetJsonBlobKindEnum(aKind : TDBExtBlobKind) : TJsonCfgEnum;
    function GetJsonStrStyleEnum(aStyle : TDBIndxStringsStyle) : TJsonCfgEnum;
    function GetJsonIndxClassEnum(aClass : TBaseSinExprsClass) : TJsonCfgEnum;
    function GetJsonFuncClassEnum(aClass : TSqlite3FunctionClass) : TJsonCfgEnum;

  public
    AppPath : String;

  public
    constructor Create;
    destructor Destroy; override;

    procedure DrawListItem(Control: TWinControl; Index: Integer;
                              ARect: TRect; State: TOwnerDrawState);
    procedure InitThreadPool(AThreadCnt : integer);

    function VersionStr : String;
    function VersionMag : Word;
    function VersionMin : Word;
    function VersionNum : Cardinal;

    procedure LoadFromConfig(aConfig : TJSONPropStorage);
    procedure SaveToConfig(aConfig : TJSONPropStorage);

    property Sqlite3AttrsDefs : TSynAttrsDefs read FAttrsDefs;
    property Sqlite3Highlighter : TSynSQLite3Syn read FSynSQL;
    property KwFormat : TSqliteKwFormatOption read FKwFormat write SetKwFormat;
    property DefaultTokenVisStyles : TNestedTokenStyles read fDefaultTokenVisStyles;
    property TokenVisStyles : TNestedTokenStyles read fTokenVisStyles;
    procedure SetFontParamsFromCompletionObj(O : TCompletionObj; F : TFont; out
      imi : integer);
    property ThreadPool : TThreadPool read FThreadPool write FThreadPool;
    property Threaded : Boolean read GetThreaded;
    property DBPath : String read GetDBPath write SetDBPath;

    procedure AddStateListener(aEar : TDBHelperStateChanged);
    procedure BeginUpdate;
    procedure EndUpdate;

    property EditorFontName : String read FEditorFontName write SetEditorFontName;
    property EditorFontSize : Integer read FEditorFontSize write SetEditorFontSize;
  end;

function NestedTokenVisStyle(C: TColor; S : TFontStyles) : TNestedTokenVisStyle;
function ExtractTableField(const aPath : String; out TN, FN : String) : Boolean;

const {$IFDEF UNIX}
      cSysDelimiter = DirectorySeparator;
      cNonSysDelimiter = '\';
      {$ELSE}
      {$IFDEF WINDOWS}
      cSysDelimiter = DirectorySeparator;
      cNonSysDelimiter = '/';
      {$ENDIF}
      {$ENDIF}

var
  DBHelper : TDBHelper;


implementation

uses LazUTF8, ExtSqliteTokens, dbComposerConfigParser, dbComposerConsts;

function ExtractTableField(const aPath : String; out TN, FN : String) : Boolean;
var Expr : TSqliteExpr;
begin
  Expr := TSqliteExpr.Create(aPath);
  try
    if (Expr.Count = 3) and
       (Expr[1].Kind in [stkSymbol, stkSpace]) then
    begin
      TN := Expr[0].QuotedToken;
      FN := Expr[2].QuotedToken;
      Result := true;
    end else
    if (Expr.Count = 1) and
       (Expr[0].Kind = stkIdentifier) and
       (Expr[0].IdCnt = 2) then
    begin
      TN := Expr[0].SubToken[0].QuotedToken;
      FN := Expr[0].SubToken[1].QuotedToken;
      Result := true;
    end else
    begin
      TN := '';
      FN := '';
      Result := false;
    end;
  finally
    Expr.Free;
  end;
end;

function NestedTokenVisStyle(C : TColor; S : TFontStyles
  ) : TNestedTokenVisStyle;
begin
  Result.Color := C;
  Result.Style := S;
end;

{ TStateChanges }

function TStateChanges.DoCompare({%H-}Item1, {%H-}Item2 : Pointer) : Integer;
begin
  Result := 0;
end;

procedure TStateChanges.Cummulate(St : TDBHelperState);
begin
  if IndexOf(St) < 0 then Add(St);
end;

{ TSynAttrsDefs }

procedure TSynAttrsDefs.AddAttr(aAttr : TSynHighlighterAttributes);
begin
  AddObject(aAttr.Name, aAttr);
end;

function TSynAttrsDefs.AttrByName(const aIndex : String
  ) : TSynHighlighterAttributes;
var k : integer;
begin
  k := IndexOf(aIndex);
  if k >=0 then
    Result := TSynHighlighterAttributes(Objects[k]) else
    Result := nil;
end;

function TSynAttrsDefs.AttrByIndex(aIndex : Integer
  ) : TSynHighlighterAttributes;
begin
  Result := TSynHighlighterAttributes(Objects[aIndex]);
end;

{ TJsonIndxStringsStyle }

constructor TJsonIndxStringsStyle.Create(const aname: String;
  aKind: TDBIndxStringsStyle);
begin
  inherited Create(aname);
  FKind := aKind;
end;

{ TJsonBlobKind }

constructor TJsonBlobKind.Create(const aname: String; aKind: TDBExtBlobKind);
begin
  inherited Create(aname);
  FKind := aKind;
end;

{ TJsonFuncClass }

constructor TJsonFuncClass.Create(const aname: String;
  aClass: TSqlite3FunctionClass);
begin
  inherited Create(aname);
  ftClass := aClass;
end;

{ TJsonIndxClass }

constructor TJsonIndxClass.Create(const aname: String;
  aClass: TBaseSinExprsClass);
begin
  inherited Create(aname);
  ftClass := aClass;
end;

{ TJsonCfgEnums }

function TJsonCfgEnums.GetField(Index : Integer): TCfgField;
begin
  Result := FFields[index];
end;

function TJsonCfgEnums.GetFieldsCnt: integer;
begin
  Result := FFields.Count;
end;

constructor TJsonCfgEnums.Create;
begin
  inherited Create;
  FFields := TCfgFields.Create;
end;

destructor TJsonCfgEnums.Destroy;
begin
  FFields.Free;
  inherited Destroy;
end;

procedure TJsonCfgEnums.AddNewField(const aName: String;
  const aDefValue: Variant);
begin
  FFields.Add(TCfgField.Create(aName, aDefValue));
end;

{ TCfgField }

constructor TCfgField.Create(const aName: String; const aDefValue: Variant);
begin
  Name := aName;
  Value := aDefValue;
end;

{ TJsonCfgEnum }

function TJsonCfgEnum.GetAddField(Index: Integer): TCfgField;
begin
  Result := FAddFields[index];
end;

function TJsonCfgEnum.GetAddFieldsCnt: integer;
begin
  Result := FAddFields.Count;
end;

constructor TJsonCfgEnum.Create(const aname: String);
begin
  FName := aname;
  FAddFields := TCfgFields.Create;
end;

destructor TJsonCfgEnum.Destroy;
begin
  FAddFields.Free;
  inherited Destroy;
end;

procedure TJsonCfgEnum.AddNewAdvField(const aName: String; const aDefValue: Variant
  );
begin
  FAddFields.Add(TCfgField.Create(aName, aDefValue));
end;


{ TNestedTokenStyles }

function TNestedTokenStyles.GetStyle(index : TNestedTokenKind
  ) : TNestedTokenVisStyle;
begin
  Result := FStyle[index];
end;

procedure TNestedTokenStyles.SetStyle(index : TNestedTokenKind;
  AValue : TNestedTokenVisStyle);
begin
  FStyle[index] := AValue;
end;

{ TNestedExpr }

function TNestedExpr.GetToken(index : integer) : TNestedToken;
begin
  Result := TNestedToken(Item[index]);
end;

constructor TNestedExpr.Create(aDB : TDBStructure; const Expr : String;
  aTag : LongInt; Styles : TNestedTokenStyles);
begin
  inherited create;
  FTagValue := aTag;
  FDB := aDB;
  FTokenVisStyles := Styles;
  FExpr := Utf8Trim(Expr);
  Parse(FExpr);
end;

procedure TNestedExpr.DrawText(C : TCanvas; R : TRect);
var i : integer;
    IFC : TColor;
    IFS : TFontStyles;
    T : TNestedToken;
begin
  if not Assigned(FTokenVisStyles) then
  begin
    C.TextOut(R.Left, R.Top, FullExpr);
    Exit;
  end;
  IFC := C.Font.Color;
  IFS := C.Font.Style;
  try
    C.MoveTo(R.TopLeft);
    for i := 0 to Count-1 do
    begin
      T := Self[i];
      if (FTokenVisStyles[T.Kind].Color <> C.Font.Color) or
         (FTokenVisStyles[T.Kind].Style <> C.Font.Style) then
      begin
        C.Font.Color := FTokenVisStyles[T.Kind].Color;
        C.Font.Style := FTokenVisStyles[T.Kind].Style;
      end;
      C.TextOut(C.PenPos.X, R.Top, T.Text);
      if C.PenPos.X > R.Right then Break;
    end;
  finally
    C.Font.Color := IFC;
    C.Font.Style := IFS;
  end;
end;

function TNestedExpr.Parse(const Expr : String) : Boolean;
const letters = ['a'..'z','A'..'Z','_'];
      numbers = ['-','1'..'9','0'];
      exnums  = numbers + ['.','e','E','+'];
      identif = letters + ['1'..'9','0'];
      symbols = [' ', ',', ';', ':', '.', '(', ')'];

      STR_SYM = '''';
      ID_SYM = '"';

      MODE_NONE = 0;
      MODE_ID   = 1;
      MODE_XID  = 2;
      MODE_STR  = 3;
      MODE_NUM  = 4;
      MODE_SYM  = 5;

var i, s, e : integer;
    M : Byte = MODE_NONE;
    Error : Boolean;

procedure StartToken(MN : Byte);
begin
  M := MN;
  s := i;
  e := s;
  Inc(i);
end;

procedure PushToken();
var SS : String;
    K : TNestedTokenKind;
begin
  if M = MODE_NONE then Exit;
  SS := Copy(Expr, s, e - s + 1);
  case M of
    MODE_ID : begin
        if Assigned(FDB.ByName(SS)) then
          K := ntkTableName else K := ntkFieldName;
      end;
    MODE_XID : begin
        if Assigned(FDB.ByName(sqluUnquotedId(SS))) then
          K := ntkTableName else K := ntkFieldName;
      end;
    MODE_STR :
         K := ntkString;
    MODE_NUM :
         K := ntkNumber;
    MODE_SYM :
         K := ntkSymbol;
  else
    K := ntkNone;
  end;
  AddToken(SS, K, s);
  M := MODE_NONE;
end;

var U8Len  : Integer;
    U8CLen, n : Integer;
    StrPtr : PChar;
begin
  Clear;
  Error := false;
  i := 1; s := 1; e := 1;
  U8Len := Length(Expr);
  while i <= U8Len do
  begin
    StrPtr := PChar(@(Expr[i]));
    U8CLen := UTF8CodepointSizeFast(StrPtr);
    if U8CLen = 1 then
    begin
      case M of
        MODE_NONE: begin
            if StrPtr^ in letters then begin
              StartToken(MODE_ID);
            end else
            if StrPtr^ in numbers then begin
              StartToken(MODE_NUM);
            end else
            if StrPtr^ in symbols then begin
              StartToken(MODE_SYM);
            end else
            if StrPtr^ = STR_SYM then begin
              StartToken(MODE_STR);
            end else
            if StrPtr^ = ID_SYM then begin
              StartToken(MODE_XID);
            end else
            begin
              Error := true;
              Break;
            end;
          end;
        MODE_ID : begin
            if StrPtr^ in identif then begin
              Inc(I); Inc(e);
            end else
              PushToken();
          end;
        MODE_NUM : begin
            if StrPtr^ in exnums then begin
              Inc(I); Inc(e);
            end else
              PushToken();
          end;
        MODE_STR : begin
            Inc(e);

            if StrPtr^ = STR_SYM then
              PushToken();

            Inc(I);
          end;
        MODE_XID : begin
            Inc(e);
            Inc(I);
            if (StrPtr^ = ID_SYM) then
            begin
              n := 1;
              while (i <= U8Len) do
              begin
                StrPtr := PChar(@(Expr[i]));
                U8CLen := UTF8CodepointSizeFast(StrPtr);

                if (U8CLen = 1) and (StrPtr^ = ID_SYM) then
                begin
                  Inc(e);
                  Inc(n);
                  Inc(I);
                end else
                  Break;
              end;
              if (n mod 2) = 1 then
                PushToken();
            end;
          end;
        MODE_SYM : begin
            if StrPtr^ in symbols then begin
              Inc(I); Inc(e);
            end else
              PushToken();
          end;
      end;
    end else begin
      if M in [MODE_XID, MODE_STR] then
      begin
        Inc(i, U8CLen); Inc(e, U8CLen);
      end else
      begin
        Error := true;
        Break;
      end;
    end;
  end;
  if M in [MODE_STR, MODE_XID] then Error := true;

  if Error then
  begin
    Clear;
    AddToken(Expr, ntkNone, 1)
  end
  else
  begin
    PushToken();
  end;
  Result := not Error;
end;

function TNestedExpr.GetTokenByPath(const aPath: String): String;
var
  TN, FN : String;
  TF, FF : Boolean;
  k : integer;
begin
  if ExtractTableField(aPath, TN, FN) then
  begin
    TF := false; FF := false;
    for k := 0 to Count-1 do begin
      case Token[k].Kind of
      ntkTableName:
      begin
        if TF then Exit('');
        if sqluCompareNames(Token[k].Text, TN) then
          TF := true;
      end;
      ntkFieldName:
      begin
        if FF then Exit('');
        if TF and sqluCompareNames(Token[k].Text, FN) then
          FF := true;
      end;
      ntkNumber, ntkString:
      begin
        if TF and FF then
          Exit(Token[k].UnQuotedText);
      end;
      end;
    end;
  end;
  Result := '';
end;

procedure TNestedExpr.AddToken(const Token : String; aKind : TNestedTokenKind;
  aCuLocPos : Integer);
begin
  Add(TNestedToken.Create(Token, aKind, aCuLocPos));
end;

function TNestedExpr.GetTokenAtPos(AChar : Integer) : TNestedToken;
var i : integer;
begin
  for i := 0 to Count-1 do
  begin
    if (Self[i].LocPos <= AChar) and
       ((Self[i].LocPos + Self[i].StrLen) > AChar) then begin
      Result := Self[i];
      Exit;
    end;
  end;
  Result := nil;
end;

{ TNestedToken }

function TNestedToken.GetLen : Integer;
begin
  Result := Length(FText);
end;

function TNestedToken.GetUnquotedText: String;
begin
  if Kind = ntkString then
    Result := Copy(FText, 2, GetLen - 2)
  else Result := FText;
end;

constructor TNestedToken.Create(const Token : String; aKind : TNestedTokenKind;
  aCuLocPos : Integer);
begin
  FKind := aKind;
  FCuLocPos := aCuLocPos;
  FText := Token;
end;

{ TDBHelper }

function TDBHelper.GetThreaded: Boolean;
begin
  Result := Assigned(FThreadPool);
end;

function TDBHelper.GetDBPath : String;
begin
  Result := FDBPath.Value;
end;

procedure TDBHelper.SetDBPath(AValue : String);
begin
  FDBPath.Value := AValue;
end;

procedure TDBHelper.SetEditorFontName(AValue : String);
begin
  if FEditorFontName = AValue then Exit;
  FEditorFontName := AValue;
  SendStateChanged(dbhsEditorFont);
end;

procedure TDBHelper.SetEditorFontSize(AValue : Integer);
begin
  if FEditorFontSize = AValue then Exit;
  FEditorFontSize := AValue;
  SendStateChanged(dbhsEditorFont);
end;

procedure TDBHelper.SendStateChanged(aState : TDBHelperState);
var i : integer;
begin
  if FCummulation.Value > 0 then
  begin
    for i := 0 to FStateListeners.Count-1 do
    begin
      TDBHelperStateChanged(FStateListeners[i])(aState);
    end;
  end else
    FCummulatedStates.Cummulate(aState);
end;

procedure TDBHelper.SetKwFormat(AValue: TSqliteKwFormatOption);
begin
  if FKwFormat=AValue then Exit;
  FKwFormat:=AValue;
  SendStateChanged(dbhsFormatSettings);
end;

function TDBHelper.GetJsonBlobKindEnum(aKind : TDBExtBlobKind) : TJsonCfgEnum;
var i : integer;
begin
  Result := nil;
  for i := 0 to JsonBlobKinds.Count-1 do
  begin
    if TJsonBlobKind(JsonBlobKinds[i]).Kind = aKind then
      Exit(JsonBlobKinds[i])
  end;
end;

function TDBHelper.GetJsonStrStyleEnum(aStyle : TDBIndxStringsStyle
  ) : TJsonCfgEnum;
var i : integer;
begin
  Result := nil;
  for i := 0 to JsonIndxStringsStyles.Count-1 do
  begin
    if TJsonIndxStringsStyle(JsonIndxStringsStyles[i]).Kind = aStyle then
      Exit(JsonIndxStringsStyles[i])
  end;
end;

function TDBHelper.GetJsonIndxClassEnum(aClass : TBaseSinExprsClass
  ) : TJsonCfgEnum;
var i : integer;
begin
  Result := nil;
  for i := 0 to JsonIndxClasses.Count-1 do
  begin
    if TJsonIndxClass(JsonIndxClasses[i]).tClass = aClass then
      Exit(JsonIndxClasses[i])
  end;
end;

function TDBHelper.GetJsonFuncClassEnum(aClass : TSqlite3FunctionClass
  ) : TJsonCfgEnum;
var i : integer;
begin
  Result := nil;
  for i := 0 to JsonFuncClasses.Count-1 do
  begin
    if TJsonFuncClass(JsonFuncClasses[i]).tClass = aClass then
      Exit(JsonFuncClasses[i])
  end;
end;

constructor TDBHelper.Create;
var i : integer;
    Attr : TSynHighlighterAttributes;
    st : TNestedTokenKind;
begin
  FStateListeners := TMethodList.Create;
  FCummulatedStates := TStateChanges.Create;
  FCummulation := TThreadInteger.Create(0);

  FDBPath := TThreadUtf8String.Create('');

  JsonIndxClasses := TJsonCfgEnums.Create;
  JsonFuncClasses := TJsonCfgEnums.Create;
  JsonBlobKinds := TJsonCfgEnums.Create;
  JsonIndxStringsStyles := TJsonCfgEnums.Create;

  JsonIndxClasses.Add(TJsonIndxClass.Create('None', nil));
  JsonIndxClasses.Add(TJsonIndxClass.Create('BaseSinExprs', TBaseSinExprs));
  JsonIndxClasses.Add(TJsonIndxClass.Create('TokenedSinExprs',TTokenedSinExprs));
  TJsonIndxClass(JsonIndxClasses[2]).AddNewAdvField(JSON_CFG_SEFIELD, '');
  JsonIndxClasses.AddNewField(JSON_CFG_UID, 0);
  JsonIndxClasses.AddNewField(JSON_CFG_TABLE, '');
  JsonIndxClasses.AddNewField(JSON_CFG_KFIELD, '');
  JsonIndxClasses.AddNewField(JSON_CFG_VFIELD, '');

  JsonFuncClasses.Add(TJsonFuncClass.Create('None', nil));
  JsonFuncClasses.Add(TJsonFuncClass.Create('ExprCrossHitContIdx', TExprCrossHitContIdxFunction));

  JsonIndxStringsStyles.Add(TJsonIndxStringsStyle.Create('None', issUnknown));
  JsonIndxStringsStyles.Add(TJsonIndxStringsStyle.Create('Strings', issStrings));
  JsonIndxStringsStyles.Add(TJsonIndxStringsStyle.Create('ConstList',issConstList));
  JsonIndxStringsStyles.AddNewField(JSON_CFG_UID, 0);

  JsonBlobKinds.Add(TJsonBlobKind.Create('None', ebkNo));
  JsonBlobKinds.Add(TJsonBlobKind.Create('Text', ebkText));
  JsonBlobKinds.Add(TJsonBlobKind.Create('FormattedText',ebkFormattedText));
  JsonBlobKinds.Add(TJsonBlobKind.Create('Image',ebkImage));
  JsonBlobKinds.AddNewField(JSON_CFG_TABLE, '');
  JsonBlobKinds.AddNewField(JSON_CFG_FIELD, '');
  JsonBlobKinds.AddNewField(JSON_CFG_PATH, cCURPATH);

  {$ifdef Linux}
  FEditorFontName := 'Monospace';
  {$else}
  {$ifdef Windows}
  FEditorFontName := 'Courier New';
  {$else}
  FEditorFontName := 'monospace';
  {$endif}
  {$endif}
  FEditorFontSize := -13;

  FSynSQL := TSynSQLite3Syn.Create(nil);
  FSynSQL.CommentAttri.Foreground := clGreen;
  FSynSQL.CommentAttri.Style := [fsItalic];
  FSynSQL.CommentAttri.InternalSaveDefaultValues;
  FSynSQL.NumberAttri.Foreground := clNavy;
  FSynSQL.NumberAttri.Style := [fsBold];
  FSynSQL.NumberAttri.InternalSaveDefaultValues;
  FSynSQL.SymbolAttri.Foreground := clRed;
  FSynSQL.SymbolAttri.Style := [];
  FSynSQL.SymbolAttri.InternalSaveDefaultValues;
  FSynSQL.TableNameAttri.Foreground := clPurple;
  FSynSQL.TableNameAttri.Style := [];
  FSynSQL.TableNameAttri.InternalSaveDefaultValues;
  FSynSQL.FieldNameAttri.Foreground := clFuchsia;
  FSynSQL.FieldNameAttri.Style := [fsItalic];
  FSynSQL.FieldNameAttri.InternalSaveDefaultValues;
  FSynSQL.StringAttri.Foreground := clGreen;
  FSynSQL.StringAttri.Style := [fsBold];
  FSynSQL.StringAttri.InternalSaveDefaultValues;
  FSynSQL.TableNames.Add('sqlite_master');
  FSynSQL.Enabled := true;

  FAttrsDefs := TSynAttrsDefs.Create;
  FAttrsDefs.OwnsObjects := true;
  for i := 0 to FSynSQL.AttrCount-1 do
  begin
    Attr := TSynHighlighterAttributes.Create(FSynSQL.Attribute[i].Name);
    Attr.Assign(FSynSQL.Attribute[i]);
    FAttrsDefs.AddAttr(Attr);
  end;

  fDefaultTokenVisStyles := TNestedTokenStyles.Create;
  fDefaultTokenVisStyles[ntkNone]      := NestedTokenVisStyle(Sqlite3Highlighter.IdentifierAttri.Foreground,
                                                             Sqlite3Highlighter.IdentifierAttri.Style);
  fDefaultTokenVisStyles[ntkSymbol]    := NestedTokenVisStyle(Sqlite3Highlighter.SymbolAttri.Foreground,
                                                             Sqlite3Highlighter.SymbolAttri.Style);
  fDefaultTokenVisStyles[ntkTableName] := NestedTokenVisStyle(Sqlite3Highlighter.TableNameAttri.Foreground,
                                                             Sqlite3Highlighter.TableNameAttri.Style);
  fDefaultTokenVisStyles[ntkFieldName] := NestedTokenVisStyle(Sqlite3Highlighter.FieldNameAttri.Foreground,
                                                             Sqlite3Highlighter.FieldNameAttri.Style);
  fDefaultTokenVisStyles[ntkNumber]    := NestedTokenVisStyle(Sqlite3Highlighter.NumberAttri.Foreground,
                                                             Sqlite3Highlighter.NumberAttri.Style);
  fDefaultTokenVisStyles[ntkString]    := NestedTokenVisStyle(Sqlite3Highlighter.StringAttri.Foreground,
                                                             Sqlite3Highlighter.StringAttri.Style);
  fTokenVisStyles := TNestedTokenStyles.Create;
  for st := Low(TNestedTokenKind) to High(TNestedTokenKind) do
  begin
    fTokenVisStyles[st] := fDefaultTokenVisStyles[st];
  end;

  FKwFormat := skfoUpperCase;
end;

procedure TDBHelper.DrawListItem(Control : TWinControl; Index : Integer;
  ARect : TRect; State : TOwnerDrawState);
var
  C : TCanvas;
  O : TNestedExpr;
begin
  if Control is TListBox then
  begin
    O := TNestedExpr(TListBox(Control).Items.Objects[Index]);
    C := TListBox(Control).Canvas;
  end else
  if Control is TComboBox then begin
    O := TNestedExpr(TComboBox(Control).Items.Objects[Index]);
    C := TComboBox(Control).Canvas;
  end else
  begin
    O := nil;
    C := nil;
  end;

  if Assigned(O) and Assigned(C) then
  begin
    if odFocused in State then
    begin
      C.Brush.Style := bsSolid;
      C.Brush.Color := RGB(250, 200, 220);
      C.pen.Style := psClear;
    end else
    if odSelected in State then
    begin
      C.Brush.Style := bsSolid;
      C.Brush.Color := RGB(230, 200, 130);
      C.pen.Color := clYellow;
      C.pen.Style := psDot;
    end else
    if (State * [odComboBoxEdit, odBackgroundPainted]) <> [] then
    begin
      C.Brush.Style := bsClear;
      C.pen.Style := psClear;
    end else
    begin
      C.Brush.Style := bsSolid;
      C.Brush.Color := clWhite;
      C.Pen.Style := psClear;
    end;
    C.Rectangle(aRect);
    C.Brush.Style := bsClear;
    O.DrawText(C, aRect);
  end;
end;

procedure TDBHelper.InitThreadPool(AThreadCnt: integer);
begin
  if assigned(FThreadPool) then FreeAndNil( FThreadPool );
  FThreadPool := TThreadPool.Create(AThreadCnt);
  FThreadPool.Running := true;
end;

function TDBHelper.VersionStr : String;
begin
  Result := IntToStr(VersionMag) + '.' +
            IntToStr(VersionMin div 1000) + '.' +
            IntToStr(VersionMin mod 1000);
end;

function TDBHelper.VersionMag : Word;
begin
  Result := 0;
end;

function TDBHelper.VersionMin : Word;
begin
  Result := 001001;
end;

function TDBHelper.VersionNum : Cardinal;
begin
  Result := VersionMag * 1000000 + VersionMin;
end;

const cBackground = 'Background';
      cForeground = 'Foreground';
      cStyle      = 'Style';
      cFontName   = 'FontName';
      cFontSize   = 'FontSize';
      cKeyWordFormat = 'KeyWordFormat';

procedure TDBHelper.LoadFromConfig(aConfig : TJSONPropStorage);
var i : integer;
    Attr : TSynHighlighterAttributes;
begin
  for i := 0 to FSynSQL.AttrCount-1 do
  begin
    Attr := FSynSQL.Attribute[i];
    Attr.Background := aConfig.ReadInteger(Attr.Name + cBackground, Attr.Background) ;
    Attr.Foreground := aConfig.ReadInteger(Attr.Name + cForeground, Attr.Foreground);
    Attr.IntegerStyle := aConfig.ReadInteger(Attr.Name + cStyle, Attr.IntegerStyle);
  end;
  BeginUpdate;
  try
    EditorFontName := aConfig.ReadString(cFontName, FEditorFontName);
    FEditorFontSize := aConfig.ReadInteger(cFontSize, FEditorFontSize);
    FKwFormat := TSqliteKwFormatOption(aConfig.ReadInteger(cKeyWordFormat, Ord(FKwFormat) - 1) + 1);
  finally
    EndUpdate;
  end;
end;

procedure TDBHelper.SaveToConfig(aConfig : TJSONPropStorage);
var i : integer;
    Attr : TSynHighlighterAttributes;
begin
  for i := 0 to FSynSQL.AttrCount-1 do
  begin
    Attr := FSynSQL.Attribute[i];
    aConfig.WriteInteger(Attr.Name + cBackground, Attr.Background) ;
    aConfig.WriteInteger(Attr.Name + cForeground, Attr.Foreground);
    aConfig.WriteInteger(Attr.Name + cStyle, Attr.IntegerStyle) ;
  end;
  aConfig.WriteString(cFontName, FEditorFontName);
  aConfig.WriteInteger(cFontSize, FEditorFontSize);
  aConfig.WriteInteger(cKeyWordFormat, Ord(FKwFormat) - 1);
end;

procedure TDBHelper.SetFontParamsFromCompletionObj(O : TCompletionObj;
  F : TFont; out imi : integer);
var
  Attr : TSynHighlighterAttributes;
begin
  attr := nil;
  if Assigned(O) then
  begin
    case O.Kind of
      sckKeyword : begin
        Attr :=  FSynSQL.KeyAttri;
        imi := IMG_CONFIG;
      end;
      sckFunction : begin
        Attr :=  FSynSQL.FunctionAttri;
        imi := IMG_STRUCT_ELEMENT;
      end;
      sckType : begin
        Attr :=  FSynSQL.DataTypeAttri;
        imi := IMG_DATA_TYPE;
      end;
      sckTable : begin
        Attr :=  FSynSQL.TableNameAttri;
        imi := IMG_TABLE;
      end;
      sckField : begin
        Attr :=  FSynSQL.FieldNameAttri;
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

procedure TDBHelper.AddStateListener(aEar : TDBHelperStateChanged);
begin
  FStateListeners.Add(TMethod(aEar));
end;

procedure TDBHelper.BeginUpdate;
begin
  FCummulation.Lock;
  FCummulation.IncValue;
end;

procedure TDBHelper.EndUpdate;
var i : integer;
begin
  FCummulation.DecValue;
  if FCummulation.Value = 0 then
  begin
    for i := FCummulatedStates.Count-1 downto 0 do
    begin
      SendStateChanged(FCummulatedStates[i]);
      FCummulatedStates.Delete(i);
    end;
  end;
  FCummulation.UnLock;
end;

destructor TDBHelper.Destroy;
begin
  if assigned(FThreadPool) then FreeAndNil( FThreadPool );

  JsonIndxClasses.Free;
  JsonFuncClasses.Free;
  JsonBlobKinds.Free;
  JsonIndxStringsStyles.Free;

  FDBPath.Free;
  FSynSQL.Free;

  FStateListeners.Free;

  FAttrsDefs.Free;
  fDefaultTokenVisStyles.Free;
  fTokenVisStyles.Free;

  FCummulatedStates.Free;
  FCummulation.Free;

  inherited Destroy;
end;

initialization
  DBHelper := TDBHelper.Create;

finalization
  DBHelper.Free;

end.

