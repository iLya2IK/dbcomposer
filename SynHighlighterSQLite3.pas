unit SynHighlighterSQLite3;


interface

uses
  SysUtils, Classes,
  LCLIntf, LCLType,
  Controls, Graphics,
  SynEditTypes, SynEditHighlighter,
  SynHighlighterHashEntries;

type
  TtkTokenKind = (tkComment, tkDatatype, tkDefaultPackage, tkException,
    tkFunction, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace,
    tkSQLPlus, tkString, tkSymbol, tkTableName, tkFieldName,
    tkUnknown, tkVariable);

  TRangeState = (rsUnknown, rsComment, rsString);

  TProcTableProc = procedure of object;

type
  PIdentifierTable = ^TIdentifierTable;
  TIdentifierTable = array[Char] of ByteBool;

  PHashTable = ^THashTable;
  THashTable = array[Char] of Integer;

type

  { TSynSQLite3Syn }

  TSynSQLite3Syn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    fTokenID: TtkTokenKind;
    fKeywords: TSynHashEntryList;
    fTableNames, fFieldNames: TStrings;
    fCommentAttri: TSynHighlighterAttributes;
    fDataTypeAttri: TSynHighlighterAttributes;
    fDefaultPackageAttri: TSynHighlighterAttributes;
    fExceptionAttri: TSynHighlighterAttributes;
    fFunctionAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fSQLPlusAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fTableNameAttri: TSynHighlighterAttributes;
    fFieldNameAttri: TSynHighlighterAttributes;
    fVariableAttri: TSynHighlighterAttributes;
    fIdentifiersPtr: PIdentifierTable;
    fmHashTablePtr: PHashTable;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: string): Boolean;
    procedure AndSymbolProc;
    procedure AsciiCharProc;
    procedure CRProc;
    procedure EqualProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure SetFieldNames(const AValue : TStrings);
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure SymbolAssignProc;
    procedure VariableProc;
    procedure UnknownProc;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
    procedure AnsiCProc;
    procedure DoAddKeyword(AKeyword: string; AKind: integer);
    procedure SetTableNames(const Value: TStrings);
    procedure TableNamesChanged(Sender: TObject);
    procedure FieldNamesChanged(Sender: TObject);
    procedure InitializeKeywordLists;
    procedure PutTableAndFieldNamesInKeywordList;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource : String; override;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetToken: string; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;

    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    function IsKeyword(const AKeyword: string): boolean; override;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetLine(const NewValue: string; LineNumber: Integer); override;
    procedure SetRange(Value: Pointer); override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property DataTypeAttri: TSynHighlighterAttributes read fDataTypeAttri
      write fDataTypeAttri;
    property DefaultPackageAttri: TSynHighlighterAttributes
      read fDefaultPackageAttri write fDefaultPackageAttri;
    property ExceptionAttri: TSynHighlighterAttributes read fExceptionAttri
      write fExceptionAttri;
    property FunctionAttri: TSynHighlighterAttributes read fFunctionAttri
      write fFunctionAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property SQLPlusAttri: TSynHighlighterAttributes read fSQLPlusAttri
      write fSQLPlusAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property TableNameAttri: TSynHighlighterAttributes read fTableNameAttri
      write fTableNameAttri;
    property FieldNameAttri: TSynHighlighterAttributes read fFieldNameAttri
      write fFieldNameAttri;
    property TableNames: TStrings read fTableNames write SetTableNames;
    property FieldNames: TStrings read fFieldNames write SetFieldNames;
    property VariableAttri: TSynHighlighterAttributes read fVariableAttri
      write fVariableAttri;
  end;

implementation

uses
  SynEditStrConst, ExtSqliteUtils;

const
SYNS_XML_AttrFieldName            =  'Field Name';

var
  Identifiers: TIdentifierTable;
  mHashTable: THashTable;

procedure MakeIdentTable;
var
  c: char;
begin
  FillChar(Identifiers, SizeOf(Identifiers), 0);
  for c := 'a' to 'z' do
    Identifiers[c] := TRUE;
  for c := 'A' to 'Z' do
    Identifiers[c] := TRUE;
  for c := '0' to '9' do
    Identifiers[c] := TRUE;
  Identifiers['_'] := TRUE;
  Identifiers['#'] := TRUE;
  Identifiers['$'] := TRUE;

  FillChar(mHashTable, SizeOf(mHashTable), 0);
  mHashTable['_'] := 1;
  for c := 'a' to 'z' do
    mHashTable[c] := 2 + Ord(c) - Ord('a');
  for c := 'A' to 'Z' do
    mHashTable[c] := 2 + Ord(c) - Ord('A');
end;

function TSynSQLite3Syn.KeyHash(ToHash: PChar): Integer;
var
  Start: PChar;
begin
  Result := 0;
  Start := ToHash;
  while fIdentifiersPtr^[ToHash^] do begin
  
    Result := (2 * Result + fmHashTablePtr^[ToHash^]) and $FFFFFF;
    inc(ToHash);
  end;
  Result := Result and $FF; // 255
  fStringLen := ToHash - Start;
end;

function TSynSQLite3Syn.KeyComp(const aKey: string): Boolean;
var
  i: integer;
  pKey1, pKey2: PChar;
begin
  pKey1 := fToIdent;
  // Note: fStringLen is always > 0 !
  pKey2 := pointer(aKey);
  for i := 1 to fStringLen do
  begin
    if mHashTable[pKey1^] <> mHashTable[pKey2^] then
    begin
      Result := FALSE;
      exit;
    end;
    Inc(pKey1);
    Inc(pKey2);
  end;
  Result := TRUE;
end;

function TSynSQLite3Syn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  Entry: TSynHashEntry;
begin
  fToIdent := MayBe;
  Entry := fKeywords[KeyHash(MayBe)];
  while Assigned(Entry) do begin
    if Entry.KeywordLen > fStringLen then
      break
    else if Entry.KeywordLen = fStringLen then
      if KeyComp(Entry.Keyword) then begin
        Result := TtkTokenKind(Entry.Kind);
        exit;
      end;
    Entry := Entry.Next;
  end;
  Result := tkIdentifier;
end;

procedure TSynSQLite3Syn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
       #0: fProcTable[I] := @NullProc;
      #10: fProcTable[I] := @LFProc;
      #13: fProcTable[I] := @CRProc;
      #39: fProcTable[I] := @AsciiCharProc;
      '=': fProcTable[I] := @EqualProc;
      '>': fProcTable[I] := @GreaterProc;
      '<': fProcTable[I] := @LowerProc;
      '-': fProcTable[I] := @MinusProc;
      '|': fProcTable[I] := @OrSymbolProc;
      '+': fProcTable[I] := @PlusProc;
      '/': fProcTable[I] := @SlashProc;
      '&': fProcTable[I] := @AndSymbolProc;
      #34: fProcTable[I] := @StringProc;
      ':', '@':
        fProcTable[I] := @VariableProc;
      'A'..'Z', 'a'..'z', '_':
        fProcTable[I] := @IdentProc;
      '0'..'9':
        fProcTable[I] := @NumberProc;
      #1..#9, #11, #12, #14..#32:
        fProcTable[I] := @SpaceProc;
      '^', '%', '*', '!':
        fProcTable[I] := @SymbolAssignProc;
      '{', '}', '.', ',', ';', '?', '(', ')', '[', ']', '~':
        fProcTable[I] := @SymbolProc;
      else
        fProcTable[I] := @UnknownProc;
    end;
end;

constructor TSynSQLite3Syn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fKeywords := TSynHashEntryList.Create;
  fTableNames := TStringList.Create;
  TStringList(fTableNames).OnChange := @TableNamesChanged;
  fFieldNames := TStringList.Create;
  TStringList(fFieldNames).OnChange := @FieldNamesChanged;
  fCommentAttri := TSynHighlighterAttributes.Create(@SYNS_AttrComment, SYNS_XML_AttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fDataTypeAttri := TSynHighlighterAttributes.Create(@SYNS_AttrDataType, SYNS_XML_AttrDataType);
  fDataTypeAttri.Style := [fsBold];
  AddAttribute(fDataTypeAttri);
  fDefaultPackageAttri :=
    TSynHighlighterAttributes.Create(@SYNS_AttrDefaultPackage, SYNS_XML_AttrDefaultPackage);
  fDefaultPackageAttri.Style := [fsBold];
  AddAttribute(fDefaultPackageAttri);
  fExceptionAttri := TSynHighlighterAttributes.Create(@SYNS_AttrException, SYNS_XML_AttrException);
  fExceptionAttri.Style := [fsItalic];
  AddAttribute(fExceptionAttri);
  fFunctionAttri := TSynHighlighterAttributes.Create(@SYNS_AttrFunction, SYNS_XML_AttrFunction);
  fFunctionAttri.Style := [fsBold];
  AddAttribute(fFunctionAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(@SYNS_AttrIdentifier, SYNS_XML_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(@SYNS_AttrReservedWord, SYNS_XML_AttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(@SYNS_AttrNumber, SYNS_XML_AttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(@SYNS_AttrSpace, SYNS_XML_AttrSpace);
  AddAttribute(fSpaceAttri);
  fSQLPlusAttri:=TSynHighlighterAttributes.Create(@SYNS_AttrSQLPlus, SYNS_XML_AttrSQLPlus);
  fSQLPlusAttri.Style := [fsBold];
  AddAttribute(fSQLPlusAttri);
  fStringAttri := TSynHighlighterAttributes.Create(@SYNS_Attrstring, SYNS_XML_Attrstring);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(@SYNS_AttrSymbol, SYNS_XML_AttrSymbol);
  AddAttribute(fSymbolAttri);
  fTableNameAttri := TSynHighlighterAttributes.Create(@SYNS_AttrTableName, SYNS_XML_AttrTableName);
  AddAttribute(fTableNameAttri);
  fFieldNameAttri := TSynHighlighterAttributes.Create(@SYNS_AttrTableName, SYNS_XML_AttrFieldName);
  AddAttribute(fFieldNameAttri);
  fVariableAttri := TSynHighlighterAttributes.Create(@SYNS_AttrVariable, SYNS_XML_AttrVariable);
  AddAttribute(fVariableAttri);
  SetAttributesOnChange(@DefHighlightChange);
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterSQL;
  fRange := rsUnknown;
end;

destructor TSynSQLite3Syn.Destroy;
begin
  fKeywords.Free;
  fTableNames.Free;
  fFieldNames.Free;
  inherited Destroy;
end;

procedure TSynSQLite3Syn.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

procedure TSynSQLite3Syn.SetLine(const NewValue: string; LineNumber: Integer);
begin
  inherited;
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynSQLite3Syn.AndSymbolProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] in ['=', '&'] then Inc(Run);
end;

procedure TSynSQLite3Syn.AsciiCharProc;
begin
  if fLine[Run] = #0 then
    NullProc
  else begin
    fTokenID := tkString;        
    if (Run > 0) or (fRange <> rsString) or (fLine[Run] <> #39) then begin
        fRange := rsString;
        repeat
          Inc(Run);
        until fLine[Run] in [#0, #10, #13, #39];
    end;
    if fLine[Run] = #39 then begin
        Inc(Run);
        fRange := rsUnknown;
    end;
  end;
end;

procedure TSynSQLite3Syn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then Inc(Run);
end;

procedure TSynSQLite3Syn.EqualProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] in ['=', '>'] then Inc(Run);
end;

procedure TSynSQLite3Syn.GreaterProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] in ['=', '>'] then Inc(Run);
end;

procedure TSynSQLite3Syn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  if fTokenID = tkComment then begin
    while not (fLine[Run] in [#0, #10, #13]) do
      Inc(Run);
  end else
    while fIdentifiersPtr^[fLine[Run]] do inc(Run);
end;

procedure TSynSQLite3Syn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynSQLite3Syn.LowerProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  case fLine[Run] of
    '=': Inc(Run);
    '<': begin
           Inc(Run);
           if fLine[Run] = '=' then Inc(Run);
         end;
  end;
end;

procedure TSynSQLite3Syn.MinusProc;
begin
  Inc(Run);
  if fLine[Run] = '-' then begin
    fTokenID := tkComment;
    repeat
      Inc(Run);
    until fLine[Run] in [#0, #10, #13];
  end else
    fTokenID := tkSymbol;
end;

procedure TSynSQLite3Syn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynSQLite3Syn.NumberProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9'] do inc(Run);
  if (FLine[Run]='.') and not(fLine[Run+1]='.')  then begin
    inc(Run);
    while FLine[Run] in ['0'..'9'] do inc(Run);
  end;
  if (FLine[Run]='e') or (fLine[Run]='E')  then begin
    inc(Run);
    if (FLine[Run]='+') or (fLine[Run]='-')  then inc(Run);
    while FLine[Run] in ['0'..'9'] do inc(Run);
  end;
end;

procedure TSynSQLite3Syn.OrSymbolProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] in ['=', '|'] then Inc(Run);
end;

procedure TSynSQLite3Syn.PlusProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] in ['=', '+'] then Inc(Run);
end;

procedure TSynSQLite3Syn.SetFieldNames(const AValue : TStrings);
begin
  fFieldNames.Assign(AValue);
end;

procedure TSynSQLite3Syn.SlashProc;
begin
  Inc(Run);
  case fLine[Run] of
    '*':
      begin
        fRange := rsComment;
        fTokenID := tkComment;
        repeat
          Inc(Run);
          if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then begin
            fRange := rsUnknown;
            Inc(Run, 2);
            break;
          end;
        until fLine[Run] in [#0, #10, #13];
      end;
    '=':
      begin
        Inc(Run);
        fTokenID := tkSymbol;
      end;
    else
      fTokenID := tkSymbol;
  end;
end;

procedure TSynSQLite3Syn.SpaceProc;
begin
  fTokenID := tkSpace;
  repeat
    Inc(Run);
  until (fLine[Run] > #32) or (fLine[Run] in [#0, #10, #13]);
end;

procedure TSynSQLite3Syn.StringProc;
begin
  fTokenID := tkString;
  Inc(Run);
  while not (fLine[Run] in [#0, #10, #13]) do begin
    case fLine[Run] of
      '\': if fLine[Run + 1] = #34 then
             Inc(Run);
      #34: if fLine[Run + 1] <> #34 then
           begin
             Inc(Run);
             break;
           end else
             Inc(Run);
    end;
    Inc(Run);
  end;
end;

procedure TSynSQLite3Syn.SymbolProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynSQLite3Syn.SymbolAssignProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] = '=' then Inc(Run);
end;

procedure TSynSQLite3Syn.VariableProc;
var
  i: integer;
begin
  if (fLine[Run] = '@') then
    SymbolProc
  else
  if (fLine[Run] = ':') then
    SymbolProc
  else begin
    fTokenID := tkVariable;
    i := Run;
    repeat
      Inc(i);
    until not (fIdentifiersPtr^[fLine[i]]);
    Run := i;
  end;
end;

procedure TSynSQLite3Syn.UnknownProc;
begin
  inc(Run);
  while (fLine[Run] in [#128..#191]) OR // continued utf8 subcode
   ((fLine[Run]<>#0) and (fProcTable[fLine[Run]] = @UnknownProc)) do inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynSQLite3Syn.AnsiCProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    else begin
        repeat
          if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then begin
            fRange := rsUnknown;
            Inc(Run, 2);
            break;
          end;
          Inc(Run);
        until fLine[Run] in [#0, #10, #13];
      end;
  end;
end;

function TSynSQLite3Syn.IsKeyword(const AKeyword: string): boolean;
var
  tk: TtkTokenKind;
begin
  tk := IdentKind(PChar(AKeyword));
  Result := tk in [tkDatatype, tkException, tkFunction, tkKey,tkDefaultPackage];
end;

procedure TSynSQLite3Syn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsComment:
      AnsiCProc;
    rsString:
      AsciiCharProc;
  else
    fProcTable[fLine[Run]]();
  end;
end;

function TSynSQLite3Syn.GetDefaultAttribute(Index: integer):
  TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
    SYN_ATTR_NUMBER: Result := fNumberAttri;
    SYN_ATTR_VARIABLE: Result := fVariableAttri;
  else
    Result := nil;
  end;
end;

function TSynSQLite3Syn.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynSQLite3Syn.GetRange: Pointer;
begin
  Result := Pointer(PtrInt(fRange));
end;

function TSynSQLite3Syn.GetToken: string;
var
  Len: LongInt;
begin
  Result := '';
  Len := Run - fTokenPos;
  Setstring(Result, (FLine + fTokenPos), Len);
end;

procedure TSynSQLite3Syn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength:=Run-fTokenPos;
  TokenStart:=FLine + fTokenPos;
end;

function TSynSQLite3Syn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynSQLite3Syn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkDatatype: Result := fDataTypeAttri;
    tkDefaultPackage: Result := fDefaultPackageAttri;
    tkException: Result := fExceptionAttri;
    tkFunction: Result := fFunctionAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkSQLPlus: Result := fSQLPlusAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkTableName: Result := fTableNameAttri;
    tkFieldName: Result := fFieldNameAttri;
    tkVariable: Result := fVariableAttri;
    tkUnknown: Result := fIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynSQLite3Syn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynSQLite3Syn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TSynSQLite3Syn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynSQLite3Syn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(PtrUInt(Value));
end;

function TSynSQLite3Syn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

class function TSynSQLite3Syn.GetLanguageName: string;
begin
  Result := SYNS_LangSQL;
end;

procedure TSynSQLite3Syn.DoAddKeyword(AKeyword: string; AKind: integer);
var
  HashValue: integer;
begin
  HashValue := KeyHash(PChar(AKeyword));
  fKeywords[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
end;

procedure TSynSQLite3Syn.SetTableNames(const Value: TStrings);
begin
  fTableNames.Assign(Value);
end;

procedure TSynSQLite3Syn.TableNamesChanged(Sender: TObject);
begin
  InitializeKeywordLists;
end;

procedure TSynSQLite3Syn.FieldNamesChanged(Sender : TObject);
begin
  InitializeKeywordLists;
end;

procedure TSynSQLite3Syn.PutTableAndFieldNamesInKeywordList;
var
  i: Integer;
  Entry: TSynHashEntry;
begin
  for i := 0 to (fTableNames.Count - 1) do
  begin
    Entry := fKeywords[KeyHash(PChar(fTableNames[i]))];
    while Assigned(Entry) do
    begin
      if (UpperCase(Entry.Keyword) = Uppercase(fTableNames[i])) then
        Break;
      Entry := Entry.Next;
    end;
    if not Assigned(Entry) then
      DoAddKeyword(fTableNames[i], Ord(tkTableName));
  end;
  for i := 0 to (fFieldNames.Count - 1) do
  begin
    Entry := fKeywords[KeyHash(PChar(fFieldNames[i]))];
    while Assigned(Entry) do
    begin
      if (UpperCase(Entry.Keyword) = Uppercase(fFieldNames[i])) then
        Break;
      Entry := Entry.Next;
    end;
    if not Assigned(Entry) then
      DoAddKeyword(fFieldNames[i], Ord(tkFieldName));
  end;
end;

procedure TSynSQLite3Syn.InitializeKeywordLists;
begin
  fKeywords.Clear;

  fIdentifiersPtr := @Identifiers;
  fmHashTablePtr := @mHashTable;
  
  EnumerateKeywords(Ord(tkDatatype), sqluGetDataTypesList, IdentChars,
        @DoAddKeyword);
  EnumerateKeywords(Ord(tkFunction), sqluGetFunctionsList, IdentChars,
        @DoAddKeyword);
  EnumerateKeywords(Ord(tkKey), sqluGetKeyWordsList, IdentChars, @DoAddKeyword);
  
  PutTableAndFieldNamesInKeywordList;
  DefHighlightChange(Self);
end;


function TSynSQLite3Syn.GetSampleSource: String;
begin
  Result:= '';
end;

initialization
  MakeIdentTable;
  RegisterPlaceableHighlighter(TSynSQLite3Syn);

end. 
