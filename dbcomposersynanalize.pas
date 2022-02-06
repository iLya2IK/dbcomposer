{
 dbComposerSynAnalize:
   Classes for parsing sqlite3 expressions.
   TDBSqliteSynAnalizer can create TDBTable from sqlite3 expression.

   Part of dbComposer project

   Copyright (c) 2021-2022 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit dbComposerSynAnalize;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils,
  OGLFastNumList,
  ExtSqliteTokens,
  ExtSqliteSynCheck,
  dbComposerStruct;

type

  TSqliteAnalizeCreateTableOption = (ctoAddToStruct, ctoCheckForeignKeys,
                                     ctoCheckExists);

  TSqliteAnalizeCreateTableOptions = set of TSqliteAnalizeCreateTableOption;

  { TDBSqliteSynAnalizer }

  TDBSqliteSynAnalizer = class(TSqliteSynAnalizer)
  public
    function CreateTable(aStructure: TDBStructure;
                         aOptions : TSqliteAnalizeCreateTableOptions): TDBTable;
    class function CheckIsCreateTable(aExpr : TSqliteExpr) : Boolean;
    class function CheckIsBegin(aExpr : TSqliteExpr) : Boolean;
  end;


implementation

uses
  LazUTF8, ExtSqliteUtils;

function TDBSqliteSynAnalizer.CreateTable(aStructure : TDBStructure;
  aOptions : TSqliteAnalizeCreateTableOptions) : TDBTable;
var
  T, lT : TSqliteToken;
  i, i0, li : integer;

  State, State0, lState : Byte;
  States : TFastByteList;
  curField : TDBField;
  FN, FT, FD, cExpr, lcExpr, iExpr, cName, S : String;
  aConstr : TDBConstraint;
  aConstrs : TDBConstraints;
  IsPK, flag, IsTmp, ChkExists,
    TableConstrMode, IndexedColIsSingle, DTComplete : Boolean;

procedure SaveCursor;
begin
  lT := T;
  li := i;
  lState := State;
  lcExpr := cExpr;
end;

procedure RestoreCursor;
begin
  T := lT;
  i := li;
  cExpr := lcExpr;
  State := lState;
end;

procedure GoNextNonspace;
var ind : integer;
begin
  for ind := i+1 to FSynExpr.count-1 do
  begin
    if T.Kind in [stkIdentifier, stkKeyWord, stkDataType] then
    begin
      if Length(cExpr) > 0 then
        cExpr := cExpr + ' ';
      cExpr := cExpr + T.QuotedToken;
    end;
    Inc(i);
    T := FSynExpr[i];
    if not (T.Kind in [stkSpace, stkComment]) then begin
      Exit;
    end;
  end;
end;

procedure ChangeStateGoNextNonspace(nst : Byte);
begin
  States[States.Count-1] := nst;
  GoNextNonspace;
end;

procedure ChangeState(nst : Byte);
begin
  States[States.Count-1] := nst;
end;

procedure PushState(nst : Byte);
begin
  States.Add(nst);
end;

procedure PopState;
begin
  States.Delete(States.Count-1);
  State := States[States.Count-1];
end;

function CheckKeyword(kw : Cardinal) : Boolean;
begin
  Result := (T.Kind = stkKeyWord) and
            (T.KeyWordIndx = kw);
end;

function CheckId(len : Cardinal) : Boolean;
begin
  Result := (T.Kind = stkIdentifier) and
            (T.IdCnt <= len);
end;

function CheckBracketOpen : Boolean;
begin
  Result := (T.Kind = stkSymbol) and
            (T.SymbolOrd = STOK_BRACKET_OPEN);
end;

function GoToClosedBracket : String;
var ind, brc : integer;
begin
  Result := '';
  brc := 1;
  try
    for ind := i+1 to FSynExpr.count-1 do
    begin
      Inc(i);
      T := FSynExpr[i];
      if (T.Kind = stkSymbol) then
      begin
        case T.Tag[0] of
        STOK_BRACKET_OPEN  : Inc(brc);
        STOK_BRACKET_CLOSE :
          begin
            Dec(brc);
            if brc = 0 then  Exit;
          end;
        end;
      end;

      case T.Kind of
        stkSpace :
         if Length(Result) > 0 then
           Result := Result + ' ';
        stkComment : ;
      else begin
          Result := Result + T.QuotedToken;
        end;
      end;
    end;
  finally
    Result := UTF8Trim(Result);
  end;
end;

procedure NewConstraint(aCKind : TSqliteConstrKind);
begin
  if assigned(aConstr) then aConstr.Free;
  aConstr := TDBConstraint.Create(aCKind, cName);
end;

procedure AddOption(kw : Cardinal);
begin
  aConstr.AddOption(sqluGetIndexedKeyWord(kw));
end;

function CheckTableColumns(aTable : TDBTable; const S : String) : Boolean;
var
  SL : TStringList;
  i : integer;
begin
  SL := TStringList.Create;
  try
    SL.Delimiter := Chr(STOK_PERIOD);
    SL.DelimitedText := S;
    for i := 0 to SL.Count-1 do
    begin
      if not Assigned(aTable.ByName(sqluUnquotedIdIfNeeded(UTF8Trim(SL[0])))) then
        Exit(false);
    end;
    Result := true;
  finally
    SL.Free;
  end;
end;

var aForTable : TDBTable;
    aForField : String;
begin
  aForField := '';
  if not Assigned(FSynExpr) then
  begin
    FErrorCode := SAER_SYNEXPR_NOT_ASSIGNED;
    Exit(nil);
  end;
  if FSynExpr.Count = 0 then
  begin
    FErrorCode := SAER_SYNEXPR_EMPTY;
    Exit(nil);
  end;

  Result := nil;
  curField := nil;
  FErrorToken := nil;
  FErrorCode := SAER_UNEXPECTED;
  try
    aConstrs := TDBConstraints.Create;
    aConstr := nil;
    States := TFastByteList.Create;
    try
      i := 0;
      State := 0;
      lState := 0; li := 0; lT := nil; lcExpr := '';
      States.Add(State);
      IsTmp := false;
      ChkExists := false;
      TableConstrMode := false;
      DTComplete := false;
      cExpr := '';
      while i < FSynExpr.Count do
      begin
        i0 := i;
        State := States[States.Count-1];
        State0 := State;
        T := FSynExpr[i];
        case State of
          0 : if CheckKeyword(kwCREATE) then
                ChangeStateGoNextNonspace(1);
          1 : if CheckKeyword(kwTEMP) or CheckKeyword(kwTEMPORARY) then
              begin
                IsTmp := True;
                ChangeStateGoNextNonspace(2);
              end else
                ChangeState(2);
          2 : if CheckKeyword(kwTABLE) then
                ChangeStateGoNextNonspace(3);
          3 : if CheckKeyword(kwIF) then
                ChangeStateGoNextNonspace(4) else
                ChangeState(6);
          4 : if CheckKeyword(kwNOT) then
                ChangeStateGoNextNonspace(5);
          5 : if CheckKeyword(kwEXISTS) then begin
                ChkExists := true;
                ChangeStateGoNextNonspace(6);
              end;
          6 : if CheckID(2) then //schema-name.table-name or table-name
              begin
                if Assigned(aStructure) and
                        ((not ChkExists) or (ctoCheckExists in aOptions)) then
                begin
                  if T.Tag[1] = 2 then
                  begin
                    if Assigned(aStructure.BySchemaAndName(T.SubTokenStr[0],
                                                           T.SubTokenStr[1])) then
                    begin
                      FErrorCode := SAER_ALREADY_EXISTS;
                      Exit;
                    end;
                  end else
                  if Assigned(aStructure.ByName(T.Token)) then
                  begin
                    FErrorCode := SAER_ALREADY_EXISTS;
                    Exit;
                  end;
                end;
                if T.Tag[1] = 2 then
                begin
                  Result := TDBTable.Create(aStructure, T.SubTokenStr[1]);
                  Result.SetSchema(T.SubTokenStr[0]);
                end else
                  Result := TDBTable.Create(aStructure, T.Token);
                Result.IsTemp := IsTmp;
                Result.CheckExists := ChkExists;
                ChangeStateGoNextNonspace(7);
              end;
          7 : begin
                if CheckKeyword(kwAS) then
                begin
                  // create-as-select is not allowed
                  FErrorCode := SAER_NOT_SUPPORTED;
                  Exit;
                end;
                if CheckBracketOpen then
                begin
                  ChangeState(8);
                  PushState(10);
                  GoNextNonspace;
                end;
              end;
          8 : if CheckKeyword(kwWITHOUT) then
              begin
                ChangeStateGoNextNonspace(9);
              end else begin
                ChangeState(255);
                Break; //stop
              end;
          9 : if CheckKeyword(kwROWID) then
              begin
                ChangeState(255);
                Result.WithoutRowID := true;
                Break; //stop
              end;
          10 : if CheckId(1) then
               begin
                 //column-def
                 if Assigned(Result) and
                    Assigned(Result.ByName(T.Token)) then
                 begin
                   FErrorCode := SAER_ALREADY_EXISTS;
                   Exit;
                 end;
                 FN := T.Token;
                 TableConstrMode := false;
                 DTComplete := false;
                 //clear any field state
                 FD := ''; FT := ''; aForTable := nil;
                 IsPK := false;
                 aConstrs.Clear;
                 ChangeStateGoNextNonspace(11);
               end else
               if (Result.Count > 0) and (T.Kind = stkKeyWord) then
               begin //table-constrs wo fields -> empty table is not allowed
                 TableConstrMode := true;
                 IndexedColIsSingle := false;
                 // set table-constraint mode
                 ChangeState(13);
               end;
          11 : if (T.Kind = stkSymbol)  then
               begin
                 case T.Tag[0] of
                   STOK_PERIOD:
                    begin
                      ChangeStateGoNextNonspace(10);
                      flag := true;
                    end;
                   STOK_BRACKET_CLOSE:
                    begin
                      GoNextNonspace;
                      PopState;
                      flag := true;
                    end;
                   else
                     flag := false;
                 end;
                 if flag then
                 begin
                   curField := TDBField.Create(Result, FN, FT, FD, IsPK);
                   curField.Constraints.Assign(aConstrs);
                   Result.Add(curField);
                   if Assigned(aForTable) then
                     curField.UpdateForeignKeyInfo(aForTable, aForField);
                   aConstrs.Clear;
                   if Assigned(aConstr) then FreeAndNil(aConstr);

                   if IsPK and not SameText(FT, sqluAffinityToStr(dtaInteger)) then
                   begin
                     FErrorCode := SAER_PK_ONLY_INTEGER;
                     Exit;
                   end;
                 end;
               end else
               if T.Kind = stkKeyWord then
                 ChangeState(20)
               else
               if not DTComplete then
                 ChangeState(12);
          12 : if (T.Kind = stkDataType) or
                   (CheckId(1) and (T.Tag[0] = 0)) then
               begin
                 if Length(FT) > 0 then FT := FT + ' ';
                 FT := FT + UTF8UpperCase(T.Token);
                 ChangeStateGoNextNonspace(12);
               end else
               if CheckBracketOpen then
               begin
                 if Length(FT) > 0 then FT := FT + ' (';
                 FT := FT + UTF8UpperCase(GoToClosedBracket) + ')';
                 ChangeStateGoNextNonspace(20);
               end else
                 ChangeState(20);
          13 : if CheckKeyword(kwCONSTRAINT) then
                 ChangeStateGoNextNonspace(21) else
               begin
                 cName := '';
                 ChangeState(14);
               end;
          14 : if CheckKeyword(kwPRIMARY) then
               begin
                 NewConstraint(dbckPrimaryKey);
                 ChangeStateGoNextNonspace(15);
               end else
               if CheckKeyword(kwUNIQUE) then begin
                 NewConstraint(dbckUnique);
                 ChangeStateGoNextNonspace(16);
               end else
               if CheckKeyword(kwCHECK) then begin
                 NewConstraint(dbckCheck);
                 ChangeStateGoNextNonspace(17);
               end else
               if CheckKeyword(kwFOREIGN) then begin
                 NewConstraint(dbckForeignKey);
                 ChangeStateGoNextNonspace(18);
               end;
          15 : if CheckKeyword(kwKEY) then
                 ChangeStateGoNextNonspace(16);
          16 : if CheckBracketOpen then
               begin
                 iExpr := '';
                 IndexedColIsSingle := true;
                 ChangeStateGoNextNonspace(61);
               end;
          61 : if CheckId(1) then
               begin
                 curField := Result.ByName(T.Token);
                 iExpr := T.QuotedToken;
                 if Assigned(curField) then
                   ChangeStateGoNextNonspace(62)
                 else
                   ChangeState(63);
               end else
                 ChangeState(60);
          62 : if CheckKeyword(kwCOLLATE) then
                 ChangeStateGoNextNonspace(64)
               else
                 ChangeState(65);
          63 : begin
                 IndexedColIsSingle := false;
                 iExpr := iExpr + ', ' + GoToClosedBracket;
                 ChangeState(60);
               end;
          64 : if CheckId(1) then
                 aConstr.AddOption('COLLATE ' + T.QuotedToken);
          65 : if CheckKeyword(kwASC) or
                  CheckKeyword(kwDESC) then
               begin
                 aConstr.AddOption( T.Token );
                 ChangeStateGoNextNonspace(60);
               end else
                 ChangeState(60);
          60 :
            if (T.Kind = stkSymbol) then
            begin
              case T.Tag[0] of
                STOK_PERIOD:  ChangeState(63);
                STOK_BRACKET_CLOSE :
                  begin
                    aConstr.AddValue(iExpr, dbvkIndexedColumns);
                    ChangeStateGoNextNonspace(50);
                    PushState(25); // goto conflict-clause
                  end;
              end;
            end;
          17 : if CheckBracketOpen then
               begin
                 aConstr.AddValue(GoToClosedBracket, dbvkExpression);
                 ChangeStateGoNextNonspace(50);
               end;
          18 : if CheckKeyword(kwKEY) then
                 ChangeStateGoNextNonspace(19);
          19 : if CheckBracketOpen then
               begin
                  iExpr := GoToClosedBracket;
                  curField := Result.ByName(iExpr);
                  IndexedColIsSingle :=  Assigned(curField);
                  if not IndexedColIsSingle then
                  begin
                    if not CheckTableColumns(Result, iExpr) then
                    begin
                      FErrorPar := iExpr;
                      FErrorCode := SAER_NO_SUCH_COLUMN;
                      Exit;
                    end;
                    aConstr.AddValue(iExpr, dbvkExpression);
                  end;
                  ChangeStateGoNextNonspace(51);
               end;
          51 : if CheckKeyword(kwREFERENCES) then
               begin
                 aForTable := nil;
                 ChangeStateGoNextNonspace(50);
                 PushState(29);
               end;
          50: if (T.Kind = stkSymbol)  then
                begin
                  case T.Tag[0] of
                    STOK_PERIOD :
                    begin
                      ChangeStateGoNextNonspace(13);
                      flag := true;
                    end;
                    STOK_BRACKET_CLOSE :
                    begin
                      GoNextNonspace;
                      PopState;
                      flag := true;
                    end;
                    else
                      flag := false;
                  end;
                  if flag then
                  begin
                    if Assigned(aConstr) then
                    begin
                      if IndexedColIsSingle then
                      begin
                        if Assigned(curField) then
                        begin
                          curField.Constraints.Add(aConstr);
                          aConstr := nil;
                          if Assigned(aForTable) then
                            curField.UpdateForeignKeyInfo(aForTable, aForField);
                        end;
                      end
                      else
                      begin
                        Result.Constraints.Add(aConstr);
                        aConstr := nil;
                      end;
                    end;
                  end;
                end;
          20 : begin
                 DTComplete := true;
                 if CheckKeyword(kwCONSTRAINT) then // column-constraint
                   ChangeStateGoNextNonspace(21) else
                 begin
                   cName := '';
                   ChangeState(22);
                 end;
               end;
          21 : if CheckId(1) then
               begin
                 cName := T.Token;
                 if TableConstrMode then
                   ChangeStateGoNextNonspace(14)
                 else
                   ChangeStateGoNextNonspace(22);
               end;
          22 : if CheckKeyword(kwPRIMARY) then
               begin
                 NewConstraint(dbckPrimaryKey);
                 ChangeStateGoNextNonspace(23);
               end else
               if CheckKeyword(kwNOT) then
               begin
                 NewConstraint(dbckNotNull);
                 ChangeStateGoNextNonspace(24);
               end else
               {if CheckKeyword(kwNOTNULL) then
               begin
                 NewConstraint(dbckNotNull);
                 ChangeStateGoNextNonspace(33);
                 PushState(25);
               end else}
               if CheckKeyword(kwUNIQUE) then begin
                 NewConstraint(dbckUnique);
                 ChangeStateGoNextNonspace(33);
                 PushState(25);
               end else
               if CheckKeyword(kwCHECK) then begin
                 NewConstraint(dbckCheck);
                 ChangeStateGoNextNonspace(26);
               end else
               if CheckKeyword(kwDEFAULT) then begin
                 NewConstraint(dbckDefault);
                 ChangeStateGoNextNonspace(27);
               end else
               if CheckKeyword(kwCOLLATE) then begin
                 NewConstraint(dbckCollate);
                 ChangeStateGoNextNonspace(28);
               end else
               if CheckKeyword(kwREFERENCES) then begin
                 NewConstraint(dbckForeignKey);
                 ChangeStateGoNextNonspace(49);
                 PushState(29);
               end else
               if CheckKeyword(kwGENERATED) then begin
                 NewConstraint(dbckGenerated);
                 ChangeStateGoNextNonspace(30);
               end else
               if CheckKeyword(kwAS) then begin
                 NewConstraint(dbckGenerated);
                 ChangeStateGoNextNonspace(31);
               end else
               if T.Kind <> stkKeyWord then
                 ChangeState(11);
          23 : if CheckKeyword(kwKEY) then
                 ChangeStateGoNextNonspace(32);
          24 : if CheckKeyword(kwNULL) then begin
                 ChangeStateGoNextNonspace(33);
                 PushState(25);
               end;
          25 : if CheckKeyWord(kwON) then  //conflict-clause
               begin
                 cExpr := '';
                 ChangeStateGoNextNonspace(34);
               end else
                 PopState;
          26 : if CheckBracketOpen then
               begin
                 aConstr.AddValue(GoToClosedBracket, dbvkExpression);
                 ChangeStateGoNextNonspace(33);
               end;
          27 : if CheckBracketOpen then
               begin
                 FD := GoToClosedBracket;
                 aConstr.AddValue(FD, dbvkDefaultValue);
                 ChangeStateGoNextNonspace(33);
               end else
               if CheckKeyword(kwNULL) or
                  CheckKeyword(kwTRUE) or
                  CheckKeyword(kwFALSE) or
                  CheckKeyword(kwCURRENT_TIME) or
                  CheckKeyword(kwCURRENT_DATE) or
                  CheckKeyword(kwCURRENT_TIMESTAMP) then
               begin
                 FD := T.Token;
                 aConstr.AddValue(T.QuotedToken, dbvkDefaultValue);
                 ChangeStateGoNextNonspace(33);
               end else
               if (T.Kind in [stkString, stkNumber]) or
                  (CheckId(1) and (T.Tag[0] = 1)) then
               begin
                 FD := T.Token;
                 aConstr.AddValue(T.QuotedToken, dbvkDefaultValue);
                 ChangeStateGoNextNonspace(33);
               end;
          28 : if CheckId(1) then begin
                 aConstr.AddValue(T.QuotedToken, dbvkCollationName);
                 ChangeStateGoNextNonspace(33);
               end;
          29 : if CheckId(1) then begin
                 aConstr.AddValue(T.QuotedToken, dbvkTable); // Table name
                 if (Assigned(aStructure)) and
                    (ctoCheckForeignKeys in aOptions) then
                 begin
                   aForTable := aStructure.ByName(T.Token);
                   if not Assigned(aForTable) then
                   begin
                     FErrorToken := T;
                     FErrorCode := SAER_NO_SUCH_TABLE;
                     Exit;
                   end;
                 end;
                 ChangeStateGoNextNonspace(35);
               end;
          30 : if CheckKeyWord(kwALWAYS) then
                 ChangeStateGoNextNonspace(36);
          31 : if CheckBracketOpen then
               begin
                 aConstr.AddValue(GoToClosedBracket, dbvkExpression);
                 ChangeStateGoNextNonspace(37);
               end;
          32 : begin
                 if CheckKeyWord(kwASC) then
                 begin
                   // set asc
                   AddOption(kwASC);
                   ChangeStateGoNextNonspace(38);
                 end else
                 if CheckKeyWord(kwDESC) then
                 begin
                   // set desc
                   AddOption(kwDESC);
                   ChangeStateGoNextNonspace(38);
                 end else
                   ChangeState(38);
                 PushState(25);
               end;
          33 : begin
                 aConstrs.Add(aConstr);
                 aConstr := nil;
                 if (T.Kind = stkSymbol)  then
                   ChangeState(11)
                 else
                   ChangeState(20);
               end;
          34 : if CheckKeyword(kwCONFLICT) then
                 ChangeStateGoNextNonspace(39);
          35 : begin
                 cExpr := '';
                 if CheckBracketOpen then
                 begin
                   S := GoToClosedBracket;
                   aConstr.AddValue(S, dbvkColumns);
                   if Assigned(aStructure) and
                      (ctoCheckForeignKeys in aOptions) and
                      Assigned(aForTable) then
                   begin
                     if not CheckTableColumns(aForTable, S) then
                     begin
                       FErrorPar := S;
                       FErrorCode := SAER_NO_SUCH_COLUMN;
                       Exit;
                     end;
                   end;
                   aForField := S;
                   ChangeStateGoNextNonspace(40);
                 end else
                   ChangeState(40);
               end;
          36 : if CheckKeyword(kwAS) then
                 ChangeStateGoNextNonspace(31);
          37 : if CheckKeyWord(kwSTORED) then
               begin
                 // set STORED
                 AddOption(kwSTORED);
                 ChangeStateGoNextNonspace(33);
               end else
               if CheckKeyWord(kwVIRTUAL) then
               begin
                 // set VIRTUAL
                 AddOption(kwVIRTUAL);
                 ChangeStateGoNextNonspace(33);
               end else
                 ChangeState(33);
          38 : begin
                 IsPK := true;
                 if CheckKeyWord(kwAUTOINCREMENT) then
                 begin
                   // set AUTOINCREMENT
                   AddOption(kwAUTOINCREMENT);
                   ChangeStateGoNextNonspace(33);
                 end else
                   ChangeState(33);
               end;
          39 : if CheckKeyWord(kwROLLBACK) then
               begin
                 // set ROLLBACK
                 GoNextNonspace;
                 aConstr.AddOption(cExpr);
                 PopState;
               end else
               if CheckKeyWord(kwABORT) then
               begin
                 // set ABORT
                 GoNextNonspace;
                 aConstr.AddOption(cExpr);
                 PopState;
               end else
               if CheckKeyWord(kwFAIL) then
               begin
                 // set FAIL
                 GoNextNonspace;
                 aConstr.AddOption(cExpr);
                 PopState;
               end else
               if CheckKeyWord(kwIGNORE) then
               begin
                 // set IGNORE
                 GoNextNonspace;
                 aConstr.AddOption(cExpr);
                 PopState;
               end else
               if CheckKeyWord(kwREPLACE) then
               begin
                 // set REPLACE
                 GoNextNonspace;
                 aConstr.AddOption(cExpr);
                 PopState;
               end;
          40 : begin
                 if (Length(cExpr) > 0) then
                 begin
                   aConstr.AddOption(cExpr);
                   cExpr := '';
                 end;
                 if CheckKeyWord(kwON) then
                   ChangeStateGoNextNonspace(41) else
                 if CheckKeyWord(kwMATCH) then
                   ChangeStateGoNextNonspace(42) else
                 if CheckKeyWord(kwNOT) then begin
                   // may be next constrain
                   SaveCursor;
                   GoNextNonspace;
                   if CheckKeyword(kwNULL) then
                   begin
                     RestoreCursor;
                     PopState;
                   end else
                     ChangeState(43)
                 end else
                 if CheckKeyWord(kwDEFERRABLE) then
                   ChangeStateGoNextNonspace(44) else
                   PopState;
               end;
          41 : if CheckKeyWord(kwDELETE) or
                  CheckKeyWord(kwUPDATE) then
                  ChangeStateGoNextNonspace(45);
          42 : if CheckId(1) then
                 ChangeStateGoNextNonspace(40);
          43 : if CheckKeyWord(kwDEFERRABLE) then
                 ChangeStateGoNextNonspace(44);
          44 : if CheckKeyWord(kwINITIALLY) then
                 ChangeStateGoNextNonspace(46) else
                 PopState;
          45 : if CheckKeyWord(kwSET) then
                 ChangeStateGoNextNonspace(47) else
               if CheckKeyWord(kwCASCADE) or
                  CheckKeyWord(kwRESTRICT) then
                 ChangeStateGoNextNonspace(40) else
               if CheckKeyWord(kwNO) then
                 ChangeStateGoNextNonspace(48);
          46 : if CheckKeyWord(kwDEFERRED) or
                  CheckKeyWord(kwIMMEDIATE) then
               begin
                 GoNextNonspace;
                 PopState;
               end;
          47 : if CheckKeyWord(kwNULL) or
                  CheckKeyWord(kwDEFAULT) then
                 ChangeStateGoNextNonspace(40);
          48 : if CheckKeyWord(kwACTION) then
                 ChangeStateGoNextNonspace(40);
          49 : begin
                 if (Length(cExpr) > 0) then
                 begin
                   aConstr.AddOption(cExpr);
                   cExpr := '';
                 end;
                 ChangeState(33);
               end;
        end;

        if (States.Count = 0) then
          Break;

        State := States[States.Count-1];

        if (i = i0) and (State0 = State) then
          Break;
      end;
      if (States.Count = 1) and (States[0] = 255) then
      begin
        FErrorCode := SAER_NOERROR;
        if Assigned(aStructure) and
           (not Assigned(aStructure.ByName(Result.Name))) and
           (ctoAddToStruct in aOptions) then
        begin
          aStructure.Add(Result);
        end;
      end
      else
      begin
        FErrorToken := T;
        case State of
          0 : FErrorCode := SAER_KEYWORD_EXPECTED or kwCREATE + 1;
          2 : FErrorCode := SAER_KEYWORD_EXPECTED or kwTABLE + 1;
          4 : FErrorCode := SAER_KEYWORD_EXPECTED or kwNOT + 1;
          5 : FErrorCode := SAER_KEYWORD_EXPECTED or kwEXISTS + 1;
          6, 64, 21, 28, 29, 42 : FErrorCode := SAER_IDENTIFIER_EXPECTED;
          7, 16, 17, 19, 26, 31 : FErrorCode := SAER_SYMBOL_EXPECTED or STOK_BRACKET_OPEN;
          9 : FErrorCode := SAER_KEYWORD_EXPECTED or kwROWID + 1;
          10 : FErrorCode := SAER_KEYWORD_EXPECTED or SAER_IDENTIFIER_EXPECTED;
          14, 39, 41, 45, 46, 47 : FErrorCode :=  SAER_KEYWORD_EXPECTED;
          15, 18, 23 : FErrorCode := SAER_KEYWORD_EXPECTED or kwKEY + 1;
          60, 50 : FErrorCode := SAER_SYMBOL_EXPECTED;
          51 : FErrorCode := SAER_KEYWORD_EXPECTED or kwREFERENCES + 1;
          24 : FErrorCode := SAER_KEYWORD_EXPECTED or kwNULL + 1;
          27 : FErrorCode := SAER_VALUE_EXPECTED;
          30 : FErrorCode := SAER_KEYWORD_EXPECTED or kwALWAYS + 1;
          34 : FErrorCode := SAER_KEYWORD_EXPECTED or kwCONFLICT + 1;
          36 : FErrorCode := SAER_KEYWORD_EXPECTED or kwAS + 1;
          43 : FErrorCode := SAER_KEYWORD_EXPECTED or kwDEFERRABLE + 1;
          48 : FErrorCode := SAER_KEYWORD_EXPECTED or kwACTION + 1;
        else
          FErrorCode := SAER_UNEXPECTED;
        end;
      end;
    finally
      States.Free;
      if Assigned(aConstr) then FreeAndNil(aConstr);
      if Assigned(aConstrs) then FreeAndNil(aConstrs);
    end;
  finally
    if FErrorCode <> SAER_NOERROR then
      if assigned(Result) then FreeAndNil(Result);
  end;
end;

class function TDBSqliteSynAnalizer.CheckIsCreateTable(aExpr : TSqliteExpr
  ) : Boolean;
begin
  Result := CheckStmt(stmtCreateTable, aExpr);
end;

class function TDBSqliteSynAnalizer.CheckIsBegin(aExpr : TSqliteExpr) : Boolean;
begin
  Result := CheckStmt(stmtBegin, aExpr);
end;

end.


