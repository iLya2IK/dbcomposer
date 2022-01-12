{
 dbComposerWizStrings:
   Wizard that creates TokenedSinExprs in database

   Part of dbComposer project

   Copyright (c) 2021-2022 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit dbComposerWizStrings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls,
  fpjson,
  ExtSqlite3DS, ExtSqliteUtils, ExprSqlite3Funcs,
  dbComposerWizards, dbComposerConfigParser,
  dbComposerStruct, dbComposerUtils, dbComposerConsts,
  dbComposerWizNewTable;

type

  { TDBWizStrings }

  TDBWizStrings = class(TDBWizard)
  private
    FCfg : TDBJSONConfig;
    FStruct : TDBStructure;
    function DoOk(Sender : TObject) : Boolean;
  protected
    procedure Initialize; override;
  public
    function Launch({%H-}Data : TExtSqlite3Dataset; cfg : TDBJSONConfig;
                      struct : TDBStructure) : Boolean; override;
    class function FriendlyName : String; override;
    class function Description : String; override;
  end;

implementation

resourcestring
  rsWizStrsListName = 'Strings Structure';
  rsWizStrsDescript = 'Creates TokenedSinExprs in database';

{ TDBWizStrings }

function TDBWizStrings.DoOk(Sender : TObject) : Boolean;
var T : TDBTable;
    F : TDBField;
    df : TSqliteKwFormatOption;
    jObj : TJSONObject;
    id : integer;
begin
  FCfg.Lock;
  try
    //create table, add internal indexes
    df := skfoUpperCase;
    T := TDBTable.Create(nil, WizNewTable.TableNameEdit.Text);
    try
      F := T.AddField(cIdField, dtaInteger, true);
      F.Constraints[0].AddOption(kwAUTOINCREMENT, df);
      T.AddField(cValField, dtaText);
      T.AddField(cSinExprField, dtaText, sqluGetIndexedKeyWord(kwNULL, df));
      T.AddField(cIsTmpField, dtaText, '0');
      T.AddField(cAddedField, dtaText, sqluGetIndexedKeyWord(kwCURRENT_TIMESTAMP, df));
      T.Options[toCheckExists] := true;
      FCfg.AddExpr(T.BuildCreateExpression(true, df));

      id := FCfg.GetUniqID(JSON_CFG_INDXS);
      jObj := FCfg.AddNewObj(JSON_CFG_INDXS);
      jObj.Add(JSON_CFG_KIND,  DBHelper.GetJsonIndxClassEnum(TTokenedSinExprs).Name);
      jObj.Add(JSON_CFG_UID,   id);
      jObj.Add(JSON_CFG_TABLE, T.Name);
      jObj.Add(JSON_CFG_KFIELD, cIdField);
      jObj.Add(JSON_CFG_VFIELD, cValField);
      jObj.Add(JSON_CFG_SEFIELD, cSinExprField);

      jObj := FCfg.AddNewObj(JSON_CFG_FORINDXS);
      jObj.Add(JSON_CFG_KIND,  DBHelper.GetJsonStrStyleEnum(issStrings).Name);
      jObj.Add(JSON_CFG_UID,   id);
    finally
      T.Free;
    end;
  finally
    FCfg.UnLock;
  end;

  Result := true;
end;

procedure TDBWizStrings.Initialize;
begin
  inherited Initialize;
  FChanges := [dbwcConfig, dbwcStruct];
end;

function TDBWizStrings.Launch(Data : TExtSqlite3Dataset; cfg : TDBJSONConfig;
  struct : TDBStructure) : Boolean;
begin
  FCfg := cfg;
  FStruct := struct;
  WizNewTable.Init(struct);
  WizNewTable.OnOk := @DoOk;
  WizNewTable.ShowModal;
  Result := WizNewTable.ModalResult = mrOK;
end;

class function TDBWizStrings.FriendlyName : String;
begin
  Result := rsWizStrsListName;
end;

class function TDBWizStrings.Description : String;
begin
  Result := rsWizStrsDescript;
end;

end.

