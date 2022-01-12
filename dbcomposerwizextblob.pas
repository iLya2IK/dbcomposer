{
 dbComposerWizExtBlob:
   Wizard that create and config an external blob in database

   Part of dbComposer project

   Copyright (c) 2021-2022 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit dbComposerWizExtBlob;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  FileUtil, fpjson,
  ExtCtrls, ExtSqlite3DS, ExtSqliteUtils, dbComposerWizards,
  dbComposerConfigParser, dbComposerStruct, dbComposerUtils, dbComposerConsts;

type

  { TDBWizExtBlob }

  TDBWizExtBlob = class(TDBWizard)
  protected
    procedure Initialize; override;
  public
    function Launch({%H-}Data : TExtSqlite3Dataset; cfg : TDBJSONConfig;
                    struct : TDBStructure) : Boolean; override;
    class function FriendlyName : String; override;
    class function Description : String; override;
  end;


  { TWizExtBlob }

  TWizExtBlob = class(TForm)
    ComboBox1 : TComboBox;
    Image2 : TImage;
    Label1 : TLabel;
    Label2 : TLabel;
    Panel1 : TPanel;
    Panel2 : TPanel;
    Panel3 : TPanel;
    Panel4 : TPanel;
    SelectDirectoryDialog1 : TSelectDirectoryDialog;
    SpeedButton1 : TSpeedButton;
    SpeedButton2 : TSpeedButton;
    SpeedButton3 : TSpeedButton;
    PathEdit : TEdit;
    TableNameEdit : TEdit;
    procedure SpeedButton1Click(Sender : TObject);
    procedure SpeedButton2Click(Sender : TObject);
    procedure SpeedButton3Click(Sender : TObject);
  private
    FCfg : TDBJSONConfig;
    FStruct : TDBStructure;
    procedure Init(cfg : TDBJSONConfig; struct : TDBStructure);
  public

  end;

var
  WizExtBlob : TWizExtBlob;

implementation

uses LazUTF8;

resourcestring
  rsWizExtBlobName = 'External Blob';
  rsWizExtBlobDescript = 'Create and config an external blob in database';

{$R *.lfm}

{ TWizExtBlob }

procedure TWizExtBlob.SpeedButton1Click(Sender : TObject);
var S : String;
begin
  S := PathEdit.Text;
  S := UTF8StringReplace(S, cDBPATH, DBHelper.DBPath, []);
  S := UTF8StringReplace(S, cCURPATH, DBHelper.AppPath, []);
  if DirectoryExists(S) then
  begin
    SelectDirectoryDialog1.InitialDir := S;
  end else
  begin
    if length(DBHelper.DBPath) > 0 then
      SelectDirectoryDialog1.InitialDir := DBHelper.DBPath else
      SelectDirectoryDialog1.InitialDir := DBHelper.AppPath;
  end;
  if SelectDirectoryDialog1.Execute then
  begin
    S := SelectDirectoryDialog1.FileName + cSysDelimiter;
    S := UTF8StringReplace(S, DBHelper.DBPath, cDBPATH, []);
    S := UTF8StringReplace(S, DBHelper.AppPath, cCURPATH, []);
    PathEdit.Text := S;
  end;
end;

procedure TWizExtBlob.SpeedButton2Click(Sender : TObject);
var T : TDBTable;
    F : TDBField;
    ok : Boolean;
    df : TSqliteKwFormatOption;
    jObj : TJSONObject;
begin
  ok := true;
  T := FStruct.ByName(TableNameEdit.Text);
  if assigned(T) then
  begin
    ok := false;
    Label1.Caption := Format(rsTableExists, [TableNameEdit.Text]);
  end;
  if ComboBox1.ItemIndex < 0 then ok := false;
  if ok then
  begin
    FCfg.Lock;
    try
      //create table, add ext blobs
      df := skfoUpperCase;
      T := TDBTable.Create(nil, TableNameEdit.Text);
      try
        //TableName(id integer primary key autoincrement,
        //          filepath text unique not null)
        F := T.AddField(cIdField, dtaInteger, true);
        F.Constraints[0].AddOption(kwAUTOINCREMENT, df);
        F := T.AddField(cFilePathField, dtaText);
        F.Constraints.Add(dbckUnique, '');
        F.Constraints.Add(dbckNotNull, '');
        T.Options[toCheckExists] := true;
        FCfg.AddExpr(T.BuildCreateExpression(true, df));

        jObj := FCfg.AddNewObj(JSON_CFG_EXTBLOBS);
        jObj.Add(JSON_CFG_KIND,  DBHelper.JsonBlobKinds[ComboBox1.ItemIndex].Name);
        jObj.Add(JSON_CFG_PATH,  UTF8Trim(PathEdit.Text));
        jObj.Add(JSON_CFG_TABLE, T.Name);
        jObj.Add(JSON_CFG_FIELD, cFilePathField);
      finally
        T.Free;
      end;
    finally
      FCfg.UnLock;
    end;
    ModalResult := mrOK;
  end;
end;

procedure TWizExtBlob.SpeedButton3Click(Sender : TObject);
begin
  ModalResult := mrCancel;
end;

procedure TWizExtBlob.Init(cfg : TDBJSONConfig; struct : TDBStructure);
var B : TBitmap;
    i : integer;
begin
  FCfg := cfg;
  FStruct := struct;
  if length(DBHelper.DBPath) > 0 then
    SelectDirectoryDialog1.InitialDir := DBHelper.DBPath else
    SelectDirectoryDialog1.InitialDir := DBHelper.AppPath;
  ComboBox1.Items.Clear;
  //
  for i := 0 to DBHelper.JsonBlobKinds.Count-1 do
  begin
    ComboBox1.Items.Add(DBHelper.JsonBlobKinds[i].Name);
  end;
  if ComboBox1.Items.Count > 0 then
    ComboBox1.ItemIndex := 0 else
    ComboBox1.ItemIndex := -1;
  TableNameEdit.Text := rsSetNewTableName;
  PathEdit.Text := cDBPATH;
  Label1.Caption := '';
  B := TBitmap.Create;
  try
    B.Width := 16;
    B.Height := 16;
    SpeedButton1.Images.Getbitmap(IMG_TABLE, B);
    Image2.Picture.Bitmap := B;
  finally
    B.Free;
  end;
end;

{ TDBWizExtBlob }

procedure TDBWizExtBlob.Initialize;
begin
  inherited Initialize;
  FChanges := [dbwcConfig, dbwcStruct];
  Application.CreateForm(TWizExtBlob, WizExtBlob);
end;

function TDBWizExtBlob.Launch(Data : TExtSqlite3Dataset; cfg : TDBJSONConfig;
  struct : TDBStructure) : Boolean;
begin
  WizExtBlob.Init(cfg, struct);
  WizExtBlob.ShowModal;
  Result := WizExtBlob.ModalResult = mrOK;
end;

class function TDBWizExtBlob.FriendlyName : String;
begin
  Result := rsWizExtBlobName;
end;

class function TDBWizExtBlob.Description : String;
begin
  Result := rsWizExtBlobDescript;
end;

end.

