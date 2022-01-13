{
 dbComposerWizNewTable:
   Helping control to create new table for wizards

   Part of dbComposer project

   Copyright (c) 2021-2022 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit dbComposerWizNewTable;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons,
  dbComposerStruct, dbComposerConsts;

type

  TWizOkEvent = function (Sender : TObject) : Boolean of object;

  { TWizNewTable }

  TWizNewTable = class(TForm)
    Image1 : TImage;
    Label1 : TLabel;
    Panel1 : TPanel;
    Panel3 : TPanel;
    SpeedButton2 : TSpeedButton;
    SpeedButton3 : TSpeedButton;
    TableNameEdit : TEdit;
    procedure SpeedButton2Click(Sender : TObject);
    procedure SpeedButton3Click(Sender : TObject);
  private
    FStruct : TDBStructure;
    FOnOk : TWizOkEvent;
  public
    procedure Init(struct : TDBStructure);
    property OnOk : TWizOkEvent read FOnOk write FOnOk;
  end;

var
  WizNewTable : TWizNewTable;

implementation

{$R *.lfm}

{ TWizNewTable }

procedure TWizNewTable.SpeedButton3Click(Sender : TObject);
begin
  ModalResult := mrCancel;
end;

procedure TWizNewTable.SpeedButton2Click(Sender : TObject);
var T : TDBTable;
    ok : boolean;
begin
  ok := true;
  if Assigned(FStruct) then
  begin
    T := FStruct.ByName(TableNameEdit.Text);
    if assigned(T) then
    begin
      ok := false;
      Label1.Caption := Format(rsTableExists, [TableNameEdit.Text]);
    end;
  end;
  if Assigned(FOnOk) and ok then
    ok := FOnOk(Self);
  if ok then
    ModalResult := mrOK;
end;

procedure TWizNewTable.Init(struct : TDBStructure);
var B : TBitmap;
begin
  FStruct := struct;
  TableNameEdit.Text := rsSetNewTableName;
  Label1.Caption := '';
  B := TBitmap.Create;
  try
    B.Width := 16;
    B.Height := 16;
    SpeedButton2.Images.Getbitmap(IMG_TABLE, B);
    Image1.Picture.Bitmap := B;
  finally
    B.Free;
  end;
end;

end.

