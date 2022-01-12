{
 dbComposerWizFixedList:
   Wizard that creates an fixed list of values in database

   Part of dbComposer project

   Copyright (c) 2021-2022 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit dbComposerWizFixedList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ExtCtrls, ComCtrls,
  fpjson,
  ExtSqlite3DS, ExtSqliteUtils, ExtSqliteTokens, ExprSqlite3Funcs,
  dbComposerWizards, dbComposerConfigParser,
  dbComposerStruct, dbComposerUtils, dbComposerConsts,
  Types, LCLType, LCLIntf;

type

  { TDBWizFixedList }

  TDBWizFixedList = class(TDBWizard)
  protected
    procedure Initialize; override;
  public
    function Launch({%H-}Data : TExtSqlite3Dataset; cfg : TDBJSONConfig;
                      struct : TDBStructure) : Boolean; override;
    class function FriendlyName : String; override;
    class function Description : String; override;
  end;

  { TWizFixedList }

  TWizFixedList = class(TForm)
    CheckBox1 : TCheckBox;
    Edit1 : TEdit;
    Image1 : TImage;
    Label1 : TLabel;
    TableNameEdit : TEdit;
    ListBox1 : TListBox;
    Panel1 : TPanel;
    Panel2 : TPanel;
    Panel3 : TPanel;
    Panel4 : TPanel;
    SpeedButton1 : TSpeedButton;
    SpeedButton2 : TSpeedButton;
    SpeedButton3 : TSpeedButton;
    ToolBar1 : TToolBar;
    ToolButton1 : TToolButton;
    ToolButton2 : TToolButton;
    ToolButton3 : TToolButton;
    ToolButton4 : TToolButton;
    ToolButton5 : TToolButton;
    procedure CheckBox1Change(Sender : TObject);
    procedure ListBox1DrawItem(Control : TWinControl; Index : Integer;
      ARect : TRect; State : TOwnerDrawState);
    procedure SpeedButton1Click(Sender : TObject);
    procedure SpeedButton2Click(Sender : TObject);
    procedure SpeedButton3Click(Sender : TObject);
    procedure ToolButton1Click(Sender : TObject);
    procedure ToolButton2Click(Sender : TObject);
    procedure ToolButton3Click(Sender : TObject);
    procedure ToolButton4Click(Sender : TObject);
  private
    FCfg : TDBJSONConfig;
    FStruct : TDBStructure;
  public
    procedure Init(cfg : TDBJSONConfig; struct : TDBStructure);
  end;

var
  WizFixedList : TWizFixedList;

implementation

resourcestring
  rsWizFixedListName = 'Fixed List';
  rsWizFixedDescript = 'Creates an fixed list of values in database';

{$R *.lfm}

{ TWizFixedList }

procedure TWizFixedList.SpeedButton2Click(Sender : TObject);
var T : TDBTable;
    Expr : TSqliteExpr;
    ok : Boolean;
    df : TSqliteKwFormatOption;
    i, id : integer;
    jObj : TJSONObject;
begin
  ok := true;
  T := FStruct.ByName(TableNameEdit.Text);
  if assigned(T) then
  begin
    ok := false;
    Label1.Caption := Format(rsTableExists, [TableNameEdit.Text]);
  end;
  if ok then
  begin
    FCfg.Lock;
    try
      //create table, populate it with values, add internal indexes
      df := skfoUpperCase;
      T := TDBTable.Create(nil, TableNameEdit.Text);
      try
        T.AddField(cIdField, dtaInteger, true);
        T.AddField(cValField, dtaText);
        T.Options[toCheckExists] := true;
        FCfg.AddExpr(T.BuildCreateExpression(true, df));

        Expr := TSqliteExpr.Create('');
        try
          Expr.AddKs([kwINSERT, kwOR, kwREPLACE, kwINTO], df);
          Expr.AddId(T.Name);
          Expr.OpenBracket;
          Expr.AddIdsListed([cIdField, cValField]);
          Expr.CloseBracket;
          Expr.AddK(kwVALUES, df);
          if CheckBox1.Checked then
           id := 0 else id := 1;

          for i := 0 to ListBox1.Items.Count-1 do
          begin
            if i > 0 then Expr.Period;
            Expr.OpenBracket;
            Expr.AddInt(id);
            Expr.Period;
            Expr.AddStr(ListBox1.Items[i]);
            Expr.CloseBracket;
            inc(id);
          end;

          FCfg.AddExpr(Expr);
        finally
          Expr.Free;
        end;

        id := FCfg.GetUniqID(JSON_CFG_INDXS);
        jObj := FCfg.AddNewObj(JSON_CFG_INDXS);
        jObj.Add(JSON_CFG_KIND,  DBHelper.GetJsonIndxClassEnum(TBaseSinExprs).Name);
        jObj.Add(JSON_CFG_UID,   id);
        jObj.Add(JSON_CFG_TABLE, T.Name);
        jObj.Add(JSON_CFG_KFIELD, cIdField);
        jObj.Add(JSON_CFG_VFIELD, cValField);

        jObj := FCfg.AddNewObj(JSON_CFG_FORINDXS);
        jObj.Add(JSON_CFG_KIND,  DBHelper.GetJsonStrStyleEnum(issConstList).Name);
        jObj.Add(JSON_CFG_UID,   id);
      finally
        T.Free;
      end;
    finally
      FCfg.UnLock;
    end;
    ModalResult := mrOK;
  end;
end;

procedure TWizFixedList.ListBox1DrawItem(Control : TWinControl;
  Index : Integer; ARect : TRect; State : TOwnerDrawState);
var
  C : TCanvas;
  S, S0 : String;
  inc : integer;
begin
  C := TListBox(Control).Canvas;

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

  C.Font.Style := [fsBold];

  if CheckBox1.Checked then
  begin
    inc := 0;
  end else
  begin
    inc := 1;
  end;
  S := IntToStr(ListBox1.Items.Count+inc) + ' ';
  FillChar(S[1], Length(S), ' ');
  S0 := IntToStr(Index+inc);
  Move(S0[1], S[1], Length(S0));
  DrawText(C.Handle, PChar(S), -1, aRect, DT_LEFT or
                                          DT_VCENTER or
                                          DT_SINGLELINE or
                                          DT_NOPREFIX);

  aRect.Left := aRect.Left + C.TextWidth(S) + 2;
  S := ListBox1.Items[Index];
  C.Font.Style := [];
  DrawText(C.Handle, PChar(S), -1, aRect, DT_LEFT or
                                          DT_VCENTER or
                                          DT_SINGLELINE or
                                          DT_NOPREFIX);
end;

procedure TWizFixedList.CheckBox1Change(Sender : TObject);
begin
  ListBox1.Invalidate;
end;

procedure TWizFixedList.SpeedButton1Click(Sender : TObject);
begin
  ListBox1.Items.Add(Edit1.Text);
end;

procedure TWizFixedList.SpeedButton3Click(Sender : TObject);
begin
  ModalResult := mrCancel;
end;

procedure TWizFixedList.ToolButton1Click(Sender : TObject);
begin
  ListBox1.Clear;
end;

procedure TWizFixedList.ToolButton2Click(Sender : TObject);
begin
  if ListBox1.ItemIndex >= 0 then
  begin
    ListBox1.Items.Delete(ListBox1.ItemIndex);
  end;
end;

procedure TWizFixedList.ToolButton3Click(Sender : TObject);
var id : integer;
begin
  id := ListBox1.ItemIndex;
  if id > 0 then
  begin
    ListBox1.Items.Move(id, id - 1);
    ListBox1.ItemIndex := id - 1;
  end;
end;

procedure TWizFixedList.ToolButton4Click(Sender : TObject);
var id : integer;
begin
  id := ListBox1.ItemIndex;
  if (id >= 0) and
     (id < (ListBox1.Items.Count-1)) then
  begin
    ListBox1.Items.Move(id, id + 1);
    ListBox1.ItemIndex := id + 1;
  end;
end;

procedure TWizFixedList.Init(cfg : TDBJSONConfig; struct : TDBStructure);
var B : TBitmap;
begin
  FCfg := cfg;
  FStruct := struct;
  ListBox1.Items.Clear;
  TableNameEdit.Text := rsSetNewTableName;
  Edit1.Text := rsSetNewValue;
  Label1.Caption := '';
  B := TBitmap.Create;
  try
    B.Width := 16;
    B.Height := 16;
    SpeedButton1.Images.Getbitmap(IMG_TABLE, B);
    Image1.Picture.Bitmap := B;
  finally
    B.Free;
  end;
end;

{ TDBWizFixedList }

procedure TDBWizFixedList.Initialize;
begin
  inherited Initialize;
  FChanges := [dbwcConfig, dbwcStruct];
  Application.CreateForm(TWizFixedList, WizFixedList);
end;

function TDBWizFixedList.Launch({%H-}Data : TExtSqlite3Dataset; cfg : TDBJSONConfig;
  struct : TDBStructure) : Boolean;
begin
  WizFixedList.Init(cfg, struct);
  WizFixedList.ShowModal;
  Result := WizFixedList.ModalResult = mrOK;
end;

class function TDBWizFixedList.FriendlyName : String;
begin
  Result := rsWizFixedListName;
end;

class function TDBWizFixedList.Description : String;
begin
  Result := rsWizFixedDescript;
end;

end.

