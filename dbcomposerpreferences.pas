{
 dbComposerPreferences:
   Preferences dialog for dbComposer

   Part of dbComposer project

   Copyright (c) 2021-2022 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit dbComposerPreferences;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Buttons, StdCtrls, ColorBox, SynEdit, SynHighlighterSQLite3,
  SynEditHighlighter;

type

  { TPrefsDlg }

  TPrefsDlg = class(TForm)
    BackChooseButton : TButton;
    Button1 : TButton;
    ForeChooseButton : TButton;
    ForeColorBox : TColorBox;
    BackColorBox : TColorBox;
    FontStyleSelector : TCheckGroup;
    ColorDialog1 : TColorDialog;
    AttribSelector : TComboBox;
    FontDialog1 : TFontDialog;
    Label1 : TLabel;
    Label2 : TLabel;
    FontSample : TLabel;
    Notebook1 : TNotebook;
    EditorPage : TPage;
    Panel1 : TPanel;
    OKButton : TSpeedButton;
    CancelButton : TSpeedButton;
    Panel2 : TPanel;
    Panel3 : TPanel;
    SpeedButton1 : TSpeedButton;
    Splitter1 : TSplitter;
    PrefsTree : TTreeView;
    SampleSynEdit : TSynEdit;
    procedure AttribSelectorChange(Sender : TObject);
    procedure BackChooseButtonClick(Sender : TObject);
    procedure BackColorBoxChange(Sender : TObject);
    procedure Button1Click(Sender : TObject);
    procedure CancelButtonClick(Sender : TObject);
    procedure FontStyleSelectorItemClick(Sender : TObject; Index : integer);
    procedure ForeChooseButtonClick(Sender : TObject);
    procedure ForeColorBoxChange(Sender : TObject);
    procedure FormCreate(Sender : TObject);
    procedure OKButtonClick(Sender : TObject);
    procedure SpeedButton1Click(Sender : TObject);
  private
    FSynSQL : TSynSQLite3Syn;
    FItemSwitching : Boolean;
    procedure InitState;
    procedure UpdateAttr;
    function GetCurAttr : TSynHighlighterAttributes;
    procedure UpdateFont(const aName : String; aHeigth : Integer);
  public

  end;

function ShowPreferences : Boolean;

var
  PrefsDlg : TPrefsDlg;

implementation

uses dbComposerUtils;

function ShowPreferences : Boolean;
begin
  PrefsDlg.InitState;
  PrefsDlg.ShowModal;
  Result := PrefsDlg.ModalResult = mrOK;
end;

{$R *.lfm}

{ TPrefsDlg }

procedure TPrefsDlg.FormCreate(Sender : TObject);
begin
  TStringList(AttribSelector.Items).OwnsObjects := false;

  FSynSQL := TSynSQLite3Syn.Create(Self);
  FSynSQL.FieldNames.Add('id');
  FSynSQL.FieldNames.Add('age');
  FSynSQL.FieldNames.Add('name');
  FSynSQL.TableNames.Add('users');
  FSynSQL.Enabled := true;
  SampleSynEdit.Highlighter := FSynSQL;
end;

procedure TPrefsDlg.OKButtonClick(Sender : TObject);
var i : integer;
begin
  DBHelper.BeginUpdate;
  try
    for i := 0 to DBHelper.Sqlite3Highlighter.AttrCount-1 do
    begin
      DBHelper.Sqlite3Highlighter.Attribute[i].Assign(FSynSQL.Attribute[i]);
    end;
    DBHelper.EditorFontName := FontDialog1.Font.Name;
    DBHelper.EditorFontSize := FontDialog1.Font.Height;
  finally
    DBHelper.EndUpdate;
  end;
  ModalResult := mrOK;
end;

procedure TPrefsDlg.SpeedButton1Click(Sender : TObject);
var Attr, DefAttr : TSynHighlighterAttributes;
begin
  if (AttribSelector.ItemIndex >= 0) and (not FItemSwitching) then
  begin
    Attr := GetCurAttr;
    DefAttr := DBHelper.Sqlite3AttrsDefs.AttrByName(Attr.Name);
    if assigned(DefAttr) then
    begin
      Attr.Assign(DefAttr);
      UpdateAttr;
    end;
  end;
end;

procedure TPrefsDlg.AttribSelectorChange(Sender : TObject);
begin
  UpdateAttr;
end;

procedure TPrefsDlg.BackChooseButtonClick(Sender : TObject);
begin
  ColorDialog1.Color := BackColorBox.Selected;
  if ColorDialog1.Execute then
  begin
    BackColorBox.Selected := ColorDialog1.Color;
  end;
end;

procedure TPrefsDlg.BackColorBoxChange(Sender : TObject);
var Attr : TSynHighlighterAttributes;
begin
  if (AttribSelector.ItemIndex >= 0) and (not FItemSwitching) then
  begin
    Attr := GetCurAttr;
    Attr.Background := BackColorBox.Selected;
  end;
end;

procedure TPrefsDlg.Button1Click(Sender : TObject);
begin
  if FontDialog1.Execute then
  begin
    UpdateFont(FontDialog1.Font.Name, FontDialog1.Font.Height);
  end;
end;

procedure TPrefsDlg.CancelButtonClick(Sender : TObject);
begin
  ModalResult := mrCancel;
end;

procedure TPrefsDlg.FontStyleSelectorItemClick(Sender : TObject; Index : integer
  );
var Attr : TSynHighlighterAttributes;
begin
  if (AttribSelector.ItemIndex >= 0) and (not FItemSwitching) then
  begin
    Attr := GetCurAttr;
    if FontStyleSelector.Checked[0] then
      Attr.Style := Attr.Style + [fsBold] else
      Attr.Style := Attr.Style - [fsBold];
    if FontStyleSelector.Checked[1] then
      Attr.Style := Attr.Style + [fsItalic] else
      Attr.Style := Attr.Style - [fsItalic];
    if FontStyleSelector.Checked[2] then
      Attr.Style := Attr.Style + [fsUnderline] else
      Attr.Style := Attr.Style - [fsUnderline];
  end;
end;

procedure TPrefsDlg.ForeChooseButtonClick(Sender : TObject);
begin
  ColorDialog1.Color := ForeColorBox.Selected;
  if ColorDialog1.Execute then
  begin
    ForeColorBox.Selected := ColorDialog1.Color;
  end;
end;

procedure TPrefsDlg.ForeColorBoxChange(Sender : TObject);
var Attr : TSynHighlighterAttributes;
begin
  if (AttribSelector.ItemIndex >= 0) and (not FItemSwitching) then
  begin
    Attr := GetCurAttr;
    Attr.Foreground := ForeColorBox.Selected;
  end;
end;

procedure TPrefsDlg.InitState;
var i : integer;
begin
  FSynSQL.Assign(DBHelper.Sqlite3Highlighter);

  UpdateFont(DBHelper.EditorFontName, DBHelper.EditorFontSize);

  FItemSwitching := false;

  AttribSelector.Items.Clear;
  for i := 0 to FSynSQL.AttrCount-1 do
  begin
    AttribSelector.Items.AddObject(FSynSQL.Attribute[i].Caption^,
                                   FSynSQL.Attribute[i]);
  end;
  AttribSelector.ItemIndex := 0;

  ForeChooseButton.Height := ForeColorBox.Height;
  BackChooseButton.Height := BackColorBox.Height;

  UpdateAttr;
end;

procedure TPrefsDlg.UpdateAttr;
var Attr : TSynHighlighterAttributes;
begin
  FItemSwitching := true;
  try
    if AttribSelector.ItemIndex >= 0 then
    begin
      Attr := GetCurAttr;
      ForeColorBox.Selected := Attr.Foreground;
      BackColorBox.Selected := Attr.Background;
      FontStyleSelector.Checked[0] := fsBold in Attr.Style;
      FontStyleSelector.Checked[1] := fsItalic in Attr.Style;
      FontStyleSelector.Checked[2] := fsUnderline in Attr.Style;
    end;
  finally
    FItemSwitching := false;
  end;
end;

function TPrefsDlg.GetCurAttr : TSynHighlighterAttributes;
begin
  Result := TSynHighlighterAttributes(AttribSelector.Items.Objects[AttribSelector.ItemIndex])
end;

procedure TPrefsDlg.UpdateFont(const aName : String; aHeigth : Integer);
begin
  FontDialog1.Font.Name := aName;
  FontDialog1.Font.Height := aHeigth;
  FontSample.Font.Name := aName;
  FontSample.Font.Height := aHeigth;

  SampleSynEdit.Font.Assign(FontSample.Font);

  FontSample.Caption := FontSample.Font.Name + ' ' + IntToStr(FontSample.Font.Height) + 'px';
end;

end.

