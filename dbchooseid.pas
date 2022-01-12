{
 dbChooseId:
   Dialog for choosing an identifier

   Part of dbComposer project

   Copyright (c) 2021-2022 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit dbchooseid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  dbcomposergrid, dbComposerStruct, dbctypes,
  ExtSqlite3DS, LCLIntf, LCLType, StdCtrls, ExtCtrls, Buttons;

type

  TOnFilterTemplate = procedure (const Sec: String; SL : TStrings) of object;

  { TChooseIdDlg }

  TChooseIdDlg = class(TForm)
    BitBtn1 : TBitBtn;
    BitBtn2 : TBitBtn;
    FiltersComboBox : TComboBox;
    Panel1 : TPanel;
    ToolBar1 : TToolBar;
    ToolButton1 : TToolButton;
    ToolButton2 : TToolButton;
    procedure BitBtn1Click(Sender : TObject);
    procedure BitBtn2Click(Sender : TObject);
    procedure FormClose(Sender : TObject; var CloseAction : TCloseAction);
    procedure FormCreate(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure FormShow(Sender : TObject);
    procedure ToolButton1Click(Sender : TObject);
    procedure ToolButton2Click(Sender : TObject);
  private
    FFilter : String;
    FNeedUpdate : Boolean;
    FOnLoadTemplates : TOnFilterTemplate;
    FOnWriteTemplate : TOnFilterTemplate;
    FRequest : TSQLRequest;
    FResult : Integer;
    SG : TDBExtGrid;

    function GetDataSet: TExtSqlite3Dataset;
    function GetDB: TDBStructure;
    procedure SetDataSet(AValue: TExtSqlite3Dataset);
    procedure SetDB(AValue: TDBStructure);
    procedure SetFilter(AValue : String);
    procedure SetRequest(AValue : TSQLRequest);
    procedure UpdateTable;
    procedure DoWriteFilterTemplate;
    procedure DoLoadFilterTemplate;

    procedure SGHeaderClick(Sender : TObject; IsColumn : Boolean;
      Index : Integer);
    procedure SGSelection(Sender : TObject; aCol, aRow : Integer);
  public
    procedure DoIdle;
    property DataSet : TExtSqlite3Dataset read GetDataSet write SetDataSet;
    property DB : TDBStructure read GetDB write SetDB;
    property Request : TSQLRequest read FRequest write SetRequest;
    property Filter : String read FFilter write SetFilter;
    property ResultId : integer read FResult write FResult;
    property OnLoadTemplates : TOnFilterTemplate read FOnLoadTemplates write FOnLoadTemplates;
    property OnWriteTemplate : TOnFilterTemplate read FOnWriteTemplate write FOnWriteTemplate;
  end;

function ShowChooseIdDlg(Sel : TSQLRequest) : Integer;

var
  ChooseIdDlg : TChooseIdDlg;

implementation

uses LazUTF8;

{$R *.lfm}

function ShowChooseIdDlg(Sel : TSQLRequest) : Integer;
begin
  ChooseIdDlg.Request := Sel;
  ChooseIdDlg.ResultId := -1;
  ChooseIdDlg.ShowModal;
  if ChooseIdDlg.ModalResult = mrOK then
    Result := ChooseIdDlg.ResultId else
    Result := -1;
end;

{ TChooseIdDlg }

procedure TChooseIdDlg.FormShow(Sender : TObject);
begin
  DoLoadFilterTemplate;
  UpdateTable;
end;

procedure TChooseIdDlg.SGHeaderClick(Sender : TObject; IsColumn : Boolean;
  Index : Integer);
begin
  if IsColumn then
  begin
    FiltersComboBox.Text := FiltersComboBox.Text + SG.Cells[Index, 0];
  end;
end;

procedure TChooseIdDlg.SGSelection(Sender : TObject; aCol, aRow : Integer);
begin
  if aRow > 0 then begin
    ResultId := SG.IdAtRow[SG.Row-1];
  end;
end;

procedure TChooseIdDlg.DoIdle;
begin
  SG.DoIdle;
end;

procedure TChooseIdDlg.ToolButton1Click(Sender : TObject);
begin
  ModalResult := mrOK;
end;

procedure TChooseIdDlg.ToolButton2Click(Sender : TObject);
begin
  ModalResult := mrCancel;
end;

procedure TChooseIdDlg.FormClose(Sender : TObject;
  var CloseAction : TCloseAction);
begin
  SG.ClearGrid;
  ChooseIdDlg.Request.Clear;
  FNeedUpdate := false;
end;

procedure TChooseIdDlg.FormCreate(Sender : TObject);
begin
  FRequest := TSQLRequest.Create;

  SG := TDBExtGrid.Create(Self);
  SG.Top := Panel1.Top + Panel1.Height + 5;
  SG.Parent := Self;
  SG.OnSelection := @SGSelection;
  SG.OnHeaderClick := @SGHeaderClick;

  SG.Align := alClient;
end;

procedure TChooseIdDlg.FormDestroy(Sender : TObject);
begin
  FRequest.Free;
end;

procedure TChooseIdDlg.BitBtn1Click(Sender : TObject);
var S : String;
begin
  S := Utf8Trim(FiltersComboBox.Text);
  if FiltersComboBox.Items.IndexOf(S) < 0 then
  begin
    FiltersComboBox.Items.Add(S);
    DoWriteFilterTemplate;
  end;

  Filter := S;

  UpdateTable;
end;

procedure TChooseIdDlg.BitBtn2Click(Sender : TObject);
begin
  FiltersComboBox.Text := '';
  Filter := '';

  UpdateTable;
end;

procedure TChooseIdDlg.SetRequest(AValue : TSQLRequest);
begin
  if FRequest.Compare(AValue) then  Exit;
  FRequest.Assign( AValue );
  FNeedUpdate := true;
end;

procedure TChooseIdDlg.SetFilter(AValue : String);
begin
  if FFilter = AValue then Exit;
  FFilter := AValue;
  FNeedUpdate := true;
end;

procedure TChooseIdDlg.SetDB(AValue: TDBStructure);
begin
  SG.DB := AValue;
end;

function TChooseIdDlg.GetDB: TDBStructure;
begin
  Result := SG.DB;
end;

function TChooseIdDlg.GetDataSet: TExtSqlite3Dataset;
begin
  Result := SG.DataSet;
end;

procedure TChooseIdDlg.SetDataSet(AValue: TExtSqlite3Dataset);
begin
  SG.DataSet := AValue;
end;

procedure TChooseIdDlg.UpdateTable;
begin
  if FNeedUpdate then
  begin
    FNeedUpdate := false;

    SG.ExecuteSQLExpr(Request, Filter,
                               Request.SelectTable + '.' + Request.SelectId);

    if SG.RowCount > 1 then
    begin
      SG.Row := 1;
      ResultId := SG.IdAtRow[SG.Row-1];
    end;
  end;
end;

procedure TChooseIdDlg.DoWriteFilterTemplate;
begin
  if assigned(FOnWriteTemplate) then
    FOnWriteTemplate(FRequest.SelectTable, FiltersComboBox.Items);
end;

procedure TChooseIdDlg.DoLoadFilterTemplate;
begin
  if assigned(FOnLoadTemplates) then
    FOnLoadTemplates(FRequest.SelectTable, FiltersComboBox.Items);
end;

end.

