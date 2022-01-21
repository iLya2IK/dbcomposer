{
 dbComposerAbout:
   About dialog for dbComposer

   Part of dbComposer project

   Copyright (c) 2021-2022 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit dbComposerAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Types;

type

  { TAboutDlg }

  TAboutDlg = class(TForm)
    Button1 : TButton;
    Image1 : TImage;
    Label1 : TLabel;
    AboutMemo : TMemo;
    procedure FormCreate(Sender : TObject);
  private

  public

  end;

var
  AboutDlg : TAboutDlg;

implementation

uses dbComposerUtils, ExtSqliteUtils, OGLRegExprWrapper;

{$R *.lfm}

{ TAboutDlg }

procedure TAboutDlg.FormCreate(Sender : TObject);
var s : TSize;
begin
  s.cx := 128;
  s.cy := 128;
  Image1.Picture.Icon.Assign(Application.Icon);
  Image1.Picture.Icon.Current := Image1.Picture.Icon.GetBestIndexForSize(s);

  AboutMemo.Lines.Add('version ' + DBHelper.VersionStr);
  AboutMemo.Lines.Add('https://github.com/iLya2IK/dbcomposer');
  AboutMemo.Lines.Add('');
  AboutMemo.Lines.Add('SQLite3 v' + sqluGetVersionStr());
  AboutMemo.Lines.Add('https://www.sqlite.org');
  AboutMemo.Lines.Add('');
  AboutMemo.Lines.Add('Lazarus LCL v' + LCLVersion);
  AboutMemo.Lines.Add('https://www.lazarus-ide.org');
  AboutMemo.Lines.Add('');
  AboutMemo.Lines.Add('CommonUtils');
  AboutMemo.Lines.Add('https://github.com/iLya2IK/commonutils');
  AboutMemo.Lines.Add('');
  AboutMemo.Lines.Add('TRegExpr v' + rewRegExprVersionStr);
  AboutMemo.Lines.Add('https://github.com/andgineer/TRegExpr');
  AboutMemo.Lines.Add('');
  AboutMemo.Lines.Add('kcThreadPool - thread pools - Copyright (c) 2011, Maciej Kaczkowski / keit.co');
end;

end.

