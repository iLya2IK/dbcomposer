{
 dbComposer:
   A program for constructing hierarchical databases based on the relational
   DBMS Sqlite3

   Part of dbComposer project

   Copyright (c) 2021-2022 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

program dbcomposer;

{$mode objfpc}{$H+}
{$define UseCThreads}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  cmem,
  {$ENDIF}{$ENDIF}
  sysutils,
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, dbcomposermain, dbchooseid, dbctypes, dbcomposertypes,
  dbcomposergrid, dbcomposerstruct, dbComposerUtils, dbComposerSQLEditor,
  dbComposerTreeView, dbComposerConsts, dbComposerCompleteHint,
  //wizards
  dbComposerWizards, dbComposerWizFixedList, dbComposerWizExtBlob,
  dbComposerWizNewTable, dbComposerWizStrings;

{$R *.res}

{$IFDEF DEBUG}
const cHeapTraceFile = 'heap.trc';
{$ENDIF}

begin
  {$IFDEF DEBUG}
  if FileExists(cHeapTraceFile) then
    DeleteFile(cHeapTraceFile);
  SetHeapTraceOutput(cHeapTraceFile);
  {$ENDIF}
  DefaultFormatSettings.DecimalSeparator := '.';
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.CreateForm(TChooseIdDlg, ChooseIdDlg);
  Application.CreateForm(TEditSQLDialog, EditSQLDialog);
  Application.CreateForm(TWizNewTable, WizNewTable);
  EditSQLDialog.SetSynHighlighter(Main.SynSQLSyn, @(Main.SetFontParamsFromCompletionObj));
  EditSQLDialog.SetCompletion(Main.SynCompletion1, Main.CompletionKeys);
  RegisterWizard(TDBWizExtBlob.Create);
  RegisterWizard(TDBWizFixedList.Create);
  RegisterWizard(TDBWizStrings.Create);
  Application.Run;
end.

