{
 dbComposerConsts:
   Constants and resource strings for project

   Part of dbComposer project

   Copyright (c) 2021-2022 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit dbComposerConsts;

{$mode objfpc}{$H+}

interface

const
  cDefaultDBName = 'storedb.json';
  cDBExt = '.sqlite';
  cfldDBName = 'DBName';
  cfldFilters = 'Filters_';
  cCURPATH = '%CURPATH%';
  cDBPATH = '%DBPATH%';

  cIdField = 'id';
  cValField = 'val';
  cDescrField = 'descr';
  cSinExprField = 'sinexpr';
  cFilePathField = 'filepath';
  cAddedField = 'added';
  cIsTmpField = 'istmp';

  IMG_OPEN             = 0;
  IMG_REFRESH          = 1;
  IMG_EDIT             = 2;
  IMG_MINUS            = 3;
  IMG_PLUS             = 4;
  IMG_SAVE             = 5;
  IMG_DEFAULT          = 6;
  IMG_CONFIG_OPT       = 7;
  IMG_RUN              = 8;
  IMG_TABLE            = 9;
  IMG_FILTER           = 10;
  IMG_ERROR            = 11;
  IMG_ADD_AND_EDIT     = 12;
  IMG_OK               = 13;
  IMG_BACK             = 14;
  IMG_CLEAR            = 15;
  IMG_STRUCT           = 16;
  IMG_CONFIG           = 17;
  IMG_STRUCT_ELEMENT   = 18;
  IMG_DATA_TYPE        = 19;
  IMG_USER             = 20;
  IMG_NEW              = 21;
  IMG_IMAGE            = 22;
  IMG_TEXT_DOC         = 23;
  IMG_CONST            = 24;
  IMG_VARIANT          = 25;
  IMG_KEY              = 26;
  IMG_CONNECTION       = 27;
  IMG_LIST             = 28;
  IMG_DICT             = 29;
  IMG_UP_ARROW         = 30;
  IMG_DWN_ARROW        = 31;
  IMG_SAVE_AS          = 32;
  IMG_CHECK            = 33;
  IMG_BULB_OFF         = 34;
  IMG_BULB_ON          = 35;
  IMG_UNDO             = 36;
  IMG_REDO             = 37;
  IMG_HALT             = 38;
  IMG_COMMENT          = 39;
  IMG_INFORM           = 40;
  IMG_NOTE             = 41;
  IMG_QUESTION         = 42;
  IMG_TABLE_NOT_EQUAL  = 43;
  IMG_TABLE_NOT_EXISTS = 44;
  IMG_EDIT_RECORD      = 45;
  IMG_WARNING          = 46;
  IMG_CRITICAL_ERROR   = 47;
  IMG_WIZARD           = 48;
  IMG_TABLEADD         = 49;


resourcestring
  rsDBExistsDelete = 'File ''%s'' exists. '+
                     'This operation will delete all existing data. Continue?';
  rsTableWasChanged = 'The table ''%s'' has been changed. This operation will '+
                      'reject all changed. Continue?';
  rsValuesAreEmpty = 'Fields ''%s'' are not defined. '+
                     'Set the marked fields to create a new record.';
  rsRemoveElement  = 'Remove element %d from array?';

  rsTableMalformed = 'The table is malformed - %s';
  rsTableNotExists = 'The table ''%s'' does not exist in the database. '+
                     'Save the config file or run this query.';
  rsTableNotEqual = 'The description for table %s in the config file does not '+
                     'match the existing table in the database. Try modifying '+
                     'the config file or altering an existing table.';
  rsTransactionWithinTransaction = 'Nested transaction found. '+
                     'All multi-queries are executed in the BEGIN ... COMMIT '+
                     'block. Try using nested SAVEPOINT ... RELEASE commands '+
                     'instead.';
  rsTransactionClosedWrong = 'The current transaction was closed incorrectly';
  rsNotClosedTransactions = 'Not all transactions are closed';
  rsNoOpenedTransactions = 'There are no open transactions';
  rsMalformedTransaction = 'Incorrect transaction management detected';
  rsNoSuchSavepoint = 'No matching savepoint-name found "%s"';

  rsChangedNeedToBeSaved = 'The config file is changed and need to be saved';

  rsColumnExists  = 'The field ''%s'' aready exists!';
  rsTableExists  = 'The table ''%s'' aready exists!';
  rsNoErrors = 'OK';

  rsEdit = 'Edit';
  rsExecute = 'Execute';
  rsRemove  = 'Remove';
  rsSaveConfig = 'Save Config';
  rsSetFromDatabase = 'Update config from database';
  rsModifyDatabase = 'Modify database';

  rsSetNewTableName = 'Set new table name';
  rsSetNewValue = 'Set new value';

  rsRecordSuccessUpdated = 'Record with id %d successfully updated in table ''%s''';
  rsRecordSuccessAdded = 'Record successfully added to table ''%s''. New id = %d';
  rsSqliteResultAsComment = '-- %s(%d)';

  rsMenuDelete = 'Delete';
  rsMenuMoveUp = 'Move Up';
  rsMenuMoveDown = 'Move Down';
  rsMenuAddS = 'Add %s';
  rsMenuAdd = 'Add';
  rsMenuAddT = 'Add new table';
  rsMenuRun = 'Run';
  rsMenuEdit = 'Edit';


implementation

end.

