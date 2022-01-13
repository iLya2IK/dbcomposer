{
 dbComposerWizards:
   Basic types and classes for wizards

   Part of dbComposer project

   Copyright (c) 2021-2022 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit dbComposerWizards;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  OGLFastList,
  ExtSqlite3DS,
  dbComposerStruct, dbComposerConfigParser;

type
  TDBWizChange = (dbwcStruct, dbwcConfig, dbwcDatabase);
  TDBWizChanges = set of TDBWizChange;

  { TDBWizard }

  TDBWizard = class
  protected
    FChanges : TDBWizChanges;
    procedure Initialize; virtual;
  public
    constructor Create;
    function Launch(Data : TExtSqlite3Dataset; cfg : TDBJSONConfig;
                      struct : TDBStructure) : Boolean; virtual; abstract;
    property Changes : TDBWizChanges read FChanges;
    class function FriendlyName : String; virtual; abstract;
    class function Description : String; virtual; abstract;
  end;

  TWizCollection = class (specialize TFastBaseCollection<TDBWizard>);

  TDBWizardClass = class of TDBWizard;

procedure RegisterWizard(DBWiz : TDBWizard);
function GetWizardList : TWizCollection;

implementation

var vWizards : TWizCollection;

procedure RegisterWizard(DBWiz : TDBWizard);
begin
  vWizards.Add(DBWiz);
end;

function GetWizardList : TWizCollection;
begin
  Result := vWizards;
end;

{ TDBWizard }

procedure TDBWizard.Initialize;
begin
  FChanges := [];
end;

constructor TDBWizard.Create;
begin
  Initialize;
end;

initialization
  vWizards := TWizCollection.Create;

finalization
  vWizards.Free;

end.

