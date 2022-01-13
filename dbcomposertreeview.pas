{
 dbComposerTreeView:
   TreeVeiw wrapper for workarounding the bug

   Part of dbComposer project

   Copyright (c) 2021-2022 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit dbComposerTreeView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, StdCtrls;

type
  { TDBComposerTreeView }

  TDBComposerTreeView = class(TTreeView)
  private
    function GetIndent : Integer;
    function GetScrolledLeft : Integer;
  public
    NodeEditor : TEdit;
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    property Editor : TEdit read FEditor write FEditor;
    procedure EndEditing(Cancel: boolean = false); override;
    property ScrolledLeft : Integer read GetScrolledLeft;
    property Indent: Integer read GetIndent;
  end;

implementation

{ TDBComposerTreeView }

function TDBComposerTreeView.GetIndent : Integer;
begin
  Result := inherited Indent;
end;

function TDBComposerTreeView.GetScrolledLeft : Integer;
begin
  Result := inherited ScrolledLeft;
end;

constructor TDBComposerTreeView.Create(AnOwner : TComponent);
begin
  inherited Create(AnOwner);
  NodeEditor := TEdit.Create(nil);
  NodeEditor.Parent := Self;
  NodeEditor.OnEditingDone:=@(EditorEditingDone);
  NodeEditor.OnKeyDown:=@(EditorKeyDown);
  NodeEditor.Visible := false;
  Editor := NodeEditor;
end;

destructor TDBComposerTreeView.Destroy;
begin
  Editor := nil;
  inherited Destroy;
  NodeEditor.Free;
end;

procedure TDBComposerTreeView.EndEditing(Cancel : boolean);
var NewText : string;
    Node : TTreeNode;
begin
  if Assigned(Editor) then begin
    NewText := Editor.Text;
    Editor.Visible := false;
  end else
    NewText := '';
  Editor := nil;
  Node:=EditingItem;

  inherited EndEditing(Cancel);

  Editor := NodeEditor;
  if not Cancel then begin
    if Assigned(Node) then begin
      if Assigned(OnEdited) then
        OnEdited(Self,Node,NewText);
      Node.Text:=NewText;
    end;
  end;
  if Assigned(OnEditingEnd) then OnEditingEnd(Self, Node, Cancel);
end;


end.

