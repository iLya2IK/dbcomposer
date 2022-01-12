{
 SynExportWordWrap:
   TSynCustomExporter for automatic word wrapping

   Part of dbComposer project

   Copyright (c) 2021-2022 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit SynExportWordWrap;

interface

uses
  Classes,
  LCLIntf, Graphics,
  SynEditHighlighter, SynEditExport;

type
  { TSynExporterWordWrap }

  TSynExporterWordWrap = class(TSynCustomExporter)
  private
    FColSize : Integer;
    FColPos  : Integer;
    FWaitForQuoteToken : Boolean;
    FCurTokenIsSpace, FCurTokenIsString, FLastTokenIsString : Boolean;
    FStartQuote : String;
  protected
    procedure FormatBeforeFirstAttribute({%H-}BackgroundChanged,
      {%H-}ForegroundChanged: boolean; {%H-}FontStylesChanged: TFontStyles);
      override;
    procedure FormatAttributeDone({%H-}BackgroundChanged, {%H-}ForegroundChanged: boolean;
      {%H-}FontStylesChanged: TFontStyles); override;
    procedure FormatAttributeInit({%H-}BackgroundChanged, {%H-}ForegroundChanged: boolean;
      {%H-}FontStylesChanged: TFontStyles); override;
    procedure FormatAfterLastAttribute; override;

    procedure FormatBeforeFirstAttributeImmediate({%H-}BG, {%H-}FG: TColor); override;
    procedure FormatAttributeInitImmediate({%H-}Attri: TSynHighlighterAttributes; IsSpace: Boolean); override;
    procedure FormatAfterLastAttributeImmediate; override;
    procedure FormatAttributeDoneImmediate({%H-}Attri: TSynHighlighterAttributes; {%H-}IsSpace: Boolean); override;

    procedure FormatToken(Token: string); override;
    procedure FormatNewLine; override;

    function GetFooter: string; override;
    function GetHeader: string; override;

    function  GetFormatName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ColSize: Integer read fColSize write fColSize;
    property Highlighter;
  end;

implementation

uses
  SysUtils, LazUtf8;

const SYNS_ExporterFormatWordWrap = 'Exporter Format WordWrap';

{ TSynExporterWordWrap }

constructor TSynExporterWordWrap.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColSize := 80;
  FColPos := 0;
  ImmediateAttrWrite := true;
  FCurTokenIsSpace := false;
  FCurTokenIsString := false;
  FWaitForQuoteToken := false;
  FStartQuote := '';
end;

procedure TSynExporterWordWrap.FormatBeforeFirstAttribute({%H-}BackgroundChanged,
  {%H-}ForegroundChanged: boolean; {%H-}FontStylesChanged: TFontStyles);
begin
  //
end;

procedure TSynExporterWordWrap.FormatAttributeDone({%H-}BackgroundChanged,
  {%H-}ForegroundChanged: boolean; {%H-}FontStylesChanged: TFontStyles);
begin
  //
end;

procedure TSynExporterWordWrap.FormatAttributeInit({%H-}BackgroundChanged,
  {%H-}ForegroundChanged: boolean; {%H-}FontStylesChanged: TFontStyles);
begin
  //
end;

procedure TSynExporterWordWrap.FormatAfterLastAttribute;
begin
  //
end;

procedure TSynExporterWordWrap.FormatBeforeFirstAttributeImmediate({%H-}BG,
  {%H-}FG: TColor);
begin
  FCurTokenIsSpace := false;
  FColPos := 0;
end;

procedure TSynExporterWordWrap.FormatAttributeInitImmediate(
  {%H-}Attri: TSynHighlighterAttributes; IsSpace: Boolean);
var B : Boolean;
begin
  FCurTokenIsSpace := IsSpace;
  B := Attri = Highlighter.StringAttribute;
  FLastTokenIsString := FCurTokenIsString;
  if B <> FCurTokenIsString then
  begin
    FCurTokenIsString := B;
    FWaitForQuoteToken := B;
  end;
end;

procedure TSynExporterWordWrap.FormatAfterLastAttributeImmediate;
begin
  //
end;

procedure TSynExporterWordWrap.FormatAttributeDoneImmediate(
  {%H-}Attri: TSynHighlighterAttributes; {%H-}IsSpace: Boolean);
begin
  //
end;

procedure TSynExporterWordWrap.FormatToken(Token: string);
var L : Integer;
begin
  if FWaitForQuoteToken then
  begin
    FWaitForQuoteToken := false;
    FStartQuote := UTF8Copy(Token, 1, 1);
  end;

  L := UTF8Length(Token);
  if FCurTokenIsString and FLastTokenIsString then
    L := L + UTF8Length( FStartQuote );
  if (L + FColPos) > FColSize then
  begin
    if not FCurTokenIsSpace then
    begin
      if (L > FColSize) then
      begin
        if (FColPos > L) then
          FormatNewLine;
      end else
          FormatNewLine;
    end;
  end;
  Inc(FColPos, L);
  inherited FormatToken(Token);
end;

procedure TSynExporterWordWrap.FormatNewLine;
begin
  FColPos := 0;
  if FCurTokenIsString and FLastTokenIsString then
    AddData( FStartQuote );
  AddData(#13#10);
  if FCurTokenIsString and FLastTokenIsString then
    AddData( FStartQuote );
end;

function TSynExporterWordWrap.GetFooter: string;
begin
  Result := '';
end;

function TSynExporterWordWrap.GetHeader: string;
begin
  Result := '';
end;

function TSynExporterWordWrap.GetFormatName: string;
begin
  Result := SYNS_ExporterFormatWordWrap;
end;

end.

