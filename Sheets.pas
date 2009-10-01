unit Sheets;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, rxPlacemnt, Grids, ComCtrls, StdCtrls, ExtCtrls, Menus, Buttons, OmniXML,
  OmniXMLUtils, SimpleParser;

type
  TSheet = class;
  TSheetCellList = class;

  TCellType = (ctText, ctFormula, ctButton);
  TCellButtonState = (cstButtonUp, cstButtonDown);

  TSheetCell = class
    sheet: TSheet;
    row, col: integer;
    text, name, formula: string;
    value: double;
    NumberFormat: string;
    CellType: TCellType;
    CellButtonState: TCellButtonState;

    SourceCellsList, DependentCellsList: TSheetCellList;

    constructor Create;
    destructor Destroy; override;
    function DisplayText: string;
    procedure ParseText(txt: string);
  end;

  TSheetCellList = class(TList)
  private
  protected
    function GetItems(Index: Integer): TSheetCell;
    procedure SetItems(Index: Integer; ASheetCell: TSheetCell);
  public
    function Add(ASheetCell: TSheetCell): Integer;
    function Extract(Item: TSheetCell): TSheetCell;
    function Remove(ASheetCell: TSheetCell): Integer;
    function IndexOf(ASheetCell: TSheetCell): Integer;
    function First: TSheetCell;
    function Last: TSheetCell;
    procedure Insert(Index: Integer; ASheetCell: TSheetCell);
    property Items[Index: Integer]: TSheetCell read GetItems write SetItems; default;
    procedure ClearAll;
    function IndexFromName(aName: string): integer;
  end;

  TSheet = class
    name: string;
    CellList: TSheetCellList;
    SGrid: TSTringGrid;
    DefaultSheetCell: TSheetCell;
    Parser: TSimpleParser;

    constructor Create;
    destructor Destroy; override;

    function Cell(r, c: integer): TSheetCell;
    function EditCell(r, c: integer): TSheetCell;
  end;


type
  TFSheets = class(TForm)
    PanelFormula: TPanel;
    CBNames: TComboBox;
    EditFormula: TEdit;
    StatusBar: TStatusBar;
    PageControl: TPageControl;
    TabGlobal: TTabSheet;
    SGGlobal: TStringGrid;
    FormStorage: TFormStorage;
    MainMenu1: TMainMenu;
    PopupMenu: TPopupMenu;
    MenuButton: TMenuItem;
    SpeedButtonOK: TSpeedButton;
    MenuFile: TMenuItem;
    MenuSave: TMenuItem;
    MenuReLoad: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SGGlobalMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SGGlobalDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure SGGlobalMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure MenuButtonClick(Sender: TObject);
    procedure SpeedButtonOKClick(Sender: TObject);
    procedure MenuSaveClick(Sender: TObject);
    procedure MenuReLoadClick(Sender: TObject);
    procedure SGGlobalKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditFormulaKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SGGlobalKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SGGlobalKeyPress(Sender: TObject; var Key: Char);
    procedure EditFormulaExit(Sender: TObject);
  private
    procedure FillHeaders(SGrid: TStringGrid);
    //procedure ParseFormulaText;
    { Private declarations }
  public
    ActSheet: TSheet;
    Last_x, Last_y: integer;
    Last_r, Last_c: integer;
    MouseDown: boolean;
    Escaping: boolean;

    procedure SetRCString(r, c: integer; s: string);

    procedure SaveSheet(XMLFile: string; Sheet: TSheet);
    procedure LoadSheet(Sheet: TSheet; XMLFile: string);
  end;

var
  FSheets: TFSheets;

function GetRCValue(const v: array of double): double;

implementation

{$R *.dfm}

uses Viewer, ProjConfig;


function GetRCValue(const v: array of double): double;
var SheetCell: TSheetCell;
begin
  SheetCell :=  FSheets.ActSheet.Cell(round(v[0]), round(v[1]));
  //   TCellType = (ctText, ctFormula, ctButton);
  case SheetCell.CellType  of
    ctText:
      result := SheetCell.value;
    ctFormula:
      result := SheetCell.value;
    ctButton: begin
      result := SheetCell.value;
      SheetCell.value := 0; // Assuming that if is a button, it is not the defaultCell
    end;
    else
      result := 0;
  end;
end;

{ TSheetCellList }

function TSheetCellList.Add(ASheetCell: TSheetCell): Integer;
begin
  Result := inherited Add(ASheetCell);
end;

procedure TSheetCellList.ClearAll;
var i: integer;
begin
  For i := 0 to count-1 do begin
    GetItems(i).Free;
  end;
  clear;
end;

function TSheetCellList.Extract(Item: TSheetCell): TSheetCell;
begin
  Result := TSheetCell(inherited Extract(Item));
end;

function TSheetCellList.First: TSheetCell;
begin
  Result := TSheetCell(inherited First);
end;

function TSheetCellList.GetItems(Index: Integer): TSheetCell;
begin
  Result := TSheetCell(inherited Items[Index]);
end;

function TSheetCellList.IndexFromName(aName: string): integer;
var i: integer;
begin
  result := -1;
  for i := 0 to Count - 1 do begin
    if Items[i].Name = aName then begin
      result := i;
      exit;
    end;
  end;
end;

function TSheetCellList.IndexOf(ASheetCell: TSheetCell): Integer;
begin
  Result := inherited IndexOf(ASheetCell);
end;

procedure TSheetCellList.Insert(Index: Integer; ASheetCell: TSheetCell);
begin
  inherited Insert(Index, ASheetCell);
end;

function TSheetCellList.Last: TSheetCell;
begin
  Result := TSheetCell(inherited Last);
end;

function TSheetCellList.Remove(ASheetCell: TSheetCell): Integer;
begin
  Result := inherited Remove(ASheetCell);
end;

procedure TSheetCellList.SetItems(Index: Integer; ASheetCell: TSheetCell);
begin
  inherited Items[Index] := ASheetCell;
end;


procedure TFSheets.SetRCString(r, c: integer; s: string);
begin
  ActSheet.SGrid.cells[c, r] := s;
end;



procedure TFSheets.FormCreate(Sender: TObject);
begin
  FormStorage.IniFileName := GetIniFineName;
  ActSheet := TSheet.Create;
end;

procedure TFSheets.FormDestroy(Sender: TObject);
begin
  ActSheet.Free;
end;

procedure TFSheets.FormShow(Sender: TObject);
begin
  ActSheet.SGrid := SGGlobal;
  FillHeaders(SGGlobal);
  if FileExists('Global.S2Sheet') then
    LoadSheet(ActSheet, 'Global.S2Sheet');
end;

procedure TFSheets.FormClose(Sender: TObject; var Action: TCloseAction);
begin
//
end;


procedure TFSheets.FillHeaders(SGrid: TStringGrid);
var i: integer;
begin
  for i := 0 to Sgrid.ColCount -1 do begin
    SGrid.Cells[i, 0] := inttostr(i);
  end;
  for i := 0 to Sgrid.RowCount -1 do begin
    SGrid.Cells[0, i] := inttostr(i);
  end;
end;


procedure TFSheets.SGGlobalMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var r, c: integer;
    Grid: TStringGrid;
    SheetCell: TSheetCell;
    RClkPoint: TPoint;
begin
  MouseDown := true;
  Last_x := x;
  Last_y := y;

  Grid:= TStringGrid(Sender);
  Grid.MouseToCell(x, y, c, r);
  if (c < 0) or (r < 0) then exit;  //  The user's right-click was not within a cell.

  Last_r := r;
  Last_c := c;
  //Grid.Cells[c, r] := 'x';
  SheetCell := ActSheet.Cell(r, c);

  if (Button = mbRight) then begin
    if (c < Grid.FixedCols) or (r < Grid.FixedRows) then exit; //  The user clicked on a fixed cell
    RClkPoint := Grid.ClientToScreen(Point(x, y));
    MenuButton.Checked := (SheetCell.CellType = ctButton);
    PopupMenu.Popup(RClkPoint.X, RClkPoint.Y);
  end;

  //if (Button = mbLEft)  and  (SheetCell.CellType = ctButton) then begin
  if SheetCell.CellType = ctButton then begin
    SheetCell.CellButtonState := cstButtonDown;
    grid.Invalidate;
  end;

  EditFormula.Text := SheetCell.DisplayText;
end;


procedure TFSheets.SGGlobalMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var //r, c: integer;
    Grid: TStringGrid;
    SheetCell: TSheetCell;
begin
  MouseDown := false;
  Grid:= TStringGrid(Sender);
  //Grid.MouseToCell(x, y, c, r);
  //SGGlobal.MouseToCell(last_x, last_y, c, r);
  //Grid.Cells[Last_c, Last_r] := '';
  SheetCell := ActSheet.Cell(Last_r, Last_c);

  if SheetCell.CellType = ctButton then begin
    SheetCell.CellButtonState := cstButtonUp;
    grid.Invalidate;
  end;

end;

procedure Sto_DrawCellText(const Canvas: TCanvas; const Rect: TRect;
  const Text: String; const BackColor, TextColor: TColor;
  const Alignment: TAlignment);
const
  X_BORDER_WIDTH = 2;
  Y_BORDER_WIDTH = 1;
var
  iLeftBorder: Integer;
begin
  // calculate the left border
  iLeftBorder := 0;
  case Alignment of
    taLeftJustify : iLeftBorder := Rect.Left + X_BORDER_WIDTH;
    taRightJustify: iLeftBorder := Rect.Right - X_BORDER_WIDTH - Canvas.TextWidth(Text) -1;
    taCenter      : iLeftBorder := Rect.Left + (Rect.Right - Rect.Left - Canvas.TextWidth(Text)) div 2;
  end;
  // set colors
  Canvas.Font.Color := TextColor;
  Canvas.Brush.Color := BackColor;
  // paint the text
  //ExtTextOut(Canvas.Handle, iLeftBorder, Rect.Top + Y_BORDER_WIDTH, ETO_CLIPPED or ETO_OPAQUE,
  //           @Rect, PChar(Text), Length(Text), nil);
  //Canvas.Brush.Style := bsclear;
  Canvas.TextRect(Rect, iLeftBorder, Rect.Top + Y_BORDER_WIDTH, Text);
end;

procedure DrawButtonText(const Canvas: TCanvas; const Rect: TRect;
  const Text: String; const BackColor, TextColor: TColor; Pushed: boolean);
const
  X_BORDER_WIDTH = 2;
  Y_BORDER_WIDTH = 1;
var
  iLeftBorder: Integer;
begin
  // calculate the left border
  iLeftBorder := Rect.Left + (Rect.Right - Rect.Left - Canvas.TextWidth(Text)) div 2;
  // set colors
  Canvas.Font.Color := TextColor;
  Canvas.Brush.Color := BackColor;

  Canvas.Brush.Style := bsclear;
  // paint the text
  Canvas.TextRect(Rect, iLeftBorder + ord(Pushed), Rect.Top + Y_BORDER_WIDTH + ord(Pushed), Text);
end;


procedure TFSheets.SGGlobalDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  grdColored: TStringGrid;
  sText: String;
  myBackColor: TColor;
  myTextColor: TColor;
  myAlignment: TAlignment;
  SheetCell: TSheetCell;
begin
  grdColored := TStringGrid(Sender);
  SheetCell := ActSheet.Cell(Arow, Acol);
  // set default values for the parameters,
  // get the values depending on the grid settings.
  sText := grdColored.Cells[ACol, ARow];
  myAlignment := taLeftJustify;
  if (ARow < grdColored.FixedRows) or (ACol < grdColored.FixedCols) then begin
    myBackColor := grdColored.FixedColor;
    myAlignment := taCenter;
    myTextColor := grdColored.Font.Color;
  end else begin
    myBackColor := grdColored.Color;
    // set your own values...
    myTextColor := clBtnText;
  end;
  // draw the text in the cell
  Sto_DrawCellText(grdColored.Canvas, Rect, sText, myBackColor, myTextColor, myAlignment);

  //if (Acol = 2) and (Arow = 2) then begin
  if SheetCell.CellType = ctButton then begin
    if SheetCell.CellButtonState = cstButtonDown then begin
      DrawFrameControl(grdColored.Canvas.Handle, Rect, DFC_BUTTON, DFCS_BUTTONPUSH or DFCS_PUSHED);
      DrawButtonText(grdColored.Canvas, Rect, SheetCell.text, myBackColor, myTextColor, true);
    end else begin
      DrawFrameControl(grdColored.Canvas.Handle, Rect, DFC_BUTTON, DFCS_BUTTONPUSH);
      DrawButtonText(grdColored.Canvas, Rect, SheetCell.text, myBackColor, myTextColor, false);
    end;
  end;
  //end;
end;



procedure TFSheets.MenuButtonClick(Sender: TObject);
var SheetCell: TSheetCell;
begin
  SheetCell := ActSheet.EditCell(Last_r, Last_c);
  if MenuButton.checked then begin
    SheetCell.CellType := ctButton;
    SheetCell.CellButtonState := cstButtonUp;
  end else begin
    // TODO: must reparse?
    SheetCell.CellType := ctText;
  end;
  //ActGrid.Cells[Last_c, Last_r] := '0';
end;


procedure TFSheets.SpeedButtonOKClick(Sender: TObject);
begin
  //ParseFormulaText;
  ActSheet.EditCell(Last_r, Last_c).ParseText(EditFormula.Text);
end;


procedure TFSheets.SaveSheet(XMLFile: string; Sheet: TSheet);
var XML: IXMLDocument;
    node, prop: IXMLElement;
    PI: IXMLProcessingInstruction;
    i: integer;
begin
  XML := CreateXMLDoc;
  PI := XML.CreateProcessingInstruction('xml', 'version="1.0"');
  XML.InsertBefore(PI, XML.DocumentElement);

  XML.DocumentElement := XML.CreateElement('sheet');

  // Save columns width if they are different from the default value
  node := XML.CreateElement('columns');
  XML.DocumentElement.AppendChild(node);

  prop := XML.CreateElement('defaultcol');
  prop.SetAttribute('width', format('%d',[Sheet.SGrid.DefaultColWidth]));
  node.AppendChild(prop);

  for i := 1 to Sheet.SGrid.ColCount - 1 do begin
    if Sheet.SGrid.ColWidths[i] = Sheet.SGrid.DefaultColWidth then continue;
    prop := XML.CreateElement('col');
    prop.SetAttribute('num', format('%d',[i]));
    prop.SetAttribute('width', format('%d',[Sheet.SGrid.ColWidths[i]]));
    node.AppendChild(prop);
  end;


  node := XML.CreateElement('cells');
  XML.DocumentElement.AppendChild(node);

  for i := 0 to Sheet.CellList.Count - 1 do begin
    prop := XML.CreateElement('cell');
    prop.SetAttribute('r', format('%d',[Sheet.CellList[i].row]));
    prop.SetAttribute('c', format('%d',[Sheet.CellList[i].col]));
    prop.SetAttribute('text', format('%s',[Sheet.CellList[i].text]));
    node.AppendChild(prop);
  end;

  XML.Save(XMLFile, ofIndent);
end;


procedure TFSheets.LoadSheet(Sheet: TSheet; XMLFile: string);
var XML: IXMLDocument;
    root, node, prop: IXMLNode;
    w, num: integer;
    r, c: integer;
    s: string;
begin
  XML := LoadXML(XMLFile, nil);
  if XML = nil then exit;

  root:=XML.SelectSingleNode('/sheet');
  if root = nil then exit;

  node := root.FirstChild;
  while node <> nil do begin
    if node.NodeName = 'columns' then begin
      prop := node.FirstChild;
      while prop <> nil do begin
        // default values

        if prop.NodeName = 'defaultcol' then begin
          w := Sheet.Sgrid.ColWidths[0]; //Preserve fixed column width
          Sheet.SGrid.DefaultColWidth := GetNodeAttrInt(prop, 'width', Sheet.SGrid.DefaultColWidth);
          Sheet.Sgrid.ColWidths[0] := w;
        end else if prop.NodeName = 'col' then begin
          num := GetNodeAttrInt(prop, 'num', -1);
          w := GetNodeAttrInt(prop, 'width', Sheet.SGrid.DefaultColWidth);
          if num > 0 then begin
            Sheet.SGrid.ColWidths[num] := w;
          end;
        end;

        prop := prop.NextSibling;
      end;

    end else if node.NodeName = 'cells' then begin
      prop := node.FirstChild;
      while prop <> nil do begin
        // default values

        if prop.NodeName = 'cell' then begin
          r := GetNodeAttrInt(prop, 'r', -1);
          c := GetNodeAttrInt(prop, 'c', -1);
          s := GetNodeAttrStr(prop, 'text', '');
          if (r >= 0) and (c >= 0) then begin
            Sheet.EditCell(r, c).ParseText(s);
          end;
        end;

        prop := prop.NextSibling;
      end;
    end;
    node := node.NextSibling;
  end;

end;

procedure TFSheets.MenuSaveClick(Sender: TObject);
begin
  SaveSheet('Global.S2Sheet', ActSheet);
end;

procedure TFSheets.MenuReLoadClick(Sender: TObject);
begin
  LoadSheet(ActSheet, 'Global.S2Sheet');
end;

{ TSheet }

function TSheet.Cell(r, c: integer): TSheetCell;
begin
  result := DefaultSheetCell;
  if SGrid.Objects[c, r] = nil then exit;
  result := TSheetCell(SGrid.Objects[c, r]);
end;


constructor TSheet.Create;
begin
  DefaultSheetCell := TSheetCell.Create;
  CellList:= TSheetCellList.Create;
  Parser := TSimpleParser.Create;
  Parser.RegisterFunction('RC', @getRCValue, 2);
end;

destructor TSheet.Destroy;
begin
  Parser.free;
  CellList.ClearAll;
  CellList.Free;
  DefaultSheetCell.Free;
  inherited;
end;

function TSheet.EditCell(r, c: integer): TSheetCell;
begin
  if SGrid.Objects[c, r] <> nil then begin
    // if it was already alocated use it
    Result := TSheetCell(SGrid.Objects[c, r]);
  end else begin
    // Must create a new one and add it to the list
    Result := TSheetCell.Create;
    CellList.Add(result);
    SGrid.Objects[c, r] := Result;
    Result.sheet := self;
    Result.row := r;
    Result.col := c;
  end;
end;

{ TSheetCell }

constructor TSheetCell.Create;
begin
  row := -1;
  col := -1;
  NumberFormat := '%g';
  SourceCellsList := TSheetCellList.Create;
  DependentCellsList := TSheetCellList.Create;
end;

destructor TSheetCell.Destroy;
begin
  SourceCellsList.Free;
  DependentCellsList.Free;

  inherited;
end;

function TSheetCell.DisplayText: string;
begin
  if CellType = ctFormula then begin
    result := '=' + formula;
  end else begin
    result := text;
  end;
end;


procedure TFSheets.SGGlobalKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = VK_RETURN then
    EditFormula.SetFocus;

end;

procedure TFSheets.EditFormulaKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = VK_RETURN then begin
    //ParseFormulaText;
    ActSheet.EditCell(Last_r, Last_c).ParseText(EditFormula.Text);
    ActSheet.SGrid.SetFocus;
  end else if key = VK_ESCAPE then begin
    Escaping := true;
    ActSheet.SGrid.SetFocus;
  end;
end;


procedure TFSheets.SGGlobalKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Last_c := ActSheet.SGrid.Selection.Left;
  Last_r := ActSheet.SGrid.Selection.Top;

  EditFormula.Text := ActSheet.Cell(Last_r, Last_c).DisplayText;
end;

procedure TFSheets.SGGlobalKeyPress(Sender: TObject; var Key: Char);
begin
  if ord(key) < ord(' ') then exit;
  EditFormula.SetFocus;
  EditFormula.Text := key;
  EditFormula.SelStart := maxint;
end;

procedure TSheetCell.ParseText(txt: string);
var s: string;
begin
  text := txt;
  s := trim(txt);
  if (s <> '') and (s[1] = '=') then begin
    formula := copy(s, 2, maxint);
    value := Sheet.Parser.Calc(formula);
    Sheet.SGrid.Cells[col, row] := format(NumberFormat, [value]);
    CellType := ctFormula;
  end else begin
    value := strToFloatDef(s, 0);
    Sheet.SGrid.Cells[col, row] := text;
    CellType := ctText;
  end;
end;

procedure TFSheets.EditFormulaExit(Sender: TObject);
begin
  if not escaping then
    ActSheet.EditCell(Last_r, Last_c).ParseText(EditFormula.Text);
  Escaping := false;
end;

end.


