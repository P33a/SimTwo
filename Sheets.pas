unit Sheets;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, rxPlacemnt, Grids, ComCtrls, StdCtrls, ExtCtrls, Menus, Buttons, OmniXML,
  OmniXMLUtils, SimpleParser, math, dynmatrix;

type
  TSheet = class;
  TSheetCellList = class;

  TCellType = (ctText, ctFormula, ctButton);
  TCellButtonState = (cstButtonUp, cstButtonDown);

  TSheetCell = class
    sheet: TSheet;
    row, col: integer;
    text, name, expression: string;    // Text: the raw edit text
    CompiledExpr: TStringList;
    value: double;
    NumberFormat: string;
    CellType: TCellType;
    CellButtonState: TCellButtonState;

    //SourceCellsList, DependentCellsList: TSheetCellList;
    SourceCellsList: TStringList;
    level: integer;

    constructor Create;
    destructor Destroy; override;
    function DisplayText_: string;
    procedure ParseText(txt: string);
    function CalcSourceLevel: integer;
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
    function IndexFromRC(aRow, aCol: integer): integer;
  end;

  TSheet = class
    name: string;
    CellList: TSheetCellList; // List of all, non empty, cells (owns each TSheetCell)
    SGrid: TSTringGrid;
    DefaultSheetCell: TSheetCell;  // Empty TSheetCell
    Parser: TSimpleParser;
    TmpSourceCells: TStringList;   // "Global" that holds the SourceList that is being filled by the parser activity
    TmpSourceCellsIdx: integer;    // "Global" position in the SourceList for the dependency checks
    MustRebuildSourceCells: boolean; // "Global" that signals a change in the source cells list
    CalcSequence: TSheetCellList;    // Recalc sequence that only evaluates each cell once  (does not owns each TSheetCell)

    constructor Create;
    destructor Destroy; override;

    function Cell(r, c: integer): TSheetCell;
    function EditCell(r, c: integer): TSheetCell;
    procedure BuildCalcSequence;
    procedure ShowCalcSequence(SL: TStrings);
    function ReCalc: integer;
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
    MainMenu: TMainMenu;
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
    procedure PanelFormulaClick(Sender: TObject);
  private
    procedure FillHeaders(SGrid: TStringGrid);
  public
    ActSheet: TSheet;
    Last_x, Last_y: integer;
    Last_r, Last_c: integer;
    MouseDown: boolean;
    Escaping: boolean;


    procedure SaveSheet(XMLFile: string; Sheet: TSheet);
    procedure LoadSheet(Sheet: TSheet; XMLFile: string);
    procedure EnterFormula;
  end;

var
  FSheets: TFSheets;

procedure SetRCValue(r, c: integer; s: string);
function GetRCValue(r, c: integer): double;
function RCButtonPressed(r, c: integer): boolean;
function RangeToMatrix(r, c, rows, cols: integer): TDMatrix;
procedure MatrixToRange(r, c: integer; const M: TDMatrix);
procedure RefreshSheets;

function ColFromPack(v: TObject): integer;
function RowFromPack(v: TObject): integer;

function RCValue(const v: array of double): double;

implementation

{$R *.dfm}

uses Viewer, ProjConfig;

// This function is regietered on the simpleparser to evaluate the RC(r,c) function
// EVIL: a global (SourceCells) is being used to create a side effect where when
//       there is an evaluation, the evaluated cell is added to the SourceCells list
// EVEN MORE EVIL: Another global (TmpSourceCellsIdx) is used to signal that the evaluation
//                 should check if the the source cells are the same or there was a change:
//                 In that case the calc sequence may be invalid and should be recalculated
function RCValue(const v: array of double): double;
var SheetCell: TSheetCell;
    r, c: integer;
    o: TObject;
begin
  r := round(v[0]);
  c := round(v[1]);
  SheetCell :=  FSheets.ActSheet.Cell(r, c);

  if FSheets.ActSheet.TmpSourceCells <> nil then begin
    if FSheets.ActSheet.TmpSourceCellsIdx = -1 then begin   // Building the dependency list;
      FSheets.ActSheet.TmpSourceCells.AddObject(format('%4d,%4d',[r, c]), TObject(((r and $FFFF) shl 16) or (c and $FFFF)));
    end else begin
      with FSheets.ActSheet do begin                        // Checking the dependency list;
        o := TmpSourceCells.Objects[TmpSourceCellsIdx];
        if (rowfromPack(o) <> r) or (colfromPack(o) <> c) then begin
          MustRebuildSourceCells := true;
          TmpSourceCells.Objects[TmpSourceCellsIdx] :=  TObject(((r and $FFFF) shl 16) or (c and $FFFF));
        end;
        inc(TmpSourceCellsIdx);
      end;
    end;
  end;

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

function TSheetCellList.IndexFromRC(aRow, aCol: integer): integer;
var i: integer;
begin
  result := -1;
  for i := 0 to Count - 1 do begin
    if (Items[i].row = aRow) and (Items[i].col = aCol) then begin
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


procedure SetRCValue(r, c: integer; s: string);
begin
  FSheets.ActSheet.EditCell(r, c).ParseText(s);
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
  StatusBar.Panels[0].Text := format('%3d: %3d',[1, 1]);
end;

procedure TFSheets.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveSheet('Global.S2Sheet', ActSheet);
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
var i, r, c: integer;
    Grid: TStringGrid;
    SheetCell: TSheetCell;
    o: Tobject;
//    RClkPoint: TPoint;
begin
  MouseDown := true;
  Last_x := x;
  Last_y := y;

  Grid:= TStringGrid(Sender);
  Grid.MouseToCell(x, y, c, r);
  StatusBar.Panels[0].Text := format('%3d: %3d',[r, c]);

  if (c < 0) or (r < 0) then exit;  //  The user's right-click was not within a cell.

  Last_r := r;
  Last_c := c;
  SheetCell := ActSheet.Cell(r, c);

  {if (Button = mbRight) then begin
    if (c < Grid.FixedCols) or (r < Grid.FixedRows) then exit; //  The user clicked on a fixed cell
    RClkPoint := Grid.ClientToScreen(Point(x, y));
    MenuButton.Checked := (SheetCell.CellType = ctButton);
    PopupMenu.Popup(RClkPoint.X, RClkPoint.Y);
  end;}

  if SheetCell.CellType = ctButton then begin
    SheetCell.CellButtonState := cstButtonDown;
    SheetCell.value := SheetCell.value + 1;
    grid.Invalidate;
  end;

  EditFormula.Text := SheetCell.Text;

  CBNames.Items.Clear;
  CBNames.Items.add(inttostr(SheetCell.SourceCellsList.Count));
  //CBNames.Items.AddStrings(SheetCell.SourceCellsList);
  for i := 0 to  SheetCell.SourceCellsList.Count -1 do begin
    o := SheetCell.SourceCellsList.Objects[i];
    CBNames.Items.add(format('(%d,%d)', [ rowFRomPack(o), colFromPack(o)]));
  end;

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

procedure MyDrawCellText(const Canvas: TCanvas; const Rect: TRect;
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
  if (SheetCell.CellType = ctText) or (SheetCell.CellType = ctFormula) then begin
    MyDrawCellText(grdColored.Canvas, Rect, sText, myBackColor, myTextColor, myAlignment);
  end else if SheetCell.CellType = ctButton then begin
    if SheetCell.CellButtonState = cstButtonDown then begin
      DrawFrameControl(grdColored.Canvas.Handle, Rect, DFC_BUTTON, DFCS_BUTTONPUSH or DFCS_PUSHED);
      DrawButtonText(grdColored.Canvas, Rect, sText, myBackColor, myTextColor, true);
    end else begin
      DrawFrameControl(grdColored.Canvas.Handle, Rect, DFC_BUTTON, DFCS_BUTTONPUSH);
      DrawButtonText(grdColored.Canvas, Rect, sText, myBackColor, myTextColor, false);
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
  EnterFormula; // 1st way on accepting an edited formula
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

  Sheet.BuildCalcSequence;
  while Sheet.ReCalc <> Sheet.CalcSequence.Count do begin
    Sheet.BuildCalcSequence;
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

procedure TSheet.BuildCalcSequence;
var i, lvl, FormulaCount, added: integer;
    SheetCell: TSheetCell;
begin
  CalcSequence.Clear;   // New start

  FormulaCount := 0;
  for i := 0 to CellList.Count -1 do begin
    CellList[i].level := 0;
    if CellList[i].CellType = ctFormula then begin        // We only need to recalc formulas,
      if CellList[i].SourceCellsList.Count > 0 then begin // that are dependent on other cells
        inc(FormulaCount);
        CellList[i].level := maxint;   // These are the cells that must be in the CalcSequence
      end;
    end;
  end;

  while CalcSequence.Count < FormulaCount do begin // We  must add FormulaCount cells to the CalcSequence list
    added := 0;
    for i := 0 to CellList.Count -1 do begin
      SheetCell := CellList[i];
      if SheetCell.level <> maxint then continue; // These cells already have been processed
      lvl := SheetCell.CalcSourceLevel;
      if lvl < maxint then begin      // If all the parents have level (that means they are on the list)
        CalcSequence.Add(SheetCell);  // then it can be on the list
        SheetCell.level := lvl + 1;   // And have level
        inc(added);
      end;
    end;
    if added = 0 then
      raise Exception.Create('Circular Reference');
  end;

end;

function TSheet.Cell(r, c: integer): TSheetCell;
begin
  result := DefaultSheetCell;
  if SGrid.Objects[c, r] = nil then exit;
  result := TSheetCell(SGrid.Objects[c, r]);
end;


constructor TSheet.Create;
begin
  DefaultSheetCell := TSheetCell.Create;
  CellList := TSheetCellList.Create;
  Parser := TSimpleParser.Create;
  Parser.RegisterFunction('RC', @RCValue, 2);
  CalcSequence := TSheetCellList.Create;
  TmpSourceCellsIdx := -1;
end;

destructor TSheet.Destroy;
begin
  CalcSequence.Free;
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

function TSheet.ReCalc: integer;
var i: integer;
    SheetCell: TSheetCell;
begin
  result := CalcSequence.Count;
  for i := 0 to CalcSequence.Count - 1 do begin
    SheetCell := CalcSequence[i];
    with SheetCell do begin
      //value := Sheet.Parser.Calc(expression);
        //value := Sheet.Parser.Run(CompiledExpr);
      try
        Sheet.TmpSourceCells := SourceCellsList;
        Sheet.TmpSourceCellsIdx := 0;
        MustRebuildSourceCells := false;
        value := Sheet.Parser.Calc(expression);
      finally
        Sheet.TmpSourceCells := nil;
        Sheet.TmpSourceCellsIdx := -1;
      end;

      Sheet.SGrid.Cells[col, row] := format(NumberFormat, [value]);

      if MustRebuildSourceCells then begin
        result := i;
        exit;
      end;

    end;
  end;
end;

procedure TSheet.ShowCalcSequence(SL: TStrings);
var i: integer;
    //SheetCell: TSheetCell;
begin
  with Fsheets do begin
    SL.Clear;
    SL.add(inttostr(CalcSequence.Count));
    for i := 0 to CalcSequence.Count -1 do begin
      SL.add(format('(%d,%d)', [ CalcSequence[i].row, CalcSequence[i].col]));
    end;
  end;
end;
{
procedure TSheet.ShowCalcSequence(SL: TStringList);
var i: integer;
    //SheetCell: TSheetCell;
begin
  with Fsheets do begin
    CBNames.Items.Clear;
    CBNames.Items.add(inttostr(CalcSequence.Count));
    for i := 0 to CalcSequence.Count -1 do begin
      CBNames.Items.add(format('(%d,%d)', [ CalcSequence[i].row, CalcSequence[i].col]));
    end;
  end;
end;
}
{ TSheetCell }

function RowFromPack(v: TObject): integer;
begin
  result := (integer(v) shr 16) and $FFFF;
end;

function ColFromPack(v: TObject): integer;
begin
  result := integer(v) and $FFFF;
end;

function TSheetCell.CalcSourceLevel: integer;
var i, row, col, idx: integer;
    o: TObject;
    SheetCell: TSheetCell;
begin
  result := 0;
  for i := 0 to SourceCellsList.Count -1 do begin
    o := SourceCellsList.Objects[i];
    row := RowFromPack(o);
    col := ColFromPack(o);
    idx := Sheet.CellList.IndexFromRC(row, col);
    if idx >= 0 then begin
      SheetCell := Sheet.CellList[idx];
    end else begin
      SheetCell := Sheet.DefaultSheetCell;
    end;
    result := max(result, SheetCell.level);
  end;
end;

constructor TSheetCell.Create;
begin
  row := -1;
  col := -1;
  NumberFormat := '%g';
  SourceCellsList := TStringList.Create;
  CompiledExpr := TStringList.Create;
  //SourceCellsList := TSheetCellList.Create;
  //DependentCellsList := TSheetCellList.Create;
end;

destructor TSheetCell.Destroy;
begin
  CompiledExpr.Free;
  SourceCellsList.Free;
  //DependentCellsList.Free;

  inherited;
end;

function TSheetCell.DisplayText_: string;
begin
  if CellType = ctFormula then begin
    result := '=' + expression;
  end else if CellType = ctButton then begin
    result := expression;
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

procedure TFSheets.EditFormulaKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var SelRect: TGridRect;
begin
  if key in [VK_RETURN, VK_DOWN, VK_UP] then begin // 2nd way of accepting an edited formula
    //ActSheet.EditCell(Last_r, Last_c).ParseText(EditFormula.Text);
    EnterFormula;
    ActSheet.SGrid.SetFocus;
    if (key = VK_DOWN) then begin
      SelRect := ActSheet.SGrid.Selection;
      if SelRect.Top < ActSheet.SGrid.RowCount then begin
        SelRect.Top := SelRect.Top + 1;
        SelRect.Bottom := SelRect.Bottom + 1;
        ActSheet.SGrid.Selection := SelRect;
      end;
    end;
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
  StatusBar.Panels[0].Text := format('%3d: %3d',[Last_r, Last_c]);

  EditFormula.Text := ActSheet.Cell(Last_r, Last_c).Text;
end;

procedure TFSheets.SGGlobalKeyPress(Sender: TObject; var Key: Char);
begin
  if ord(key) < ord(' ') then exit;  // Filter non printable codes
  EditFormula.SetFocus;
  EditFormula.Text := key;
  EditFormula.SelStart := maxint;
end;

procedure TSheetCell.ParseText(txt: string);
var s: string;
    len: integer;
begin
  if txt = text then exit; //No change!
  text := txt;
  s := trim(txt);
  len := length(s);

  if (len > 0) and (s[1] = '=') then begin
    expression := copy(s, 2, maxint);
    try
      SourceCellsList.Clear;  // Clean dependencies

      Sheet.TmpSourceCells := SourceCellsList;  // Pass it, to build the source cells for this expression
      Sheet.TmpSourceCellsIdx := -1;

      value := Sheet.Parser.Calc(expression);
      //Sheet.Parser.Compile(expression, CompiledExpr);
      //value := Sheet.Parser.Run(CompiledExpr);
    finally
      Sheet.TmpSourceCells := nil;
    end;
    Sheet.SGrid.Cells[col, row] := format(NumberFormat, [value]);
    CellType := ctFormula;
  end else if (len > 0) and (s[1] = '[') and (s[len] = ']') then begin
    expression := copy(s, 2, len - 2);
    value := 0;
    Sheet.SGrid.Cells[col, row] := expression;
    CellType := ctButton;
  end else begin
    expression := '';
    value := strToFloatDef(s, 0);
    Sheet.SGrid.Cells[col, row] := text;
    CellType := ctText;
  end;
end;

procedure TFSheets.EditFormulaExit(Sender: TObject);
begin
  if not escaping then // 3rd way of accepting an edited formula
    EnterFormula;
    //ActSheet.EditCell(Last_r, Last_c).ParseText(EditFormula.Text);
  Escaping := false;
end;

function GetRCValue(r, c: integer): double;
begin
  result := FSheets.ActSheet.Cell(r, c).value;
end;

function RCButtonPressed(r, c: integer): boolean;
var SheetCell: TSheetCell;
begin
  result := false;
  SheetCell := FSheets.ActSheet.Cell(r, c);

  if SheetCell.CellType = ctButton then begin
    if SheetCell.value > 0 then begin
      result := true;
      SheetCell.value := 0;
    end;
  end;
end;

function RangeToMatrix(r, c, rows, cols: integer): TDMatrix;
var ir, ic: integer;
begin
  result := Mzeros(rows, cols);
  for ir := 0 to rows - 1 do begin
    for ic := 0 to cols - 1 do begin
      Msetv(result, ir, ic, FSheets.ActSheet.Cell(r + ir, c + ic).value);
    end;
  end;
end;

procedure MatrixToRange(r, c: integer; const M: TDMatrix);
var ir, ic: integer;
begin
  for ir := 0 to M.rows - 1 do begin
    for ic := 0 to M.cols - 1 do begin
      FSheets.ActSheet.EditCell(r + ir, c + ic).ParseText(format('%g',[MGetv(M, ir ,ic)]));
    end;
  end;
end;


procedure TFSheets.PanelFormulaClick(Sender: TObject);
begin
  ActSheet.ShowCalcSequence(CBNames.Items);
  ActSheet.ReCalc;
  ActSheet.SGrid.Invalidate;
end;

procedure TFSheets.EnterFormula;
var i64_start, i64_end, i64_freq: int64;
begin
  ActSheet.EditCell(Last_r, Last_c).ParseText(EditFormula.Text);
  ActSheet.BuildCalcSequence;

  QueryPerformanceCounter(i64_start);

  //ActSheet.ReCalc;
  while ActSheet.ReCalc <> ActSheet.CalcSequence.Count do begin
    ActSheet.BuildCalcSequence;
  end;

  QueryPerformanceCounter(i64_end);
  QueryPerformanceFrequency(i64_freq);

  StatusBar.Panels[3].Text := format('%f',[1e6*(i64_end-i64_start)/i64_freq]);
//  ActSheet.SGrid.Invalidate;
end;

procedure RefreshSheets;
begin
  with FSheets.ActSheet do begin
    BuildCalcSequence;
    while ReCalc <> CalcSequence.Count do begin
      BuildCalcSequence;
    end;
  end;
end;


end.




