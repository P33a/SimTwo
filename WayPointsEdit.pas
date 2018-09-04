unit WayPointsEdit;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFWayPointsEdit = class(TForm)
    LBWayPoints: TListBox;
    BClose: TButton;
    Label1: TLabel;
    BAdd: TButton;
    BDelete: TButton;
    BInsert: TButton;
    BInterpolate: TButton;
    EditID: TEdit;
    EditFinalT: TEdit;
    Label2: TLabel;
    BSet: TButton;
    procedure FormShow(Sender: TObject);
    procedure BCloseClick(Sender: TObject);
    procedure LBWayPointsClick(Sender: TObject);
    procedure BSetClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    r: integer;
  end;

var
  FWayPointsEdit: TFWayPointsEdit;

implementation

uses Params, Viewer, ODERobots;

{$R *.lfm}

procedure TFWayPointsEdit.FormShow(Sender: TObject);
begin
  r := FParams.LBRobots.ItemIndex;
  if (r < 0) or (r >= WorldODE.Robots.Count) then exit;

  LBWayPoints.Clear;
  LBWayPoints.Items.AddStrings(FParams.ComboWayPointName.Items);
  LBWayPoints.ItemIndex := FParams.ComboWayPointName.ItemIndex;
  LBWayPointsClick(Sender);
end;

procedure TFWayPointsEdit.BCloseClick(Sender: TObject);
begin
  close;
end;

procedure TFWayPointsEdit.LBWayPointsClick(Sender: TObject);
var i: integer;
begin
  if WorldODE.Robots[r].Axes.Count = 0 then exit;
  i := LBWayPoints.ItemIndex;
  if (i < 0) then exit;
  EditID.Text := WorldODE.Robots[r].AxesWayPointsIDs[i];
  EditFinalT.Text := format('%.2f', [WorldODE.Robots[r].Axes[0].WayPoints[i].t]);
end;

procedure TFWayPointsEdit.BSetClick(Sender: TObject);
var i, j: integer;
    new_t: double;
begin
  if WorldODE.Robots[r].Axes.Count = 0 then exit;
  i := LBWayPoints.ItemIndex;
  if (i < 0) then exit;

  new_t := strtofloatdef(EditFinalT.Text, WorldODE.Robots[r].Axes[0].WayPoints[i].t);

  for j := 0 to WorldODE.Robots[r].Axes.Count -1 do begin
    WorldODE.Robots[r].Axes[j].WayPoints[i].t := new_t;
  end;
  WorldODE.Robots[r].AxesWayPointsIDs[i] := EditID.Text;

  FParams.ComboWayPointNameUpdate(WorldODE.Robots[r]);
  LBWayPoints.Items[i] := FParams.ComboWayPointName.items[i];
end;

procedure TFWayPointsEdit.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  FParams.ComboWayPointName.ItemIndex := LBWayPoints.ItemIndex;
end;

end.
