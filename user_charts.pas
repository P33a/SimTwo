unit user_charts;

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics, TAGraph, TASeries, FastChart;

procedure ChartSeriesAddXY(ChartIndex, SeriesIndex: integer; X, Y: double);
procedure ChartSeriesClear(ChartIndex, SeriesIndex: integer);

procedure ChartSetAxisMinMax(ChartIndex: integer; Xmin, Xmax, Ymin, Ymax: double);

procedure ChartSeriesSetColor(ChartIndex, SeriesIndex: integer; newColor: TColor);

function ChartSeriesGetCount(ChartIndex: integer): integer;
procedure ChartSeriesSetCount(ChartIndex, SeriesCount: integer);

function ChartSeriesCreate(ChartIndex: integer): integer;
procedure ChartSeriesDeleteAll(ChartIndex: integer);

implementation


procedure ChartSeriesAddXY(ChartIndex, SeriesIndex: integer; X, Y: double);
begin
  (UserCharts[ChartIndex].Series[SeriesIndex] as TLineSeries).AddXY(X, Y);
end;


procedure ChartSeriesClear(ChartIndex, SeriesIndex: integer);
begin
  (UserCharts[ChartIndex].Series[SeriesIndex] as TLineSeries).Clear;
end;


procedure ChartSetAxisMinMax(ChartIndex: integer; Xmin, Xmax, Ymin, Ymax: double);
var chart: TChart;
begin
  chart := UserCharts[ChartIndex];
  with Chart.AxisList[0].Range do begin
    UseMax := true;
    UseMin := true;
    Max := Ymax;
    Min := Ymin;
  end;

  with Chart.AxisList[1].Range do begin
    UseMax := true;
    UseMin := true;
    Max := Xmax;
    Min := Xmin;
  end;

end;


function ChartSeriesGetCount(ChartIndex: integer): integer;
var chart: TChart;
begin
  chart := UserCharts[ChartIndex];
  result := Chart.Series.Count;
end;


procedure ChartSeriesSetColor(ChartIndex, SeriesIndex: integer; newColor: TColor);
var LineSeries: TLineSeries;
    chart: TChart;
begin
  chart := UserCharts[ChartIndex];
  (UserCharts[ChartIndex].Series[SeriesIndex] as TLineSeries).SeriesColor := newColor;
end;


procedure ChartSeriesSetCount(ChartIndex, SeriesCount: integer);
var LineSeries: TLineSeries;
    chart: TChart;
    i: integer;
begin
  chart := UserCharts[ChartIndex];

  if Chart.Series.Count < SeriesCount then begin // Must create some series
    for i := Chart.Series.Count to SeriesCount - 1 do begin
      LineSeries := TLineSeries.Create(chart);
      chart.AddSeries(LineSeries);
    end;
  end;
  // TODO: safe way to remove some series
end;


function ChartSeriesCreate(ChartIndex: integer): integer;
var LineSeries: TLineSeries;
    chart: TChart;
begin
  chart := UserCharts[ChartIndex];
  LineSeries := TLineSeries.Create(chart);
  chart.AddSeries(LineSeries);
  result := Chart.Series.Count - 1;
end;


procedure ChartSeriesDeleteAll(ChartIndex: integer);
var chart: TChart;
begin
  chart := UserCharts[ChartIndex];
  chart.Series.Clear;
end;


end.

