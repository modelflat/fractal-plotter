unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Menus, TeEngine, Series, ExtCtrls, TeeProcs,
  Chart, VarCmplx, Math, SettingsForm, DataStorage, MinkovskyDim;

type
  TForm1 = class(TForm)
    Chart1: TChart;
    pointer: TPointSeries;
    initialVertices: TPointSeries;
    algoPoints: TPointSeries;
    clear_btn: TButton;
    iterationsCount_edt: TLabeledEdit;
    drawFast_btn: TButton;
    setVertices_btn: TButton;
    showVertices_cb: TCheckBox;
    showPointer_cb: TCheckBox;
    m_parameter: TLabeledEdit;
    setSP_btn: TButton;
    save_btn: TButton;
    load_btn: TButton;
    savdlg: TSaveDialog;
    opndlg: TOpenDialog;
    Label2: TLabel;
    mode_gb: TGroupBox;
    mode_poly: TRadioButton;
    mode_affine: TRadioButton;
    logs: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure clear_btnClick(Sender: TObject);
    procedure iterationsCount_edtKeyPress(Sender: TObject; var Key: Char);
    procedure drawFast_btnClick(Sender: TObject);
    procedure showPointer_cbClick(Sender: TObject);
    procedure showVertices_cbClick(Sender: TObject);
    procedure setSP_btnClick(Sender: TObject);
    procedure m_parameterKeyPress(Sender: TObject; var Key: Char);
    procedure setVertices_btnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure mode_affineClick(Sender: TObject);
    procedure mode_polyClick(Sender: TObject);
    procedure save_btnClick(Sender: TObject);
    procedure load_btnClick(Sender: TObject);
  private
    { Private declarations }
  public
    settings : TSettings;
    pointerCoords : array of Variant;
    pointersCount : integer;
    setVertUsed : boolean;
    setIFSUsed : boolean;
    calcdAffine_precision : integer;
    procedure drawPointer;
    procedure drawVertices;
    procedure getNextPolygonal;
    procedure getNextAffine;
    procedure playGameFast;
    procedure grabSettings;
    procedure applySettings;
    procedure log(s : string; append : boolean);
    function saveChartImage(filename : string) : TBitmapPointer;
    function calcd : double;
    function getRandomAT : Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.grabSettings;
begin
    if mode_affine.Checked then
    begin
        settings.mode := mAffineTransform;
        settings.getNextFunc := @TForm1.getNextAffine;
    end
    else
    begin
        settings.mode := mPolygonal;
        settings.getNextFunc := @TForm1.getNextPolygonal;
    end;
    settings.iterationsCount := StrToInt(iterationsCount_edt.Text);
    settings.m := StrToFloat(m_parameter.Text);
    settings.showVertices := showVertices_cb.Checked;
    settings.showPointer := showPointer_cb.Checked;
end;

procedure TForm1.applySettings;
begin
    iterationsCount_edt.Text := IntToStr(settings.iterationsCount);
    showPointer_cb.Checked := settings.showPointer;
    showVertices_cb.Checked := settings.showVertices;
    if settings.mode = mPolygonal then
    begin
        settings.getNextFunc := @TForm1.getNextPolygonal;
        mode_poly.Checked := true;
        settings.atCount := 0;
        showVertices_cb.Visible := true;
        m_parameter.Visible := true;
    end
    else
    begin
        settings.getNextFunc := @TForm1.getNextAffine;
        mode_affine.Checked := true;
        settings.verticesCount := 0;
        m_parameter.Visible := false;
        showVertices_cb.Visible := false;
    end;
    m_parameter.Text := FloatToStr(settings.m);
    drawPointer;
    drawVertices;
end;

function TForm1.getRandomAT : integer;
var i : integer;
    x, prob: double;
begin
    x := Random;
    prob := settings.at[0].p;
    for i := 0 to settings.atCount - 1 do
        if x < prob then
        begin
            getRandomAT := i;
            Exit;
        end
        else prob := prob + settings.at[i+1].p;
    getRandomAT := settings.atCount;
end;

procedure TForm1.getNextAffine;
var at : TAffineTransform;
    i : integer;
    x : double;
begin
    for i := 0 to pointersCount-1 do
    begin
        x := pointerCoords[i].Real;
        at := settings.at[getRandomAT];
        pointerCoords[i].Real := at.a * pointerCoords[i].Real +
            at.b * pointerCoords[i].Imaginary + at.e;
        pointerCoords[i].Imaginary := at.c * x +
            at.d * pointerCoords[i].Imaginary + at.f;
    end;
end;

procedure TForm1.getNextPolygonal;
var i : integer;
begin
    for i := 0 to pointersCount-1 do
        with settings do
            pointerCoords[i] := 1/m*pointerCoords[i] +
                (m-1)/m*vertices[Random(verticesCount)];
end;

procedure TForm1.playGameFast;
var i, j: integer;
    fNext : System.Pointer;
begin
    fNext := settings.getNextFunc;
    log('Drawing...', false);
    for i := 1 to settings.iterationsCount do
    begin
        for j := 0 to pointersCount-1 do
            algoPoints.AddXY(pointerCoords[j].Real, pointerCoords[j].Imaginary);
        asm
            mov     eax,ebx
            call    fNext
        end;
    end;
    log('Computing D (this may take a while!)...', true);
    log('D: ' + FloatToStr(calcd), false);
    log('Total points: ' + IntToStr(algoPoints.Count), true);
end;

procedure TForm1.drawVertices;
var i : integer;
begin
    if not settings.showVertices then Exit;
    if initialVertices.Count > 0 then initialVertices.Clear;
    for i := 0 to settings.verticesCount-1 do
        initialVertices.AddXY(settings.vertices[i].Real, settings.vertices[i].Imaginary);
end;

procedure TForm1.drawPointer;
var i : integer;
begin
    if not settings.showPointer then Exit;
    if pointer.count > 0 then pointer.Clear;
    if settings.resetPointer then
    begin
        settings.resetPointer := false;
        pointersCount := settings.startPointsCount;
        SetLength(pointerCoords, settings.startPointsCount);
        for i := 0 to settings.startPointsCount-1 do
            pointerCoords[i] := settings.startPoints[i];
    end;
    for i := 0 to pointersCount-1 do
        pointer.AddXY(pointerCoords[i].Real, pointerCoords[i].Imaginary);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    calcdAffine_precision := 8;
    if FileExists('tests\StartingTest.cgmf') then
    begin
        loadSettingsFromFile(@settings, 'tests\StartingTest.cgmf');
        applySettings;
        playGameFast;
        drawPointer;
    end;
end;

procedure TForm1.clear_btnClick(Sender: TObject);
begin
    algoPoints.Clear;
    logs.Lines.Clear;
end;

procedure TForm1.m_parameterKeyPress(Sender: TObject; var Key: Char);
begin
    if not (Key in ['0'..'9',#8,',']) then key := #0;
end;

procedure TForm1.iterationsCount_edtKeyPress(Sender: TObject; var Key: Char);
begin
    if not (Key in ['0'..'9',#8]) then key := #0;
end;

procedure TForm1.drawFast_btnClick(Sender: TObject);
begin
    grabSettings;
    if settings.mode = mPolygonal then drawVertices;
    playGameFast;
    drawPointer;
end;

procedure TForm1.showPointer_cbClick(Sender: TObject);
begin
    if showPointer_cb.Checked then
    begin
        settings.showPointer := true;
        drawPointer;
    end
    else
    begin
        pointer.Clear;
        settings.showPointer := false;
    end;
end;

procedure TForm1.showVertices_cbClick(Sender: TObject);
begin
    if showVertices_cb.Checked then
    begin
        settings.showVertices := true;
        drawVertices;
    end
    else
    begin
        initialVertices.Clear;
        settings.showVertices := false;
    end;
end;

procedure TForm1.setSP_btnClick(Sender: TObject);
begin
    Form2.setMode(mSetStartPoints, @settings);
    Form2.Show;
    Form2.Enabled := true;
end;

procedure TForm1.setVertices_btnClick(Sender: TObject);
begin
    if mode_poly.Checked then
        Form2.setMode(mSetVertices, @settings)
    else
        Form2.setMode(mSetIFS, @settings);
    Form2.Show;
    Form2.Enabled := true;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
    if (settings.needRedrawVertices and settings.showVertices) then
    begin
        settings.needRedrawVertices := false;
        drawVertices;
    end;
    if (settings.needRedrawPointers and settings.showPointer) then
    begin
        settings.needRedrawPointers := false;
        drawPointer;
    end;
end;

procedure TForm1.mode_affineClick(Sender: TObject);
begin
    setVertices_btn.Caption := 'Set IFS...';
    settings.verticesCount := 0;
    showVertices_cb.Visible := false;
    m_parameter.Visible := false;
    settings.showVertices := false;
    showVertices_cb.Checked := false;
    initialVertices.Clear;
end;

procedure TForm1.mode_polyClick(Sender: TObject);
begin
    setVertices_btn.Caption := 'Set vertices...';
    settings.atCount := 0;
    showVertices_cb.Visible := true;
    m_parameter.Visible := true;
end;

procedure TForm1.save_btnClick(Sender: TObject);
begin
    if savdlg.Execute then saveSettingsToFile(@settings, savdlg.FileName);
end;

procedure TForm1.load_btnClick(Sender: TObject);
begin
    if opndlg.Execute then
    begin
        loadSettingsFromFile(@settings, opndlg.FileName);
        applySettings;
        algoPoints.Clear;
        logs.Lines.Clear;
    end;
end;

function TForm1.calcd : Double;
begin
    case settings.mode of
    mPolygonal:
        calcd := ln(settings.verticesCount) / ln(settings.m);
    mAffineTransform:
        calcd := BoxCountingDimension(saveChartImage('')^);
    else
        calcd := 0;
    end;
end;

procedure TForm1.log(s : string; append: boolean);
begin
    if append then
        logs.Lines.Add(s)
    else
    begin
        logs.Lines.Clear;
        logs.Lines.Add(s);
    end;
end;

function TForm1.saveChartImage(filename : string) : TBitmapPointer;
var bm : TBitmap;
begin
    bm := TBitmap.Create;
    bm.PixelFormat := pf24bit;
    bm.Width := Chart1.ClientWidth;
    bm.Height := Chart1.ClientHeight;
    Chart1.Frame.Visible := False;
    Chart1.PrintPartialCanvas(bm.Canvas, Chart1.ClientRect);
    Chart1.Frame.Visible := True;
    if filename <> '' then
        bm.SaveToFile(filename);
    saveChartImage := @bm;
end;

end.

