unit SettingsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, VarCmplx, Grids, Math, DataStorage;

type TMode = (mSetIFS, mSetVertices, mSetStartPoints, mBeforeStart);

type
  TForm2 = class(TForm)
    apply: TButton;
    cancel: TButton;
    table: TStringGrid;
    addrow: TButton;
    remrow: TButton;
    regularPolygon: TButton;
    procedure applyClick(Sender: TObject);
    procedure cancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure addrowClick(Sender: TObject);
    procedure remrowClick(Sender: TObject);
    procedure regularPolygonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    settings : TSettingsPointer;
    mode, lastMode: TMode;
    print, parse : System.Pointer;
    procedure setMode(m : TMode; p : TSettingsPointer);
    procedure parseVertices;
    procedure printVertices;
    function parseIFS : boolean;
    procedure printIFS;
    procedure parseStartPoints;
    procedure printStartPoints;
    procedure handleHotKeys(var msg : TMessage); message WM_HOTKEY;
    procedure setColWidth;
    function toFloat(s : string) : double;
  end;

const ADDROW_HOTKEY = 0;
      APPLY_HOTKEY = 1;
      CANCEL_HOTKEY = 2;
      REMROW_HOTKEY = 3;
      _VK_M = $4D;
      _VK_Q = $51;
      _VK_R = $52;
      _VK_S = $53;

const REGULAR_POLYGON_X_CENTER = 0.5;
// R and Yc use this value as well

const COL_RESIZE_FIX = 1;
const INFO_COL_SIZE = 40;

var
  Form2: TForm2;

implementation

{$R *.dfm}

function TForm2.toFloat(s : string) : double;
begin
    if s = '' then
        toFloat := 0
    else
        toFloat := StrToFloat(s);
end;

procedure TForm2.handleHotKeys(var msg : TMessage);
begin
    if (msg.LParamLo and MOD_CONTROL <> 0) then
        case msg.LParamHi of
        _VK_M: addrow.Click; // M
        _VK_R: remrow.Click; // R
        _VK_Q: cancel.Click; // Q
        _VK_S: apply.Click; // S
        end;
end;

procedure TForm2.printIFS;
var i : integer;
begin
    for i := 1 to settings.atCount do
    begin
        table.Cells[0, i] := IntToStr(i);
        table.Cells[1, i] := FloatToStr(settings.at[i-1].a);
        table.Cells[2, i] := FloatToStr(settings.at[i-1].b);
        table.Cells[3, i] := FloatToStr(settings.at[i-1].c);
        table.Cells[4, i] := FloatToStr(settings.at[i-1].d);
        table.Cells[5, i] := FloatToStr(settings.at[i-1].e);
        table.Cells[6, i] := FloatToStr(settings.at[i-1].f);
        table.Cells[7, i] := FloatToStr(settings.at[i-1].p);
    end;
end;

function TForm2.parseIFS : boolean;
var i, j, res: integer;
    t : TAffineTransform;
    totalProbability, rest, prob: double;
begin
    //with Form2 do
    begin
        settings.atCount := table.RowCount - 1;
        SetLength(settings.at, settings.atCount);
        totalProbability := 0;
        for i := 1 to settings.atCount do
        begin
            settings.at[i-1].a := toFloat(table.Cells[1, i]);
            settings.at[i-1].b := toFloat(table.Cells[2, i]);
            settings.at[i-1].c := toFloat(table.Cells[3, i]);
            settings.at[i-1].d := toFloat(table.Cells[4, i]);
            settings.at[i-1].e := toFloat(table.Cells[5, i]);
            settings.at[i-1].f := toFloat(table.Cells[6, i]);
            prob := toFloat(table.Cells[7, i]);
            totalProbability := totalProbability + prob;
            if totalProbability < 1.0001 then
                settings.at[i-1].p := prob
            else
            begin
                settings.at[i-1].p := 0;
                table.Cells[7, i] := '0';
            end;
        end;
        if (totalProbability > 1.0001) then
        begin
            res := MessageBox(Form2.Handle, 'Total probability should be equal to 1.' +
                ' Now it`s greater than 1, ' + #13#10 +
                ' therefore several transformations cannot be performed.' + #13#10 +
                ' Do you want to save IFS as it is?', 'Error!', MB_YESNO);
            if res = IDNO then
            begin
                parseIFS := false;
                Exit;
            end;
        end;
        if totalProbability < 1 then
        begin
            res := MessageBox(Form2.Handle, 'Total probability should be equal to 1.' +
                ' Now it`s lesser than 1. ' + #13#10 +
                ' Do you want to distribute the rest of it (regularly,' +
                ' among all rows) automatically?', 'Error!', MB_YESNO);
            if res = IDYES then
            begin
                rest := (1 - totalProbability) / settings.atCount;
                for i := 0 to settings.atCount-1 do
                    settings.at[i].p := settings.at[i].p + rest;
            end
            else
            begin
                parseIFS := false;
                Exit;
            end;
        end;{
        for i := 0 to settings.atCount - 1 do
            for j := i to settings.atCount - 1 do
                if settings.at[i].p < settings.at[j].p then
                begin
                    t := settings.at[i];
                    settings.at[i] := settings.at[j];
                    settings.at[j] := t;
                end;    }
    end;
    parseIFS := true;
end;

procedure TForm2.printVertices;
var i : integer;
begin
    for i := 1 to settings.verticesCount do
    begin
        table.Cells[0, i] := IntToStr(i);
        table.Cells[1, i] := FloatToStr(settings.vertices[i-1].Real);
        table.Cells[2, i] := FloatToStr(settings.vertices[i-1].Imaginary);
    end;
end;

procedure TForm2.parseVertices;
var i : integer;
begin
    with Form2 do
    begin
        settings.verticesCount := table.RowCount - 1;
        SetLength(settings.vertices, settings.verticesCount);
        for i := 1 to settings.verticesCount do
        begin
            settings.vertices[i-1] := VarComplexCreate(
                StrToFloat(table.Cells[1, i]), StrToFloat(table.Cells[2, i]));
        end;
    end;
end;

procedure TForm2.printStartPoints;
var i : integer;
begin
    for i := 1 to settings.startPointsCount do
    begin
        table.Cells[0, i] := IntToStr(i);
        table.Cells[1, i] := FloatToStr(settings.startPoints[i-1].Real);
        table.Cells[2, i] := FloatToStr(settings.startPoints[i-1].Imaginary);
    end;
end;

procedure TForm2.parseStartPoints;
var i : integer;
begin
    with Form2 do
    begin
        settings.startPointsCount := table.RowCount - 1;
        settings.resetPointer := true;
        SetLength(settings.startPoints, settings.startPointsCount);
        for i := 1 to settings.startPointsCount do
        begin
            settings.startPoints[i-1] := VarComplexCreate(
                StrToFloat(table.Cells[1, i]), StrToFloat(table.Cells[2, i]));
        end;
    end;
end;

procedure TForm2.applyClick(Sender: TObject);
var fParse : System.Pointer;
    res : boolean;
begin
    fParse := parse;
    asm
        mov     eax, ebx
        call    fParse
        mov     [res], al
    end;
    if not res then Exit;
    if (mode = mSetVertices) then
    begin
        settings.needRedrawVertices := true;
        regularPolygon.Visible := false;
    end;
    if (mode = mSetStartPoints) then
        settings.needRedrawPointers := true;
    lastMode := mode;
    Form2.Hide;
    Form2.Enabled := False;
end;

procedure TForm2.cancelClick(Sender: TObject);
begin
    if (mode = mSetVertices) then
        regularPolygon.Visible := false;
    lastMode := mode;
    Hide;
    Form2.Enabled := False;
end;

procedure TForm2.setMode(m : TMode; p : TSettingsPointer);
var k : integer;
begin
    settings := p;
    mode := m;
    case mode of
    mSetIFS:
    begin
        Form2.Caption := 'Set IFS';
        table.Cells[1, 0] := 'a';
        table.Cells[2, 0] := 'b';
        table.Cells[3, 0] := 'c';
        table.Cells[4, 0] := 'd';
        table.Cells[5, 0] := 'e';
        table.Cells[6, 0] := 'f';
        table.Cells[7, 0] := 'p';
        table.ColCount := 8;
        parse := @TForm2.parseIFS;
        if settings.atCount = 0 then k := 2 else k := 1;
        table.RowCount := settings.atCount + k;
        printIFS;
    end;
    mSetVertices:
    begin
        regularPolygon.Visible := True;
        Form2.Caption := 'Set vertices';
        table.Cells[1, 0] := 'Real';
        table.Cells[2, 0] := 'Imaginary';
        table.ColCount := 3;
        parse := @TForm2.parseVertices;
        if settings.verticesCount = 0 then k := 2 else k := 1;
        table.RowCount := settings.verticesCount + k;
        printVertices;
    end;
    mSetStartPoints:
    begin
        Form2.Caption := 'Set start points';
        table.Cells[1, 0] := 'Real';
        table.Cells[2, 0] := 'Imaginary';
        table.ColCount := 3;
        parse := @TForm2.parseStartPoints;
        if settings.startPointsCount = 0 then k := 2 else k := 1;
        table.RowCount := settings.startPointsCount + k;
        printStartPoints;
    end;
    end;
    table.Cells[0, 1] := '1';
    setColWidth;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
    RegisterHotKey(Form2.Handle, ADDROW_HOTKEY, MOD_CONTROL, _VK_M);
    RegisterHotKey(Form2.Handle, REMROW_HOTKEY, MOD_CONTROL, _VK_R);
    RegisterHotKey(Form2.Handle, CANCEL_HOTKEY, MOD_CONTROL, _VK_Q);
    RegisterHotKey(Form2.Handle, APPLY_HOTKEY, MOD_CONTROL, _VK_S);
    lastMode := mBeforeStart;
end;

procedure TForm2.addrowClick(Sender: TObject);
begin
    table.RowCount := table.RowCount + 1; // inc(table.RowCount);
    table.Cells[0, table.RowCount - 1] := IntToStr(table.RowCount - 1);
end;

procedure TForm2.remrowClick(Sender: TObject);
begin
    if table.RowCount > 2 then
        table.RowCount := table.RowCount - 1; // dec(table.RowCount);
end;

procedure TForm2.regularPolygonClick(Sender: TObject);
var result : string;
    i, anglesCount, rottok: integer;
    xc, yc, r, phi: double;
    xi, yi : double;
begin
    xc := REGULAR_POLYGON_X_CENTER;
    yc := xc;
    r := xc;
    result := InputBox('Enter angles number', '', '');
    if not (result = '') then
    begin
        rottok := Pos(':', result); // polygon rotation
        if rottok <> 0 then
        begin
            phi := DegToRad(StrToInt(Copy(result, rottok+1, length(result)-rottok+1)));
            result := Copy(result, 1, rottok-1);
        end
        else
            phi := 0;
        anglesCount := StrToInt(result);
        if anglesCount < 2 then Exit;
        table.RowCount := anglesCount + 1;
        settings.verticesCount := anglesCount;
        SetLength(settings.vertices, anglesCount + 1);
        for i := 0 to anglesCount-1 do
        begin
            xi := xc + r*cos(phi + 2*Pi*i/anglesCount);
            yi := yc + r*sin(phi + 2*Pi*i/anglesCount);
            table.Cells[0, i+1] := IntToStr(i+1);
            table.Cells[1, i+1] := FloatToStr(xi);
            table.Cells[2, i+1] := FloatToStr(yi);
            settings.vertices[i] := VarComplexCreate(xi, yi);
        end;
        //needParse := false;
    end;
end;

procedure TForm2.setColWidth;
var i, a: integer;
begin
    a := (table.Width - INFO_COL_SIZE) div table.ColCount;
    for i := 1 to table.ColCount-1 do
        table.ColWidths[i] := a;
    table.ColWidths[0] := table.ColWidths[0] - COL_RESIZE_FIX;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
    UnregisterHotKey(Form2.Handle, APPLY_HOTKEY);
    UnregisterHotKey(Form2.Handle, ADDROW_HOTKEY);
    UnregisterHotKey(Form2.Handle, CANCEL_HOTKEY);
    UnregisterHotKey(Form2.Handle, REMROW_HOTKEY);
end;

end.
