unit MinkovskyDim;

interface

uses Math, Graphics;

type TBitmapPointer = ^TBitmap;

type Point = record
    x : double;
    y : double;
end;

function BoxCountingDimension(bm : TBitmap): double;

implementation

function normalize(a : integer) : integer;
begin
    if a = 0 then
        normalize := 0
    else
        normalize := 1;
end;

function clcmp_rgb(cl1, cl2: TColor): boolean;
begin
    clcmp_rgb := (cl1 and $00FFFFFF) = (cl2 and $00FFFFFF);
end;

function NormalEquations2D(x: array of double; y : array of double): Point;
var i : integer;
    d : double;
    xtx : array [0..2, 0..2] of double;
    xtxInv : array [0..2, 0..2] of double;
    xtxInvxt0 : array of double;
    xtxInvxt1 : array of double;
    theta : Point;
begin
    for i := 0 to Length(x) - 1 do
    begin
        xtx[0, 1] := xtx[0, 1] + x[i];
        xtx[0, 0] := xtx[0, 0] + x[i] * x[i];
    end;
    xtx[1, 0] := xtx[0, 1];
    xtx[1, 1] := Length(x);
    d := 1/(xtx[0, 0]*xtx[1, 1] - xtx[1, 0]*xtx[0, 1]);

    xtxInv[0, 0] := xtx[1, 1]*d;
    xtxInv[0, 1] := -xtx[0, 1]*d;
    xtxInv[1, 0] := -xtx[1, 0]*d;
    xtxInv[1, 1] := xtx[0, 0]*d;

    SetLength(xtxInvxt0, Length(x));
    SetLength(xtxInvxt1, Length(x));

    for i := 0 to Length(x) - 1 do
    begin
        xtxInvxt0[i] := xtxInv[0, 0]*x[i] + xtxInv[0, 1];
        xtxInvxt1[i] := xtxInv[1, 0]*x[i] + xtxInv[1, 1];
    end;
    for i := 0 to Length(x) - 1 do
    begin
        theta.x := theta.x + xtxInvxt0[i]*y[i];
        theta.y := theta.y + xtxInvxt1[i]*y[i];
    end;

    NormalEquations2D := theta;
end;

function BoxCountingDimension(bm : TBitmap): double;
var //baList : array of array of double;
    baList_0 : array of double;
    baList_1 : array of double;
    filledBoxes : array of array of boolean;
    x, y, b, a: integer;
    eth : TColor;
begin
    SetLength(baList_0, 6);
    SetLength(baList_1, 6);
    for b := 1 to 6 do
    begin
        SetLength(filledBoxes,
            bm.Width div b + normalize(bm.Width mod b),
            bm.Height div b + normalize(bm.Height mod b)
        );

        eth := bm.Canvas.Pixels[0, 0];
        a := 0;

        for x := 0 to bm.Width - 1 do
            for y := 0 to bm.Height - 1 do
                if not clcmp_rgb(bm.Canvas.Pixels[x, y], eth) then
                begin
                    if not filledBoxes[x div b, y div b] then inc(a);
                    filledBoxes[x div b, y div b] := true;
                end;

        baList_0[b-1] := math.Log2(1/b);
        baList_1[b-1] := math.Log2(a);
    end;

    BoxCountingDimension := NormalEquations2D(baList_0, baList_1).x;
end;

end.
