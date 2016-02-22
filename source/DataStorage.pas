unit DataStorage;

interface

uses Classes, VarCmplx;

type TDrawMode = (mAffineTransform, mPolygonal);

type TAffineTransform = packed record
    a, b, c, d, e, f: double;
    p : double;
end;

type TSettings = packed record
    iterationsCount : integer;
    getNextFunc : pointer;
    mode : TDrawMode;
    m : double;
    l : double;
    showVertices : boolean;
    showPointer : boolean;
    resetPointer : boolean;
    needRedrawVertices : boolean;
    needRedrawPointers : boolean;
    startPointsCount : integer;
    verticesCount : integer;
    atCount : integer;
    startPoints : array of Variant;
    vertices : array of Variant;
    at : array of TAffineTransform;
end;

type TSettingsPointer = ^TSettings;
type TMemoryStreamPointer = ^TMemoryStream;

const BOOL_SIZE = 1;
const INT_SIZE = 4;
const DOUBLE_SIZE = 8;

procedure packSettingsToMemoryStream(settings : TSettingsPointer;
    ms : TMemoryStreamPointer);
procedure loadSettingsFromMemoryStream(settings : TSettingsPointer;
    ms : TMemoryStreamPointer);
procedure saveSettingsToFile(settings : TSettingsPointer; fileName : string);
procedure loadSettingsFromFile(settings : TSettingsPointer; fileName : string);

implementation

procedure packSettingsToMemoryStream(settings : TSettingsPointer;
    ms : TMemoryStreamPointer);
var r, im : double;
    i : integer;
    mtrue : boolean;
begin
    mtrue := true;
    ms.WriteBuffer(settings.startPointsCount, INT_SIZE);
    ms.WriteBuffer(settings.verticesCount, INT_SIZE);
    ms.WriteBuffer(settings.atCount, INT_SIZE);
    ms.WriteBuffer(settings.iterationsCount, INT_SIZE);
    ms.WriteBuffer(settings.mode, SizeOf(TDrawMode));
    ms.WriteBuffer(settings.m, DOUBLE_SIZE);
    ms.WriteBuffer(settings.l, DOUBLE_SIZE); //not used
    ms.WriteBuffer(settings.showPointer, BOOL_SIZE);
    ms.WriteBuffer(settings.showVertices, BOOL_SIZE);
    ms.WriteBuffer(mtrue, BOOL_SIZE); // refresh pointer
    ms.WriteBuffer(mtrue, BOOL_SIZE); // refresh vertices
    ms.WriteBuffer(mtrue, BOOL_SIZE); // reset pointer
    for i := 0 to settings.startPointsCount-1 do
    begin
        r := settings.startPoints[i].Real;
        ms.WriteBuffer(r, DOUBLE_SIZE);
        im := settings.startPoints[i].Imaginary;
        ms.WriteBuffer(im, DOUBLE_SIZE);
    end;
    for i := 0 to settings.verticesCount-1 do
    begin
        r := settings.vertices[i].Real;
        ms.WriteBuffer(r, DOUBLE_SIZE);
        im := settings.vertices[i].Imaginary;
        ms.WriteBuffer(im, DOUBLE_SIZE);
    end;
    for i := 0 to settings.atCount-1 do
        ms.WriteBuffer(settings.at[i], SizeOf(TAffineTransform));
end;

procedure loadSettingsFromMemoryStream(settings : TSettingsPointer;
    ms : TMemoryStreamPointer);
var i : integer;
    r, im : double;
begin
    settings.getNextFunc := nil;
    ms.ReadBuffer(settings.startPointsCount, INT_SIZE);
    ms.ReadBuffer(settings.verticesCount, INT_SIZE);
    ms.ReadBuffer(settings.atCount, INT_SIZE);
    ms.ReadBuffer(settings.iterationsCount, INT_SIZE);
    ms.ReadBuffer(settings.mode, SizeOf(TDrawMode));
    ms.ReadBuffer(settings.m, DOUBLE_SIZE);
    ms.ReadBuffer(settings.l, DOUBLE_SIZE); // not used
    ms.ReadBuffer(settings.showPointer, BOOL_SIZE);
    ms.ReadBuffer(settings.showVertices, BOOL_SIZE);
    ms.ReadBuffer(settings.needRedrawPointers, BOOL_SIZE); // refresh pointer
    ms.ReadBuffer(settings.needRedrawVertices, BOOL_SIZE); // refresh vertices
    ms.ReadBuffer(settings.resetPointer, BOOL_SIZE); // reset pointer
    SetLength(settings.startPoints, settings.startPointsCount);
    for i := 0 to settings.startPointsCount-1 do
    begin
        ms.ReadBuffer(r, DOUBLE_SIZE);
        ms.ReadBuffer(im, DOUBLE_SIZE);
        settings.startPoints[i] := VarComplexCreate(r, im);
    end;
    SetLength(settings.vertices, settings.verticesCount);
    for i := 0 to settings.verticesCount-1 do
    begin
        ms.ReadBuffer(r, DOUBLE_SIZE);
        ms.ReadBuffer(im, DOUBLE_SIZE);
        settings.vertices[i] := VarComplexCreate(r, im);
    end;
    SetLength(settings.at, settings.atCount);
    for i := 0 to settings.atCount-1 do
        ms.Read(settings.at[i], SizeOf(TAffineTransform));
end;

procedure saveSettingsToFile(settings : TSettingsPointer; fileName : string);
var ms : TMemoryStream;
begin
    try
        ms := TMemoryStream.Create;
        packSettingsToMemoryStream(settings, @ms);
        ms.SaveToFile(fileName);
    finally
        ms.Free;
    end;
end;

procedure loadSettingsFromFile(settings : TSettingsPointer; fileName : string);
var ms : TMemoryStream;
begin
    try
        ms := TMemoryStream.Create;
        ms.LoadFromFile(fileName);
        loadSettingsFromMemoryStream(settings, @ms);
    finally
        ms.Free;
    end;
end;

end.
