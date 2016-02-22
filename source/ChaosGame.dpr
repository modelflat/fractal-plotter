program ChaosGame;

uses
  Forms,
  MainForm in 'MainForm.pas' {Form1},
  SettingsForm in 'SettingsForm.pas' {Form2},
  DataStorage in 'DataStorage.pas',
  MinkovskyDim in 'MinkovskyDim.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
