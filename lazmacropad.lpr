program lazmacropad;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, about, keymap, macrolog, params, macrodef,
  options;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TLogForm, LogForm);
  Application.CreateForm(TMacroForm, MacroForm);
  Application.CreateForm(TOptionsForm, OptionsForm);
  Application.Run;
end.

