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
  Forms, main, about, keymap, macrolog, params, macrodef, options,
  custompastecommand, editStringMacro, editKbdMacro, editpastedelays;

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
  Application.CreateForm(TCustomPasteForm, CustomPasteForm);
  Application.CreateForm(TEditStringForm, EditStringForm);
  Application.CreateForm(TEditKbdMacroForm, EditKbdMacroForm);
  Application.CreateForm(TPasteDelaysForm, PasteDelaysForm);
  Application.Run;
end.

