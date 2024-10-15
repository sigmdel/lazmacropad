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
  Forms, ulog, main, macrolog, custompastecommand, editKbdMacro,
  editpastedelays, editStringMacro, macrodef, options, about, keymap,
  uconfig, ukbdev, umacros, userial, lazmouseandkeyinput
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TLogForm, LogForm);
  Application.CreateForm(TCustomPasteForm, CustomPasteForm);
  Application.CreateForm(TEditKbdMacroForm, EditKbdMacroForm);
  Application.CreateForm(TPasteDelaysForm, PasteDelaysForm);
  Application.CreateForm(TEditStringForm, EditStringForm);
  Application.CreateForm(TMacroForm, MacroForm);
  Application.CreateForm(TOptionsForm, OptionsForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TLayoutForm, LayoutForm);
  Application.Run;
end.


