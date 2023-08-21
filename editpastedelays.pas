unit editpastedelays;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin;

type

  { TPasteDelaysForm }

  TPasteDelaysForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CtrlVDelaySpinEdit: TSpinEdit;
    VkReturnDelaySpinEdit: TSpinEdit;
    Label5: TLabel;
    ShiftInsertDelaySpinEdit: TSpinEdit;
    CustomDelaySpinEdit: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
  private

  public

  end;

var
  PasteDelaysForm: TPasteDelaysForm;

function doEditPasteDelays: boolean;

implementation

{$R *.lfm}

uses
  params;

// returns true if at least one delay was modified
function doEditPasteDelays: boolean;
begin
  result := false;
  with PasteDelaysForm do begin
    CtrlVDelaySpinEdit.Value := PasteCommands[ord(pcCtrlV)].Delayms;
    ShiftInsertDelaySpinEdit.Value := PasteCommands[ord(pcShiftInsert)].Delayms;
    CustomDelaySpinEdit.Value := PasteCommands[ord(pcCustom)].Delayms;
    VkReturnDelaySpinEdit.Value := VkReturnDelay;
    if ShowModal = mrOk then begin
      if PasteCommands[ord(pcCtrlV)].Delayms <> CtrlVDelaySpinEdit.Value then begin
        PasteCommands[ord(pcCtrlV)].Delayms := CtrlVDelaySpinEdit.Value;
        result := true;
      end;

      if PasteCommands[ord(pcShiftInsert)].Delayms <> ShiftInsertDelaySpinEdit.Value then begin
        PasteCommands[ord(pcShiftInsert)].Delayms := ShiftInsertDelaySpinEdit.Value;
        result := true;
      end;

      if PasteCommands[ord(pcCustom)].Delayms <> CustomDelaySpinEdit.Value then begin
        PasteCommands[ord(pcCustom)].Delayms := CustomDelaySpinEdit.Value;
        result := true;
      end;

      if VkReturnDelay <> VkReturnDelaySpinEdit.value then begin
        VkReturnDelay := VkReturnDelaySpinEdit.value;
        result := true;
      end;
    end;
  end;
end;

end.

