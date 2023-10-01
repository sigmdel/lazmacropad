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
  umacros;

// returns true if at least one delay was modified
function doEditPasteDelays: boolean;
begin
  result := false;
  with PasteDelaysForm do begin
    CtrlVDelaySpinEdit.Value := macros.PasteDelay[pcCtrlV];
    ShiftInsertDelaySpinEdit.Value := macros.PasteDelay[pcShiftInsert];
    CustomDelaySpinEdit.Value := macros.PasteDelay[pcCustom];
    VkReturnDelaySpinEdit.Value := macros.ReturnDelay;
    if ShowModal = mrOk then begin
      if macros.PasteDelay[pcCtrlV] <> CtrlVDelaySpinEdit.Value then begin
        macros.PasteDelay[pcCtrlV] := CtrlVDelaySpinEdit.Value;
        result := true;
      end;

      if macros.PasteDelay[pcShiftInsert] <> ShiftInsertDelaySpinEdit.Value then begin
        macros.PasteDelay[pcShiftInsert] := ShiftInsertDelaySpinEdit.Value;
        result := true;
      end;

      if macros.PasteDelay[pcCustom] <> CustomDelaySpinEdit.Value then begin
        macros.PasteDelay[pcCustom] := CustomDelaySpinEdit.Value;
        result := true;
      end;

      if macros.ReturnDelay <> VkReturnDelaySpinEdit.value then begin
        macros.ReturnDelay := VkReturnDelaySpinEdit.value;
        result := true;
      end;
    end;
  end;
end;

end.

