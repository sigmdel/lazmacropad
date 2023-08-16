unit custompastecommand;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin, kbdev;

type

  { TCustomPasteForm }

  TCustomPasteForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    DelaySpinEdit: TSpinEdit;
    KeyNameComboBox: TComboBox;
    Label2: TLabel;
    ShiftCheckBox: TCheckBox;
    CtrlCheckBox: TCheckBox;
    AltCheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  CustomPasteForm: TCustomPasteForm;

function EditCustomPasteCommand(var ke: TKbdEvent): boolean;

implementation

{$R *.lfm}

function EditCustomPasteCommand(var ke: TKbdEvent): boolean;
var
  i: integer;
begin
  if not KeyCodeFind(ke.Code, i) then
    i := 0;
  with CustomPasteForm do begin
    KeyNameComboBox.ItemIndex := i;
    ShiftCheckBox.Checked := ssShift In ke.Shift.State;
    CtrlCheckBox.Checked := ssCtrl In ke.Shift.State;
    AltCheckBox.Checked := ssAlt In ke.Shift.state;
    DelaySpinEdit.Value := ke.Delayms;
    result := ShowModal = mrOk;
    ke.shift.state := [];
    ke.Code := 0;
    if result then begin
      ke.Code := byte(PtrUInt(KeyNameComboBox.Items.Objects[KeyNameComboBox.ItemIndex]));
      if ShiftCheckBox.Checked then include(ke.Shift.state, ssShift);
      if CtrlCheckBox.Checked then include(ke.Shift.state, ssCtrl);
      if AltCheckBox.Checked then include(ke.Shift.state, ssAlt);
      ke.Delayms := DelaySpinEdit.Value;
    end
  end;
end;

{ TCustomPasteForm }

procedure TCustomPasteForm.FormCreate(Sender: TObject);
begin
  AssignKeyNamesCodes(KeyNameComboBox.Items)
end;


end.

