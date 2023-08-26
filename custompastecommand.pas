unit custompastecommand;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin, kbdev;

type

  { TCustomPasteForm }

  TCustomPasteForm = class(TForm)
    AltGrCheckBox: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    DelaySpinEdit: TSpinEdit;
    KeyNameComboBox: TComboBox;
    Label1: TLabel;
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
    ShiftCheckBox.Checked := ksShift In ke.Shift;
    CtrlCheckBox.Checked := ksCtrl In ke.Shift;
    AltCheckBox.Checked := ksAlt In ke.Shift;
    AltGrCheckBox.Checked := ksAltGr In ke.Shift;
    DelaySpinEdit.Value := ke.Delayms;
    result := ShowModal = mrOk;
    ke.Shift := [];
    ke.Code := 0;
    if result then begin
      ke.Code := byte(PtrUInt(KeyNameComboBox.Items.Objects[KeyNameComboBox.ItemIndex]));
      if ShiftCheckBox.Checked then include(ke.Shift, ksShift);
      if CtrlCheckBox.Checked then include(ke.Shift, ksCtrl);
      if AltCheckBox.Checked then include(ke.Shift, ksAlt);
      if AltGrCheckBox.Checked then include(ke.Shift, ksAltGr);
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

