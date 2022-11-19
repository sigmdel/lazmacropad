unit editStringMacro;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TEditStringForm }

  TEditStringForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Memo1: TMemo;
  private
  public
    procedure SetEditText(const s: string);
    function GetEditText: string;
  end;

var
  EditStringForm: TEditStringForm;

function RemoveEscSequences(const ins: string): string;

implementation

{$R *.lfm}

uses StrUtils;

function RemoveEscSequences(const ins: string): string;
begin
  result := replaceStr(ins, '\n', #13);
  result := replaceStr(result, '\t', #9);
  result := replacestr(result, '\\', '\');
end;

function InsertEscSequences(const ins: string): string;
begin
  result := replacestr(ins, '\', '\\'); // must be first!
  result := replaceStr(result, #9, '\t');
  // this should take care of all eol
  result := replaceStr(result, #10#13, '\n');
  result := replaceStr(result, #13#10, '\n');
  result := replaceStr(result, #13, '\n');
  result := replaceStr(result, #10, '\n');
end;


{ TEditStringForm }

procedure TEditStringForm.SetEditText(const s: string);
begin
  Memo1.Text := RemoveEscSequences(s);
end;

function TEditStringForm.GetEditText: string;
begin
  result := InsertEscSequences(Memo1.Text);
end;

end.

