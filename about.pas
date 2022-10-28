unit about;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Label4Click(Sender: TObject);
  private

  public

  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

uses LCLIntf, fileinfo;

{ TAboutForm }

procedure TAboutForm.FormCreate(Sender: TObject);
var
  Version: TProgramVersion;
begin
  Label1.Caption := changefileext(extractfilename(application.exename), '');
  if GetProgramVersion(Version) then with Version do
    Label2.caption := Format('Version %d.%d.%d', [Major,Minor,Revision]);
end;

procedure TAboutForm.Label4Click(Sender: TObject);
begin
  with Sender as TControl do
    OpenUrl(hint);
end;

end.

