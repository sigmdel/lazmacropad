unit about;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Image1: TImage;
    Label1: TLabel;
    buildDateLabel: TLabel;
    fpcVersionLabel: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    platformLabel: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lazVersionLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Label4Click(Sender: TObject);
  private

  public

  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

uses LCLIntf, LCLVersion, LCLPlatformDef, InterfaceBase, FPCAdds, fileinfo;

// source Lazarus IDE  aboutfrm.pas
{The compiler generated date string is always of the form y/m/d.
 This function gives it a string respresentation according to the
 shortdateformat}
function GetLocalizedBuildDate(): string;
var
  BuildDate: string;
  SlashPos1, SlashPos2: integer;
  Date: TDateTime;
begin
  BuildDate := {$I %date%};
  SlashPos1 := Pos('/',BuildDate);
  SlashPos2 := SlashPos1 +
    Pos('/', Copy(BuildDate, SlashPos1+1, Length(BuildDate)-SlashPos1));
  Date := EncodeDate(StrToWord(Copy(BuildDate,1,SlashPos1-1)),
    StrToWord(Copy(BuildDate,SlashPos1+1,SlashPos2-SlashPos1-1)),
    StrToWord(Copy(BuildDate,SlashPos2+1,Length(BuildDate)-SlashPos2)));
  Result := FormatDateTime('yyyy-mm-dd', Date);
end;


{ TAboutForm }


procedure TAboutForm.FormCreate(Sender: TObject);
var
  Version: TProgramVersion;
begin
  Label1.Caption := changefileext(extractfilename(application.exename), '');
  if GetProgramVersion(Version) then with Version do
    Label2.caption := Format('%d.%d.%d', [Major,Minor,Revision])
  else
    Label2.caption := '-';
  fpcVersionLabel.Caption := {$I %FPCVERSION%};
  LazVersionLabel.Caption := lcl_version;
  buildDateLabel.Caption := GetLocalizedBuildDate;
  platformLabel.Caption:= lowerCase({$I %FPCTARGETCPU%})
                         + '-' + lowerCase({$I %FPCTARGETOS%})
                         + '-' + LCLPlatformDisplayNames[GetDefaultLCLWidgetType];
end;

procedure TAboutForm.FormShow(Sender: TObject);
var
  aPt: TPoint;
begin
  aPt := screen.PrimaryMonitor.WorkareaRect.bottomRight;
  top := aPt.y - height
  {$ifdef Windows}
    - GetSystemMetrics(SM_CYSIZE) - GetSystemMetrics(SM_CYSIZEFRAME);
  {$endif};
  left := aPt.x - width;
end;

procedure TAboutForm.Label4Click(Sender: TObject);
begin
  with Sender as TControl do
    OpenUrl(hint);
end;

end.

