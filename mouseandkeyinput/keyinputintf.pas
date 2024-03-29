{ KeyInputIntf

  Copyright (C) 2008 Tom Gregorovic

  This source is free software; you can redistribute it and/or modify it under the terms of the
  GNU General Public License as published by the Free Software Foundation; either version 2 of the
  License, or (at your option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
  even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  A copy of the GNU General Public License is available on the World Wide Web at
  <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing to the Free Software
  Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}
unit KeyInputIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;
  
type
  { TKeyInput }

  TKeyInput = class
  protected
    procedure DoDown(Key: Word); dynamic; abstract;
    procedure DoUp(Key: Word); dynamic; abstract;
  public
    procedure Down(Key: Word);
    procedure Up(Key: Word);
    
    procedure Press(Key: Word);
    procedure Press(StringValue : String);
    
    procedure Apply(Shift: TShiftState);
    procedure Unapply(Shift: TShiftState);
  end;

implementation

uses LCLType;

{ TKeyInput }

procedure TKeyInput.Down(Key: Word);
begin
  DoDown(Key);
  Application.ProcessMessages;
end;

procedure TKeyInput.Up(Key: Word);
begin
  DoUp(Key);
  Application.ProcessMessages;
end;

procedure TKeyInput.Press(Key: Word);
begin
  Down(Key);
  Up(Key);
end;

procedure TKeyInput.Press(StringValue: String);
var
  i : Integer;
begin
  i :=1;
  while (i <= Length(StringValue)) do
    begin
      Press(Ord(StringValue[i]));
      Inc(i);
    end;
end;

procedure TKeyInput.Apply(Shift: TShiftState);
begin
  if ssCtrl in Shift then Down(VK_CONTROL);
  if ssAlt in Shift then Down(VK_MENU);
  if ssShift in Shift then Down(VK_SHIFT);
  if ssAltGr in Shift then Down(VK_RMENU);
end;

procedure TKeyInput.Unapply(Shift: TShiftState);
begin
  if ssShift in Shift then Up(VK_SHIFT);
  if ssCtrl in Shift then Up(VK_CONTROL);
  if ssAlt in Shift then Up(VK_MENU);
  if ssAltGr in Shift then Up(VK_RMENU);
end;

end.

