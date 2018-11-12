{ $OmniXML: OmniXML/OmniXMLConf.pas,v 1.4 2007/02/11 19:16:45 mremec Exp $ }
(*******************************************************************************
* The contents of this file are subject to the Mozilla Public License Version  *
* 1.1 (the "License"); you may not use this file except in compliance with the *
* License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ *
*                                                                              *
* Software distributed under the License is distributed on an "AS IS" basis,   *
* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for *
* the specific language governing rights and limitations under the License.    *
*                                                                              *
* The Original Code is OmniXMLConf.pas                                         *
*                                                                              *
* The Initial Developer of the Original Code is Miha Vrhovnik                  *
*   http://simail.sourceforge.net/, http://xcollect.sourceforge.net            *
*                                                                              *
* Last changed: 2004-04-10                                                     *
*                                                                              *
* History:                                                                     *
*     1.0.7: 2007-02-08                                                        *
*       - check if root node of xml indeed is a config file                    *
*           if it's not then recreate it with conf as default root element     *
*     1.0.6: 2006-12-10                                                        *
*       - config file now opens xml via stream and holds file open this        *
*          puts a lot less stress on computers with slower processors and it   *
*          also gets rid of EFCreateError... file already used by another      *
*          process if those computers are using antivirus software             *
*     1.0.5: 2005-05-12                                                        *
*       - rewritten class constructor and thus fixed possiblity                *
*           of getting exception because  FxmlRoot = nil                       *
*     1.0.4: 2004-05-28                                                        *
*       - added function Read / Write WideString                               *
*     1.0.3: 2004-04-10                                                        *
*       - added property SaveAfterChange if this property is True,             *
*          then document is saved after each change                            *
*       - procedure SaveConfig is now public so you can force document change  *
*     1.0.2: 2003-12-14                                                        *
*       - document can be marked as read only (no changes are saved)           *
*     1.0.1: 2003-11-01                                                        *
*       - fixed bug in  WriteIdentNode                                         *
*     1.0.0: 2003-10-25                                                        *
*       - initial version                                                      *
*                                                                              *
* Contributor(s):                                                              *
*******************************************************************************)

unit OmniXMLConf.Vcl;

{$I OmniXML.inc}

{$IFDEF OmniXML_DXE4_UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses SysUtils, Classes, OmniXML, OmniXMLUtils, OmniXMLConf, Controls, Forms
  {$IFDEF TNT_UNICODE}, TntSysUtils, TntClasses{$ENDIF};

//you may use this class as replacement for TIniFile
type TxmlConfVcl=class(TxmlConf)
  public
    procedure ReadControlSettings(Control: TControl; ctlName: WideString = '');
    procedure WriteControlSettings(Control: TControl; ctlName: WideString = '');
end;

implementation

{ TxmlConfVcl }

procedure TxmlConfVcl.ReadControlSettings(Control: TControl; ctlName: WideString = '');
var t,l: Integer;
begin
  if ctlName = '' then
    ctlName := Control.Name;

  if Control is TForm then begin
    //damn this throws an exception
    //(Control as TForm).Position := poDesigned;

    //set form width & height only if form is sizeable
    if (Control as TForm).BorderStyle = bsSizeable then begin
      Control.Width := ReadInteger(ctlName, 'width', Control.Width);
      Control.Height := ReadInteger(ctlName, 'height', Control.Height);
    end;

    t := (Screen.Height div 2) - (Control.Height div 2);
    l := (Screen.Width div 2) - (Control.Width div 2);

    Control.Top := ReadInteger(ctlName, 'top', t);
    Control.Left := ReadInteger(ctlName, 'left', l);
  end
  else begin
    Control.Width := ReadInteger(ctlName, 'width', Control.Width);
    Control.Height := ReadInteger(ctlName, 'height', Control.Height);
    Control.Top := ReadInteger(ctlName, 'top', Control.Top);
    Control.Left := ReadInteger(ctlName, 'left', Control.Left);
  end;

end;

procedure TxmlConfVcl.WriteControlSettings(Control: TControl; ctlName: WideString = '');
begin
  if ctlName = '' then
    ctlName := Control.Name;

  WriteInteger(ctlName, 'width', Control.Width);
  WriteInteger(ctlName, 'height', Control.Height);
  WriteInteger(ctlName, 'top', Control.Top);
  WriteInteger(ctlName, 'left', Control.Left);
  if SaveAfterChange then
    SaveConfig;
end;

end.
