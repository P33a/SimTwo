unit MainRemote;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TFMainRemote = class(TForm)
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FMainRemote: TFMainRemote;

implementation

uses RemoteControl;

{$R *.dfm}

procedure TFMainRemote.FormShow(Sender: TObject);
begin
  FRemoteControl.show;
end;

end.
