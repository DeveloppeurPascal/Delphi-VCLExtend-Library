program SelectDirectoryVCL;

uses
  Vcl.Forms,
  fMain in 'fMain.pas' {Form1},
  Olf.VCL.SelectDirectory in '..\..\src\Olf.VCL.SelectDirectory.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
