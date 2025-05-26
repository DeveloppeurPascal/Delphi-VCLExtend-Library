(* C2PP
  ***************************************************************************

  Delphi VCL Extend Library

  Copyright 2021-2025 Patrick Prémartin under AGPL 3.0 license.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.

  ***************************************************************************

  Set of VCL components for Delphi Windows projects.

  ***************************************************************************

  Author(s) :
  Patrick PREMARTIN

  Site :
  https://vclextend.developpeur-pascal.fr/

  Project site :
  https://github.com/DeveloppeurPascal/Delphi-VCLExtend-Library

  ***************************************************************************
  File last update : 2025-02-09T11:03:48.263+01:00
  Signature : aa45ee4e7c611861f8abd4efef5498dfcd1b2c3a
  ***************************************************************************
*)

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
