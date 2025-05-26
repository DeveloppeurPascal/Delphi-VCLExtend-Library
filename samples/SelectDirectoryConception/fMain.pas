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
  Signature : 11ad412860673ec05057cb87145611412f072c4c
  ***************************************************************************
*)

unit fMain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  System.IOUtils,
  Olf.Vcl.SelectDirectory;

procedure TForm1.Button1Click(Sender: TObject);
var
  fs: TOlfSelectDirectoryDialog;
begin
  fs := TOlfSelectDirectoryDialog.Create(self);
  try
    fs.Caption := 'Choisir un dossier';
    fs.root := '';
    fs.Directory := tpath.GetDocumentsPath;
    Memo1.lines.add('Current directory is "' + fs.Directory + '"');
    if fs.Execute then
      Memo1.lines.add('Selected folder: ' + fs.Directory)
    else
      Memo1.lines.add('No folder selected. Current directory is "' +
        fs.Directory + '"');
  finally
    fs.free;
  end;
end;

end.
