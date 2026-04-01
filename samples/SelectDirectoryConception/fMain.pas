(* C2PP
  ***************************************************************************

  Delphi VCL Extend Library
  Copyright (c) 2021-2026 Patrick PREMARTIN

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
  File last update : 2025-05-26T15:43:37.846+02:00
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
