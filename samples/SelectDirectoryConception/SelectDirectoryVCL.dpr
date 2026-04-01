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
  File last update : 2025-05-26T15:43:38.000+02:00
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
