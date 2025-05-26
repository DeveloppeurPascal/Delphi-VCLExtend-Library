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
  Signature : bdba975cca2ef69538e71ecc81a3299dd8651d83
  ***************************************************************************
*)

unit Olf.VCL.SelectDirectory;

interface

Uses
  VCL.FileCtrl,
  System.Classes;

type

  /// <summary>
  /// Open a modal dialog box to choose a directory.
  /// </summary>
  /// <remarks>
  /// This non visual component use SelectDirectory from VCL.FileCtrl.
  /// The properties are used as it when calling the function.
  /// http://docwiki.embarcadero.com/Libraries/en/VCL.FileCtrl.SelectDirectory
  /// </remarks>
{$IF CompilerVersion >= 33.0}
  [ComponentPlatformsAttribute(pfidWindows)]
{$ELSE}
  [ComponentPlatformsAttribute(pidWin32 + pidWin64)]
{$ENDIF}

  TOlfSelectDirectoryDialog = class(TComponent)
  private
    FDirectory: string;
    FCaption: string;
    FRoot: string;
    FOptions: TSelectDirExtOpts;
    procedure SetDirectory(const Value: string);
    procedure SetRoot(const Value: string);
    procedure SetCaption(const Value: string);
    procedure SetOptions(const Value: TSelectDirExtOpts);
  protected
  public
    function Execute: boolean;
    constructor Create(AOwner: TComponent); override;
  published
    /// <summary>
    /// Text used as the dialog box title.
    /// </summary>
    property Caption: string read FCaption write SetCaption;
    /// <summary>
    /// Root directory to filter the selection (can be empty)
    /// </summary>
    property Root: string read FRoot write SetRoot;
    /// <summary>
    /// Initial directory in input. Selected directory in output if the user config the selection.
    /// If nothing is selected, the output directory is the same as input directory.
    /// </summary>
    property Directory: string read FDirectory write SetDirectory;
    /// <summary>
    /// Used to personalize the dialog box. See Delphi documentation if you want
    /// to know how to use it.
    /// </summary>
    property Options: TSelectDirExtOpts read FOptions write SetOptions
      default [sdNewUI];
  end;

procedure Register;

implementation

Uses
  VCL.Controls;

procedure Register;
begin
  RegisterComponents('OlfSoftware', [TOlfSelectDirectoryDialog]);
end;

{ TOlfSelectDirectoryDialog }

constructor TOlfSelectDirectoryDialog.Create(AOwner: TComponent);
begin
  inherited;
  FDirectory := '';
  FCaption := '';
  FRoot := '';
  FOptions := [sdNewUI]; // default value used in SelectDirectory procedure
end;

function TOlfSelectDirectoryDialog.Execute: boolean;
var
  LDir: String;
begin
  LDir := FDirectory;
  result := SelectDirectory(FCaption, FRoot, LDir, FOptions);
  if result then
    FDirectory := LDir;
end;

procedure TOlfSelectDirectoryDialog.SetDirectory(const Value: string);
begin
  FDirectory := Value;
end;

procedure TOlfSelectDirectoryDialog.SetOptions(const Value: TSelectDirExtOpts);
begin
  FOptions := Value;
end;

procedure TOlfSelectDirectoryDialog.SetRoot(const Value: string);
begin
  FRoot := Value;
end;

procedure TOlfSelectDirectoryDialog.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

initialization

{$IF NOT DEFINED(CLR)}
  StartClassGroup(VCL.Controls.TControl);
ActivateClassGroup(VCL.Controls.TControl);
GroupDescendentsWith(TOlfSelectDirectoryDialog, VCL.Controls.TControl);
{$ENDIF}

end.
