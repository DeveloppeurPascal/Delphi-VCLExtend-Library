unit Olf.VCL.SelectDirectory;

interface

Uses
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
  [ComponentPlatformsAttribute(pfidWindows)]
  TOlfSelectDirectoryDialog = class(TComponent)
  private
    FDirectory: string;
    FCaption: string;
    FRoot: string;
    procedure SetDirectory(const Value: string);
    procedure SetRoot(const Value: string);
    procedure SetCaption(const Value: string);
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
  end;

procedure Register;

implementation

Uses
  VCL.FileCtrl,
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
end;

function TOlfSelectDirectoryDialog.Execute: boolean;
var
  LDir: String;
begin
  LDir := FDirectory;
  result := SelectDirectory(FCaption, FRoot, LDir);
  if result then
    FDirectory := LDir;
end;

procedure TOlfSelectDirectoryDialog.SetDirectory(const Value: string);
begin
  FDirectory := Value;
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
