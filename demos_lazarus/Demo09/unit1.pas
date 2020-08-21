unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, PairSplitter, PythonEngine, PythonGUIInputOutput;

type

  { TForm1 }

  TForm1 = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    Button1: TButton;
    Splitter1: TSplitter;
    Panel1: TPanel;
    PythonEngine1: TPythonEngine;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PyInit_demodll(Sender: TObject);
  private
    procedure DoPy_InitEngine;
  public
  end;

var
  Form1: TForm1;

implementation

uses
  LclType, proc_py;

{$R unit1.lfm}

const
    cPyLibraryWindows = 'python37.dll';
    cPyLibraryLinux = 'libpython3.7m.so.1.0';
    cPyLibraryMac = '/Library/Frameworks/Python.framework/Versions/3.7/lib/libpython3.7.dylib';
    cPyZipWindows = 'python37.zip';

procedure TForm1.Button1Click(Sender: TObject);
begin
  PythonEngine1.ExecStrings( memo1.Lines );
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  DoPy_InitEngine;
end;

procedure TForm1.PyInit_demodll(Sender: TObject);
begin

end;

procedure TForm1.DoPy_InitEngine;
var
  S: string;
begin
  S:=
    {$ifdef windows} cPyLibraryWindows {$endif}
    {$ifdef linux} cPyLibraryLinux {$endif}
    {$ifdef darwin} cPyLibraryMac {$endif} ;
  PythonEngine1.DllPath:= ExtractFileDir(S);
  PythonEngine1.DllName:= ExtractFileName(S);
  PythonEngine1.LoadDll;
end;

end.
