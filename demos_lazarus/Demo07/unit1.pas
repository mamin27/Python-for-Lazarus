unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, PairSplitter, PythonGUIInputOutput, PythonEngine,
  Messages, ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    Panel1: TPanel;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Splitter1: TSplitter;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    PythonType1: TPythonType;
    PythonModule1: TPythonModule;
    PythonDelphiVar1: TPythonDelphiVar;
    PythonEngine1: TPythonEngine;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    procedure Button1Click(Sender: TObject);
    procedure PythonModule1Initialization(Sender: TObject);
    procedure PythonType1Initialization(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure PythonDelphiVar1Change(Sender: TObject);
    procedure PythonDelphiVar1GetData(Sender: TObject; var Data: Variant);
    procedure PythonDelphiVar1SetData(Sender: TObject; Data: Variant);
    procedure FormCreate(Sender: TObject);
  private
    { D�clarations priv�es }
    procedure DoPy_InitEngine;
    function  spam_foo( pself, args : PPyObject ) : PPyObject; cdecl;
    function  spam_CreatePoint( pself, args : PPyObject ) : PPyObject; cdecl;
    function  spam_getdouble( pself, args : PPyObject ) : PPyObject; cdecl;
    function  spam_getdouble2( pself, args : PPyObject ) : PPyObject; cdecl;
  public
    { D�clarations publiques }
  end;

  PyPointRec = record
    ob_refcnt      : NativeInt;
    ob_type        : PPyTypeObject;
    po_x           : Integer;
    po_y           : Integer;
  end;
  PPyPoint = ^PyPointRec;

  procedure PyPoint_dealloc(obj : PPyObject); cdecl;
  function  PyPoint_getattr(obj : PPyObject; key : PAnsiChar) : PPyObject; cdecl;
  function  PyPoint_setattrfunc(obj : PPyObject; key : PAnsiChar; value : PPyObject) : Integer; cdecl;
  function  PyPoint_repr(obj : PPyObject) : PPyObject; cdecl;

var
  Form1: TForm1;

implementation

uses
  LclType, proc_py;

{$R *.lfm}

const
  cPyLibraryWindows = 'python37.dll';
  cPyLibraryLinux = 'libpython3.7m.so.1.0';
  cPyLibraryMac = '/Library/Frameworks/Python.framework/Versions/3.7/lib/libpython3.7.dylib';
  cPyZipWindows = 'python37.zip';

procedure TForm1.Button1Click(Sender: TObject);
begin
  PythonEngine1.ExecStrings( Memo1.Lines );
end;

// Here's an example of functions defined for the module spam

function TForm1.spam_foo( pself, args : PPyObject ) : PPyObject; cdecl;
begin
  with GetPythonEngine do
    begin
      ShowMessage( 'args of foo: '+PyObjectAsString(args) );
//      ShowMessage( 'Form''s caption = ' + Caption );
      Result := ReturnNone;
    end;
end;

// This function is used to create a PyPoint instance
function  TForm1.spam_CreatePoint( pself, args : PPyObject ) : PPyObject; cdecl;
var
  x, y : Integer;
  p : PPyPoint;
begin
  with GetPythonEngine do
    begin
      // We want x and y values as argument
      if PyArg_ParseTuple( args, 'ii:CreatePoint',@x, @y) <> 0 then
        begin
          new(p);
          with p^ do
            begin
              ob_refcnt := 1;
              ob_type := TypeByName('Point');
              // or we could write, because it's quicker:
              // ob_type := Form1.PythonType1.TheTypePtr;
              po_x := x;
              po_y := y;
            end;
          Result := PPyObject(p);
        end
      else
        Result := nil;
    end;
end;

function TForm1.spam_getdouble( pself, args : PPyObject ) : PPyObject; cdecl;
// you need to pass floating point numbers as doubles to Py_BuildValue
Const
  d1 : double = 2.7172;
  d2 : double = 3.14159;
  d3 : double = 1.2e-12;
var x: Currency;
    y: Double;
    s: PAnsiChar;
    i: integer;
begin
  with GetPythonEngine do
    begin
      self.PythonGUIInputOutput1.write('Hello, World!'+LF);
      x := 13.5;
      y := 42.0;
      i := 42;
      s := 'Hallo';
      Result := Py_BuildValue('(sddiiddid)',s,x,y,i,815,d1,d2,4711,d3);
    end;
end;

function TForm1.spam_getdouble2( pself, args : PPyObject ) : PPyObject; cdecl;
var x: Currency;
    y: Double;
    s: PAnsiChar;
    i: integer;
begin
  // this is the same function as the previous one,
  // except it uses ArrayToPyTuple
  with GetPythonEngine do
    begin
      self.PythonGUIInputOutput1.write('Hello, World!'+LF);
      x := 13.5;
      y := 42.0;
      i := 42;
      s := 'Hallo';
      Result := ArrayToPyTuple( [ s,x,y,i,815,2.7172,3.14159,4711,1.2e-12, PyInt_FromLong(333) ] );
    end;
end;

procedure TForm1.PythonModule1Initialization(Sender: TObject);
begin
  // In a module initialization, we just need to add our
  // new methods
  with Sender as TPythonModule do
    begin
      AddDelphiMethod( 'foo', @spam_foo, 'foo' );
      AddDelphiMethod( 'CreatePoint',
                  @spam_CreatePoint,
                  'function CreatePoint'+LF+
                  'Args: x, y'+LF+
                  'Result: a new Point object' );
      AddDelphiMethod( 'getdouble',
                  @spam_getdouble,
                  'getdouble' );
      AddDelphiMethod( 'getdouble2',
                  @spam_getdouble2,
                  'getdouble2' );
    end;
end;

// Here's an example of a new type object.
// That's more complex than a new module, but here's a
// template that you can follow.

// Here's the destructor of the object
procedure PyPoint_dealloc(obj : PPyObject); cdecl;
begin
  Dispose(obj);
end;

// Here's the read access to the attributes of an object.
// In fact it is called each time you write:
// object.value
// object.method(args)
function  PyPoint_getattr(obj : PPyObject; key : PAnsiChar) : PPyObject; cdecl;
begin
  with GetPythonEngine, PPyPoint(obj)^ do
    begin
      // Check for attribute x
      if key = 'x' then
        Result := PyInt_FromLong( po_x )
      // Check for attribute y
      else if key = 'y' then
        Result := PyInt_FromLong( po_y )
      else
        begin
          // Else check for a method
          Result := PyObject_GenericGetAttr(obj, PyString_FromString(key));
          if not Assigned(Result) then
            PyErr_SetString (PyExc_AttributeError^, PAnsiChar(Format('Unknown attribute "%s"',[key])));
        end;
    end;
end;

// Here's the write access to the attributes of an object.
// In fact it is called each time you write:
// object.value = 1
function  PyPoint_setattrfunc(obj : PPyObject; key : PAnsiChar; value : PPyObject) : Integer; cdecl;
begin
  Result := -1;
  with GetPythonEngine, PPyPoint(obj)^ do
    begin
      // Check for attribute x
      if key = 'x' then begin
        if PyInt_Check(value) then
          begin
            po_x := PyInt_AsLong(value);
            Result := 0;
          end
        else
          PyErr_SetString (PyExc_AttributeError^, PAnsiChar(Format('Attribute "%s" needs an integer',[key])));
      // Check for attribute y
      end else if key = 'y' then begin
        if PyInt_Check(value) then
          begin
            po_y := PyInt_AsLong(value);
            Result := 0;
          end
        else
          PyErr_SetString (PyExc_AttributeError^, PAnsiChar(Format('Attribute "%s" needs an integer',[key])));
      end else
        PyErr_SetString (PyExc_AttributeError^, PAnsiChar(Format('Unknown attribute "%s"',[key])));
    end;
end;

// Here's how an object should be represented, when printed for instance.
function  PyPoint_repr(obj : PPyObject) : PPyObject; cdecl;
begin
  with GetPythonEngine, PPyPoint(obj)^ do
    begin
      Result := PyString_FromString( PAnsiChar(Format('(%d, %d)',[po_x, po_y]) ) );
    end;
end;

// Here's a method of the object PyPoint
function  PyPoint_OffsetBy(self, args : PPyObject) : PPyObject; cdecl;
var
  x, y : Integer;
begin
  with GetPythonEngine, PPyPoint(self)^ do
    begin
      if PyArg_ParseTuple( args, 'ii:OffsetBy',@x, @y) <> 0 then
        begin
          Inc( po_x, x );
          Inc( po_y, y );
          Result := ReturnNone;
        end
      else
        Result := nil;
    end;
end;

procedure TForm1.PythonType1Initialization(Sender: TObject);
Var
  PyType : PyTypeObject;
begin
  with (Sender as TPythonType) do
    begin
      // In the initialization of a new type, we must
      // define the attributes of this type
      PyType := TheType;
      with PyType do
        begin
          tp_basicsize := sizeof(PyPointRec);
          tp_dealloc   := @PyPoint_dealloc;
          tp_getattr   := @PyPoint_getattr;
          tp_setattr   := @PyPoint_setattrfunc;
          tp_repr      := @PyPoint_repr;
          tp_str       := @PyPoint_repr;
        end;
        TheType := PyType;
      // And then add the methods of the object, if needed
      AddMethod( 'OffsetBy', @PyPoint_OffsetBy, 'OffsetBy(dx, dy)' );
    end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  with OpenDialog1 do
    begin
      if Execute then
        Memo1.Lines.LoadFromFile( FileName );
    end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  with SaveDialog1 do
    begin
      if Execute then
        Memo1.Lines.SaveToFile( FileName );
    end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  ShowMessage( 'Value = ' + PythonDelphiVar1.ValueAsString );
end;

procedure TForm1.PythonDelphiVar1Change(Sender: TObject);
begin
  with Sender as TPythonDelphiVar do
    ShowMessage( 'Var test changed: ' + ValueAsString );
end;

procedure TForm1.PythonDelphiVar1GetData(Sender: TObject;
  var Data: Variant);
begin
  Data := Edit1.Text;
end;

procedure TForm1.PythonDelphiVar1SetData(Sender: TObject; Data: Variant);
begin
  Edit1.Text := Data;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  DoPy_InitEngine;
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
