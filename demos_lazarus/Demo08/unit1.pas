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
    Splitter1: TSplitter;
    Memo1: TMemo;
    Memo2: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    PythonEngine1: TPythonEngine;
    PythonModule1: TPythonModule;
    PythonType1: TPythonType;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    procedure Button1Click(Sender: TObject);
    procedure PythonType1Initialization(Sender: TObject);
    procedure PythonModule1Initialization(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { D�clarations priv�es }
    procedure DoPy_InitEngine;
  public
    { D�clarations publiques }
  end;

  // This is a Delphi class implementing a new Python type
  // it must derive from TPyObject or one of its descendants.
  // Then it must override some methods, like the constructors,
  // the RegisterMethods and the type services' virtual methods.
  TPyPoint = class(TPyObject)
    x, y : Integer;

    // Constructors & Destructors
    constructor Create( APythonType : TPythonType ); override;
    constructor CreateWith( APythonType : TPythonType; args : PPyObject ); override;
 {
    // Type services
    ////////////////

    // Basic services
    function  GetAttr(key : PAnsiChar) : PPyObject; override;
    function  SetAttr(key : PAnsiChar; value : PPyObject) : Integer; override;
    function  Repr : PPyObject; override;
    function  RichCompare( obj : PPyObject; Op : TRichComparisonOpcode) : PPyObject; override;

    // Class methods
    class procedure RegisterMethods( APythonType : TPythonType ); override;

    // Methods of TPyPoint
    procedure OffsetBy( dx, dy : Integer );

    // Interface methods
    function DoOffsetBy( args : PPyObject ) : PPyObject; cdecl;
    function DoRaiseError( args : PPyObject ) : PPyObject; cdecl;      }
  end;

  PyPointRec = record
    ob_refcnt      : NativeInt;
    ob_type        : PPyTypeObject;
    po_x           : Integer;
    po_y           : Integer;
  end;
  PPyPoint = ^PyPointRec;

  function  spam_foo( self, args : PPyObject ) : PPyObject; cdecl;
  function  spam_CreatePoint( self, args : PPyObject ) : PPyObject; cdecl;
  function  spam_getdouble( self, args : PPyObject ) : PPyObject; cdecl;

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

// First, we need to initialize the property PyObjectClass with
// the class of our Type object
procedure TForm1.PythonType1Initialization(Sender: TObject);
begin
  PythonType1.PyObjectClass := TPyPoint;
end;

// Here's an example of functions defined for the module spam

function spam_foo( self, args : PPyObject ) : PPyObject; cdecl;
begin
  with GetPythonEngine do
    begin
      ShowMessage( 'args of foo: '+PyObjectAsString(args) );
      Result := ReturnNone;
    end;
end;

// This function is used to create a PyPoint instance
function  spam_CreatePoint( self, args : PPyObject ) : PPyObject; cdecl;
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

function spam_getdouble( self, args : PPyObject ) : PPyObject; cdecl;
// you need to pass floating point numbers as doubles to Py_BuildValue
Const
  x : double = 2.7172;
  y : double = 3.14159;
  z : double = 1.2e-12;
begin
  with GetPythonEngine do
    begin
      Result := Py_BuildValue('(iiddid)',42,815,x,y,4711,z);
    end;
end;

procedure TForm1.PythonModule1Initialization(Sender: TObject);
begin
  // In a module initialization, we just need to add our
  // new methods
  with Sender as TPythonModule do
    begin
      AddMethod( 'foo', @spam_foo, 'foo' );
      AddMethod( 'CreatePoint', @spam_CreatePoint,
                 'function CreatePoint'+LF+
                 'Args: x, y'+LF+
                 'Result: a new Point object' );
      AddMethod( 'getdouble', @spam_getdouble, 'getdouble' );
    end;
end;

// We override the constructors

constructor TPyPoint.Create( APythonType : TPythonType );
begin
  inherited;
  x := 0;
  y := 0;
end;

// Don't call the Create constructor of TPyPoint, because
// we call the inherited constructor CreateWith that calls
// the Create constructor first, and because the constructors
// are virtual, TPyPoint.Create will be automatically be called.

constructor TPyPoint.CreateWith( APythonType : TPythonType; args : PPyObject );
begin
  inherited;
  with GetPythonEngine do
    begin
      if PyArg_ParseTuple( args, 'ii:CreatePoint',@x, @y ) = 0 then
        exit;
    end;
end;

// Then we override the needed services
{
function  TPyPoint.GetAttr(key : PAnsiChar) : PPyObject;
begin
  with GetPythonEngine do
    begin
      if key = 'x' then
        Result := VariantAsPyObject( x )
        // Or  Result := PyInt_FromLong( x )
      else if key = 'y' then
        Result := PyInt_FromLong( y )
        // or  Result := PyInt_FromLong( y )
      else
        Result := inherited GetAttr(key);
    end;
end;

function  TPyPoint.SetAttr(key : PAnsiChar; value : PPyObject) : Integer;
begin
  Result := 0;
  with GetPythonEngine do
    begin
      if key = 'x' then
        begin
          if PyArg_Parse( value, 'i:Point.SetAttr', @x ) = 0 then
            Result := -1;
        end
      else if key = 'y' then
        begin
          if PyArg_Parse( value, 'i:Point.SetAttr', @y ) = 0 then
            Result := -1;
        end
      else
        Result := inherited SetAttr(key, value);
    end;
end;

function  TPyPoint.Repr : PPyObject;
begin
  with GetPythonEngine do
    Result := VariantAsPyObject(Format('(%d, %d)',[x, y]));
    // or Result := PyString_FromString( PAnsiChar(Format('(%d, %d)',[x, y])) );
end;

function  TPyPoint.RichCompare( obj : PPyObject; Op : TRichComparisonOpcode) : PPyObject;
begin
  with GetPythonEngine do
    Result := PyInt_FromLong(1); // Return True by default, just for testing the API.
end;

// Class methods
// We register the methods of our type

class procedure TPyPoint.RegisterMethods( APythonType : TPythonType );
begin
  inherited;
  with APythonType do
    begin
   //   AddMethod( 'OffsetBy', @TPyPoint.DoOffsetBy, 'Point.OffsetBy( dx, dy )' );
   //   AddMethod( 'RaiseError', @TPyPoint.DoRaiseError, 'Point.RaiseError()' );
    end;
end;

// Methods of TPyPoint
// They do the real actions on the object
// It's better to split the functions that interface
// Delphi to Python and the functions that do the
// real implementation.

procedure TPyPoint.OffsetBy( dx, dy : Integer );
begin
  Inc( x, dx );
  Inc( y, dy );
end;

// Interface methods
// They will be called directly by Python, so we extract the
// python arguments and we call the method that will really do
// the action.

function TPyPoint.DoOffsetBy( args : PPyObject ) : PPyObject; cdecl;
var
  dx, dy : Integer;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      // first we extract the arguments
      if PyArg_ParseTuple( args, 'ii:Point.Offset',@dx, @dy ) <> 0 then
        begin
          // if it's ok, then we call the method that does the job
          // with the correct arguments
          OffsetBy( dx, dy );
          // Finally, we return nothing
          Result := ReturnNone;
        end
      else // the arguments were not right
        Result := nil;
    end;
end;

// Here's an example of how you can raise errors defined
// in the module linked to our type.
function TPyPoint.DoRaiseError( args : PPyObject ) : PPyObject; cdecl;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      // This is a simple call:
      //GetModule.RaiseError( 'PointError', 'this is an example of raising an error !' );
      // This is an advanced call:
      // We provide the instance vars as a dictionary, so that we can intercept the
      // error with "except" and extract informations from the error object.
      // ArrayToPyDict needs a list of pairs: varName (string), varValue (anything)
      GetModule.RaiseErrorObj( 'EBadPoint', 'this is an example of raising an error !',
                               ArrayToPyDict( ['a', 1, 'b', 2, 'c', 3] ) );
      Result := nil;
    end;
end;

/////////////////////////////////////////////////

}
procedure TForm1.Button1Click(Sender: TObject);
var
  DelphiPoint : TPyPoint;
  p : PPyObject;
begin
  // Here's how you can create/read Python vars from Delphi with
  // Delphi/Python objects.

  // You should ask to the TPythonType to create an instance of its type
  // because it will do some initialization. You can use CreateInstanceWith
  // if you want to transmit some Python arguments.
  // We receive a Python object pointer
  p := PythonType1.CreateInstance;
  // Then we cast the python object to the right delphi type
  DelphiPoint := TPyPoint( PythonToDelphi(p) );
  // We do some changes on the delphi object
  DelphiPoint.X:=10;
  DelphiPoint.Y:=20;
  // Add variable "myPoint" in the module "spam".
  // So you'll access to the var in the module called spam with:
  //   import spam
  //   print spam.myPoint
  PythonModule1.SetVar( 'myPoint', p );
  PythonEngine1.Py_DecRef(p);
      {
        Of course, if you want to retrieve a Python var from a module,
        just use the PythonModule1.GetVar or PythonModule1.GetVarAsVariant
        Example:
          p := PythonModule1.GetVar('myPoint');
          if Assigned(p) then
          begin
            DelphiPoint := PythonToDelphi(p) as TPyPoint;
            ...
            Py_XDecRef(p);
          end;

    end; }
  // Excecute the script
  PythonEngine1.ExecStrings( Memo1.Lines );
  // Add the following line at the end of the script:
  // print spam.myPoint

  // Note, that you must not free the delphi point yourself.
  // Instead use the GetPythonEngine.Py_XDECREF(obj) method,
  // because the object may be used by another Python object.
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
