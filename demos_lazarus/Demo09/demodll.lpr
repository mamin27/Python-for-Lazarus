library demodll;

{$mode Delphi}

uses
  SysUtils,
  Classes, Interfaces,
  module in 'module.pas';

exports
  initdemodll,
  PyInit_demodll;
{$IFDEF MSWINDOWS}
{$E pyd}
{$ENDIF}
{$IFDEF LINUX}
//{$SONAME 'demodll'}

{$ENDIF}

begin
end.
