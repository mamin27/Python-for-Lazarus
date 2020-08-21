program ThrdDemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
     {$IFDEF UseCThreads}
  cthreads,
     {$ENDIF}
  {$ENDIF}
  Forms, Interfaces,
  ThSort in 'ThSort.pas' {ThreadSortForm},
  SortThds in 'SortThds.pas';

{$IFDEF WINDOWS}
{$R project1.rc}
{$ENDIF}
{$IFDEF UNIX}
{$R *.res}
{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TThreadSortForm, ThreadSortForm);
  Application.Run;
end.

