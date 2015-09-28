Program ceosdaemon;

Uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  CThreads,
{$ENDIF}{$ENDIF}
  interfaces, DaemonApp, lazdaemonapp, ceosdaemonmapper, svcdaemon, ceosmw
  { add your units here };

begin
  Application.Initialize;
  Application.Run;
end.
