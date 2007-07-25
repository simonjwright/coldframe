with ColdFrame.Project.Events.Monitoring.Test;
separate (Event_Test.Events)
procedure Initialize is
   subtype Milliseconds is Integer;
   Monitor_Report_Trigger_Ms : Milliseconds;
   pragma Import (Ada, Monitor_Report_Trigger_Ms, "monitor_report_trigger");
begin
   Monitor_Report_Trigger_Ms := 1;
   Dispatcher := new ColdFrame.Project.Events.Monitoring.Test.Event_Queue;
end Initialize;
