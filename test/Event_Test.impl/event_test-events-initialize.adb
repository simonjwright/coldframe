with ColdFrame.Project.Events.Monitoring.Test;
separate (Event_Test.Events)
procedure Initialize is
begin
   Dispatcher := new ColdFrame.Project.Events.Monitoring.Test.Event_Queue;
end Initialize;
