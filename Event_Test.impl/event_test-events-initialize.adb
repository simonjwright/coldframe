with ColdFrame.Project.Events.Standard.Test_Trace;
separate (Event_Test.Events)
procedure Initialize is
begin
   Dispatcher := new ColdFrame.Project.Events.Standard.Test_Trace.Event_Queue;
end Initialize;
