with ColdFrame.Project.Events.Standard.Test;
with Debugging;
separate (Event_Test.Events)
procedure Initialize is
begin
   Dispatcher := new Debugging.Debugging_Queues.Event_Queue;
--     Dispatcher := new ColdFrame.Project.Events.Standard.Test.Event_Queue;
end Initialize;
