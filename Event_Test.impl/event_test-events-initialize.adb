with ColdFrame.Project.Events.Standard.Test_Debug;
separate (Event_Test.Events)
procedure Initialize is
begin
   Dispatcher := new ColdFrame.Project.Events.Standard.Test_Debug.Event_Queue;
end Initialize;
