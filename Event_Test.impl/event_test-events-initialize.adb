with ColdFrame.Project.Events.Standard;
separate (Event_Test.Events)
procedure Initialize is
begin
   Dispatcher := new ColdFrame.Project.Events.Standard.Event_Queue;
end Initialize;
