with ColdFrame.Project.Events.Standard.Test;
separate (Event_Test.Events)
procedure Initialize is
begin
   Dispatcher := new ColdFrame.Project.Events.Standard.Test.Event_Queue;
end Initialize;
