with ColdFrame.Project.Events.Standard.Debug;

separate (House_Management.Events)
procedure Initialize is
begin

   Dispatcher := new ColdFrame.Project.Events.Standard.Event_Queue;

end Initialize;
