with ColdFrame.Project.Events.Standard.Debug;

separate (Interrupt_Handling.Events)
procedure Initialize is
begin

--     Dispatcher := new ColdFrame.Project.Events.Standard.Debug.Event_Queue;
   Dispatcher := new ColdFrame.Project.Events.Standard.Event_Queue;

end Initialize;
