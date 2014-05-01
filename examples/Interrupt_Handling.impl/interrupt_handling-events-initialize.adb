with ColdFrame.Project.Events.Standard;

separate (Interrupt_Handling.Events)
procedure Initialize is
begin
   Dispatcher := new ColdFrame.Project.Events.Standard.Event_Queue;
end Initialize;
