with ColdFrame.Events.Standard.Debug;
separate (States.Events)
procedure Initialize is
begin
   Dispatcher := new ColdFrame.Events.Standard.Debug.Event_Queue;
end Initialize;
