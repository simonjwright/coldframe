with ColdFrame.Events.Wall_Timer.Debug;
separate (States.Events)
procedure Initialize is
begin
   Dispatcher := new ColdFrame.Events.Wall_Timer.Debug.Event_Queue;
end Initialize;
