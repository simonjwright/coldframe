with ColdFrame.Project.Events.Standard.Debug;
separate (States.Events)
procedure Initialize is
begin
   Dispatcher := new ColdFrame.Project.Events.Standard.Debug.Event_Queue
                       (Start_Started => True);
end Initialize;
