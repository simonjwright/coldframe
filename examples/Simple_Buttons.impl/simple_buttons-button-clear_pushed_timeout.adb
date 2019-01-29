with ColdFrame.Project.Events;
with Simple_Buttons.Events;

separate (Simple_Buttons.Button)
procedure Clear_Pushed_Timeout
  (This : not null Handle) is
begin
   ColdFrame.Project.Events.Unset (The_Timer => This.Pushed_Timer,
                                   On        => Events.Dispatcher);
end Clear_Pushed_Timeout;
