with ColdFrame.Project.Events;
with Simple_Buttons.Events;

separate (Simple_Buttons.Button)
procedure Set_Pushed_Timeout
  (This : not null Handle) is
begin
   ColdFrame.Project.Events.Set (The_Timer => This.Pushed_Timer,
                                 On        => Events.Dispatcher,
                                 To_Fire   => new Push_Timeout (This),
                                 After     => 1.0);
end Set_Pushed_Timeout;
