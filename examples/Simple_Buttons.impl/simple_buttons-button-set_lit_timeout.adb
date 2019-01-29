with ColdFrame.Project.Calendar;
with ColdFrame.Project.Events;
with Simple_Buttons.Events;

separate (Simple_Buttons.Button)
procedure Set_Lit_Timeout
  (This : not null Handle) is
   Since_Pushed : constant Duration
     := ColdFrame.Project.Calendar."-" (ColdFrame.Project.Calendar.Clock,
                                        This.Pushed_Time);
begin
   ColdFrame.Project.Events.Set (The_Timer => This.Lit_Timer,
                                 On        => Events.Dispatcher,
                                 To_Fire   => new Lit_Timeout (This),
                                 After     => 5.0 - Since_Pushed);
end Set_Lit_Timeout;
