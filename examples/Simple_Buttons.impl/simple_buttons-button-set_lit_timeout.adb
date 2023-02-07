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
                                 After     => 3.0 - Since_Pushed);
end Set_Lit_Timeout;
