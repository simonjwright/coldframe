--  Stores the time at which the Button was pushed: the Lit timeout
--  runs from this time, not the time of Button release.

with ColdFrame.Project.Calendar;

separate (Simple_Buttons.Button)
procedure Note_Pushed_Time
  (This : not null Handle) is
begin
   This.Pushed_Time := ColdFrame.Project.Calendar.Clock;
end Note_Pushed_Time;
