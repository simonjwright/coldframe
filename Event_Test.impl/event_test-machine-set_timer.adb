with Event_Test.Events;

separate (Event_Test.Machine)
procedure Set_Timer
  (This : Handle;
   To : Duration) is
   Ev : constant ColdFrame.Project.Events.Event_P
     := new Machine.Kill (This);
begin
   ColdFrame.Project.Events.Set (The_Timer => This.T,
                                 On => Events.Dispatcher,
                                 To_Fire => Ev,
                                 After => To);
end Set_Timer;
