separate (States.Monitor)
procedure Set_6_Second_Warmup_Timeout is
   E : ColdFrame.States.Event_P := new Warmup_Timeout (This);
begin
   ColdFrame.States.Timers.Set (The => This.Warmup_Timer,
                                To_Fire => E,
                                After => 6.0);
end Set_6_Second_Warmup_Timeout;
