separate (States.Monitor)
procedure Clear_Warmup_Timeout is
begin
   ColdFrame.States.Timers.Unset (The => This.Warmup_Timer);
end Clear_Warmup_Timeout;
