separate (States.Monitor)
procedure Clear_Heartbeat_Timeout is
begin
   ColdFrame.States.Timers.Unset (The => This.Heartbeat_Timer);
end Clear_Heartbeat_Timeout;
