with States.Events;

separate (States.Monitor)
procedure Clear_Warmup_Timeout (This : Handle) is
begin
   ColdFrame.Events.Unset (The => This.Warmup_Timer,
                           On => Events.Dispatcher);
end Clear_Warmup_Timeout;
