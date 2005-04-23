separate (Regressions.Self_Immolator)
task body T is
begin
   accept Quit;
   Terminate_Self (Handle (This));
end T;
