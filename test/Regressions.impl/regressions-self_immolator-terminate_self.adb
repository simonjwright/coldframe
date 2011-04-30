separate (Regressions.Self_Immolator)
procedure Terminate_Self
  (This : Handle) is
   H : Handle := This;
begin
   Delete (H);
end Terminate_Self;
