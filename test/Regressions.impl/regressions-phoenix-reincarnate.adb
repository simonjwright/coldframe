--  Delete the current instance and create a new one; which should be
--  in the Initial state.

separate (Regressions.Phoenix)
procedure Reincarnate
  (This : Handle) is
   Old_One : Handle := This;
   New_One : Handle;
   pragma Unreferenced (New_One);
begin
   Delete (Old_One);
   New_One := Create;
end Reincarnate;
