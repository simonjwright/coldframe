with Regressions.PT_Owner;

separate (Regressions.PT_User)
procedure Set_State
  (To : Boolean) is
   pragma Assert (Domain_Initialized, "Regressions not initialized");
begin
   PT_Owner.Get_H_Access.Set_State (To);
end Set_State;
