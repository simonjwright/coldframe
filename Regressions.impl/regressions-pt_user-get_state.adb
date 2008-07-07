with Regressions.PT_Owner;

separate (Regressions.PT_User)
function Get_State
  return Boolean is
   pragma Assert (Domain_Initialized, "Regressions not initialized");
begin
   return PT_Owner.Get_H_Access.Get_State;
end Get_State;
