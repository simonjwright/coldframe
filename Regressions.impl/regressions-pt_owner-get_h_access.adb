separate (Regressions.PT_Owner)
function Get_H_Access
  return PT_Holder_P is
begin
   return This.H'Access;
end Get_H_Access;
