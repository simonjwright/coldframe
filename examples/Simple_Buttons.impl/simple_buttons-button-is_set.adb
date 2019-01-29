--  Indicates whether the Button is set or not. It's set if it's in
--  any of the states Pushed, Held, Timed, Pushed_Again.

separate (Simple_Buttons.Button)
function Is_Set
  (This : not null Handle)
  return Boolean is
   Set_In_State : constant array (State_Machine_State_T) of Boolean
     := (Pushed | Held | Timed | Pushed_Again => True,
         others => False);
begin
   return Set_In_State (This.State_Machine_State);
end Is_Set;
