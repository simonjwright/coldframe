separate (Regressions)
protected body PT_Holder is


   function Get_State
     return Boolean is
   begin
      return State;
   end Get_State;


   procedure Set_State
     (To : Boolean) is
   begin
      State := To;
   end Set_State;


end PT_Holder;
