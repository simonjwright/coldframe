package body Digital_IO_Support is

   Impl : Implementation_Class_P;

   procedure Register (Impl : Implementation_Class_P)
   is
   begin
      if Digital_IO_Support.Impl /= null then
         raise Program_Error
           with "Digital_IO_Support: already registered";
      end if;
      if Impl = null then
         raise Constraint_Error
           with "Digital_IO_Support.Register: Impl is null";
      end if;
      Digital_IO_Support.Impl := Impl;
   end Register;

   procedure Set (O : Output_Signal; To : Boolean)
   is
   begin
      if Digital_IO_Support.Impl = null then
         raise Program_Error
           with "Digital_IO_Support: not registered";
      end if;
      Impl.Set (O, To);
   end Set;

end Digital_IO_Support;
