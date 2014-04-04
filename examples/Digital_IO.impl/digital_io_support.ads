package Digital_IO_Support is

   type Input_Signal is mod 16;
   type Output_Signal is mod 16;

   type Implementation is abstract tagged null record;

   procedure Set (This : Implementation;
                  For_Output : Output_Signal;
                  To : Boolean)
     is abstract;

   type Implementation_Class_P is access all Implementation'Class;

   procedure Register (Impl : Implementation_Class_P);

   procedure Set (O : Output_Signal; To : Boolean);

end Digital_IO_Support;
