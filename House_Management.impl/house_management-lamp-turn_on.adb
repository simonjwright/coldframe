with Digital_IO.Application;

separate (House_Management.Lamp)
procedure Turn_On
  (This : Handle) is

   Signals : constant array (Lamp_Name) of Digital_IO.Signal_Name
     := (Second_Floor => Digital_IO.Lamp_A,
         First_Floor => Digital_IO.Lamp_B,
         Ground_Floor => Digital_IO.Lamp_C,
         Basement => Digital_IO.Lamp_D);

begin

   Digital_IO.Application.Set_Output (Signals (This.Name), To_State => True);

end Turn_On;
