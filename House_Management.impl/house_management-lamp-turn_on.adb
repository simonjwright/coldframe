with Digital_IO.Application;

separate (House_Management.Lamp)
procedure Turn_On
  (This : Handle) is

   Signals : constant array (Lamp_Name) of Digital_IO.Signal_Name
     := (Lamp_A => Digital_IO.Lamp_A,
         Lamp_B => Digital_IO.Lamp_B,
         Lamp_C => Digital_IO.Lamp_C);

begin

   Digital_IO.Application.Set_Output (Signals (This.Name), To_State => True);

end Turn_On;
