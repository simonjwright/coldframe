--  $Id: house_management-lamp-turn_on.adb,v c837ef8247ad 2003/01/07 20:10:08 simon $

--  This state entry action turns on the associated signal via Digital
--  IO.

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
