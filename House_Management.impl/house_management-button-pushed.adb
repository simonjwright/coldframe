--  $Id: house_management-button-pushed.adb,v c837ef8247ad 2003/01/07 20:10:08 simon $

--  The button has been pushed; tell the associated Lamp(s).

with House_Management.Lamp.Collections;
with House_Management.Lamp.Iterate;
with House_Management.A1;

separate (House_Management.Button)
procedure Pushed
  (This : Handle) is

   procedure Process is new Lamp.Iterate (Lamp.Button_Pushed);

   LHs : constant Lamp.Collections.Collection
     := A1.Is_Controlled_By (This);

begin

   Process (LHs);

end Pushed;
