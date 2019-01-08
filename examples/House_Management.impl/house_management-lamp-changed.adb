--------------------------------------------
--  Automatically generated:  edit this!  --
--  PLEASE DELETE THIS BANNER AFTERWARDS  --
--------------------------------------------

--  This operation is used by a linked Button to tell the Lamp
--  to evaluate all its linked Buttons to see if any are Set (in which
--  case the Lamp should be lit) or all are Reset (in which case the
--  Lamp should be off).

with Digital_IO;
with House_Management.A1;
with House_Management.Button;

separate (House_Management.Lamp)
procedure Changed
  (This : not null Handle) is
   Buttons : constant Button.Vectors.Vector := A1.Controls (This);
begin
   Digital_IO.Set
     (Output_For_Lamp (This),
      To_State => (for some B of Buttons => Button.Get_State (B)));
end Changed;
