with House_Management.Button;
with House_Management.Button_To_Lamp;
with House_Management.A1;
with Digital_IO.Signal_State_Callback;

separate (House_Management.Lamp)
procedure Initialize is

   LH : Lamp.Handle;
   BH : Button.Handle;
   BTLH : Button_To_Lamp.Handle;

begin

   --  Lamp_A is between, and is controlled by, Floor_0 and Floor_1.
   --  Lamp_B is between, and is controlled by, Floor_1 and Floor_2.
   --  Lamp_C is between, and is controlled by, Floor_2 and Floor_3.
   BH := Button.Create ((Name => Floor_0));
   LH := Lamp.Create ((Name => Lamp_A));
   BTLH := A1.Link (Controls => LH, Is_Controlled_By => BH);
   BH := Button.Create ((Name => Floor_1));
   BTLH := A1.Link (Controls => LH, Is_Controlled_By => BH);
   LH := Lamp.Create ((Name => Lamp_B));
   BTLH := A1.Link (Controls => LH, Is_Controlled_By => BH);
   BH := Button.Create ((Name => Floor_2));
   BTLH := A1.Link (Controls => LH, Is_Controlled_By => BH);
   LH := Lamp.Create ((Name => Lamp_C));
   BTLH := A1.Link (Controls => LH, Is_Controlled_By => BH);
   BH := Button.Create ((Name => Floor_3));
   BTLH := A1.Link (Controls => LH, Is_Controlled_By => BH);

   --  Register for button state changes.
   Digital_IO.Signal_State_Callback.Register (Button.Changed'Access);

end Initialize;
