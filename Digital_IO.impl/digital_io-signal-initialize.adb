with Digital_IO.Input;
with Digital_IO.Output;

separate (Digital_IO.Signal)
procedure Initialize is

   SH : Signal.Handle;
   IH : Input.Handle;
   OH : Output.Handle;

   subtype CIH is ColdFrame.Instances.Handle;

begin

   SH := Signal.Create ((S => Floor_0));
   IH := Input.Create ((G1_Parent => CIH (SH)));
   SH := Signal.Create ((S => Floor_1));
   IH := Input.Create ((G1_Parent => CIH (SH)));
   SH := Signal.Create ((S => Floor_2));
   IH := Input.Create ((G1_Parent => CIH (SH)));
   SH := Signal.Create ((S => Floor_3));
   IH := Input.Create ((G1_Parent => CIH (SH)));

   SH := Signal.Create ((S => Lamp_A));
   OH := Output.Create ((G1_Parent => CIH (SH)));
   SH := Signal.Create ((S => Lamp_B));
   OH := Output.Create ((G1_Parent => CIH (SH)));
   SH := Signal.Create ((S => Lamp_C));
   OH := Output.Create ((G1_Parent => CIH (SH)));

end Initialize;
