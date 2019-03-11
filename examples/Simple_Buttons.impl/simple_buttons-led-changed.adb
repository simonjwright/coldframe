--  Called when a controlling Button has changed to evaluate whether
--  the LED should be lit (if any of the controlling Buttons is set)
--  or not.

separate (Simple_Buttons.LED)
procedure Changed
  (This : not null Handle) is
   Buttons : constant Button.Vectors.Vector := A1.Controls (This);
begin
   Digital_IO.Set
     (Output_Signal_For_LED (This),
      To_State => (for some B of Buttons => Button.Is_Set (B)));
end Changed;
