--  The state of the button has changed; tell the controlled LEDs to
--  reevaluate their own states (by checking whether any of the
--  Buttons they are controlled by is set).

separate (Simple_Buttons.Button)
procedure Changed
  (This : not null Handle) is
   LEDs : constant LED.Vectors.Vector := A1.Is_Controlled_By (This);
begin
   for L of LEDs loop
      LED.Changed (L);
   end loop;
end Changed;
