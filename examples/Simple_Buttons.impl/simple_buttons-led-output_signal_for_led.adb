--  Maps the LED to the corresponding Digital_IO output pin.

separate (Simple_Buttons.LED)
function Output_Signal_For_LED
  (This : not null Handle)
  return Output_Signal is
   pragma Unreferenced (This);
begin
   return 0;
end Output_Signal_For_LED;
