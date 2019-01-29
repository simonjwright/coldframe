--  This operation initializes the Lamps and Buttons.

with Digital_IO;
with Simple_Buttons.Button;
with Simple_Buttons.A1;

separate (Simple_Buttons.LED)
procedure Initialize is

   procedure Connect (The_Button : Button_Name; To : LED_Name);
   procedure Connect (The_Button : Button_Name; To : LED_Name) is
      BH : constant Button.Handle := Button.Find ((Name => The_Button));
      LH : constant LED.Handle := LED.Find ((Name => To));
      CH : constant A1.Handle
        := A1.Link (Controls => LH, Is_Controlled_By => BH);
      pragma Warnings (Off, CH);
   begin
      null;
   end Connect;

begin

   --  Create the LEDs ..
   for L in LED_Name loop
      declare
         LH : constant LED.Handle := LED.Create ((Name => L))
           with Unreferenced;
      begin
         null;
      end;
   end loop;

   --  Turn off all the LEDs ..
   for L in Digital_IO.Output_Signal loop
      Digital_IO.Set (O => L, To_State => False);
   end loop;

   --  .. and the buttons
   for B in Button_Name loop
      declare
         BH : constant Button.Handle
           := Button.Create ((Name => B));
         pragma Warnings (Off, BH);
      begin
         null;
      end;
   end loop;

   Connect (The_Button => B1, To => L1);

end Initialize;
