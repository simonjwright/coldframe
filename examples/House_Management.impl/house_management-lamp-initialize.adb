--  Copyright (C) Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

--  This operation initializes the Lamps and Buttons.

with Digital_IO;
with House_Management.Button;
with House_Management.Timed_Button.Inheritance;
with House_Management.Toggle_Button.Inheritance;
with House_Management.A1;

separate (House_Management.Lamp)
procedure Initialize is

   procedure Connect (The_Button : Button_Name; To : Lamp_Name);
   procedure Connect (The_Button : Button_Name; To : Lamp_Name) is
      BH : constant Button.Handle := Button.Find ((Name => The_Button));
      LH : constant Lamp.Handle := Lamp.Find ((Name => To));
      CH : constant A1.Handle
        := A1.Link (Controls => LH, Is_Controlled_By => BH);
      pragma Warnings (Off, CH);
   begin
      null;
   end Connect;

begin

   --  Create the lamps ..
   for L in Lamp_Name loop
      declare
         LH : constant Lamp.Handle := Lamp.Create ((Name => L))
           with Unreferenced;
      begin
         null;
      end;
   end loop;

   --  Turn off all the lamps ..
   for L in Digital_IO.Output_Signal loop
      Digital_IO.Set (O => L, To_State => False);
   end loop;

   --  .. and the buttons
   for B in Timed_Button_Name loop
      declare
         BH : constant Button.Handle
           := Button.Create ((Name => B));
         pragma Warnings (Off, BH);
         TBH : constant Timed_Button.Handle
           := Timed_Button.Inheritance.Create_Tree
             (ColdFrame.Instances.Handle (BH));
         pragma Warnings (Off, TBH);
      begin
         null;
      end;
   end loop;
   for B in Toggle_Button_Name loop
      declare
         BH : constant Button.Handle
           := Button.Create ((Name => B));
         pragma Warnings (Off, BH);
         TBH : constant Toggle_Button.Handle
           := Toggle_Button.Inheritance.Create_Tree
             (ColdFrame.Instances.Handle (BH));
         pragma Warnings (Off, TBH);
      begin
         null;
      end;
   end loop;

   --  Timed buttons:

   --  The second floor lamp is controlled by the buttons on the first
   --  and second floors.

   --  The first floor lamp is controlled by the buttons on the
   --  ground, first and second floors.

   --  The ground floor lamp is controlled by the buttons in the
   --  basement and on the ground and first floors.

   --  The basement lamp is controlled by the basement button only.

   Connect (The_Button => Second_Floor_Timed, To => Second_Floor);
   Connect (The_Button => First_Floor_Timed, To => Second_Floor);

   Connect (The_Button => Second_Floor_Timed, To => First_Floor);
   Connect (The_Button => First_Floor_Timed, To => First_Floor);
   Connect (The_Button => Ground_Floor_Timed, To => First_Floor);

   Connect (The_Button => First_Floor_Timed, To => Ground_Floor);
   Connect (The_Button => Ground_Floor_Timed, To => Ground_Floor);
   Connect (The_Button => Basement_Timed, To => Ground_Floor);

   Connect (The_Button => Basement_Timed, To => Basement);

   --  Toggle buttons: each button is connected to the lamp on its own
   --  floor.

   Connect (The_Button => Second_Floor_Toggle, To => Second_Floor);

   Connect (The_Button => First_Floor_Toggle, To => First_Floor);

   Connect (The_Button => Ground_Floor_Toggle, To => Ground_Floor);

   Connect (The_Button => Basement_Toggle, To => Basement);

end Initialize;
