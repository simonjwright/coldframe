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

with House_Management.Button;
with House_Management.Button_To_Lamp;
with House_Management.A1;

separate (House_Management.Lamp)
procedure Initialize is

   procedure Connect (The_Button : Button_Name; To : Lamp_Name);
   procedure Connect (The_Button : Button_Name; To : Lamp_Name) is
      BH : constant Button.Handle := Button.Find ((Name => The_Button));
      LH : constant Lamp.Handle := Lamp.Find ((Name => To));
      CH : constant Button_To_Lamp.Handle
        := A1.Link (Controls => LH, Is_Controlled_By => BH);
      pragma Warnings (Off, CH);
   begin
      null;
   end Connect;

   --  The Output_Signals used are in reverse order, because of a
   --  foible of the electrician.
   Lamp_Output_Signal : constant array (Lamp_Name) of Output_Signal
     := (Basement     => 3,
         Ground_Floor => 2,
         First_Floor  => 1,
         Second_Floor => 0);

begin

   --  Create the lamps ..
   for L in Lamp_Name loop
      declare
         LH : constant Lamp.Handle := Lamp.Create ((Name => L));
         Ev : Set_Signal (LH);
      begin
         Ev.Payload := Lamp_Output_Signal (L);
         Ev.Handler;
      end;
   end loop;

   --  .. and the buttons ..
   for B in Button_Name loop
      declare
         BH : constant Button.Handle := Button.Create ((Name => B));
         pragma Warnings (Off, BH);
      begin
         null;
      end;
   end loop;

   --  The second floor lamp is controlled by the buttons on the first
   --  and second floors.

   --  The first floor lamp is controlled by the buttons on the
   --  ground, first and second floors.

   --  The ground floor lamp is controlled by the buttons in the
   --  basement and on the ground and first floors.

   --  The basement lamp is controlled by the basement button only.

   Connect (The_Button => Second_Floor, To => Second_Floor);
   Connect (The_Button => First_Floor, To => Second_Floor);

   Connect (The_Button => Second_Floor, To => First_Floor);
   Connect (The_Button => First_Floor, To => First_Floor);
   Connect (The_Button => Ground_Floor, To => First_Floor);

   Connect (The_Button => First_Floor, To => Ground_Floor);
   Connect (The_Button => Ground_Floor, To => Ground_Floor);
   Connect (The_Button => Basement, To => Ground_Floor);

   Connect (The_Button => Basement, To => Basement);

   --  Read the initial button states.
   Button.Read_Initial_States;

end Initialize;
