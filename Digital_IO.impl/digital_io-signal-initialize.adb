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

--  $RCSfile: digital_io-signal-initialize.adb,v $
--  $Revision: 42a0dc129c12 $
--  $Date: 2004/04/25 17:01:45 $
--  $Author: simon $

--  Creates all the Signals.

with Digital_IO.Input.Inheritance;
with Digital_IO.Output.Inheritance;

separate (Digital_IO.Signal)
procedure Initialize is

   procedure Create (S : Signal_Name; As_Input : Boolean);
   procedure Create (S : Signal_Name; As_Input : Boolean) is
      SH : constant ColdFrame.Instances.Handle
        := ColdFrame.Instances.Handle  (Signal.Create ((S => S)));
   begin
      if As_Input then
         declare
            IH : constant Input.Handle
              := Input.Inheritance.Create_Tree (SH);
            pragma Warnings (Off, IH);
         begin
            null;
         end;
      else
         declare
            OH : constant Output.Handle
              := Output.Inheritance.Create_Tree (SH);
            pragma Warnings (Off, OH);
         begin
            null;
         end;
      end if;
   end Create;

begin

   for I in Floor_0 .. Floor_3 loop
      Create (I, As_Input => True);
   end loop;

   for O in Lamp_A .. Lamp_D loop
      Create (O, As_Input => False);
   end loop;

end Initialize;
