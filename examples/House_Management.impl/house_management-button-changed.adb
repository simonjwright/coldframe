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

--  Acts as receiver of state changes from Digital IO, via Signal
--  State Callback. If a Button has been pushed, posts
--  Button_Pushed events to the Lamps which are controlled by that
--  Button. Button releases are ignored.

with House_Management.Lamp.Vectors;
with House_Management.Lamp.Iterate;
with House_Management.A1;

separate (House_Management.Button)
procedure Changed
  (S : Input_Signal_State) is

   subtype Valid_Input_Signal is Digital_IO.Input_Signal range 0 .. 3;

   --  For some reason, the input signals are mapped "upside down".
   Buttons : constant array (Valid_Input_Signal) of Button_Name
     := (0 => Second_Floor,
         1 => First_Floor,
         2 => Ground_Floor,
         3 => Basement);

begin

   if S.State then

      if S.S in Valid_Input_Signal then

         declare
            procedure Button_Pushed (L : not null Lamp.Handle);
            pragma Inline (Button_Pushed);
            procedure Process is new Lamp.Iterate (Button_Pushed);
            procedure Button_Pushed (L : not null Lamp.Handle) is
               Ev : Lamp.Button_Push (For_The_Instance => L);
               pragma Warnings (Off, Ev);
            begin
               Lamp.Handler (Ev);
            end Button_Pushed;

            BH : constant Handle := Find ((Name => Buttons (S.S)));
            LHS : constant Lamp.Vectors.Vector
              := A1.Is_Controlled_By (BH);
         begin
            Process (LHS);
         end;

      else
         raise Constraint_Error;
      end if;

   end if;

end Changed;
