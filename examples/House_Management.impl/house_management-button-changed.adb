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

--  $RCSfile: house_management-button-changed.adb,v $
--  $Revision: f3a9cc2c7d9c $
--  $Date: 2014/01/11 14:11:13 $
--  $Author: simonjwright $

--  Acts as receiver of state changes from Digital IO, via Signal
--  State Callback. If a Button has been pushed, posts
--  Button_Pushed events to the Lamps which are controlled by that
--  Button. Button releases are ignored.

with House_Management.Lamp.Collections;
with House_Management.Lamp.Iterate;
with House_Management.A1;

separate (House_Management.Button)
procedure Changed
  (S : Signal_State) is

   subtype Floors is Digital_IO.Signal_Name
     range Digital_IO.Floor_0 .. Digital_IO.Floor_3;

   Buttons : constant array (Floors) of Button_Name
     := (Digital_IO.Floor_0 => Second_Floor,
         Digital_IO.Floor_1 => First_Floor,
         Digital_IO.Floor_2 => Ground_Floor,
         Digital_IO.Floor_3 => Basement);

begin

   if S.State then

      if S.S in Floors then

         declare
            procedure Button_Pushed (L : Lamp.Handle);
            pragma Inline (Button_Pushed);
            procedure Process is new Lamp.Iterate (Button_Pushed);
            procedure Button_Pushed (L : Lamp.Handle) is
               Ev : Lamp.Button_Push (For_The_Instance => L);
            begin
               Lamp.Handler (Ev);
            end Button_Pushed;

            BH : constant Handle := Find ((Name => Buttons (S.S)));
            LHS : constant Lamp.Collections.Collection
              := A1.Is_Controlled_By (BH);
         begin
            Process (LHS);
         end;

      else
         raise Constraint_Error;
      end if;

   end if;

end Changed;
