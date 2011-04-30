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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

--  Acts as receiver of state changes from Digital IO, via Signal
--  State Callback. Posts a (class) Button Event (only if the button
--  has been pushed).

with ColdFrame.Project.Events;
with House_Management.Events;

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
            E : constant ColdFrame.Project.Events.Event_P
              := new Button_Event;
            P : Button_Name renames Button_Event (E.all).Payload;
         begin
            P := Buttons (S.S);
            ColdFrame.Project.Events.Post (E, On => Events.Dispatcher);
         end;

      else
         raise Constraint_Error;
      end if;

   end if;

end Changed;
