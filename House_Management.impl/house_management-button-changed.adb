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
--  $Revision: 0f7eda971a8e $
--  $Date: 2003/02/07 05:55:57 $
--  $Author: simon $

--  Acts as receiver of state changes from Digital IO.

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

   if S.S in Floors then

      if S.State then

         Pushed (Find ((Name => Buttons (S.S))));

      end if;

   else
      raise Constraint_Error;
   end if;

end Changed;
