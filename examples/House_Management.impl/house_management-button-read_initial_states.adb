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

--  This initialization operation retrieves, for each Button, the
--  current state of the corresponding Digital IO's Input Signal.

--  If the Input Signal is set, calls the Changed operation to
--  simulate a callback.

separate (House_Management.Button)
procedure Read_Initial_States is

   subtype Valid_Input_Signal is Digital_IO.Input_Signal range 0 .. 3;

   --  For some reason, the input signals are mapped "upside down".
   Inputs : constant array (Button_Name) of Valid_Input_Signal
     := (Second_Floor => 0,
         First_Floor  => 1,
         Ground_Floor => 2,
         Basement     => 3);

begin
   for B in Button_Name loop
      The_Container (B).State := Digital_IO.Get (Inputs (B));
      --  If it wasn't set, it hasn't changed from the initial
      --  assumption; no need to say so.
      if The_Container (B).State then
         Changed (Digital_IO.Input_Signal_State'(S => Inputs (B),
                                                 State => True));
      end if;
   end loop;
end Read_Initial_States;
