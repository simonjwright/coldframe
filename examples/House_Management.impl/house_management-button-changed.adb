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
--  State Callback. Calls the instance Changed so the Button can take
--  the appropriate action.

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
   if S.S in Valid_Input_Signal then
      declare
         B : constant Handle := Find ((Name => Buttons (S.S)));
      begin
         --  If there are any events involved, they must be processed
         --  synchronously.
         Changed (This => B, Pushed => S.State);
      end;
   end if;
end Changed;
