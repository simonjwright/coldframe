--  Copyright (C) Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under the terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

--  $RCSfile: callback_manager_test_support.adb,v $
--  $Revision: 60df48bec9e0 $
--  $Date: 2010/10/16 19:18:46 $
--  $Author: simonjwright $

with Ada.Text_IO; use Ada.Text_IO;

package body Callback_Manager_Test_Support is


   procedure Callback_Handler (Value : Integer)
   is
   begin
      Put_Line ("Callback_Manager_Test_Support.Callback_Handler:"
                  & " value:" & Value'Img);
   end Callback_Handler;


   procedure Handler (Ev : Class_Event)
   is
   begin
      Integer_Callbacks.Call_Callbacks (Ev.Value);
   end Handler;


end Callback_Manager_Test_Support;
