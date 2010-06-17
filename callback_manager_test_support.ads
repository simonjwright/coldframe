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

--  $RCSfile: callback_manager_test_support.ads,v $
--  $Revision: 215173d4855f $
--  $Date: 2010/06/17 21:54:03 $
--  $Author: simonjwright $

with ColdFrame.Callbacks;
with ColdFrame.Project.Events.Standard.Callback_Manager;

package Callback_Manager_Test_Support is

   package Integer_Callbacks is new ColdFrame.Callbacks (Integer);

   package Managed_Integer_Callbacks
   is new ColdFrame.Project.Events.Standard.Callback_Manager.Callback_Manager_G
     (T => Integer,
      Callback => Integer_Callbacks);

   procedure Callback_Handler (Value : Integer);

   type Class_Event is new ColdFrame.Project.Events.Event_Base with record
      Value : Integer;
   end record;
   procedure Handler (Ev : Class_Event);

end Callback_Manager_Test_Support;
