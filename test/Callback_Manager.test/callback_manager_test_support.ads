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

with ColdFrame.Callbacks;
with ColdFrame.Project.Events.Callback_Manager;

package Callback_Manager_Test_Support is

   package Integer_Callbacks is new ColdFrame.Callbacks (Integer);

   package Managed_Integer_Callbacks
   is new ColdFrame.Project.Events.Callback_Manager.Callback_Manager_G
     (T => Integer,
      Callback => Integer_Callbacks);

   procedure Callback_Handler (Value : Integer);

end Callback_Manager_Test_Support;
