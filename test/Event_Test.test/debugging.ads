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

with ColdFrame.Project.Events.Standard.Debug;
with ColdFrame.Events_G.Test_G;
package Debugging is

   package Debugging_Queues is new ColdFrame.Project.Events.Test_G
     (Standard_Queue =>
        ColdFrame.Project.Events.Standard.Debug.Event_Queue_Base);

   subtype Event_Queue
      is Debugging_Queues.Event_Queue;

end Debugging;
