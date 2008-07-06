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

--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License.  This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

--  This unit provides a mechanism for an active class's tasks to be
--  held after instance deletion until they are actually terminated
--  and their storage can be freed (GNAT silently ignores frees if the
--  task isn't terminated).
--
--  Instantiations of ColdFrame.Task_Deletion_G register here; their
--  Deletion_Proc will be called (at low priority, as determined by
--  Deleting_Task_Priority) to delete any terminated tasks.

--  $RCSfile: coldframe-project-task_deletion.ads,v $
--  $Revision: 17b662230fa7 $
--  $Date: 2008/07/06 18:48:12 $
--  $Author: simonjwright $

with System;

package ColdFrame.Project.Task_Deletion is

   Deleting_Task_Priority : constant System.Priority
     := System.Default_Priority - 1;

   type Deletion_Proc is access procedure;
   procedure Register (It : Deletion_Proc);
   --  'It' will be called regularly, at Deleting_Task_Priority, to
   --  delete terminated tasks.

   procedure Add_Using_Domain;
   --  Called by domains during Initialize.

   procedure Remove_Using_Domain;
   --  Called by domains during Tear_Down.

end ColdFrame.Project.Task_Deletion;
