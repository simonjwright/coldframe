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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

with BC.Containers.Queues.Unmanaged;

generic

   type Task_Type (<>) is limited private;
   --  The class's task type (T).

   type Task_Type_P is access Task_Type;
   --  The class's pointer-to-task (T_P).

   with function Is_Terminated (It : Task_Type_P) return Boolean;
   --  We can't say that Task_Type is actually a task, so we can't use
   --  'Identity.

package ColdFrame.Task_Deletion_G is

   procedure Free (It : Task_Type_P);
   --  Puts It on the queue of tasks to be freed.

private

   --  The queue of tasks to be deleted has to be declared in the spec
   --  (GCC 4.3.0).

   package Abstract_Containers is new BC.Containers (Task_Type_P);
   package Abstract_Queues is new Abstract_Containers.Queues;
   package Queues is new Abstract_Queues.Unmanaged;

end ColdFrame.Task_Deletion_G;
