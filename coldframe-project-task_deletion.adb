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
--  Deletion_Proc will be called (at low priority) to delete any
--  terminated tasks.

--  $RCSfile: coldframe-project-task_deletion.adb,v $
--  $Revision: 77156b56042e $
--  $Date: 2008/07/06 16:22:32 $
--  $Author: simonjwright $

with BC.Containers.Collections.Unmanaged;
with BC.Support.Synchronization;

package body ColdFrame.Project.Task_Deletion is


   package Abstract_Containers is new BC.Containers (Deletion_Proc);
   package Abstract_Collections is new Abstract_Containers.Collections;
   package Collections is new Abstract_Collections.Unmanaged;

   Semaphore : aliased BC.Support.Synchronization.Semaphore;
   C : Collections.Collection;


   task type T is
      pragma Priority (Deleting_Task_Priority);
   end T;
   type T_P is access T;
   Task_Deleter : T_P;


   procedure Register (It : Deletion_Proc)
   is
      L : BC.Support.Synchronization.Lock (Semaphore'Access);
      pragma Unreferenced (L);
   begin
      if Task_Deleter = null then
         Task_Deleter := new T;
      end if;
      Collections.Append (C, It);
   end Register;


   task body T is
   begin
      loop
         delay 1.0;
         declare
            L : BC.Support.Synchronization.Lock (Semaphore'Access);
            pragma Unreferenced (L);
            It : Abstract_Containers.Iterator'Class
              := Collections.New_Iterator (C);
            use Abstract_Containers;
         begin
            while not Is_Done (It) loop
               Current_Item (It).all;
               Next (It);
            end loop;
         end;
      end loop;
   end T;


end ColdFrame.Project.Task_Deletion;
