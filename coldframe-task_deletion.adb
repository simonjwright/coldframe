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

--  $RCSfile: coldframe-task_deletion.adb,v $
--  $Revision: fc22e3084517 $
--  $Date: 2008/07/03 05:03:53 $
--  $Author: simonjwright $

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with BC.Support.Synchronization;
with ColdFrame.Project.Task_Deletion;

package body ColdFrame.Task_Deletion is


   Q : Queues.Queue;
   --  The queue of tasks to be deleted.
   Semaphore : aliased BC.Support.Synchronization.Semaphore;
   --  Locking for Q.


   procedure Free (It : Task_Type_P)
   is
      L : BC.Support.Synchronization.Lock (Semaphore'Access);
      pragma Unreferenced (L);
   begin
      Queues.Append (Q, It);
   end Free;


   procedure Delete_Terminated;
   procedure Delete_Terminated
   is
      procedure Free is new Ada.Unchecked_Deallocation (Task_Type,
                                                        Task_Type_P);
      L : BC.Support.Synchronization.Lock (Semaphore'Access);
      pragma Unreferenced (L);
      It : Abstract_Containers.Iterator'Class
        := Queues.New_Iterator (Q);
      Count : Natural := Queues.Length (Q);
      use Abstract_Containers;
   begin
      while not Is_Done (It) loop
         declare
            P : Task_Type_P := Current_Item (It);
         begin
            if Is_Terminated (P) then
               Count := Count - 1;
               Free (P);
               Delete_Item_At (It);
            else
               exit;
--                 Next (It);
            end if;
         end;
      end loop;
      Put_Line ("remaining items:" & Count'Img);
   end Delete_Terminated;


begin
   Project.Task_Deletion.Register (Delete_Terminated'Unrestricted_Access);
end ColdFrame.Task_Deletion;
