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

--  $RCSfile: coldframe-task_deletion_g.adb,v $
--  $Revision: f6d9ce14c0aa $
--  $Date: 2014/04/21 15:48:31 $
--  $Author: simonjwright $

--  with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;
with ColdFrame.Synchronization;
with ColdFrame.Task_Deletion;

package body ColdFrame.Task_Deletion_G is


   procedure Delete_Terminated;
   --  The procedure that actually does the deletion. To be called at
   --  suitable intervals.

   package Queues is new Ada.Containers.Vectors
     (Index_Type => Positive,
      Element_Type => Task_Type_P);

   Q : Queues.Vector;
   --  The queue of tasks to be deleted.

   Semaphore : aliased ColdFrame.Synchronization.Semaphore;
   --  Locking for Q.


   procedure Free (It : not null Task_Type_P)
   is
      L : ColdFrame.Synchronization.Lock (Semaphore'Access);
      pragma Unreferenced (L);
   begin
      Queues.Append (Q, It);
   end Free;


   procedure Delete_Terminated
   is
      procedure Free is new Ada.Unchecked_Deallocation (Task_Type,
                                                        Task_Type_P);
      L : ColdFrame.Synchronization.Lock (Semaphore'Access);
      pragma Unreferenced (L);
      It : Queues.Cursor := Q.First;
      use type Queues.Cursor;
   begin
      while It /= Queues.No_Element loop
         declare
            P : Task_Type_P := Queues.Element (It);
         begin
            if Is_Terminated (P) then
               Free (P);
               Q.Delete (It);  -- NB, sets It to No_Element
            else
               Queues.Next (It);
            end if;
         end;
      end loop;
   end Delete_Terminated;


begin
   --  Register with the deletion manager task.
   Task_Deletion.Register (Delete_Terminated'Unrestricted_Access);
end ColdFrame.Task_Deletion_G;
