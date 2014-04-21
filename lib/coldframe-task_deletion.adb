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

--  $RCSfile: coldframe-task_deletion.adb,v $
--  $Revision: f6d9ce14c0aa $
--  $Date: 2014/04/21 15:48:31 $
--  $Author: simonjwright $

with Ada.Containers.Vectors;
with ColdFrame.Synchronization;

package body ColdFrame.Task_Deletion is


   type Containable_Deletion_Proc is access procedure;
   --  Can't be null-excluding, or Containers would raise CE.

   package Containers is new Ada.Containers.Vectors
     (Index_Type => Positive,
      Element_Type => Containable_Deletion_Proc);


   Semaphore : aliased ColdFrame.Synchronization.Semaphore;
   Deletion_Procedures : Containers.Vector;


   task type Deleting_Task is
      pragma Priority (Deleting_Task_Priority);
      entry Add_Using_Domain;
      entry Remove_Using_Domain;
   end Deleting_Task;
   type Deleting_Task_P is access Deleting_Task;
   Task_Deleter : Deleting_Task_P;


   procedure Register (It : Deletion_Proc)
   is
      L : ColdFrame.Synchronization.Lock (Semaphore'Access);
      pragma Unreferenced (L);
   begin
      if Task_Deleter = null then
         Task_Deleter := new Deleting_Task;
      end if;
      Deletion_Procedures.Append (Containable_Deletion_Proc (It));
   end Register;


   procedure Add_Using_Domain
   is
      pragma Assert (Task_Deleter /= null, "no Task_Deleter");
   begin
      Task_Deleter.Add_Using_Domain;
   end Add_Using_Domain;


   procedure Remove_Using_Domain
   is
      pragma Assert (Task_Deleter /= null, "no Task_Deleter");
   begin
      Task_Deleter.Remove_Using_Domain;
   end Remove_Using_Domain;


   task body Deleting_Task is
      Users : Natural := 0;
   begin
      loop
         select
            accept Add_Using_Domain do
               Users := 1;
            end Add_Using_Domain;
         or
            terminate;
         end select;
         loop
            select
               accept Add_Using_Domain do
                  Users := Users + 1;
               end Add_Using_Domain;
            or
               accept Remove_Using_Domain do
                  Users := Users - 1;
               end Remove_Using_Domain;
               exit when Users = 0;
            or
               delay 1.0;
               declare
                  L : ColdFrame.Synchronization.Lock (Semaphore'Access);
                  pragma Unreferenced (L);
                  It : Containers.Cursor := Deletion_Procedures.First;
                  use type Containers.Cursor;
               begin
                  while It /= Containers.No_Element loop
                     Containers.Element (It).all;
                     Containers.Next (It);
                  end loop;
               end;
            end select;
         end loop;
      end loop;
   end Deleting_Task;


end ColdFrame.Task_Deletion;
