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

--  $RCSfile: coldframe-callbacks.adb,v $
--  $Revision: fe5de807621d $
--  $Date: 2003/08/28 21:12:05 $
--  $Author: simon $

with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

package body ColdFrame.Callbacks is


   type Cell;

   type Cell_P is access all Cell;

   type Cell is record
      Next : Cell_P;
      CB : Callback;
   end record;

   procedure Delete is new Ada.Unchecked_Deallocation (Cell, Cell_P);


   The_Registered_Procedures : aliased Cell;


   procedure Register (Proc : Callback) is
      Current_Next : constant Cell_P := The_Registered_Procedures.Next;
   begin
      The_Registered_Procedures.Next := new Cell'(Current_Next, Proc);
   end Register;


   procedure Deregister (Proc : Callback) is
      Current : Cell_P := The_Registered_Procedures'Access;
   begin
      loop
         exit when Current.Next = null;
         if Current.Next.CB = Proc then
            declare
               To_Be_Removed : Cell_P := Current.Next;
            begin
               Current.Next := Current.Next.Next;
               Delete (To_Be_Removed);
            end;
         else
            Current := Current.Next;
         end if;
      end loop;
   end Deregister;


   procedure Call_Callbacks (With_Param : T) is
      Current : Cell_P := The_Registered_Procedures.Next;
      Error : Ada.Exceptions.Exception_Occurrence_Access;
      use type Ada.Exceptions.Exception_Occurrence_Access;
   begin
      loop
         exit when Current = null;
         begin
            Current.CB (With_Param);
         exception
            when E : others =>
               if Error = null then
                  Error := Ada.Exceptions.Save_Occurrence (E);
               end if;
         end;
         Current := Current.Next;
      end loop;
      if Error /= null then
         Ada.Exceptions.Reraise_Occurrence (Error.all);
         --  This will probably leak the allocated
         --  Exception_Occurrence_Access; let's hope it doesn't happen
         --  too often.
      end if;
   end Call_Callbacks;


   procedure Clear is
   begin
      loop
         exit when The_Registered_Procedures.Next = null;
         declare
            Next : constant Cell_P := The_Registered_Procedures.Next.Next;
         begin
            Delete (The_Registered_Procedures.Next);
            The_Registered_Procedures.Next := Next;
         end;
      end loop;
   end Clear;


end ColdFrame.Callbacks;
