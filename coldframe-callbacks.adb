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
--  $Revision: 2f10d942e470 $
--  $Date: 2004/04/26 12:02:32 $
--  $Author: simon $

with Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with ColdFrame.Project.Log_Error;

package body ColdFrame.Callbacks is


   type Cell;

   type Cell_P is access all Cell;

   type Cell is record
      Next : Cell_P;
      CB : Callback;
   end record;

   procedure Delete is new Ada.Unchecked_Deallocation (Cell, Cell_P);

   --  Local assertion support.
   function Is_Present (CB : Callback) return Boolean;

   The_Registered_Procedures : aliased Cell;


   procedure Call_Callbacks (With_Param : T) is
      Current : Cell_P := The_Registered_Procedures.Next;
   begin
      loop
         exit when Current = null;
         begin
            Current.CB (With_Param);
         exception
            when E : others =>
               ColdFrame.Project.Log_Error
                 (Ada.Exceptions.Exception_Information (E));
         end;
         Current := Current.Next;
      end loop;
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


   procedure Deregister (Proc : Callback) is
      pragma Assert (Is_Present (Proc),
                     "callback procedure not registered");
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


   function Is_Present (CB : Callback) return Boolean is
      Current : Cell_P := The_Registered_Procedures.Next;
   begin
      loop
         if Current = null then
            return False;
         end if;
         if Current.CB = CB then
            return True;
         end if;
         Current := Current.Next;
      end loop;
   end Is_Present;


   procedure Register (Proc : Callback) is
      pragma Assert (not Is_Present (Proc),
                     "callback procedure already registered");
      Current_Next : constant Cell_P := The_Registered_Procedures.Next;
   begin
      The_Registered_Procedures.Next := new Cell'(Current_Next, Proc);
   end Register;


end ColdFrame.Callbacks;
