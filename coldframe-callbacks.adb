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
--  $Revision: 285737f99b9a $
--  $Date: 2003/02/02 18:34:11 $
--  $Author: simon $

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
   begin
      loop
         exit when Current = null;
         Current.CB (With_Param);
         Current := Current.Next;
      end loop;
   end Call_Callbacks;


   procedure Clear is
   begin
      loop
         exit when The_Registered_Procedures.Next = null;
         declare
            Next : Cell_P := The_Registered_Procedures.Next.Next;
         begin
            Delete (The_Registered_Procedures.Next);
            The_Registered_Procedures.Next := Next;
         end;
      end loop;
   end Clear;


end ColdFrame.Callbacks;
