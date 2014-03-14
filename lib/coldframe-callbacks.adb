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
--  $Revision: 6b2a0cfb80d0 $
--  $Date: 2014/03/14 18:34:45 $
--  $Author: simonjwright $

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

   The_Registered_Procedures : Cell_P;


   procedure Call_Callbacks (With_Param : T) is
      Current : Cell_P := The_Registered_Procedures;
      Next : Cell_P;
   begin
      loop
         exit when Current = null;
         Next := Current.Next;
         --  In case the called procedure deregisters itself.  NB, no
         --  protection in case the deregistered cell is the next one!
         --  but that would be bizarre ..
         begin
            Current.CB (With_Param);
         exception
            when E : others =>
               ColdFrame.Project.Log_Error
                 (Ada.Exceptions.Exception_Information (E));
         end;
         Current := Next;
      end loop;
   end Call_Callbacks;


   procedure Clear is
   begin
      loop
         exit when The_Registered_Procedures = null;
         declare
            Next : constant Cell_P := The_Registered_Procedures.Next;
         begin
            Delete (The_Registered_Procedures);
            The_Registered_Procedures := Next;
         end;
      end loop;
   end Clear;


   procedure Deregister (Proc : Callback) is
      pragma Assert (Is_Present (Proc),
                       "callback procedure not registered");
   begin
      while The_Registered_Procedures /= null
        and then The_Registered_Procedures.CB = Proc loop
         declare
            To_Be_Freed : Cell_P := The_Registered_Procedures;
         begin
            The_Registered_Procedures := The_Registered_Procedures.Next;
            Delete (To_Be_Freed);
         end;
      end loop;
      declare
         Current : Cell_P := The_Registered_Procedures;
      begin
         loop
            exit when Current = null;
            pragma Assert (Current.CB /= Proc);
            declare
               Next : Cell_P := Current.Next;
            begin
               if Next /= null and then Next.CB = Proc then
                  Current.Next := Next.Next;
                  Delete (Next);
               else
                  Current := Next;
               end if;
            end;
         end loop;
      end;
   end Deregister;


   function Is_Present (CB : Callback) return Boolean is
      Current : Cell_P := The_Registered_Procedures;
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
      Current_Next : constant Cell_P := The_Registered_Procedures;
   begin
      The_Registered_Procedures := new Cell'(Current_Next, Proc);
   end Register;


end ColdFrame.Callbacks;
