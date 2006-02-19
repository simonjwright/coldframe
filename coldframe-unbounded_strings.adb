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

--  $RCSfile: coldframe-unbounded_strings.adb,v $
--  $Revision: f872ca30d215 $
--  $Date: 2006/02/19 21:16:16 $
--  $Author: simonjwright $

with Ada.Unchecked_Deallocation;

package body ColdFrame.Unbounded_Strings is


   procedure Free is new Ada.Unchecked_Deallocation (String, String_P);


   procedure Append (To : in out Unbounded_String; C : Character) is
   begin
      Append (To, String'((1 => C)));
   end Append;


   procedure Append (To : in out Unbounded_String; S : String) is
   begin
      if To.Last + S'Length > To.Buf'Length then
         declare
            Extended_Size : Positive := To.Buf'Length;
            New_Buffer : String_P;
         begin
            loop
               Extended_Size := Extended_Size * 2;
               exit when To.Last + S'Length <= Extended_Size;
            end loop;
            New_Buffer := new String (1 .. Extended_Size);
            New_Buffer (1 .. To.Last) := To.Buf (1 .. To.Last);
            Free (To.Buf);
            To.Buf := New_Buffer;
         end;
      end if;
      To.Buf (To.Last + 1 .. To.Last + S'Length) := S;
      To.Last := To.Last + S'Length;
   end Append;


   function To_String (S : Unbounded_String) return String is
   begin
      return S.Buf (1 .. S.Last);
   end To_String;


   procedure Initialize (U : in out Unbounded_String) is
   begin
      U.Buf := new String (1 .. 1024);
   end Initialize;


   procedure Finalize (U : in out Unbounded_String) is
   begin
      if U.Buf /= null then
         Free (U.Buf);
      end if;
   end Finalize;


end ColdFrame.Unbounded_Strings;
