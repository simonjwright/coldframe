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

--  $RCSfile: coldframe-unbounded_strings.ads,v $
--  $Revision: f872ca30d215 $
--  $Date: 2006/02/19 21:16:16 $
--  $Author: simonjwright $

with Ada.Finalization;

package ColdFrame.Unbounded_Strings is

   type Unbounded_String
      is new Ada.Finalization.Limited_Controlled with private;

   procedure Append (To : in out Unbounded_String; C : Character);
   procedure Append (To : in out Unbounded_String; S : String);

   function To_String (S : Unbounded_String) return String;

private

   type String_P is access String;

   type Unbounded_String is new Ada.Finalization.Limited_Controlled with record
      Last : Natural := 0;
      Buf : String_P;
   end record;

   procedure Initialize (U : in out Unbounded_String);
   procedure Finalize (U : in out Unbounded_String);

end ColdFrame.Unbounded_Strings;
