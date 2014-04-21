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

--  $RCSfile: coldframe-hash-access_hash.adb,v $
--  $Revision: f6d9ce14c0aa $
--  $Date: 2014/04/21 15:48:31 $
--  $Author: simonjwright $

with System.Storage_Elements;

function ColdFrame.Hash.Access_Hash
  (The_Access_Value : Access_T) return Ada.Containers.Hash_Type is
begin
   if The_Access_Value = null then
      return 0;
   else
      --  This is just a quick hack, more thought needed.
      declare
         Integer_Address : constant System.Storage_Elements.Integer_Address
           := System.Storage_Elements.To_Integer
                (The_Access_Value.all'Address);
         use type System.Storage_Elements.Integer_Address;
      begin
         return Ada.Containers.Hash_Type
           (((Integer_Address / 8) * 43) mod 10019);
      end;
   end if;
end ColdFrame.Hash.Access_Hash;
