-- Copyright (C) Simon Wright <simon@pushface.org>

-- This package is free software; you can redistribute it and/or
-- modify it under terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2, or (at
-- your option) any later version. This package is distributed in the
-- hope that it will be useful, but WITHOUT ANY WARRANTY; without even
-- the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
-- PURPOSE. See the GNU General Public License for more details. You
-- should have received a copy of the GNU General Public License
-- distributed with this package; see file COPYING.  If not, write to
-- the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
-- MA 02111-1307, USA.

-- As a special exception, if other files instantiate generics from
-- this unit, or you link this unit with other files to produce an
-- executable, this unit does not by itself cause the resulting
-- executable to be covered by the GNU General Public License.  This
-- exception does not however invalidate any other reasons why the
-- executable file might be covered by the GNU Public License.

-- $Id: coldframe-string_hash.ads,v 24a93487f94e 2001/01/12 20:33:21 simon $

with Ada.Strings.Bounded;
with Ada.Strings.Unbounded;

package Architecture.String_Hash is
  
  pragma Elaborate_Body;
  
  function Hash (S : String;
                 Hash_Table_Size : in Integer := 43)
                return Integer;
  
  generic
    with package Bounded is new Ada.Strings.Bounded.Generic_Bounded_Length (<>);
  function Bounded_Hash (S : Bounded.Bounded_String;
			 Hash_Table_Size : in Integer := 43)
			return Integer;

  function Hash (S : Ada.Strings.Unbounded.Unbounded_String;
                 Hash_Table_Size : in Integer := 43)
                return Integer;

end Architecture.String_Hash;
