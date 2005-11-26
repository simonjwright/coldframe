--  Copyright (c) Simon Wright <simon@pushface.org>

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

--  Provides maps from Unbounded_String to File_Access.

--  $Id: serialized_to_csv_support.ads,v e6c41bb0a7c7 2005/11/26 16:20:27 simonjwright $

with Ada.Strings.Unbounded;
with Ada.Text_IO;
with BC.Containers.Maps.Unmanaged;
with ColdFrame.Hash.Strings.Unbounded;

package Serialized_To_Csv_Support is

   type Access_File is access Ada.Text_IO.File_Type;

   package Abstract_Access_File_Containers
   is new BC.Containers (Access_File);

   package Abstract_Access_File_Maps
   is new Abstract_Access_File_Containers.Maps
     (Key => Ada.Strings.Unbounded.Unbounded_String,
      "=" => Ada.Strings.Unbounded."=");

   package Access_File_Maps
   is new Abstract_Access_File_Maps.Unmanaged
     (Hash => ColdFrame.Hash.Strings.Unbounded,
      Buckets => 47);

end Serialized_To_Csv_Support;
