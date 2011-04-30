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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

with BC.Support.Memory_Streams;
with ColdFrame.Serialization;

package Serialization_Support is

   Maximum_Record_Size : constant := 1024;

   subtype Base is ColdFrame.Serialization.Base;

   subtype Base_Class is Base'Class;

   subtype Buffer
      is BC.Support.Memory_Streams.Stream_Type (Maximum_Record_Size);

end Serialization_Support;
