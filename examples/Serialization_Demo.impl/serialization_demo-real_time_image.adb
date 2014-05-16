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

--  Generates an XML image (for serialization output) for the Real_Time type.

with Ada.Unchecked_Conversion;

separate (Serialization_Demo)
function Real_Time_Image
  (V : Real_Time;
   N : String)
  return String is
   function To_Duration is new Ada.Unchecked_Conversion (Ada.Real_Time.Time,
                                                         Duration);
   --  OK for GNAT
   Img : constant String := Duration'Image (To_Duration (V));
begin
   if N'Length > 0 then
      return "<field name=""" & N & """>" & Img & "</field>";
   else
      return "<field name=""Payload"">" & Img & "</field>";
   end if;
end Real_Time_Image;
