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

--  $Id: coldframe-navigate_from_many_collection.adb,v 13badf447884 2001/08/16 19:31:36 simon $

with BC.Copy;

function ColdFrame.Navigate_From_Many_Collection (Input : From) return To is


   --  Adds the result of navigating from a single instance at the
   --  'many' end of the association to the intermediate result set.
   procedure Add_Single_Navigation (This : Many_Handle; OK : out Boolean);
   pragma Inline (Add_Single_Navigation);

   --  Iterates over the input, navigating from each 'many' instance
   --  to the corresponding 'one' instance, and adding it to the
   --  intermediate result set.
   procedure Generate_Result_Set is new Many.Visit (Add_Single_Navigation);


   --  Converts the intermediate result Set to the required Collection
   --  form.
   procedure Convert is new BC.Copy
     (Item => One_Handle,
      Source => One,
      From => Set,
      Target => One,
      To => To,
      Add => Add_To_Result);


   It : Many.Iterator'Class := New_Iterator (Input);
   Result_Set : Set;
   Result : To;


   procedure Add_Single_Navigation (This : Many_Handle; OK : out Boolean) is
      Intermediate : constant One_Handle := Navigate_From_Many (This);
      Dummy : Boolean;
   begin
      OK := True;
      if Intermediate /= null then
         Add_To_Set (Result_Set, Intermediate, Dummy);
      end if;
   end Add_Single_Navigation;

begin
   Generate_Result_Set (It);
   Convert (Result_Set, Result);
   return Result;
end ColdFrame.Navigate_From_Many_Collection;
