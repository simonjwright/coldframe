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

-- $Id: coldframe-navigate_from_one_collection.adb,v c98aee53a2b4 2001/04/27 19:05:02 simon $

with BC.Copy;

function Architecture.Navigate_From_One_Collection (Input : From) return To is


  -- Used instead of a real Clear in an instantiation of BC.Copy to
  -- give incremental addition.
  procedure Null_Clear (S : in out Set);
  pragma Inline (Null_Clear);

  -- Used in an instantiation of BC.Copy to add results to the
  -- intermediate result set.
  procedure Add_Many (S : in out Set; I : Many_Handle);
  pragma Inline (Add_Many);

  -- Adds the result of a single one-to-many navigation to the
  -- intermediate result set.
  procedure Add_To_Result_Set is new BC.Copy
     (Item => Many_Handle,
      Source => Many,
      From => To,
      Target => Intermediate,
      To => Set,
      Clear => Null_Clear,
      Add => Add_Many);


  -- Adds the result of navigating from a single instance at the 'one'
  -- end of the association to the intermediate result set.
  procedure Add_Single_Navigation (This : One_Handle; OK: out Boolean);
  pragma Inline (Add_Single_Navigation);

  -- Iterates over the input, navigating from each 'one' instance to
  -- the corresponding set of 'many' instances, and adding them to the
  -- intermediate result set.
  procedure Generate_Result_Set is new One.Visit (Add_Single_Navigation);


  -- Converts the intermediate result Set to the required Collection
  -- form.
  procedure Convert is new BC.Copy
     (Item => Many_Handle,
      Source => Intermediate,
      From => Set,
      Target => Many,
      To => To,
      Add => Add_To_Result);


  It : One.Iterator'Class := New_Iterator (Input);
  Result_Set : Set;
  Result : To;


  procedure Add_Many (S : in out Set; I : Many_Handle) is
    Dummy : Boolean;
  begin
    Add_To_Set (S, I, Dummy);
  end Add_Many;

  procedure Add_Single_Navigation (This : One_Handle; OK: out Boolean) is
  begin
    OK := True;
    Add_To_Result_Set (Navigate_From_One (This), Result_Set);
  end Add_Single_Navigation;

  procedure Null_Clear (S : in out Set) is
    pragma Warnings (Off, S);
  begin
    null;
  end Null_Clear;

begin
  Generate_Result_Set (It);
  Convert (Result_Set, Result);
  return Result;
end Architecture.Navigate_From_One_Collection;
