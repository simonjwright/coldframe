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

-- $Id: coldframe-navigate_from_many_collection.ads,v 9edd793b410a 2001/04/25 19:18:32 simon $

-- This package supports navigation of a one-to-many association from
-- a set of instances of the 'one' end to the set of instances at the
-- 'many' end.
--
-- Note that the result will be a true set, that is, it will only
-- contain one copy of each instance.

with BC.Containers;

generic

  type One_Handle is private;
  -- The handle for the 'one' end of the association

  with package One is new BC.Containers (One_Handle);
  -- The Collections package for the 'one' end of the association

  type From is new One.Container with private;
  -- The Collection type for the 'one' end of the association

  type Many_Handle is private;
  -- The handle for the 'one' end of the association

  with package Intermediate is new BC.Containers (Many_Handle);
  -- A package with 'set' functionality, to support intermediate
  -- results

  type Set is new Intermediate.Container with private;
  -- To hold the set of results, ensuring uniqueness

  with procedure Add_To_Set
    (S : in out Set; I : Many_Handle; Added : out Boolean);
  -- Operation to add a result to the intermediate result

  with package Many is new BC.Containers (Many_Handle);
  -- The Collections package for the 'many' end of the association

  type To is new Many.Container with private;
  -- The Collection type for the 'many' end of the association

  with function Navigate_From_One (S : One_Handle) return To;
  -- The simple one-to-many navigation

  with procedure Clear (The_Container : in out To);
  -- Operation to empty the result Collection

  with procedure Add_To_Result
    (To_The_Container : in out To; I : Many_Handle);
  -- Operation to add a result to the result Collection

function Architecture.Navigate_From_One_Collection (Input : From) return To;
