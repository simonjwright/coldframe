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

-- $Id: coldframe-navigate_from_one_to_one_collection.ads,v 0c24f384ffae 2001/05/05 09:38:43 simon $

-- This package supports navigation of a one-to-one association from a
-- set of instances of the 'first' end to the set of instances at the
-- 'second' end.
--
-- Note that the result must be a true set, that is, it will only
-- contain one copy of each instance.

with BC.Containers;

generic

  type First_Handle is private;
  -- The handle for the 'first' end of the association

  with package First is new BC.Containers (First_Handle);
  -- The abstract Container package for the 'first' end of the association

  type From is new First.Container with private;
  -- The Collection type for the 'first' end of the association

  type Second_Handle is private;
  -- The handle for the 'second' end of the association

  with package Second is new BC.Containers (Second_Handle);
  -- The abstract Container package for the 'second' end of the association

  type To is new Second.Container with private;
  -- The Collection type for the 'second' end of the association

  with function Navigate_From_First (S : First_Handle) return Second_Handle;
  -- The simple first-to-second navigation

  with procedure Clear (The_Container : in out To);
  -- Operation to empty the result Collection

  with procedure Add_To_Result
    (To_The_Container : in out To; I : Second_Handle);
  -- Operation to add a result to the result Collection

function ColdFrame.Navigate_From_One_To_One_Collection
   (Input : From) return To;
