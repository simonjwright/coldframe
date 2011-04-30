--  Copyright (C) Simon Wright <simon@pushface.org>

--  Algorithm due to Donald Knuth; from an implementation by Daniel
--  Gaudry <Daniel.Gaudry@wanadoo.fr>

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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

function ColdFrame.Hash.Strings.Bounded
  (S : Bounded_Strings.Bounded_String) return Natural is
   K : Special_Integer := 0;
   N : Special_Integer := 0;
   use Bounded_Strings;
begin

   for M in 1 .. Length (S) loop
      N := Character_Hash (Element (S, M));
      K := K + Special_Integer (M) * N;
   end loop;

   return Natural (K);

end ColdFrame.Hash.Strings.Bounded;
