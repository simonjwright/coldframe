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

--  $RCSfile: generated_lines_support.ads,v $
--  $Revision: 1243ef613c9e $
--  $Date: 2002/09/18 20:23:26 $
--  $Author: simon $

with GNAT.Directory_Operations; use GNAT.Directory_Operations;

package Generated_Lines_Support is

   procedure Count (File_Named : Path_Name; Verbosely : Boolean);

   procedure Report;

end Generated_Lines_Support;
