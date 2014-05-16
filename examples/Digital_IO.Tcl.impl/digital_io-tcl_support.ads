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

with Digital_IO_Support;

package Digital_IO.Tcl_Support is

   procedure Initialize;

private

   type Implementation
     is new Digital_IO_Support.Implementation with null record;

   procedure Set (This : Implementation;
                  For_Output : Digital_IO_Support.Output_Signal;
                  To : Boolean);

end Digital_IO.Tcl_Support;
