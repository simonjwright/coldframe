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

--  $RCSfile: digital_io_support.ads,v $
--  $Revision: fde6fd75a1a0 $
--  $Date: 2014/04/05 13:21:13 $
--  $Author: simonjwright $

package Digital_IO_Support is

   type Input_Signal is mod 16;
   type Output_Signal is mod 16;

   type Implementation is abstract tagged null record;

   procedure Set (This : Implementation;
                  For_Output : Output_Signal;
                  To : Boolean)
     is abstract;

   type Implementation_Class_P is access all Implementation'Class;

   procedure Register (Impl : Implementation_Class_P);

   procedure Set (O : Output_Signal; To : Boolean);

end Digital_IO_Support;
