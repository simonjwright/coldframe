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

--  $RCSfile: digital_io_support.adb,v $
--  $Revision: fde6fd75a1a0 $
--  $Date: 2014/04/05 13:21:13 $
--  $Author: simonjwright $

package body Digital_IO_Support is

   Impl : Implementation_Class_P;

   procedure Register (Impl : Implementation_Class_P)
   is
   begin
      if Digital_IO_Support.Impl /= null then
         raise Program_Error
           with "Digital_IO_Support: already registered";
      end if;
      if Impl = null then
         raise Constraint_Error
           with "Digital_IO_Support.Register: Impl is null";
      end if;
      Digital_IO_Support.Impl := Impl;
   end Register;

   procedure Set (O : Output_Signal; To : Boolean)
   is
   begin
      if Digital_IO_Support.Impl = null then
         raise Program_Error
           with "Digital_IO_Support: not registered";
      end if;
      Impl.Set (O, To);
   end Set;

end Digital_IO_Support;
