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

--  $RCSfile: coldframe-exceptions-symbolic_traceback.ads,v $
--  $Revision: e91d25f64219 $
--  $Date: 2002/10/01 17:43:10 $
--  $Author: simon $

package ColdFrame.Exceptions.Symbolic_Traceback is

   --  This package enables GNAT's symbolic exception traceback
   --  facility, which is set up to report unhandled exceptions both
   --  in the main program and in tasks. To use it, you must
   --
   --  (a) "with" this package,
   --  (b) give the switch -E to the binder.
   --
   --  Note, this facility isn't available on all targets (in
   --  particular, VxWorks).

   pragma Elaborate_Body;

end ColdFrame.Exceptions.Symbolic_Traceback;
