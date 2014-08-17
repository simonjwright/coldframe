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

generic
   Callback_Type_Name : String;
   with function Value (S : String) return T is <>;
package ColdFrame.Callbacks.Scripting is
   --  Creates a Tcl command callback-<callback_type_name>
   --  (lowercased) which takes one argument: the value to be
   --  provided. If the type has more than one component, it is to be
   --  passed as a list, {component-1 component-2 ...}.
   --
   --  The callback is made at execution time.
private
   procedure Initialize;
   --  Placed here to allow the package to have a body.
end ColdFrame.Callbacks.Scripting;
