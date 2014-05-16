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

with Tcl.Async;

package body Digital_IO.Tcl_Support is

   procedure Initialize
   is
   begin
      Register (new Implementation);
   end Initialize;

   procedure Set (This : Implementation;
                  For_Output : Digital_IO_Support.Output_Signal;
                  To : Boolean)
   is
      pragma Unreferenced (This);  -- only used for dispatching
      Tcl_Key : constant Character
        := Character'Val (Character'Pos ('a') + Natural (For_Output));
   begin
      Tcl.Async.Set (Tcl_Array => "lampState",
                     Index => String'(1 => Tcl_Key),
                     Value => Integer'Image (Boolean'Pos (To)));
   end Set;

end Digital_IO.Tcl_Support;
