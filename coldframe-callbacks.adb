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

--  $Id: coldframe-callbacks.adb,v d34c9dbd812d 2001/09/11 05:16:37 simon $

package body ColdFrame.Callbacks is

   The_Registered_Procedures : Collections.Collection;

   procedure Register (Proc : Callback) is
   begin
      Collections.Append (C => The_Registered_Procedures,
                          Elem => Proc);
   end Register;

   procedure Deregister (Proc : Callback) is
      Loc : Natural;
   begin
      Loc := Collections.Location (The_Registered_Procedures, Proc);
      Collections.Remove (The_Registered_Procedures, Loc);
   end Deregister;

   procedure Call_Callbacks (With_Param : T) is
      procedure Process (The_Callback : Callback; OK : out Boolean);
      pragma Inline (Process);
      procedure Process_All is new Abstract_Containers.Visit (Process);
      It : Abstract_Containers.Iterator'Class
        := Collections.New_Iterator (The_Registered_Procedures);
      procedure Process (The_Callback : Callback; OK : out Boolean) is
      begin
         OK := True;
         The_Callback.all (With_Param);
      exception
         when others => null;  --  should actually log this!
      end Process;
   begin
      Process_All (It);
   end Call_Callbacks;

end ColdFrame.Callbacks;
