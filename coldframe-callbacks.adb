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

-- $Id: coldframe-callbacks.adb,v 35e50a57c3b6 2001/05/11 19:18:50 simon $

package body ColdFrame.Callbacks is

   procedure Register (The_Procedure : P) is
   begin
      Add (To => The_Registered_Procedures,
           The_Procedure => The_Procedure);
   end Register;

   procedure Deregister (The_Procedure : P) is
   begin
      Remove (From => The_Registered_Procedures,
              The_Procedure => The_Procedure);
   end Deregister;

   procedure Call_Registered_Procedures (With_The_T : T) is
      procedure Process (The_Callback : P; OK : out Boolean);
      pragma Inline (Process);
      procedure Process_All is new Callback_Containers.Visit (Process);
      It : Callback_Containers.Iterator'Class
        := New_Iterator (The_Registered_Procedures);
      procedure Process (The_Callback : P; OK : out Boolean) is
      begin
         The_Callback.all (With_The_T);
         OK := True;
      end Process;
   begin
      Process_All (It);
   end Call_Registered_Procedures;

end ColdFrame.Callbacks;
