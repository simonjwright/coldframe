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

--  $RCSfile: coldframe-callbacks.ads,v $
--  $Revision: 108edf00e8c7 $
--  $Date: 2003/02/02 18:34:39 $
--  $Author: simon $

generic
   type T (<>) is limited private;
package ColdFrame.Callbacks is

   pragma Elaborate_Body;

   --  The Callback Procedure type
   type Callback is access procedure (The_T : T);

   --  Called to register Proc to receive callbacks
   procedure Register (Proc : Callback);

   --  Called to stop Proc receiving callbacks
   procedure Deregister (Proc : Callback);

   --  Call all the registered callback procedures with With_Param
   procedure Call_Callbacks (With_Param : T);

   --  Clear all registered callbacks
   procedure Clear;

end ColdFrame.Callbacks;
