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

-- $Id: coldframe-callbacks.ads,v 35e50a57c3b6 2001/05/11 19:18:50 simon $

with BC.Containers;
generic
   type T is limited private;
   type P is access procedure (The_T : T);
   with package Callback_Containers is new BC.Containers (P);
   type Container is new Callback_Containers.Container with private;
   with procedure Add (To : in out Container; The_Procedure : P) is <>;
   with procedure Remove (From : in out Container; The_Procedure : P) is <>;
package ColdFrame.Callbacks is
   procedure Register (The_Procedure : P);
   procedure Deregister (The_Procedure : P);
   procedure Call_Registered_Procedures (With_The_T : T);
private
   The_Registered_Procedures : Container;
end ColdFrame.Callbacks;

