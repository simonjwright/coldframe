--  Copyright (C) Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under the terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

--  $RCSfile: coldframe-events-test_support.ads,v $
--  $Revision: 3daa7f1e04bd $
--  $Date: 2002/02/03 10:26:21 $
--  $Author: simon $

with ColdFrame.States.Wall_Timer.Debug;

package ColdFrame.States.Test_Support is

   type Instance is new Instance_Base with null record;
   function State_Image (This : Instance) return String;

   type Ev (For_The_Instance : access Instance_Base'Class)
   is new Event_Base (For_The_Instance) with null record;

   procedure Handler (This : Ev);

   Dispatcher : ColdFrame.States.Event_Queue_P
     := new ColdFrame.States.Wall_Timer.Debug.Event_Queue;

   T1, T2 : States.Timer;

end ColdFrame.States.Test_Support;
