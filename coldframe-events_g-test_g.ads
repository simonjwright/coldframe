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

--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License.  This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

--  $RCSfile: coldframe-events_g-test_g.ads,v $
--  $Revision: acb2cc29bb74 $
--  $Date: 2003/07/24 19:50:30 $
--  $Author: simon $

generic

   type Standard_Queue is new Events_G.Event_Queue_Base with private;

package ColdFrame.Events_G.Test_G is

   pragma Elaborate_Body;

   ---------------------
   --  Event queuing  --
   ---------------------

   type Event_Queue_Base (Start_Started : Boolean;
                          Priority : System.Priority;
                          Storage_Size : Positive)
   is new Standard_Queue with private;

   subtype Event_Queue is Event_Queue_Base
     (Start_Started => False,
      Priority => System.Default_Priority,
      Storage_Size => 20_000);


   --------------
   --  Timers  --
   --------------

   function Is_Set (The_Timer : Timer;
                    On : access Event_Queue_Base) return Boolean;

   function Expires_At (The_Timer : Timer;
                        On : access Event_Queue_Base) return Time.Time;
   --  Raises ColdFrame.Exceptions.Use_Error if the Timer isn't set.


   -------------------------
   --  Unit test support  --
   -------------------------

   procedure Wait_Until_Idle (The_Queue : access Event_Queue_Base;
                              Ignoring_Timers : Boolean := False);

private

   --  Determining whether there are any events left (if not, unit test can
   --  stop).
   protected type Event_Count is

      entry Wait_Until_Idle;
      --  Blocks until there are no events pending or held or on timers.

      entry Wait_Until_No_Timed_Events;
      --  Blocks until there are no events pending or held.

      procedure Add_Posted_Event;
      procedure Remove_Posted_Event;
      procedure Add_Held_Event;
      procedure Remove_Held_Event;
      procedure Add_Timer_Event;
      procedure Remove_Timer_Event;

   private

      Posted_Events : Natural := 0;
      Held_Events : Natural := 0;
      Timed_Events : Natural := 0;

   end Event_Count;


   type Event_Queue_Base (Start_Started : Boolean;
                          Priority : System.Priority;
                          Storage_Size : Positive)
   is new Standard_Queue (Start_Started => Start_Started,
                          Priority => Priority,
                          Storage_Size => Storage_Size)
   with record
      The_Event_Count : Event_Count;
   end record;


   procedure Note_Addition_Of_Posted_Event (On : access Event_Queue_Base);

   procedure Note_Removal_Of_Posted_Event (On : access Event_Queue_Base);

   procedure Note_Addition_Of_Held_Event (On : access Event_Queue_Base);

   procedure Note_Removal_Of_Held_Event (On : access Event_Queue_Base);

   procedure Note_Addition_Of_Timer_Event (On : access Event_Queue_Base);

   procedure Note_Removal_Of_Timer_Event (On : access Event_Queue_Base);


end ColdFrame.Events_G.Test_G;
