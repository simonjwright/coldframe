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

--  $RCSfile: coldframe-events-real_timer-debug.adb,v $
--  $Revision: 8f8b358d26d1 $
--  $Date: 2002/02/06 20:46:50 $
--  $Author: simon $

with Ada.Tags;
with GNAT.IO; use GNAT.IO;

package body ColdFrame.States.Real_Timer.Debug is


   procedure Post (The : Event_P;
                   On : access Event_Queue) is
   begin

      Put_Line ("posting a " & Ada.Tags.Expanded_Name (The'Tag));
      Real_Timer.Post (The => The,
                       On => Real_Timer.Event_Queue (On.all)'Access);

   end Post;


   procedure Set (The : in out Timer;
                  On : access Event_Queue;
                  To_Fire : Event_P;
                  After : Natural_Duration) is

   begin

      Put_Line ("setting a Timer for a "
                & Ada.Tags.Expanded_Name (To_Fire.all'Tag)
                & ", delay"
                & After'Img);
      Real_Timer.Set (The => The,
                      On => Real_Timer.Event_Queue (On.all)'Access,
                      To_Fire => To_Fire,
                      After => After);

   end Set;


   procedure Unset (The : in out Timer;
                    On : access Event_Queue) is

   begin

      Put_Line ("unsetting a Timer for a "
                & Ada.Tags.Expanded_Name (The.The_Event.all'Tag));
      Real_Timer .Unset (The => The,
                         On => Real_Timer.Event_Queue (On.all)'Access);

   end Unset;


   procedure Log_Retraction (The : Event_P;
                             On : access Event_Queue) is
   begin
      Put_Line ("retracting a " & Ada.Tags.Expanded_Name (The'Tag));
   end Log_Retraction;


   procedure Log_Pre_Dispatch (The : Event_P;
                               On : access Event_Queue) is
   begin
      Put_Line ("dispatching a "
                & Ada.Tags.Expanded_Name (The'Tag)
                & ": state "
                & State_Image (The.For_The_Instance.all));
   end Log_Pre_Dispatch;


   procedure Log_Post_Dispatch (The : Event_P;
                                On : access Event_Queue) is
   begin
      Put_Line (".. new state "
                & State_Image (The.For_The_Instance.all));
   end Log_Post_Dispatch;


end ColdFrame.States.Real_Timer.Debug;
