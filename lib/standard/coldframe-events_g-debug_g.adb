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

with Ada.Exceptions;
with Ada.Tags;
with ColdFrame.Exceptions;
with ColdFrame.Project.Log_Info;

package body ColdFrame.Events_G.Debug_G is


   procedure Post (The_Event : not null Event_P;
                   On : not null access Event_Queue_Base) is
   begin
      Project.Log_Info ("posting a " & Ada.Tags.Expanded_Name (The_Event'Tag));
      Post (The_Event => The_Event,
            On => Standard_Queue (On.all)'Access);
   end Post;


   procedure Post_To_Self (The_Event : not null Event_P;
                           On : not null access Event_Queue_Base) is
   begin
      Project.Log_Info
        ("posting a " & Ada.Tags.Expanded_Name (The_Event'Tag) & " to self");
      Post_To_Self (The_Event => The_Event,
                    On => Standard_Queue (On.all)'Access);
   end Post_To_Self;


   procedure Post (The_Event : not null Event_P;
                   On : not null access Event_Queue_Base;
                   To_Fire_At : Time.Time) is
   begin
      Project.Log_Info ("posting a " &
                          Ada.Tags.Expanded_Name (The_Event'Tag) &
                          ", to fire at " &
                          Time.Image (To_Fire_At));
      Post (The_Event => The_Event,
            On => Standard_Queue (On.all)'Access,
            To_Fire_At => To_Fire_At);
   end Post;


   procedure Post (The_Event : not null Event_P;
                   On : not null access Event_Queue_Base;
                   To_Fire_After : Natural_Duration) is
   begin
      Project.Log_Info ("posting a " &
                          Ada.Tags.Expanded_Name (The_Event'Tag) &
                          ", delay" &
                          To_Fire_After'Img);
      Post (The_Event => The_Event,
            On => Standard_Queue (On.all)'Access,
            To_Fire_After => To_Fire_After);
   end Post;


   procedure Set (The_Timer : in out Timer;
                  On : not null access Event_Queue_Base;
                  To_Fire : not null Event_P;
                  At_Time : Time.Time) is
   begin
      Project.Log_Info ("setting a Timer for a " &
                          Ada.Tags.Expanded_Name (To_Fire.all'Tag) &
                          ", to fire at " &
                          Time.Image (At_Time));
      Set (The_Timer => The_Timer,
           On => Standard_Queue (On.all)'Access,
           To_Fire => To_Fire,
           At_Time => At_Time);
   end Set;


   procedure Set (The_Timer : in out Timer;
                  On : not null access Event_Queue_Base;
                  To_Fire : not null Event_P;
                  After : Natural_Duration) is
   begin
      Project.Log_Info ("setting a Timer for a " &
                          Ada.Tags.Expanded_Name (To_Fire.all'Tag) &
                          ", delay" &
                          After'Img);
      Set (The_Timer => The_Timer,
           On => Standard_Queue (On.all)'Access,
           To_Fire => To_Fire,
           After => After);
   end Set;


   procedure Unset (The_Timer : in out Timer;
                    On : not null access Event_Queue_Base) is

   begin
      if The_Timer.The_Entry = null then
         Ada.Exceptions.Raise_Exception
           (Exceptions.Use_Error'Identity,
            "attempt to unset a timer that wasn't set");
      elsif Held_Event (The_Timer.The_Entry.all).The_Event = null then
         Ada.Exceptions.Raise_Exception
           (Exceptions.Use_Error'Identity,
            "attempt to unset a timer from its own event handler");
      else
         Project.Log_Info
           ("unsetting a Timer for a "
              & Ada.Tags.Expanded_Name
              (Held_Event (The_Timer.The_Entry.all).The_Event.all'Tag));
      end if;
      Unset (The_Timer => The_Timer,
             On => Standard_Queue (On.all)'Access);
   end Unset;


   procedure Log_Retraction (The_Event : not null Event_P;
                             On : not null access Event_Queue_Base) is
      pragma Warnings (Off, On);
   begin
      Project.Log_Info ("retracting a "
                          & Ada.Tags.Expanded_Name (The_Event'Tag));
   end Log_Retraction;


   procedure Log_Pre_Dispatch (The_Event : not null Event_P;
                               On : not null access Event_Queue_Base) is
      pragma Warnings (Off, On);
   begin
      if The_Event.all in Instance_Event_Base'Class then
         Project.Log_Info
           ("dispatching a "
              & Ada.Tags.Expanded_Name (The_Event'Tag)
              & ": state "
              & State_Image
              (Instance_Event_Base (The_Event.all).For_The_Instance.all));
      else
         Project.Log_Info
           ("dispatching a "
              & Ada.Tags.Expanded_Name (The_Event'Tag));
      end if;
   end Log_Pre_Dispatch;


   procedure Log_Post_Dispatch (The_Event : not null Event_P;
                                On : not null access Event_Queue_Base) is
      pragma Warnings (Off, On);
   begin
      if The_Event.all in Instance_Event_Base'Class then
         if Instance_Event_Base (The_Event.all).Instance_Deleted then
            Project.Log_Info (".. deleted");
         else
            Project.Log_Info
              (".. new state "
                 & State_Image
                 (Instance_Event_Base (The_Event.all).For_The_Instance.all));
         end if;
      end if;
   end Log_Post_Dispatch;


end ColdFrame.Events_G.Debug_G;
