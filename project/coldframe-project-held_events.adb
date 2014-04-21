--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This package provides support operations for project events.
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-held_events.adb,v $
--  $Revision: f6d9ce14c0aa $
--  $Date: 2014/04/21 15:48:31 $
--  $Author: simonjwright $

with Ada.Unchecked_Deallocation;

package body ColdFrame.Project.Held_Events is

   --  Local procedure to insert new element after all exisitng
   --  elements with earlier or the same time. This preserves order of
   --  submission.
   procedure Append_In_Order (Container : in out Time_Vectors.Vector;
                              New_Item : Time_Cell);
   procedure Append_In_Order (Container : in out Time_Vectors.Vector;
                              New_Item : Time_Cell)
   is
      package TV renames Time_Vectors;
      It : TV.Cursor := Container.First;
      use type TV.Cursor;
      use type ColdFrame.Project.Times.Time;
   begin
      while It /= TV.No_Element
        and then
        (TV.Element (It).Time_To_Fire < New_Item.Time_To_Fire
           or else TV.Element (It).Time_To_Fire = New_Item.Time_To_Fire) loop
         TV.Next (It);
      end loop;
      if It = TV.No_Element then
         Container.Append (New_Item => New_Item, Count => 1);
      else
         Container.Insert (Before => It, New_Item => New_Item);
      end if;
   end Append_In_Order;


   function Is_Empty (Q : Queue) return Boolean is
   begin
      for Kind in Q.Queues'Range loop
         if not Q.Queues (Kind).Is_Empty then
            return False;
         end if;
      end loop;
      return True;
   end Is_Empty;


   function Next_Event_Time (Q : Queue) return Ada.Real_Time.Time is
      Result : Ada.Real_Time.Time := Ada.Real_Time.Time_Last;
      use type Ada.Real_Time.Time;
   begin
      pragma Assert (not Is_Empty (Q));
      for Kind in Q.Queues'Range loop
         if not Q.Queues (Kind).Is_Empty then
            declare
               T : constant Ada.Real_Time.Time
                 := Times.Equivalent
                   (Q.Queues (Kind).First_Element.Time_To_Fire);
            begin
               if T < Result then
                  Result := T;
               end if;
            end;
         end if;
      end loop;
      return Result;
   end Next_Event_Time;


   procedure Pop (Q : in out Queue; The_Head_Event : out Events.Event_P) is
      Earliest_Time : Ada.Real_Time.Time := Ada.Real_Time.Time_Last;
      Earliest_Kind : Times.Time_Kind;
      use type Ada.Real_Time.Time;
   begin
      pragma Assert (not Is_Empty (Q));
      for Kind in Q.Queues'Range loop
         if not Q.Queues (Kind).Is_Empty then
            declare
               T : constant Ada.Real_Time.Time
                 := Times.Equivalent
                   (Q.Queues (Kind).First_Element.Time_To_Fire);
            begin
               if T < Earliest_Time then
                  Earliest_Time := T;
                  Earliest_Kind := Kind;
               end if;
            end;
         end if;
      end loop;
      The_Head_Event := Q.Queues (Earliest_Kind).First_Element.Event;
      Q.Queues (Earliest_Kind).Delete_First;
   end Pop;


   procedure Add_At_Event (E : Events.Event_P;
                           To_Run_At : Times.Time;
                           On : in out Queue) is
   begin
      if On.Started then
         Append_In_Order (On.Queues (To_Run_At.Kind),
                          (Time_To_Fire => To_Run_At,
                           Event => E));
      else
         --  We save in the order of adding, until the queue is
         --  started, at which point all the events on the Initial
         --  queue are Append[ed]_In_Order.
         On.Initial_Queue.Append ((Time_To_Fire => To_Run_At,
                                   Event => E));
      end if;
   end Add_At_Event;


   procedure Add_After_Event (E : Events.Event_P;
                              To_Run_After : Duration;
                              On : in out Queue) is
      use type Ada.Real_Time.Time;
   begin
      if On.Started then
         Add_At_Event
           (E,
            Times.Create (Ada.Real_Time.Clock
                            + Ada.Real_Time.To_Time_Span (To_Run_After)),
            On);
      else
         On.Duration_Queue.Append ((Delay_To_Fire => To_Run_After,
                                    Event => E));
      end if;
   end Add_After_Event;


   procedure Start_Processing_Events (On : in out Queue) is
      DI : Duration_Vectors.Cursor := On.Duration_Queue.First;
      II : Time_Vectors.Cursor := On.Initial_Queue.First;
      Start_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
      use type Duration_Vectors.Cursor;
      use type Time_Vectors.Cursor;
      use type Ada.Real_Time.Time;
   begin
      On.Started := True;

      while DI /= Duration_Vectors.No_Element loop
         declare
            D : Duration_Cell renames Duration_Vectors.Element (DI);
         begin
            Add_At_Event
              (D.Event,
               Times.Create (Start_Time
                               + Ada.Real_Time.To_Time_Span (D.Delay_To_Fire)),
               On);
         end;
         Duration_Vectors.Next (DI);
      end loop;
      On.Duration_Queue.Clear;

      while II /= Time_Vectors.No_Element loop
         declare
            T : Time_Cell renames Time_Vectors.Element (II);
         begin
            Add_At_Event (T.Event, T.Time_To_Fire, On);
         end;
         Time_Vectors.Next (II);
      end loop;
      On.Initial_Queue.Clear;
   end Start_Processing_Events;


   procedure Invalidate_Events (On : Queue;
                                For_The_Instance : Events.Instance_Base_P) is
      DI : Duration_Vectors.Cursor := On.Duration_Queue.First;
      II : Time_Vectors.Cursor := On.Initial_Queue.First;
      use type Duration_Vectors.Cursor;
      use type Time_Vectors.Cursor;
   begin
      while DI /= Duration_Vectors.No_Element loop
         Events.Invalidate (Duration_Vectors.Element (DI).Event,
                            If_For_Instance => For_The_Instance);
         Duration_Vectors.Next (DI);
      end loop;

      while II /= Time_Vectors.No_Element loop
         Events.Invalidate (Time_Vectors.Element (II).Event,
                            If_For_Instance => For_The_Instance);
         Time_Vectors.Next (II);
      end loop;

      for K in Times.Time_Kind loop
         declare
            KI : Time_Vectors.Cursor := On.Queues (K).First;
         begin
            while KI /= Time_Vectors.No_Element loop
               Events.Invalidate (Time_Vectors.Element (KI).Event,
                                  If_For_Instance => For_The_Instance);
               Time_Vectors.Next (KI);
            end loop;
         end;
      end loop;
   end Invalidate_Events;


   procedure Tear_Down (Q : in out Queue) is
      DI : Duration_Vectors.Cursor := Q.Duration_Queue.First;
      procedure Delete
      is new Ada.Unchecked_Deallocation (Events.Event_Base'Class,
                                         Events.Event_P);
      use type Duration_Vectors.Cursor;
   begin
      Q.Started := False;

      while DI /= Duration_Vectors.No_Element loop
         declare
            E : Events.Event_P := Duration_Vectors.Element (DI).Event;
         begin
            Events.Tear_Down (E);
            Delete (E);
         end;
         Duration_Vectors.Next (DI);
      end loop;
      Q.Duration_Queue.Clear;

      for K in Times.Time_Kind loop
         declare
            KI : Time_Vectors.Cursor := Q.Queues (K).First;
            use type Time_Vectors.Cursor;
         begin
            while KI /= Time_Vectors.No_Element loop
               declare
                  E : Events.Event_P := Time_Vectors.Element (KI).Event;
               begin
                  Events.Tear_Down (E);
                  Delete (E);
               end;
               Time_Vectors.Next (KI);
            end loop;
         end;
         Q.Queues (K).Clear;
      end loop;
   end Tear_Down;


end ColdFrame.Project.Held_Events;
