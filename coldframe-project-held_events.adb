--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This package provides support operations for project events.
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-held_events.adb,v $
--  $Revision: 63f8a818a534 $
--  $Date: 2006/03/03 22:08:25 $
--  $Author: simonjwright $

with Ada.Unchecked_Deallocation;

package body ColdFrame.Project.Held_Events is


   function Is_Empty (Q : Queue) return Boolean is
   begin
      for Kind in Q.Queues'Range loop
         if not Time_Collections.Is_Empty (Q.Queues (Kind)) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Empty;


   function Next_Event_Time (Q : Queue) return Ada.Real_Time.Time is
      Result : Ada.Real_Time.Time := Ada.Real_Time.Time_Last;
      use Time_Collections;
      use type Ada.Real_Time.Time;
   begin
      pragma Assert (not Is_Empty (Q));
      for Kind in Q.Queues'Range loop
         if not Time_Collections.Is_Empty (Q.Queues (Kind)) then
            declare
               T : constant Ada.Real_Time.Time
                 := Times.Equivalent (First (Q.Queues (Kind)).Time_To_Fire);
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
      use Time_Collections;
      use type Ada.Real_Time.Time;
   begin
      pragma Assert (not Is_Empty (Q));
      for Kind in Q.Queues'Range loop
         if not Time_Collections.Is_Empty (Q.Queues (Kind)) then
            declare
               T : constant Ada.Real_Time.Time
                 := Times.Equivalent (First (Q.Queues (Kind)).Time_To_Fire);
            begin
               if T < Earliest_Time then
                  Earliest_Time := T;
                  Earliest_Kind := Kind;
               end if;
            end;
         end if;
      end loop;
      The_Head_Event := First (Q.Queues (Earliest_Kind)).Event;
      Remove (Q.Queues (Earliest_Kind), 1);
   end Pop;


   procedure Add_At_Event (E : Events.Event_P;
                           To_Run_At : Times.Time;
                           On : in out Queue) is
      use Time_Collections;
      use Initial_Time_Collections;
   begin
      if On.Started then
         Append (On.Queues (To_Run_At.Kind),
                 Time_Cell'(Time_To_Fire => To_Run_At,
                            Event => E));
      else
         Append (On.Initial_Queue,
                 Time_Cell'(Time_To_Fire => To_Run_At,
                            Event => E));
      end if;
   end Add_At_Event;


   procedure Add_After_Event (E : Events.Event_P;
                              To_Run_After : Duration;
                              On : in out Queue) is
      use Duration_Collections;
      use type Ada.Real_Time.Time;
   begin
      if On.Started then
         Add_At_Event
           (E,
            Times.Create (Ada.Real_Time.Clock
                            + Ada.Real_Time.To_Time_Span (To_Run_After)),
            On);
      else
         Append (On.Duration_Queue,
                 Duration_Cell'(Delay_To_Fire => To_Run_After,
                                Event => E));
      end if;
   end Add_After_Event;


   procedure Start_Processing_Events (On : in out Queue) is
      DI : Abstract_Duration_Containers.Iterator'Class
        := Duration_Collections.New_Iterator (On.Duration_Queue);
      II : Abstract_Time_Containers.Iterator'Class
        := Initial_Time_Collections.New_Iterator (On.Initial_Queue);
      use Abstract_Duration_Containers;
      use Abstract_Time_Containers;
      use type Ada.Real_Time.Time;
   begin
      On.Started := True;
      while not Is_Done (DI) loop
         declare
            D : Duration_Cell renames Current_Item (DI);
         begin
            Add_At_Event
              (D.Event,
               Times.Create (Ada.Real_Time.Clock
                               + Ada.Real_Time.To_Time_Span (D.Delay_To_Fire)),
               On);
         end;
         Next (DI);
      end loop;
      while not Is_Done (II) loop
         declare
            T : Time_Cell renames Current_Item (II);
         begin
            Add_At_Event (T.Event, T.Time_To_Fire, On);
         end;
         Next (II);
      end loop;
      Duration_Collections.Clear (On.Duration_Queue);
      Initial_Time_Collections.Clear (On.Initial_Queue);
   end Start_Processing_Events;


   procedure Invalidate_Events (On : Queue;
                                For_The_Instance : Events.Instance_Base_P) is
      DI : Abstract_Duration_Containers.Iterator'Class
        := Duration_Collections.New_Iterator (On.Duration_Queue);
      use Abstract_Duration_Containers;
      use Abstract_Time_Containers;
   begin
      while not Is_Done (DI) loop
         Events.Invalidate (Current_Item (DI).Event,
                            If_For_Instance => For_The_Instance);
         Next (DI);
      end loop;
      for K in Times.Time_Kind loop
         declare
            KI : Abstract_Time_Containers.Iterator'Class
              := Time_Collections.New_Iterator (On.Queues (K));
         begin
            while not Is_Done (KI) loop
               Events.Invalidate (Current_Item (KI).Event,
                                  If_For_Instance => For_The_Instance);
               Next (KI);
            end loop;
         end;
      end loop;
   end Invalidate_Events;


   procedure Tear_Down (Q : in out Queue) is
      DI : Abstract_Duration_Containers.Iterator'Class
        := Duration_Collections.New_Iterator (Q.Duration_Queue);
      procedure Delete
      is new Ada.Unchecked_Deallocation (Events.Event_Base'Class,
                                         Events.Event_P);
      use Abstract_Duration_Containers;
      use Abstract_Time_Containers;
   begin
      Q.Started := False;
      while not Is_Done (DI) loop
         declare
            E : Events.Event_P := Current_Item (DI).Event;
         begin
            Events.Tear_Down (E);
            Delete (E);
         end;
         Next (DI);
      end loop;
      Duration_Collections.Clear (Q.Duration_Queue);
      for K in Times.Time_Kind loop
         declare
            KI : Abstract_Time_Containers.Iterator'Class
              := Time_Collections.New_Iterator (Q.Queues (K));
         begin
            while not Is_Done (KI) loop
               declare
                  E : Events.Event_P := Current_Item (KI).Event;
               begin
                  Events.Tear_Down (E);
                  Delete (E);
               end;
               Next (KI);
            end loop;
         end;
         Time_Collections.Clear (Q.Queues (K));
      end loop;
   end Tear_Down;


   function "<" (L, R : Duration_Cell) return Boolean is
   begin
      return L.Delay_To_Fire < R.Delay_To_Fire;
   end "<";


   function "<" (L, R : Time_Cell) return Boolean is
   begin
      return Times."<" (L.Time_To_Fire, R.Time_To_Fire);
   end "<";


end ColdFrame.Project.Held_Events;
