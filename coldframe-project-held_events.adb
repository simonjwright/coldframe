--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This package provides support operations for project events.
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-held_events.adb,v $
--  $Revision: 05740ccec705 $
--  $Date: 2003/03/09 19:42:26 $
--  $Author: simon $

with Ada.Unchecked_Deallocation;

package body ColdFrame.Project.Held_Events is


   function Is_Empty (Q : Queue) return Boolean is
   begin
      return Time_Collections.Is_Empty (Q.Real_Time_Queue)
        and then Time_Collections.Is_Empty (Q.Calendar_Queue);
   end Is_Empty;


   function Next_Event_Time (Q : Queue) return Ada.Real_Time.Time is
      use Time_Collections;
      use type Ada.Real_Time.Time;
   begin
      pragma Assert
        (not Is_Empty (Q.Real_Time_Queue)
           or else not Is_Empty (Q.Calendar_Queue));
      if Is_Empty (Q.Calendar_Queue) then
         return Times.Equivalent (First (Q.Real_Time_Queue).Time_To_Fire);
      elsif Is_Empty (Q.Real_Time_Queue) then
         return Times.Equivalent (First (Q.Calendar_Queue).Time_To_Fire);
      else
         declare
            A : constant Ada.Real_Time.Time :=
              Times.Equivalent (First (Q.Calendar_Queue).Time_To_Fire);
            R : constant Ada.Real_Time.Time :=
              Times.Equivalent (First (Q.Real_Time_Queue).Time_To_Fire);
         begin
            if A < R then
               return A;
            else
               return R;
            end if;
         end;
      end if;
   end Next_Event_Time;


   procedure Pop (Q : in out Queue; The_Head_Event : out Events.Event_P) is
      use Time_Collections;
      use type Ada.Real_Time.Time;
   begin
      pragma Assert (not Is_Empty (Q.Real_Time_Queue)
           or else not Is_Empty (Q.Calendar_Queue));
      if Is_Empty (Q.Calendar_Queue) then
         The_Head_Event := First (Q.Real_Time_Queue).Event;
         Remove (Q.Real_Time_Queue, 1);
      elsif Is_Empty (Q.Real_Time_Queue) then
         The_Head_Event := First (Q.Calendar_Queue).Event;
         Remove (Q.Calendar_Queue, 1);
      else
         declare
            AC : Time_Cell := First (Q.Calendar_Queue);
            A : constant Ada.Real_Time.Time :=
              Times.Equivalent (AC.Time_To_Fire);
            RC : Time_Cell := First (Q.Real_Time_Queue);
            R : constant Ada.Real_Time.Time :=
              Times.Equivalent (RC.Time_To_Fire);
         begin
            if A < R then
               The_Head_Event := AC.Event;
               Remove (Q.Calendar_Queue, 1);
            else
               The_Head_Event := RC.Event;
               Remove (Q.Real_Time_Queue, 1);
            end if;
         end;
      end if;
   end Pop;


   procedure Add_At_Event (E : Events.Event_P;
                           To_Run_At : Times.Time;
                           On : in out Queue) is
      use Time_Collections;
   begin
      case To_Run_At.Kind is
         when Times.Calendar =>
            Append (On.Calendar_Queue, Time_Cell'(Time_To_Fire => To_Run_At,
                                                  Event => E));
         when Times.Real_Time =>
            Append (On.Real_Time_Queue, Time_Cell'(Time_To_Fire => To_Run_At,
                                                   Event => E));
      end case;
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


   procedure Start_Processing_After_Events (On : in out Queue) is
      DI : Abstract_Duration_Containers.Iterator'Class
        := Duration_Collections.New_Iterator (On.Duration_Queue);
      use Abstract_Duration_Containers;
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
      Duration_Collections.Clear (On.Duration_Queue);
   end Start_Processing_After_Events;


   procedure Invalidate_Events (On : Queue;
                                For_The_Instance : Events.Instance_Base_P) is
      DI : Abstract_Duration_Containers.Iterator'Class
        := Duration_Collections.New_Iterator (On.Duration_Queue);
      RI : Abstract_Time_Containers.Iterator'Class
        := Time_Collections.New_Iterator (On.Real_Time_Queue);
      AI : Abstract_Time_Containers.Iterator'Class
        := Time_Collections.New_Iterator (On.Calendar_Queue);
      use Abstract_Duration_Containers;
      use Abstract_Time_Containers;
   begin
      while not Is_Done (DI) loop
         Events.Invalidate (Current_Item (DI).Event,
                            If_For_Instance => For_The_Instance);
         Next (DI);
      end loop;
      while not Is_Done (RI) loop
         Events.Invalidate (Current_Item (RI).Event,
                            If_For_Instance => For_The_Instance);
         Next (RI);
      end loop;
      while not Is_Done (AI) loop
         Events.Invalidate (Current_Item (AI).Event,
                            If_For_Instance => For_The_Instance);
         Next (AI);
      end loop;
   end Invalidate_Events;


   procedure Tear_Down (Q : in out Queue) is
      DI : Abstract_Duration_Containers.Iterator'Class
        := Duration_Collections.New_Iterator (Q.Duration_Queue);
      RI : Abstract_Time_Containers.Iterator'Class
        := Time_Collections.New_Iterator (Q.Real_Time_Queue);
      AI : Abstract_Time_Containers.Iterator'Class
        := Time_Collections.New_Iterator (Q.Calendar_Queue);
      use Abstract_Duration_Containers;
      use Abstract_Time_Containers;
      procedure Delete
      is new Ada.Unchecked_Deallocation (Events.Event_Base'Class,
                                         Events.Event_P);
   begin
      Q.Started := False;
      while not Is_Done (DI) loop
         declare
            E : Events.Event_P := Current_Item (DI).Event;
         begin
            Delete (E);
         end;
         Next (DI);
      end loop;
      Duration_Collections.Clear (Q.Duration_Queue);
      while not Is_Done (RI) loop
         declare
            E : Events.Event_P := Current_Item (RI).Event;
         begin
            Delete (E);
         end;
         Next (RI);
      end loop;
      Time_Collections.Clear (Q.Real_Time_Queue);
      while not Is_Done (AI) loop
         declare
            E : Events.Event_P := Current_Item (AI).Event;
         begin
            Delete (E);
         end;
         Next (AI);
      end loop;
      Time_Collections.Clear (Q.Calendar_Queue);
   end Tear_Down;


   function "<" (L, R : Time_Cell) return Boolean is
   begin
      return Times."<" (L.Time_To_Fire, R.Time_To_Fire);
   end "<";


end ColdFrame.Project.Held_Events;
