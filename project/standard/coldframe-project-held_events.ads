--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This package provides support operations for project events.
--
--  This is ColdFrame's default implementation.

with Ada.Real_Time;
with ColdFrame.Project.Events;
with ColdFrame.Project.Times;

private with Ada.Containers.Vectors;

package ColdFrame.Project.Held_Events is

   --  Types and Operations to support Held_Event_Queue_Signature.

   type Queue is limited private;

   --  Managing Events

   function Is_Empty (Q : Queue) return Boolean;

   function Next_Event_Time (Q : Queue) return Ada.Real_Time.Time;

   procedure Pop (Q : in out Queue; The_Head_Event : out Events.Event_P);

   procedure Add_At_Event (E : Events.Event_P;
                           To_Run_At : Times.Time;
                           On : in out Queue);

   procedure Add_After_Event (E : Events.Event_P;
                              To_Run_After : Duration;
                              On : in out Queue);

   procedure Start_Processing_Events (On : in out Queue);
   --  Events will not be visible until this procedure has been
   --  called.

   procedure Invalidate_Events (On : Queue;
                                For_The_Instance : Events.Instance_Base_P);

   procedure Tear_Down (Q : in out Queue);

   --  We can't actually instantiate the signature here, which would
   --  save a unit, because that would be a premature use of Queue.

private

   --  Used before the queue is started.
   type Duration_Cell is record
      Delay_To_Fire : Duration;
      Event : Events.Event_P;
   end record;

   package Duration_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Duration_Cell);

   --  Used afterwards (even for plain 'delay's).
   type Time_Cell is record
      Time_To_Fire : Times.Time;
      Event : Events.Event_P;
   end record;

   --  Note that whenever we insert a Time_Cell into a
   --  Time_Vectors.Vector, we place it after all other elements that
   --  are at the same time or earlier.
   package Time_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Time_Cell);

   type Time_Queues is array (Times.Time_Kind) of Time_Vectors.Vector;

   type Queue is limited record
      Started : Boolean := False;
      Duration_Queue : Duration_Vectors.Vector;
      Initial_Queue : Time_Vectors.Vector;
      Queues : Time_Queues;
   end record;

end ColdFrame.Project.Held_Events;
