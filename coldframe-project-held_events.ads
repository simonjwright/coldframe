--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This package provides support operations for project events.
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-held_events.ads,v $
--  $Revision: 8d79e317b2ac $
--  $Date: 2005/09/29 20:47:06 $
--  $Author: simonjwright $

with Ada.Real_Time;
with BC.Containers.Collections.Ordered.Unbounded;
with ColdFrame.Project.Events;
with ColdFrame.Project.Storage_Pools;
with ColdFrame.Project.Times;

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

   procedure Start_Processing_After_Events (On : in out Queue);
   --  After events will not be visible until this procedure has been
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

   function "<" (L, R : Duration_Cell) return Boolean;

   package Abstract_Duration_Containers
   is new BC.Containers (Duration_Cell);
   package Abstract_Duration_Collections
   is new Abstract_Duration_Containers.Collections;
   package Abstract_Ordered_Duration_Collections
   is new Abstract_Duration_Collections.Ordered;
   package Duration_Collections
   is new Abstract_Ordered_Duration_Collections.Unbounded
     (Storage => ColdFrame.Project.Storage_Pools.Pool);

   type Time_Cell is record
      Time_To_Fire : Times.Time;
      Event : Events.Event_P;
   end record;

   function "<" (L, R : Time_Cell) return Boolean;

   package Abstract_Time_Containers
   is new BC.Containers (Time_Cell);
   package Abstract_Time_Collections
   is new Abstract_Time_Containers.Collections;
   package Abstract_Ordered_Time_Collections
   is new Abstract_Time_Collections.Ordered;
   package Time_Collections
   is new Abstract_Ordered_Time_Collections.Unbounded
     (Storage => ColdFrame.Project.Storage_Pools.Pool);

   type Time_Queues is array (Times.Time_Kind) of Time_Collections.Collection;

   type Queue is limited record
      Started : Boolean := False;
      Duration_Queue : Duration_Collections.Collection;
      Queues : Time_Queues;
   end record;

end ColdFrame.Project.Held_Events;
