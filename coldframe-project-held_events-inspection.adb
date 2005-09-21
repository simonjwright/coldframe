--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This package provides support operations for inspecting project
--  events.
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-held_events-inspection.adb,v $
--  $Revision: f82037aff426 $
--  $Date: 2005/09/21 05:41:29 $
--  $Author: simonjwright $

package body ColdFrame.Project.Held_Events.Inspection is

   Unimplemented : exception;

   function Number_Of_At_Events (On : Queue) return Natural is
   begin
      raise Unimplemented;
      return 0;
   end Number_Of_At_Events;


   function At_Event (On : Queue;
                      At_Index : Positive) return Events.Event_P is
   begin
      raise Unimplemented;
      return null;
   end At_Event;


   function When_At (On : Queue;
                     At_Index : Positive) return Times.Time is
      Dummy : Times.Time (Kind => Times.Real_Time);
   begin
      raise Unimplemented;
      return Dummy;
   end When_At;


   function Number_Of_After_Events (On : Queue) return Natural is
   begin
      raise Unimplemented;
      return 0;
   end Number_Of_After_Events;


   function After_Event (On : Queue;
                         At_Index : Positive) return Events.Event_P is
   begin
      raise Unimplemented;
      return null;
   end After_Event;


   function How_Long_After (On : Queue;
                            At_Index : Positive) return Duration is
   begin
      raise Unimplemented;
      return 0.0;
   end How_Long_After;


end ColdFrame.Project.Held_Events.Inspection;
