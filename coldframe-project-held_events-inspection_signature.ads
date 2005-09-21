--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This package provides support operations for project events.
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-held_events-inspection_signature.ads,v $
--  $Revision: f82037aff426 $
--  $Date: 2005/09/21 05:41:29 $
--  $Author: simonjwright $

with ColdFrame.Events_G.Held_Event_Queue_Signature.Inspection_Signature;
with ColdFrame.Project.Held_Events.Inspection;
with ColdFrame.Project.Held_Events.Signature;

package ColdFrame.Project.Held_Events.Inspection_Signature
is new ColdFrame.Project.Held_Events.Signature.Inspection_Signature
  (Number_Of_At_Events => Inspection.Number_Of_At_Events,
   At_Event => Inspection.At_Event,
   When_At => Inspection.When_At,
   Number_Of_After_Events => Inspection.Number_Of_After_Events,
   After_Event => Inspection.After_Event,
   How_Long_After => Inspection.How_Long_After);
