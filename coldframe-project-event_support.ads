--  This package is public-domain software; you can redistribute it
--  and/or modify it as you wish. This package is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.

--  This package provides support operations for project events.
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-event_support.ads,v $
--  $Revision: 281d11e491da $
--  $Date: 2002/07/27 13:05:23 $
--  $Author: simon $

with ColdFrame.Time_Signature;
with ColdFrame.Project.Times;

package ColdFrame.Project.Event_Support is

   package Signature is new ColdFrame.Time_Signature
     (Time => Times.Time,
      Equivalent => Times.Equivalent,
      From_Now => Times.From_Now);

end ColdFrame.Project.Event_Support;
