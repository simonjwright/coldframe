--  This package is public-domain software; you can redistribute it
--  and/or modify it as you wish. This package is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.

--  This package provides support operations for project events.
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-event_support.ads,v $
--  $Revision: 6cd4886dc353 $
--  $Date: 2002/04/16 18:46:06 $
--  $Author: simon $

with Ada.Calendar;
with Ada.Real_Time;
with ColdFrame.Time_Signature;

package ColdFrame.Project.Event_Support is

   type Time_Kind is (Calendar);

   type Time (Kind : Time_Kind := Calendar) is record
      T : Ada.Calendar.Time;
   end record;
   --  This type should really be private, but we'de have to
   --  instantiate Time_Signature in a separate package.

   function Equivalent (Of_Time : Time) return Ada.Real_Time.Time;

   function From_Now (Period : Duration) return Time;

   package Signature is new ColdFrame.Time_Signature
     (Time => Time,
      Equivalent => Equivalent,
      From_Now => From_Now);

end ColdFrame.Project.Event_Support;
