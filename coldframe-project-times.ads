--  This package is public-domain software; you can redistribute it
--  and/or modify it as you wish. This package is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.

--  This package provides time support operations for project events.
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-times.ads,v $
--  $Revision: 253a6ad430b0 $
--  $Date: 2002/07/25 05:03:12 $
--  $Author: simon $

with Ada.Calendar;
with Ada.Real_Time;

package ColdFrame.Project.Times is

   type Time is private;

   function Create (From_Time : Ada.Calendar.Time) return Time;

   function Create (From_Time : Ada.Real_Time.Time) return Time;

   function Equivalent (Of_Time : Time) return Ada.Real_Time.Time;

   function From_Now (Period : Duration) return Time;

private

   type Time_Kind is (Calendar, Real_Time);

   type Time (Kind : Time_Kind := Calendar) is record
      case Kind is
         when Calendar =>
            C : Ada.Calendar.Time;
         when Real_Time =>
            R : Ada.Real_Time.Time;
      end case;
   end record;

end ColdFrame.Project.Times;
