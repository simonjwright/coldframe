--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This package provides time support operations for project events.
--
--  This is ColdFrame's default implementation.

with Ada.Calendar;
with Ada.Real_Time;

package ColdFrame.Project.Times is

   type Time_Kind is (Calendar, Real_Time);

   type Time (Kind : Time_Kind := Calendar) is private;

   --  Creation operations

   function Create (From_Time : Ada.Calendar.Time) return Time;

   function Create (From_Time : Ada.Real_Time.Time) return Time;

   --  Operations to support Time_Signature

   function From_Now (Period : Duration) return Time;

   function Image (Of_Time : Time) return String;

   --  Additional operations

   function Equivalent (Of_Time : Time) return Ada.Real_Time.Time;

   function "<" (L, R : Time) return Boolean;

private

   type Time (Kind : Time_Kind := Calendar) is record
      case Kind is
         when Calendar =>
            C : Ada.Calendar.Time;
         when Real_Time =>
            R : Ada.Real_Time.Time;
      end case;
   end record;

end ColdFrame.Project.Times;
