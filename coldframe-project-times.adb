--  This package is public-domain software; you can redistribute it
--  and/or modify it as you wish. This package is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.

--  This package provides support operations for project events.
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-times.adb,v $
--  $Revision: 253a6ad430b0 $
--  $Date: 2002/07/25 05:03:12 $
--  $Author: simon $

with Ada.Calendar;
with Ada.Real_Time;

package body ColdFrame.Project.Times is


   function Create (From_Time : Ada.Calendar.Time) return Time is
   begin
      return Time'(Kind => Calendar, C => From_Time);
   end Create;


   function Create (From_Time : Ada.Real_Time.Time) return Time is
   begin
      return Time'(Kind => Real_Time, R => From_Time);
   end Create;


   function Equivalent (Of_Time : Time) return Ada.Real_Time.Time is
      use type Ada.Calendar.Time;
      use type Ada.Real_Time.Time;
   begin
      case Of_Time.Kind is
         when Calendar =>
            declare
               Offset : constant Duration
                 := Of_Time.C - Ada.Calendar.Clock;
            begin
               return Ada.Real_Time.Clock
                 + Ada.Real_Time.To_Time_Span (Offset);
            end;
         when Real_Time =>
            return Of_Time.R;
      end case;
   end Equivalent;


   function From_Now (Period : Duration) return Time is
      use type Ada.Calendar.Time;
   begin
      return Time'(Kind => Calendar, C => Ada.Calendar.Clock + Period);
   end From_Now;


end ColdFrame.Project.Times;
