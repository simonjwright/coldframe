--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This package provides support operations for project events.
--
--  This is ColdFrame's default implementation.

--  $RCSfile: coldframe-project-times.adb,v $
--  $Revision: 05c65fa0c96a $
--  $Date: 2003/03/09 16:04:01 $
--  $Author: simon $

with Ada.Calendar;
with Ada.Real_Time;
with GNAT.Calendar.Time_IO;

package body ColdFrame.Project.Times is


   function Create (From_Time : Ada.Calendar.Time) return Time is
   begin
      return Time'(Kind => Calendar, C => From_Time);
   end Create;


   function Create (From_Time : Ada.Real_Time.Time) return Time is
   begin
      return Time'(Kind => Real_Time, R => From_Time);
   end Create;


   function From_Now (Period : Duration) return Time is
      use type Ada.Calendar.Time;
   begin
      return Time'(Kind => Calendar, C => Ada.Calendar.Clock + Period);
   end From_Now;


   function Image (Of_Time : Time) return String is
      use type Ada.Real_Time.Time;
   begin
      case Of_Time.Kind is
         when Calendar =>
            return GNAT.Calendar.Time_IO.Image (Of_Time.C, "%T");
         when Real_Time =>
            return Duration'Image
              (Ada.Real_Time.To_Duration
                 (Of_Time.R - Ada.Real_Time.Time_First));
      end case;
   end Image;


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


   function "<" (L, R : Time) return Boolean is
      use type Ada.Calendar.Time;
      use type Ada.Real_Time.Time;
   begin
      case L.Kind is
         when Calendar =>
            case R.Kind is
               when Calendar =>
                  return L.C < R.C;
               when Real_Time =>
                  return Equivalent (L) < Equivalent (R);
            end case;
         when Real_Time =>
            case R.Kind is
               when Calendar =>
                  return Equivalent (L) < Equivalent (R);
               when Real_Time =>
                  return L.R < R.R;
            end case;
      end case;
   end "<";


end ColdFrame.Project.Times;
