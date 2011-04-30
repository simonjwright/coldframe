--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  ColdFrame expects this package to exist to support time
--  management.
--
--  This is ColdFrame's default implementation.

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

package body ColdFrame.Project.Calendar is


   The_Epoch : constant Ada.Calendar.Time :=
     Ada.Calendar.Time_Of (Year => 1970,
                           Month => 1,
                           Day => 1);


   function Epoch return Time is
   begin
      return The_Epoch;
   end Epoch;


   function Image (T : Time) return String is
      use type Ada.Calendar.Time;
      Since : constant Duration := T - The_Epoch;
      --  which works just fine on GNAT!
   begin
      return Since'Img;
   end Image;


end ColdFrame.Project.Calendar;
