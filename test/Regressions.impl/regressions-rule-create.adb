--  Copyright (C) Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

with Regressions.Test_Preemption;

separate (Regressions.Rule)
procedure Create
  (Preempter : Test_Name;
   Preemptee : Test_Name;
   Preempts : Boolean := False) is
   MP : Preemptable_Test.Handle;
   MBP : Preemptable_Test.Handle;
   H : Handle;
   use type Preemptable_Test.Handle;
begin
   MP := Preemptable_Test.Find ((Name => Preempter));
   if MP = null then
      MP := Preemptable_Test.Create ((Name => Preempter));
   end if;
   MBP := Preemptable_Test.Find ((Name => Preemptee));
   if MBP = null then
      MBP := Preemptable_Test.Create ((Name => Preemptee));
   end if;
   H := Test_Preemption.Link
     (Conflicts_With_And_May_Preempt => MBP,
      Conflicts_With_And_May_Be_Preempted_By => MP);
   H.Preempts := Preempts;
end Create;
