with Van_Fleet.A2;
with Van_Fleet.Customer;
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

--  $Id: van_fleet-van-lend.adb,v a88ba45be8ba 2004/05/08 13:54:26 simon $

with Van_Fleet.Hired_Van.Inheritance;
with Van_Fleet.Pool_Van.All_Instances;
with Van_Fleet.Pool_Van.Collections;
with Van_Fleet.Pool_Van.Inheritance;

separate (Van_Fleet.Van)
function Lend
  (To : ColdFrame.Instances.Handle;
   Terminating_At : ColdFrame.Project.Calendar.Time)
  return Handle is
   PVC : constant Pool_Van.Collections.Collection := Pool_Van.All_Instances;
   PVH : Pool_Van.Handle;
   VH : Handle;
   HVH : Hired_Van.Handle;
begin
   if Pool_Van.Collections.Length (PVC) = 0 then
      raise Not_Found;
   end if;
   PVH := Pool_Van.Collections.First (PVC);
   VH := Pool_Van.Inheritance.Find_Van_Parent (PVH);
   Pool_Van.Delete (PVH);
   HVH := Hired_Van.Inheritance.Create_Tree (ColdFrame.Instances.Handle (VH));
   Hired_Van.Set_Expected_Termination (HVH, To => Terminating_At);
   A2.Link (Is_On_Loan_To => Customer.Handle (To),
            Is_Borrowing => HVH);
   return VH;
end Lend;
