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

--  $RCSfile: hierarchies-test_creations.adb,v $
--  $Revision: e6759e067874 $
--  $Date: 2003/05/10 17:14:12 $
--  $Author: simon $

with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;
with AUnit.Assertions; use AUnit.Assertions;

with ColdFrame.Exceptions;
with ColdFrame.Instances;

with Hierarchies.Initialize;
with Hierarchies.Tear_Down;

with Hierarchies.R_1;
with Hierarchies.R_1.All_Instances;
with Hierarchies.R_1.Collections;
with Hierarchies.R_2;
with Hierarchies.R_2.All_Instances;
with Hierarchies.R_2.Collections;
with Hierarchies.R_3.Inheritance;
with Hierarchies.R_3.All_Instances;
with Hierarchies.R_3.Collections;
with Hierarchies.S_1.Inheritance;
with Hierarchies.S_2.Inheritance;
with Hierarchies.S_2.All_Instances;
with Hierarchies.S_2.Collections;
with Hierarchies.S_3.Inheritance;
with Hierarchies.S_3.All_Instances;
with Hierarchies.S_3.Collections;
with Hierarchies.T_2.Inheritance;
with Hierarchies.T_2.All_Instances;
with Hierarchies.T_2.Collections;
with Hierarchies.F_2.Inheritance;
with Hierarchies.F_2.All_Instances;
with Hierarchies.F_2.Collections;

package body Hierarchies.Test_Creations is

   subtype CIH is ColdFrame.Instances.Handle;
   use type CIH;
   use type R_1.Handle;
   use type R_2.Handle;
   use type R_3.Handle;
   use type S_2.Handle;
   use type S_3.Handle;
   use type T_2.Handle;
   use type F_2.Handle;

   R1_H : R_1.Handle;
   R2_H : R_2.Handle;
   R3_H : R_3.Handle;
   S1_H : S_1.Handle;
   S2_H : S_2.Handle;
   S3_H : S_3.Handle;
   T2_H : T_2.Handle;
   F2_H : F_2.Handle;

   procedure Create_Root
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Create_Root
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
   begin
      R1_H := R_1.Create;
      Assert (R_1.Collections.Length (R_1.All_Instances) = 1,
              "R_1 missing");
      Assert (R_2.Collections.Length (R_2.All_Instances) = 0,
              "R_2 present");
      Assert (R_3.Collections.Length (R_3.All_Instances) = 0,
              "R_3 present");
      Assert (S_2.Collections.Length (S_2.All_Instances) = 0,
              "S_2 present");
      Assert (S_3.Collections.Length (S_3.All_Instances) = 0,
              "S_3 present");
      Assert (T_2.Collections.Length (T_2.All_Instances) = 0,
              "T_2 present");
      Assert (F_2.Collections.Length (F_2.All_Instances) = 0,
              "F_2 present");
   end Create_Root;

   procedure Create_First_Child
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Create_First_Child
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
   begin
      S2_H := S_2.Inheritance.Create_Tree (null);
      Assert (R_1.Collections.Length (R_1.All_Instances) = 1,
              "R_1 missing");
      Assert (R_2.Collections.Length (R_2.All_Instances) = 0,
              "R_2 present");
      Assert (R_3.Collections.Length (R_3.All_Instances) = 0,
              "R_3 present");
      Assert (S_2.Collections.Length (S_2.All_Instances) = 1,
              "S_2 missing");
      Assert (S_3.Collections.Length (S_3.All_Instances) = 0,
              "S_3 present");
      Assert (T_2.Collections.Length (T_2.All_Instances) = 0,
              "T_2 present");
      Assert (F_2.Collections.Length (F_2.All_Instances) = 0,
              "F_2 present");
      R1_H := R_1.Collections.First (R_1.All_Instances);
      Assert (CIH (R1_H) = S_2.Get_A_Parent (S2_H),
              "S_2 has wrong parent");
      Assert (S_2.Handle (R_1.Get_A_Child (R1_H).S2) = S2_H,
              "R_1 has wrong child");
   end Create_First_Child;

   procedure Create_Another_First_Child
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Create_Another_First_Child
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
   begin
      S2_H := S_2.Inheritance.Create_Tree (null);
      R1_H := R_1.Collections.First (R_1.All_Instances);
      S1_H := S_1.Inheritance.Create_Tree (CIH (R1_H));
      Assert (False, "Existing_Child exception expected");
   exception
      when ColdFrame.Exceptions.Existing_Child => null;
   end Create_Another_First_Child;

   procedure Create_Second_Child
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Create_Second_Child
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
   begin
      T2_H := T_2.Inheritance.Create_Tree (null);
      Assert (R_1.Collections.Length (R_1.All_Instances) = 1,
              "R_1 missing");
      Assert (R_2.Collections.Length (R_2.All_Instances) = 0,
              "R_2 present");
      Assert (R_3.Collections.Length (R_3.All_Instances) = 0,
              "S_3 present");
      Assert (S_2.Collections.Length (S_2.All_Instances) = 1,
              "S_2 missing");
      Assert (S_3.Collections.Length (S_3.All_Instances) = 0,
              "S_3 present");
      Assert (T_2.Collections.Length (T_2.All_Instances) = 1,
              "T_2 missing");
      Assert (F_2.Collections.Length (F_2.All_Instances) = 0,
              "F_2 present");
      R1_H := R_1.Collections.First (R_1.All_Instances);
      S2_H := S_2.Collections.First (S_2.All_Instances);
      Assert (CIH (R1_H) = S_2.Get_A_Parent (S2_H),
              "S_2 has wrong parent");
      Assert (S_2.Handle (R_1.Get_A_Child (R1_H).S2) = S2_H,
              "R_1 has wrong child");
      Assert (CIH (S2_H) = T_2.Get_D_Parent (T2_H),
              "T_2 has wrong parent");
      Assert (T_2.Handle (S_2.Get_D_Child (S2_H).T2) = T2_H,
              "S_2 has wrong child");
   end Create_Second_Child;

   procedure Create_Third_Child
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Create_Third_Child
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
   begin
      F2_H := F_2.Inheritance.Create_Tree (null, null, null);
      Assert (R_1.Collections.Length (R_1.All_Instances) = 1,
              "R_1 missing");
      Assert (R_2.Collections.Length (R_2.All_Instances) = 1,
              "R_2 missing");
      Assert (R_3.Collections.Length (R_3.All_Instances) = 1,
              "R_3 missing");
      Assert (S_2.Collections.Length (S_2.All_Instances) = 1,
              "S_2 missing");
      Assert (S_3.Collections.Length (S_3.All_Instances) = 1,
              "S_3 missing");
      Assert (T_2.Collections.Length (T_2.All_Instances) = 1,
              "T_2 missing");
      Assert (F_2.Collections.Length (F_2.All_Instances) = 1,
              "F_2 missing");
      R1_H := R_1.Collections.First (R_1.All_Instances);
      R2_H := R_2.Collections.First (R_2.All_Instances);
      R3_H := R_3.Collections.First (R_3.All_Instances);
      S2_H := S_2.Collections.First (S_2.All_Instances);
      S3_H := S_3.Collections.First (S_3.All_Instances);
      T2_H := T_2.Collections.First (T_2.All_Instances);
      Assert (CIH (R1_H) = S_2.Get_A_Parent (S2_H),
              "S_2 has wrong parent");
      Assert (S_2.Handle (R_1.Get_A_Child (R1_H).S2) = S2_H,
              "R_1 has wrong child");
      Assert (CIH (R2_H) = S_3.Get_B_Parent (S3_H),
              "S_3 has wrong B parent");
      Assert (S_3.Handle (R_2.Get_B_Child (R2_H).S3) = S3_H,
              "R_2 has wrong child");
      Assert (CIH (R3_H) = S_3.Get_C_Parent (S3_H),
              "S_3 has wrong C parent");
      Assert (S_3.Handle (R_3.Get_C_Child (R3_H).S3) = S3_H,
              "R_3 has wrong child");
      Assert (CIH (S2_H) = T_2.Get_D_Parent (T2_H),
              "T_2 has wrong parent");
      Assert (T_2.Handle (S_2.Get_D_Child (S2_H).T2) = T2_H,
              "S_2 has wrong child");
      Assert (CIH (T2_H) = F_2.Get_F_Parent (F2_H),
              "F_2 has wrong F parent");
      Assert (F_2.Handle (T_2.Get_F_Child (T2_H).F2) = F2_H,
              "T_2 has wrong child");
      Assert (CIH (S3_H) = F_2.Get_E_Parent (F2_H),
              "F_2 has wrong E parent");
      Assert (F_2.Handle (S_3.Get_E_Child (S3_H).F2) = F2_H,
              "S_3 has wrong E child");
   end Create_Third_Child;

   procedure Create_Third_Child_With_Self
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Create_Third_Child_With_Self
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
      F2_H2 : F_2.Handle;
   begin
      F2_H := F_2.Inheritance.Create_Tree (null, null, null);
      F2_H2 := F_2.Inheritance.Create_Tree
        (CIH (F2_H), CIH (F2_H), CIH (F2_H));
      Assert (F2_H = F2_H2,
              "different F_2 created");
      Assert (R_1.Collections.Length (R_1.All_Instances) = 1,
              "R_1 missing");
      Assert (R_2.Collections.Length (R_2.All_Instances) = 1,
              "R_2 missing");
      Assert (R_3.Collections.Length (R_3.All_Instances) = 1,
              "R_3 missing");
      Assert (S_2.Collections.Length (S_2.All_Instances) = 1,
              "S_2 missing");
      Assert (S_3.Collections.Length (S_3.All_Instances) = 1,
              "S_3 missing");
      Assert (T_2.Collections.Length (T_2.All_Instances) = 1,
              "T_2 missing");
      Assert (F_2.Collections.Length (F_2.All_Instances) = 1,
              "F_2 missing");
      R1_H := R_1.Collections.First (R_1.All_Instances);
      R2_H := R_2.Collections.First (R_2.All_Instances);
      R3_H := R_3.Collections.First (R_3.All_Instances);
      S2_H := S_2.Collections.First (S_2.All_Instances);
      S3_H := S_3.Collections.First (S_3.All_Instances);
      T2_H := T_2.Collections.First (T_2.All_Instances);
      Assert (CIH (R1_H) = S_2.Get_A_Parent (S2_H),
              "S_2 has wrong parent");
      Assert (S_2.Handle (R_1.Get_A_Child (R1_H).S2) = S2_H,
              "R_1 has wrong child");
      Assert (CIH (R2_H) = S_3.Get_B_Parent (S3_H),
              "S_3 has wrong B parent");
      Assert (S_3.Handle (R_2.Get_B_Child (R2_H).S3) = S3_H,
              "R_2 has wrong child");
      Assert (CIH (R3_H) = S_3.Get_C_Parent (S3_H),
              "S_3 has wrong C parent");
      Assert (S_3.Handle (R_3.Get_C_Child (R3_H).S3) = S3_H,
              "R_3 has wrong child");
      Assert (CIH (S2_H) = T_2.Get_D_Parent (T2_H),
              "T_2 has wrong parent");
      Assert (T_2.Handle (S_2.Get_D_Child (S2_H).T2) = T2_H,
              "S_2 has wrong child");
      Assert (CIH (T2_H) = F_2.Get_F_Parent (F2_H),
              "F_2 has wrong F parent");
      Assert (F_2.Handle (T_2.Get_F_Child (T2_H).F2) = F2_H,
              "T_2 has wrong child");
      Assert (CIH (S3_H) = F_2.Get_E_Parent (F2_H),
              "F_2 has wrong E parent");
      Assert (F_2.Handle (S_3.Get_E_Child (S3_H).F2) = F2_H,
              "S_3 has wrong E child");
   end Create_Third_Child_With_Self;

   procedure Create_First_Child_With_Root
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Create_First_Child_With_Root
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
   begin
      R1_H := R_1.Create;
      S2_H := S_2.Inheritance.Create_Tree (CIH (R1_H));
      Assert (R_1.Collections.Length (R_1.All_Instances) = 1,
              "R_1 missing");
      Assert (R_2.Collections.Length (R_2.All_Instances) = 0,
              "R_2 present");
      Assert (R_3.Collections.Length (R_3.All_Instances) = 0,
              "S_3 present");
      Assert (S_2.Collections.Length (S_2.All_Instances) = 1,
              "S_2 missing");
      Assert (S_3.Collections.Length (S_3.All_Instances) = 0,
              "S_3 present");
      Assert (T_2.Collections.Length (T_2.All_Instances) = 0,
              "T_2 present");
      Assert (F_2.Collections.Length (F_2.All_Instances) = 0,
              "F_2 present");
      Assert (CIH (R1_H) = S_2.Get_A_Parent (S2_H),
              "S_2 has wrong parent");
      Assert (S_2.Handle (R_1.Get_A_Child (R1_H).S2) = S2_H,
              "R_1 has wrong child");
   end Create_First_Child_With_Root;

   procedure Create_Second_Child_With_First
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Create_Second_Child_With_First
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
   begin
      S2_H := S_2.Inheritance.Create_Tree (null);
      T2_H := T_2.Inheritance.Create_Tree (CIH (S2_H));
      Assert (R_1.Collections.Length (R_1.All_Instances) = 1,
              "R_1 missing");
      Assert (R_2.Collections.Length (R_2.All_Instances) = 0,
              "R_2 present");
      Assert (R_3.Collections.Length (R_3.All_Instances) = 0,
              "S_3 present");
      Assert (S_2.Collections.Length (S_2.All_Instances) = 1,
              "S_2 missing");
      Assert (S_3.Collections.Length (S_3.All_Instances) = 0,
              "S_3 present");
      Assert (T_2.Collections.Length (T_2.All_Instances) = 1,
              "T_2 missing");
      Assert (F_2.Collections.Length (F_2.All_Instances) = 0,
              "F_2 present");
      R1_H := R_1.Collections.First (R_1.All_Instances);
      Assert (CIH (R1_H) = S_2.Get_A_Parent (S2_H),
              "S_2 has wrong parent");
      Assert (S_2.Handle (R_1.Get_A_Child (R1_H).S2) = S2_H,
              "R_1 has wrong child");
      Assert (CIH (S2_H) = T_2.Get_D_Parent (T2_H),
              "T_2 has wrong parent");
      Assert (T_2.Handle (S_2.Get_D_Child (S2_H).T2) = T2_H,
              "S_2 has wrong child");
   end Create_Second_Child_With_First;

   procedure Create_Third_Child_With_First_R1
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Create_Third_Child_With_First_R1
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
   begin
      S2_H := S_2.Inheritance.Create_Tree (null);
      F2_H := F_2.Inheritance.Create_Tree (CIH (S2_H), null, null);
      Assert (R_1.Collections.Length (R_1.All_Instances) = 1,
              "R_1 missing");
      Assert (R_2.Collections.Length (R_2.All_Instances) = 1,
              "R_2 missing");
      Assert (R_3.Collections.Length (R_3.All_Instances) = 1,
              "R_3 missing");
      Assert (S_2.Collections.Length (S_2.All_Instances) = 1,
              "S_2 missing");
      Assert (S_3.Collections.Length (S_3.All_Instances) = 1,
              "S_3 missing");
      Assert (T_2.Collections.Length (T_2.All_Instances) = 1,
              "T_2 missing");
      Assert (F_2.Collections.Length (F_2.All_Instances) = 1,
              "F_2 missing");
      R1_H := R_1.Collections.First (R_1.All_Instances);
      R2_H := R_2.Collections.First (R_2.All_Instances);
      R3_H := R_3.Collections.First (R_3.All_Instances);
      S3_H := S_3.Collections.First (S_3.All_Instances);
      T2_H := T_2.Collections.First (T_2.All_Instances);
      Assert (CIH (R1_H) = S_2.Get_A_Parent (S2_H),
              "S_2 has wrong parent");
      Assert (S_2.Handle (R_1.Get_A_Child (R1_H).S2) = S2_H,
              "R_1 has wrong child");
      Assert (CIH (R2_H) = S_3.Get_B_Parent (S3_H),
              "S_3 has wrong B parent");
      Assert (S_3.Handle (R_2.Get_B_Child (R2_H).S3) = S3_H,
              "R_2 has wrong child");
      Assert (CIH (R3_H) = S_3.Get_C_Parent (S3_H),
              "S_3 has wrong C parent");
      Assert (S_3.Handle (R_3.Get_C_Child (R3_H).S3) = S3_H,
              "R_3 has wrong child");
      Assert (CIH (S2_H) = T_2.Get_D_Parent (T2_H),
              "T_2 has wrong parent");
      Assert (T_2.Handle (S_2.Get_D_Child (S2_H).T2) = T2_H,
              "S_2 has wrong child");
      Assert (CIH (T2_H) = F_2.Get_F_Parent (F2_H),
              "F_2 has wrong F parent");
      Assert (F_2.Handle (T_2.Get_F_Child (T2_H).F2) = F2_H,
              "T_2 has wrong child");
      Assert (CIH (S3_H) = F_2.Get_E_Parent (F2_H),
              "F_2 has wrong E parent");
      Assert (F_2.Handle (S_3.Get_E_Child (S3_H).F2) = F2_H,
              "S_3 has wrong E child");
   end Create_Third_Child_With_First_R1;

   procedure Create_Third_Child_With_First_R3
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Create_Third_Child_With_First_R3
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
   begin
      R3_H := R_3.Inheritance.Create_Tree (null);
      F2_H := F_2.Inheritance.Create_Tree (null, null, CIH (R3_H));
      Assert (R_1.Collections.Length (R_1.All_Instances) = 1,
              "R_1 missing");
      Assert (R_2.Collections.Length (R_2.All_Instances) = 1,
              "R_2 missing");
      Assert (R_3.Collections.Length (R_3.All_Instances) = 1,
              "R_3 missing");
      Assert (S_2.Collections.Length (S_2.All_Instances) = 1,
              "S_2 missing");
      Assert (S_3.Collections.Length (S_3.All_Instances) = 1,
              "S_3 missing");
      Assert (T_2.Collections.Length (T_2.All_Instances) = 1,
              "T_2 missing");
      Assert (F_2.Collections.Length (F_2.All_Instances) = 1,
              "F_2 missing");
      R1_H := R_1.Collections.First (R_1.All_Instances);
      R2_H := R_2.Collections.First (R_2.All_Instances);
      S2_H := S_2.Collections.First (S_2.All_Instances);
      S3_H := S_3.Collections.First (S_3.All_Instances);
      T2_H := T_2.Collections.First (T_2.All_Instances);
      Assert (CIH (R1_H) = S_2.Get_A_Parent (S2_H),
              "S_2 has wrong parent");
      Assert (S_2.Handle (R_1.Get_A_Child (R1_H).S2) = S2_H,
              "R_1 has wrong child");
      Assert (CIH (R2_H) = S_3.Get_B_Parent (S3_H),
              "S_3 has wrong B parent");
      Assert (S_3.Handle (R_2.Get_B_Child (R2_H).S3) = S3_H,
              "R_2 has wrong child");
      Assert (CIH (R3_H) = S_3.Get_C_Parent (S3_H),
              "S_3 has wrong C parent");
      Assert (S_3.Handle (R_3.Get_C_Child (R3_H).S3) = S3_H,
              "R_3 has wrong child");
      Assert (CIH (S2_H) = T_2.Get_D_Parent (T2_H),
              "T_2 has wrong parent");
      Assert (T_2.Handle (S_2.Get_D_Child (S2_H).T2) = T2_H,
              "S_2 has wrong child");
      Assert (CIH (T2_H) = F_2.Get_F_Parent (F2_H),
              "F_2 has wrong F parent");
      Assert (F_2.Handle (T_2.Get_F_Child (T2_H).F2) = F2_H,
              "T_2 has wrong child");
      Assert (CIH (S3_H) = F_2.Get_E_Parent (F2_H),
              "F_2 has wrong E parent");
      Assert (F_2.Handle (S_3.Get_E_Child (S3_H).F2) = F2_H,
              "S_3 has wrong E child");
   end Create_Third_Child_With_First_R3;

   procedure Create_Third_Child_With_First_R2_R3
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Create_Third_Child_With_First_R2_R3
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
   begin
      S3_H := S_3.Inheritance.Create_Tree (null, null);
      F2_H := F_2.Inheritance.Create_Tree (null, CIH (S3_H), CIH (S3_H));
      Assert (R_1.Collections.Length (R_1.All_Instances) = 1,
              "R_1 missing");
      Assert (R_2.Collections.Length (R_2.All_Instances) = 1,
              "R_2 missing");
      Assert (R_3.Collections.Length (R_3.All_Instances) = 1,
              "R_3 missing");
      Assert (S_2.Collections.Length (S_2.All_Instances) = 1,
              "S_2 missing");
      Assert (S_3.Collections.Length (S_3.All_Instances) = 1,
              "S_3 missing");
      Assert (T_2.Collections.Length (T_2.All_Instances) = 1,
              "T_2 missing");
      Assert (F_2.Collections.Length (F_2.All_Instances) = 1,
              "F_2 missing");
      R1_H := R_1.Collections.First (R_1.All_Instances);
      R2_H := R_2.Collections.First (R_2.All_Instances);
      R3_H := R_3.Collections.First (R_3.All_Instances);
      S2_H := S_2.Collections.First (S_2.All_Instances);
      S3_H := S_3.Collections.First (S_3.All_Instances);
      T2_H := T_2.Collections.First (T_2.All_Instances);
      Assert (CIH (R1_H) = S_2.Get_A_Parent (S2_H),
              "S_2 has wrong parent");
      Assert (S_2.Handle (R_1.Get_A_Child (R1_H).S2) = S2_H,
              "R_1 has wrong child");
      Assert (CIH (R2_H) = S_3.Get_B_Parent (S3_H),
              "S_3 has wrong B parent");
      Assert (S_3.Handle (R_2.Get_B_Child (R2_H).S3) = S3_H,
              "R_2 has wrong child");
      Assert (CIH (R3_H) = S_3.Get_C_Parent (S3_H),
              "S_3 has wrong C parent");
      Assert (S_3.Handle (R_3.Get_C_Child (R3_H).S3) = S3_H,
              "R_3 has wrong child");
      Assert (CIH (S2_H) = T_2.Get_D_Parent (T2_H),
              "T_2 has wrong parent");
      Assert (T_2.Handle (S_2.Get_D_Child (S2_H).T2) = T2_H,
              "S_2 has wrong child");
      Assert (CIH (T2_H) = F_2.Get_F_Parent (F2_H),
              "F_2 has wrong F parent");
      Assert (F_2.Handle (T_2.Get_F_Child (T2_H).F2) = F2_H,
              "T_2 has wrong child");
      Assert (CIH (S3_H) = F_2.Get_E_Parent (F2_H),
              "F_2 has wrong E parent");
      Assert (F_2.Handle (S_3.Get_E_Child (S3_H).F2) = F2_H,
              "S_3 has wrong E child");
   end Create_Third_Child_With_First_R2_R3;

   procedure Create_Third_Child_With_Bad_R2_S3
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Create_Third_Child_With_Bad_R2_S3
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
   begin
      S3_H := S_3.Inheritance.Create_Tree (null, null);
      F2_H := F_2.Inheritance.Create_Tree (null, null, CIH (S3_H));
      Assert (False, "creation succeeded");
   exception
      when ColdFrame.Exceptions.Unexpected_Class => null;
   end Create_Third_Child_With_Bad_R2_S3;

   procedure Create_Third_Child_With_Bad_S3_S3
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Create_Third_Child_With_Bad_S3_S3
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
      S3_H2 : S_3.Handle;
   begin
      S3_H := S_3.Inheritance.Create_Tree (null, null);
      S3_H2 := S_3.Inheritance.Create_Tree (null, null);
      F2_H := F_2.Inheritance.Create_Tree (null, CIH (S3_H), CIH (S3_H2));
      Assert (False, "creation succeeded");
   exception
      when ColdFrame.Exceptions.Mismatched_Instances => null;
   end Create_Third_Child_With_Bad_S3_S3;

   procedure Create_Third_Child_With_Deleted
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Create_Third_Child_With_Deleted
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
      F2_H2 : F_2.Handle;
      F2_H3 : F_2.Handle;
      pragma Warnings (Off, F2_H3);
   begin
      F2_H := F_2.Inheritance.Create_Tree (null, null, null);
      F2_H2 := F2_H;
      Tear_Down;
      F2_H3 := F_2.Inheritance.Create_Tree
        (CIH (F2_H2), CIH (F2_H2), CIH (F2_H2));
      Assert (False, "creation succeeded");
   exception
      when Storage_Error => null;
   end Create_Third_Child_With_Deleted;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine
        (T, Create_Root'Access, "Create root");
      Register_Routine
        (T, Create_Another_First_Child'Access,
         "Create first child when one exists");
      Register_Routine
        (T, Create_First_Child'Access, "Create first child");
      Register_Routine
        (T, Create_Second_Child'Access, "Create second child");
      Register_Routine
        (T, Create_Third_Child'Access, "Create third child");
      Register_Routine
        (T,
         Create_Third_Child_With_Self'Access,
         "Create copy of third child");
      Register_Routine
        (T,
         Create_First_Child_With_Root'Access,
         "Create first child with root");
      Register_Routine
        (T,
         Create_Second_Child_With_First'Access,
         "Create second child with first");
      Register_Routine
        (T,
         Create_Third_Child_With_First_R1'Access,
         "Create third child with first R1");
      Register_Routine
        (T,
         Create_Third_Child_With_First_R3'Access,
         "Create third child with first R3");
      Register_Routine
        (T,
         Create_Third_Child_With_First_R2_R3'Access,
         "Create third child with first R2 & R3");
      Register_Routine
        (T,
         Create_Third_Child_With_Bad_R2_S3'Access,
         "Create third child with bad R2 & S3");
      Register_Routine
        (T,
         Create_Third_Child_With_Bad_S3_S3'Access,
         "Create third child with mismatched R2 & R3");
      Register_Routine
        (T,
         Create_Third_Child_With_Deleted'Access,
         "Create third child with deleted handle");
   end Register_Tests;

   function Name (T : Test_Case) return String_Access is
      pragma Warnings (Off, T);
   begin
      return new String'("Creations");
   end Name;

   procedure Set_Up (T : in out Test_Case) is
      pragma Warnings (Off, T);
   begin
      Initialize;
      R1_H := null;
      R2_H := null;
      R3_H := null;
      S1_H := null;
      S2_H := null;
      S3_H := null;
      T2_H := null;
      F2_H := null;
   end Set_Up;

   procedure Tear_Down (T :  in out Test_Case) is
      pragma Warnings (Off, T);
   begin
      Tear_Down;
   end Tear_Down;

end Hierarchies.Test_Creations;
