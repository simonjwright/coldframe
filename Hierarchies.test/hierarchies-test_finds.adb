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

--  $RCSfile: hierarchies-test_finds.adb,v $
--  $Revision: 281d11e491da $
--  $Date: 2002/07/27 13:05:23 $
--  $Author: simon $

with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;
with AUnit.Assertions; use AUnit.Assertions;

with ColdFrame.Instances;

with Hierarchies.Initialize;
with Hierarchies.Tear_Down;

with Hierarchies.R_1;
with Hierarchies.R_2;
with Hierarchies.R_3;
with Hierarchies.S_2.Inheritance;
with Hierarchies.S_3;
with Hierarchies.T_2.Inheritance;
with Hierarchies.F_2.Inheritance;

package body Hierarchies.Test_Finds is

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
   S2_H : S_2.Handle;
   S3_H : S_3.Handle;
   T2_H : T_2.Handle;
   F2_H : F_2.Handle;

   procedure Find_Root_One
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Find_Root_One
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
   begin
      Assert (S_2.Inheritance.Find_R_1_Parent (S2_H) = R1_H,
              "parent not found");
   end Find_Root_One;

   procedure Find_Root_Two
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Find_Root_Two
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
   begin
      Assert (T_2.Inheritance.Find_R_1_Parent (T2_H) = R1_H,
              "parent not found");
   end Find_Root_Two;

   procedure Find_Root_Three
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Find_Root_Three
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
   begin
      Assert (F_2.Inheritance.Find_R_1_Parent (F2_H) = R1_H,
              "parent not found");
   end Find_Root_Three;

   procedure Find_Intermediate_One
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Find_Intermediate_One
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
   begin
      Assert (F_2.Inheritance.Find_T_2_Parent (F2_H) = T2_H,
              "parent not found");
   end Find_Intermediate_One;

   procedure Find_Intermediate_Two
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Find_Intermediate_Two
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
   begin
      Assert (F_2.Inheritance.Find_S_2_Parent (F2_H) = S2_H,
              "parent not found");
   end Find_Intermediate_Two;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine
        (T, Find_Root_One'Access, "Find root (one level)");
      Register_Routine
        (T, Find_Root_Two'Access, "Find root (two levels)");
      Register_Routine
        (T, Find_Root_Three'Access, "Find root (three levels)");
      Register_Routine
        (T, Find_Intermediate_One'Access, "Find intermediate (one level)");
      Register_Routine
        (T, Find_Intermediate_Two'Access, "Find intermediate (two levels)");
   end Register_Tests;

   function Name (T : Test_Case) return String_Access is
      pragma Warnings (Off, T);
   begin
      return new String'("Finds");
   end Name;

   procedure Set_Up (T : in out Test_Case) is
      pragma Warnings (Off, T);
   begin
      Initialize;
      R1_H := R_1.Create;
      R2_H := R_2.Create;
      R3_H := R_3.Create;
      S2_H := S_2.Create ((A_Parent => CIH (R1_H)));
      S3_H := S_3.Create ((B_Parent => CIH (R2_H),
                           C_Parent => CIH (R3_H)));
      T2_H := T_2.Create ((D_Parent => CIH (S2_H)));
      F2_H := F_2.Create ((E_Parent => CIH (S3_H),
                           F_Parent => CIH (T2_H)));
   end Set_Up;

   procedure Tear_Down (T :  in out Test_Case) is
      pragma Warnings (Off, T);
   begin
      Tear_Down;
   end Tear_Down;

end Hierarchies.Test_Finds;
