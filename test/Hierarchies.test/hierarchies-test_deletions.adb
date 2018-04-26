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

with AUnit.Assertions; use AUnit.Assertions;

with Ada.Containers;
with ColdFrame.Instances;

with Hierarchies.Initialize;
with Hierarchies.Tear_Down;

with Hierarchies.R_1.Inheritance;
with Hierarchies.R_1.All_Instances;
with Hierarchies.R_2;
with Hierarchies.R_2.All_Instances;
with Hierarchies.R_3;
with Hierarchies.R_3.All_Instances;
with Hierarchies.S_2.Inheritance;
with Hierarchies.S_2.All_Instances;
with Hierarchies.S_3;
with Hierarchies.S_3.All_Instances;
with Hierarchies.T_2.Inheritance;
with Hierarchies.T_2.All_Instances;
with Hierarchies.F_2;
with Hierarchies.F_2.All_Instances;

package body Hierarchies.Test_Deletions is

   use type Ada.Containers.Count_Type;

   subtype CIH is ColdFrame.Instances.Handle;

   R1_H : R_1.Handle;
   R2_H : R_2.Handle;
   R3_H : R_3.Handle;
   S2_H : S_2.Handle;
   S3_H : S_3.Handle;
   T2_H : T_2.Handle;
   F2_H : F_2.Handle;
   pragma Unreferenced (F2_H);

   procedure Delete_Root
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Delete_Root
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (R);
   begin
      R_1.Delete (R1_H);
      Assert (R_1.Vectors.Length (R_1.All_Instances) = 0,
              "R_1 still present");
      Assert (R_2.Vectors.Length (R_2.All_Instances) = 1,
              "R_2 missing");
      Assert (R_3.Vectors.Length (R_3.All_Instances) = 1,
              "S_3 missing");
      Assert (S_2.Vectors.Length (S_2.All_Instances) = 0,
              "S_2 still present");
      Assert (S_3.Vectors.Length (S_3.All_Instances) = 1,
              "S_3 missing");
      Assert (T_2.Vectors.Length (T_2.All_Instances) = 0,
              "T_2 still present");
      Assert (F_2.Vectors.Length (F_2.All_Instances) = 0,
              "F_2 still present");
   end Delete_Root;

   procedure Delete_Root_Child
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Delete_Root_Child
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (R);
   begin
      R_1.Inheritance.Delete_Child (R1_H);
      Assert (R_1.Vectors.Length (R_1.All_Instances) = 1,
              "R_1 missing");
      Assert (R_2.Vectors.Length (R_2.All_Instances) = 1,
              "R_2 missing");
      Assert (R_3.Vectors.Length (R_3.All_Instances) = 1,
              "S_3 missing");
      Assert (S_2.Vectors.Length (S_2.All_Instances) = 0,
              "S_2 still present");
      Assert (S_3.Vectors.Length (S_3.All_Instances) = 1,
              "S_3 missing");
      Assert (T_2.Vectors.Length (T_2.All_Instances) = 0,
              "T_2 still present");
      Assert (F_2.Vectors.Length (F_2.All_Instances) = 0,
              "F_2 still present");
   end Delete_Root_Child;

   procedure Delete_Second_Child
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Delete_Second_Child
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (R);
   begin
      S_2.Inheritance.Delete_Child (S2_H);
      Assert (R_1.Vectors.Length (R_1.All_Instances) = 1,
              "R_1 missing");
      Assert (R_2.Vectors.Length (R_2.All_Instances) = 1,
              "R_2 missing");
      Assert (R_3.Vectors.Length (R_3.All_Instances) = 1,
              "S_3 missing");
      Assert (S_2.Vectors.Length (S_2.All_Instances) = 1,
              "S_2 missing");
      Assert (S_3.Vectors.Length (S_3.All_Instances) = 1,
              "S_3 missing");
      Assert (T_2.Vectors.Length (T_2.All_Instances) = 0,
              "T_2 still present");
      Assert (F_2.Vectors.Length (F_2.All_Instances) = 0,
              "F_2 still present");
   end Delete_Second_Child;

   procedure Delete_Third_Child
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Delete_Third_Child
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (R);
   begin
      T_2.Inheritance.Delete_Child (T2_H);
      Assert (R_1.Vectors.Length (R_1.All_Instances) = 1,
              "R_1 missing");
      Assert (R_2.Vectors.Length (R_2.All_Instances) = 1,
              "R_2 missing");
      Assert (R_3.Vectors.Length (R_3.All_Instances) = 1,
              "S_3 missing");
      Assert (S_2.Vectors.Length (S_2.All_Instances) = 1,
              "S_2 missing");
      Assert (S_3.Vectors.Length (S_3.All_Instances) = 1,
              "S_3 missing");
      Assert (T_2.Vectors.Length (T_2.All_Instances) = 1,
              "T_2 missing");
      Assert (F_2.Vectors.Length (F_2.All_Instances) = 0,
              "F_2 still present");
   end Delete_Third_Child;

   procedure Delete_Without_Child
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Delete_Without_Child
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (R);
   begin
      T_2.Inheritance.Delete_Child (T2_H);
      R_1.Inheritance.Delete_Child (R1_H);
      Assert (R_1.Vectors.Length (R_1.All_Instances) = 1,
              "R_1 missing");
      Assert (R_2.Vectors.Length (R_2.All_Instances) = 1,
              "R_2 missing");
      Assert (R_3.Vectors.Length (R_3.All_Instances) = 1,
              "S_3 missing");
      Assert (S_2.Vectors.Length (S_2.All_Instances) = 0,
              "S_2 still present");
      Assert (S_3.Vectors.Length (S_3.All_Instances) = 1,
              "S_3 missing");
      Assert (T_2.Vectors.Length (T_2.All_Instances) = 0,
              "T_2 missing");
      Assert (F_2.Vectors.Length (F_2.All_Instances) = 0,
              "F_2 still present");
   end Delete_Without_Child;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Registration.Register_Routine
        (T, Delete_Root'Access, "Delete root");
      Registration.Register_Routine
        (T, Delete_Root_Child'Access, "Delete root's child");
      Registration.Register_Routine
        (T, Delete_Second_Child'Access, "Delete second child");
      Registration.Register_Routine
        (T, Delete_Third_Child'Access, "Delete third child");
      Registration.Register_Routine
        (T, Delete_Without_Child'Access, "Delete in child's absence");
   end Register_Tests;

   function Name (T : Test_Case) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return new String'("Deletions");
   end Name;

   procedure Set_Up (T : in out Test_Case) is
      pragma Unreferenced (T);
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
      pragma Unreferenced (T);
   begin
      Tear_Down;
   end Tear_Down;

end Hierarchies.Test_Deletions;
