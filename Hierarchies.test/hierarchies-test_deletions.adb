with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;
with AUnit.Assertions; use AUnit.Assertions;

with ColdFrame.Instances;

with Hierarchies.Initialize;
with Hierarchies.Tear_Down;

with Hierarchies.R_1.Inheritance;
with Hierarchies.R_1.All_Instances;
with Hierarchies.R_1.Collections;
with Hierarchies.R_2;
with Hierarchies.R_2.All_Instances;
with Hierarchies.R_2.Collections;
with Hierarchies.R_3;
with Hierarchies.R_3.All_Instances;
with Hierarchies.R_3.Collections;
with Hierarchies.S_2.Inheritance;
with Hierarchies.S_2.All_Instances;
with Hierarchies.S_2.Collections;
with Hierarchies.S_3;
with Hierarchies.S_3.All_Instances;
with Hierarchies.S_3.Collections;
with Hierarchies.T_2.Inheritance;
with Hierarchies.T_2.All_Instances;
with Hierarchies.T_2.Collections;
with Hierarchies.F_2;
with Hierarchies.F_2.All_Instances;
with Hierarchies.F_2.Collections;

package body Hierarchies.Test_Deletions is

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

   procedure Delete_Root
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Delete_Root
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
   begin
      R_1.Delete (R1_H);
      Assert (R_1.Collections.Length (R_1.All_Instances) = 0,
              "R_1 still present");
      Assert (R_2.Collections.Length (R_2.All_Instances) = 1,
              "R_2 missing");
      Assert (R_3.Collections.Length (R_3.All_Instances) = 1,
              "S_3 missing");
      Assert (S_2.Collections.Length (S_2.All_Instances) = 0,
              "S_2 still present");
      Assert (S_3.Collections.Length (S_3.All_Instances) = 1,
              "S_3 missing");
      Assert (T_2.Collections.Length (T_2.All_Instances) = 0,
              "T_2 still present");
      Assert (F_2.Collections.Length (F_2.All_Instances) = 0,
              "F_2 still present");
   end Delete_Root;

   procedure Delete_Root_Child
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Delete_Root_Child
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
   begin
      R_1.Inheritance.Delete_Child (R1_H);
      Assert (R_1.Collections.Length (R_1.All_Instances) = 1,
              "R_1 missing");
      Assert (R_2.Collections.Length (R_2.All_Instances) = 1,
              "R_2 missing");
      Assert (R_3.Collections.Length (R_3.All_Instances) = 1,
              "S_3 missing");
      Assert (S_2.Collections.Length (S_2.All_Instances) = 0,
              "S_2 still present");
      Assert (S_3.Collections.Length (S_3.All_Instances) = 1,
              "S_3 missing");
      Assert (T_2.Collections.Length (T_2.All_Instances) = 0,
              "T_2 still present");
      Assert (F_2.Collections.Length (F_2.All_Instances) = 0,
              "F_2 still present");
   end Delete_Root_Child;

   procedure Delete_Second_Child
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Delete_Second_Child
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
   begin
      S_2.Inheritance.Delete_Child (S2_H);
      Assert (R_1.Collections.Length (R_1.All_Instances) = 1,
              "R_1 missing");
      Assert (R_2.Collections.Length (R_2.All_Instances) = 1,
              "R_2 missing");
      Assert (R_3.Collections.Length (R_3.All_Instances) = 1,
              "S_3 missing");
      Assert (S_2.Collections.Length (S_2.All_Instances) = 1,
              "S_2 missing");
      Assert (S_3.Collections.Length (S_3.All_Instances) = 1,
              "S_3 missing");
      Assert (T_2.Collections.Length (T_2.All_Instances) = 0,
              "T_2 still present");
      Assert (F_2.Collections.Length (F_2.All_Instances) = 0,
              "F_2 still present");
   end Delete_Second_Child;

   procedure Delete_Third_Child
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Delete_Third_Child
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
   begin
      T_2.Inheritance.Delete_Child (T2_H);
      Assert (R_1.Collections.Length (R_1.All_Instances) = 1,
              "R_1 missing");
      Assert (R_2.Collections.Length (R_2.All_Instances) = 1,
              "R_2 missing");
      Assert (R_3.Collections.Length (R_3.All_Instances) = 1,
              "S_3 missing");
      Assert (S_2.Collections.Length (S_2.All_Instances) = 1,
              "S_2 missing");
      Assert (S_3.Collections.Length (S_3.All_Instances) = 1,
              "S_3 missing");
      Assert (T_2.Collections.Length (T_2.All_Instances) = 1,
              "T_2 missing");
      Assert (F_2.Collections.Length (F_2.All_Instances) = 0,
              "F_2 still present");
   end Delete_Third_Child;

   procedure Delete_Without_Child
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Delete_Without_Child
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
   begin
      T_2.Inheritance.Delete_Child (T2_H);
      R_1.Inheritance.Delete_Child (R1_H);
      Assert (R_1.Collections.Length (R_1.All_Instances) = 1,
              "R_1 missing");
      Assert (R_2.Collections.Length (R_2.All_Instances) = 1,
              "R_2 missing");
      Assert (R_3.Collections.Length (R_3.All_Instances) = 1,
              "S_3 missing");
      Assert (S_2.Collections.Length (S_2.All_Instances) = 0,
              "S_2 still present");
      Assert (S_3.Collections.Length (S_3.All_Instances) = 1,
              "S_3 missing");
      Assert (T_2.Collections.Length (T_2.All_Instances) = 0,
              "T_2 missing");
      Assert (F_2.Collections.Length (F_2.All_Instances) = 0,
              "F_2 still present");
   end Delete_Without_Child;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine
        (T, Delete_Root'Access, "Delete root");
      Register_Routine
        (T, Delete_Root_Child'Access, "Delete root's child");
      Register_Routine
        (T, Delete_Second_Child'Access, "Delete second child");
      Register_Routine
        (T, Delete_Third_Child'Access, "Delete third child");
      Register_Routine
        (T, Delete_Without_Child'Access, "Delete in child's absence");
   end Register_Tests;

   function Name (T : Test_Case) return String_Access is
      pragma Warnings (Off, T);
   begin
      return new String'("Deletions");
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

end Hierarchies.Test_Deletions;
