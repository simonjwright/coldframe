with Hierarchies.R_1;
with Hierarchies.R_2;
with Hierarchies.R_3;
with Hierarchies.S_2;
with Hierarchies.S_3;
with Hierarchies.T_2;

package Hierarchies.F_2.Inheritance is

   type Parents is
     (R1, R2, R3, S2, S3, T2);

   type Roots is
     (R1, R2, R3);

   function Create_Tree
     (R1 : ColdFrame.Instances.Handle;
      R2 : ColdFrame.Instances.Handle;
      R3 : ColdFrame.Instances.Handle) return Handle;

   function Find_R1_Parent (This : Handle) return R_1.Handle;

   function Find_R2_Parent (This : Handle) return R_2.Handle;

   function Find_R3_Parent (This : Handle) return R_3.Handle;

   function Find_S2_Parent (This : Handle) return S_2.Handle;

   function Find_S3_Parent (This : Handle) return S_3.Handle;

   function Find_T2_Parent (This : Handle) return T_2.Handle;

end Hierarchies.F_2.Inheritance;
