with Hierarchies.R_1;
with Hierarchies.S_2;

package Hierarchies.T_2.Inheritance is

   type Parents is
     (R1, S2);

   type Roots is
     (R1);

   function Create_Tree (R1 : ColdFrame.Instances.Handle) return Handle;

   function Find_R1_Parent (This : Handle) return R_1.Handle;

   function Find_S2_Parent (This : Handle) return S_2.Handle;

   procedure Delete_Child (This : Handle);

end Hierarchies.T_2.Inheritance;
