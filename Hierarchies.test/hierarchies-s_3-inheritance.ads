with Hierarchies.R_2;
with Hierarchies.R_3;

package Hierarchies.S_3.Inheritance is

   type Parents is
     (R2, R3);

   type Roots is
     (R2, R3);

   function Create_Tree
     (R2 : ColdFrame.Instances.Handle;
      R3 : ColdFrame.Instances.Handle) return Handle;

   function Find_R2_Parent (This : Handle) return R_2.Handle;

   function Find_R3_Parent (This : Handle) return R_3.Handle;

   procedure Delete_Child (This : Handle);

end Hierarchies.S_3.Inheritance;
