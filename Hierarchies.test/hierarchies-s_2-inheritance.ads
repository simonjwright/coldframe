with Hierarchies.R_1;

package Hierarchies.S_2.Inheritance is

   type Parents is
     (R1);

   type Roots is
     (R1);

   function Create_Tree (R1 : ColdFrame.Instances.Handle) return Handle;

   function Find_R1_Parent (This : Handle) return R_1.Handle;

   procedure Delete_Child (This : Handle);

end Hierarchies.S_2.Inheritance;
