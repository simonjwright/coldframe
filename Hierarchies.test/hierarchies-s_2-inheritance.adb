with Hierarchies.R_1.Inheritance;
with Hierarchies.T_1;
with Hierarchies.T_2;

package body Hierarchies.S_2.Inheritance is

   function Create_Tree (R1 : ColdFrame.Instances.Handle) return Handle is
      R1_H : R_1.Handle;
      use type ColdFrame.Instances.Handle;
   begin
      if R1 = null
        or else not (R1.all in Instance'Class) then
         R1_H := R_1.Inheritance.Create_Tree (R1);
         return Create
           ((A_Parent => ColdFrame.Instances.Handle (R1_H)));
      else
         return Handle (R1);
      end if;
   end Create_Tree;

   function Find_R1_Parent (This : Handle) return R_1.Handle is
   begin
      return R_1.Handle (This.A_Parent);
   end Find_R1_Parent;

   procedure Delete_Child (This : Handle) is
   begin
      case This.D_Current_Child.Current is
         when T_1_T =>
            T_1.Delete
              (T_1.Handle
                 (This.D_Current_Child.T1));
         when T_2_T =>
            T_2.Delete
              (T_2.Handle
                 (This.D_Current_Child.T2));
         when Null_T =>
            null;
      end case;

   end Delete_Child;

end Hierarchies.S_2.Inheritance;
