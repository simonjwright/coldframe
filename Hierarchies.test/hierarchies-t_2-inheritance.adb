with Hierarchies.S_2.Inheritance;
with Hierarchies.F_1;
with Hierarchies.F_2;

package body Hierarchies.T_2.Inheritance is

   function Create_Tree (R1 : ColdFrame.Instances.Handle) return Handle is
      S2_H : S_2.Handle;
      use type ColdFrame.Instances.Handle;
   begin
      if R1 = null
        or else not (R1.all in Instance'Class) then
         S2_H := S_2.Inheritance.Create_Tree (R1);
         return Create
           ((D_Parent => ColdFrame.Instances.Handle (S2_H)));
      else
         return Handle (R1);
      end if;
   end Create_Tree;

   function Find_R1_Parent (This : Handle) return R_1.Handle is
   begin
      return S_2.Inheritance.Find_R1_Parent
        (S_2.Handle
           (This.D_Parent));
   end Find_R1_Parent;

   function Find_S2_Parent (This : Handle) return S_2.Handle is
   begin
      return S_2.Handle (This.D_Parent);
   end Find_S2_Parent;

   procedure Delete_Child (This : Handle) is
   begin
      case This.F_Current_Child.Current is
         when F_1_T =>
            F_1.Delete
              (F_1.Handle
                 (This.F_Current_Child.F1));
         when F_2_T =>
            F_2.Delete
              (F_2.Handle
                 (This.F_Current_Child.F2));
         when Null_T =>
            null;
      end case;

   end Delete_Child;

end Hierarchies.T_2.Inheritance;
