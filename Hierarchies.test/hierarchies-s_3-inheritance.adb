with Ada.Exceptions;
with Hierarchies.R_2;
with Hierarchies.R_3;
with Hierarchies.R_2.Inheritance;
with Hierarchies.R_3.Inheritance;
with Hierarchies.F_2;
with Hierarchies.T_3;

package body Hierarchies.S_3.Inheritance is

   function Create_Tree
     (R2 : ColdFrame.Instances.Handle;
      R3 : ColdFrame.Instances.Handle) return Handle is
      R2_H : R_2.Handle;
      R3_H : R_3.Handle;
      use type ColdFrame.Instances.Handle;
   begin
      if R2 = null and R3 = null then
         R2_H := R_2.Inheritance.Create_Tree (null);
         R3_H := R_3.Inheritance.Create_Tree (null);
         return Create
           ((B_Parent => ColdFrame.Instances.Handle (R2_H),
             C_Parent => ColdFrame.Instances.Handle (R3_H)));
      elsif R2 = null or R3 = null then
         R2_H := R_2.Inheritance.Create_Tree (R2);
         R3_H := R_3.Inheritance.Create_Tree (R3);
         return Create
           ((B_Parent => ColdFrame.Instances.Handle (R2_H),
             C_Parent => ColdFrame.Instances.Handle (R3_H)));
      elsif (R2.all in Instance'Class)
        and (R3.all in Instance'Class) then
         if not Maps.Is_Bound
           (The_Container,
            (B_Parent => Handle (R2).B_Parent,
             C_Parent => Handle (R3).C_Parent)) then
            Ada.Exceptions.Raise_Exception
              (Constraint_Error'Identity,
               "unbound handle in Create_Tree");
         else
            return Handle (R2);
         end if;
      else
         R2_H := R_2.Inheritance.Create_Tree (R2);
         R3_H := R_3.Inheritance.Create_Tree (R3);
         return Create
           ((B_Parent => ColdFrame.Instances.Handle (R2_H),
             C_Parent => ColdFrame.Instances.Handle (R3_H)));
      end if;
   end Create_Tree;

   function Find_R2_Parent (This : Handle) return R_2.Handle is
   begin
      return R_2.Handle (This.B_Parent);
   end Find_R2_Parent;

   function Find_R3_Parent (This : Handle) return R_3.Handle is
   begin
      return R_3.Handle (This.C_Parent);
   end Find_R3_Parent;

   procedure Delete_Child (This : Handle) is
   begin
      case This.E_Current_Child.Current is
         when F_2_T =>
            F_2.Delete
              (F_2.Handle
                 (This.E_Current_Child.F2));
         when T_3_T =>
            T_3.Delete
              (T_3.Handle
                 (This.E_Current_Child.T3));
         when Null_T =>
            null;
      end case;
   end Delete_Child;

end Hierarchies.S_3.Inheritance;
