with Ada.Exceptions;
with Hierarchies.S_3.Inheritance;
with Hierarchies.T_2.Inheritance;

package body Hierarchies.F_2.Inheritance is

   function Create_Tree
     (R1 : ColdFrame.Instances.Handle;
      R2 : ColdFrame.Instances.Handle;
      R3 : ColdFrame.Instances.Handle) return Handle is
      T2 : T_2.Handle;
      S3 : S_3.Handle;
      use type ColdFrame.Instances.Handle;
   begin
      if R1 = null
        or else R2 = null
        or else R3 = null
        or else not (R1.all in Instance'Class)
        or else not (R2.all in Instance'Class)
        or else not (R3.all in Instance'Class) then
         T2 := T_2.Inheritance.Create_Tree (R1);
         S3 := S_3.Inheritance.Create_Tree (R2, R3);
         return Create
           ((F_Parent => ColdFrame.Instances.Handle (T2),
             E_Parent => ColdFrame.Instances.Handle (S3)));
      elsif R1 = R2
        and then R1 = R3 then
         return Handle (R1);
      else
         Ada.Exceptions.Raise_Exception
           (Constraint_Error'Identity,
            "mismatched handles in Create_Tree");
      end if;
   end Create_Tree;

   function Find_R1_Parent (This : Handle) return R_1.Handle is
   begin
      return T_2.Inheritance.Find_R1_Parent
        (Find_T2_Parent (This));
   end Find_R1_Parent;

   function Find_R2_Parent (This : Handle) return R_2.Handle is
   begin
      return S_3.Inheritance.Find_R2_Parent
        (Find_S3_Parent (This));
   end Find_R2_Parent;

   function Find_R3_Parent (This : Handle) return R_3.Handle is
   begin
      return S_3.Inheritance.Find_R3_Parent
        (Find_S3_Parent (This));
   end Find_R3_Parent;

   function Find_S2_Parent (This : Handle) return S_2.Handle is
   begin
      return T_2.Inheritance.Find_S2_Parent
        (Find_T2_Parent (This));
   end Find_S2_Parent;

   function Find_S3_Parent (This : Handle) return S_3.Handle is
   begin
      return S_3.Handle (This.E_Parent);
   end Find_S3_Parent;

   function Find_T2_Parent (This : Handle) return T_2.Handle is
   begin
      return T_2.Handle (This.F_Parent);
   end Find_T2_Parent;

end Hierarchies.F_2.Inheritance;
