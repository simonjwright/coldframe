with Ada.Exceptions;
with Hierarchies.S_1;
with Hierarchies.S_2;

package body Hierarchies.R_1.Inheritance is

   --  XXX this is only practical if the root is an Autonumber
   --  class. I guess that everything below will be OK.

   function Create_Tree (R1 : ColdFrame.Instances.Handle) return Handle is
      use type ColdFrame.Instances.Handle;
   begin
      if R1 = null then
         return Create;
      elsif not (R1.all in Instance'Class) then
         --  XXX of course the next alternative would raise constraint
         --  error here anyway in this event
         Ada.Exceptions.Raise_Exception
           (Constraint_Error'Identity,
            "unexpected class found at root in Create_Tree");
      elsif not Maps.Is_Bound (The_Container, (Id => Handle (R1).Id)) then
         Ada.Exceptions.Raise_Exception
           (Constraint_Error'Identity,
            "unbound handle in Create_Tree");
      else
         return Handle (R1);
      end if;
   end Create_Tree;

   procedure Delete_Child (This : Handle) is
   begin
      case This.A_Current_Child.Current is
         when S_1_T =>
            S_1.Delete
              (S_1.Handle
                 (This.A_Current_Child.S1));
         when S_2_T =>
            S_2.Delete
              (S_2.Handle
                 (This.A_Current_Child.S2));
         when Null_T =>
            null;
      end case;

   end Delete_Child;

end Hierarchies.R_1.Inheritance;
