with Ada.Exceptions;
with Hierarchies.S_3;

package body Hierarchies.R_2.Inheritance is

   --  XXX this is only practical if the root is an Autonumber
   --  class. I guess that everything below will be OK.

   function Create_Tree (R2 : ColdFrame.Instances.Handle) return Handle is
      use type ColdFrame.Instances.Handle;
   begin
      if R2 = null then
         return Create;
      elsif not (R2.all in Instance'Class) then
         --  XXX of course the next alternative would raise constraint
         --  error here anyway in this event
         Ada.Exceptions.Raise_Exception
           (Constraint_Error'Identity,
            "unexpected class found at root in Create_Tree");
      elsif not Maps.Is_Bound (The_Container, (Id => Handle (R2).Id)) then
         Ada.Exceptions.Raise_Exception
           (Constraint_Error'Identity,
            "unbound handle in Create_Tree");
      else
         return Handle (R2);
      end if;
   end Create_Tree;

   procedure Delete_Child (This : Handle) is
   begin
      case This.B_Current_Child.Current is
         when S_3_T =>
            S_3.Delete
              (S_3.Handle
                 (This.B_Current_Child.S3));
         when Null_T =>
            null;
      end case;

   end Delete_Child;

end Hierarchies.R_2.Inheritance;
