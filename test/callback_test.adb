with Ada.Exceptions;
with ColdFrame.Callbacks;
with GNAT.IO; use GNAT.IO;

procedure Callback_Test is

   package String_Callback is new ColdFrame.Callbacks (String);

   procedure P (S : String);
   procedure Q (S : String);
   procedure Constraint_Err (S : String);
   procedure Program_Err (S : String);

   procedure P (S : String) is
   begin
      Put_Line ("P called with """ & S & """");
   end P;

   procedure Q (S : String) is
   begin
      Put_Line ("Q called with """ & S & """");
   end Q;

   procedure Constraint_Err (S : String) is
   begin
      raise Constraint_Error;
   end Constraint_Err;

   procedure Program_Err (S : String) is
   begin
      raise Program_Error;
   end Program_Err;

begin

   String_Callback.Call_Callbacks ("1");

   String_Callback.Register (P'Access);

   String_Callback.Call_Callbacks ("2");

   String_Callback.Register (P'Access);

   String_Callback.Call_Callbacks ("3");

   String_Callback.Register (Q'Access);

   String_Callback.Call_Callbacks ("4");

   String_Callback.Clear;

   String_Callback.Call_Callbacks ("5");

   String_Callback.Register (P'Access);

   String_Callback.Call_Callbacks ("6");

   String_Callback.Register (P'Access);

   String_Callback.Call_Callbacks ("7");

   String_Callback.Register (Q'Access);

   String_Callback.Call_Callbacks ("8");

   String_Callback.Deregister (P'Access);

   String_Callback.Call_Callbacks ("9");

   String_Callback.Deregister (Q'Access);

   String_Callback.Call_Callbacks ("10");

   String_Callback.Clear;

   String_Callback.Call_Callbacks ("11");

   String_Callback.Clear;

   begin
      String_Callback.Register (Q'Access);
      String_Callback.Register (Constraint_Err'Access);
      String_Callback.Register (P'Access);
      String_Callback.Register (Program_Err'Access);
      String_Callback.Call_Callbacks ("12");
   exception
      when E : others =>
         Put_Line (Ada.Exceptions.Exception_Information (E));
   end;

end Callback_Test;
