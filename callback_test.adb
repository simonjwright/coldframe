--  $Id: callback_test.adb,v b42a9fa631be 2003/02/02 18:35:13 simon $

with ColdFrame.Callbacks;
with GNAT.IO; use GNAT.IO;

procedure Callback_Test is

   package Callbacks is new ColdFrame.Callbacks (String);

   procedure P (S : String);
   procedure Q (S : String);

   procedure P (S : String) is
   begin
      Put_Line ("P called with """ & S & """");
   end P;

   procedure Q (S : String) is
   begin
      Put_Line ("Q called with """ & S & """");
   end Q;

begin

   Callbacks.Call_Callbacks ("1");

   Callbacks.Register (P'Access);

   Callbacks.Call_Callbacks ("2");

   Callbacks.Register (P'Access);

   Callbacks.Call_Callbacks ("3");

   Callbacks.Register (Q'Access);

   Callbacks.Call_Callbacks ("4");

   Callbacks.Clear;

   Callbacks.Call_Callbacks ("5");

   Callbacks.Register (P'Access);

   Callbacks.Call_Callbacks ("6");

   Callbacks.Register (P'Access);

   Callbacks.Call_Callbacks ("7");

   Callbacks.Register (Q'Access);

   Callbacks.Call_Callbacks ("8");

   Callbacks.Deregister (P'Access);

   Callbacks.Call_Callbacks ("9");

   Callbacks.Deregister (Q'Access);

   Callbacks.Call_Callbacks ("10");

   Callbacks.Clear;

   Callbacks.Call_Callbacks ("11");

end Callback_Test;
