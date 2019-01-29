--  Acts as receiver of state changes from Digital IO, via
--  Input_Signal State Callback. Calls the instance Changed so the
--  Button can take the appropriate action.

separate (Simple_Buttons.Button)
procedure Receive_Change
  (S : Input_Signal_State) is

   subtype Valid_Input_Signal is Digital_IO.Input_Signal range 0 .. 0;

begin
   if S.S in Valid_Input_Signal then
      declare
         BH : constant Handle := Find ((Name => Button_Name'Val (S.S)));
      begin
         --  If there are any events involved, they must be processed
         --  synchronously.
         if S.State -- pushed
         then
            declare
               Ev : Push (BH);
            begin
               Ev.Handler;
            end;
         else
            declare
               Ev : Release (BH);
            begin
               Ev.Handler;
            end;
         end if;
      end;
   end if;
end Receive_Change;
