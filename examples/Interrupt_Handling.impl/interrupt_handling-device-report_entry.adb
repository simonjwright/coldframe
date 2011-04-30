with GNAT.IO; use GNAT.IO;

separate (Interrupt_Handling.Device)
procedure Report_Entry is
begin

   Put_Line ("entered state " & This.State_Machine_State'Img);

end Report_Entry;
