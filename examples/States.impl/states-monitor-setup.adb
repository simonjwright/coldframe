separate (States.Monitor)
procedure Setup is
   H : Handle;
   pragma Warnings (Off, H);
begin
   H := Create ((Dev => Input));
   H := Create ((Dev => Output));
end Setup;
