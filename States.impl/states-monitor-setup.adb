separate (States.Monitor)
procedure Setup is
   H : Handle;
begin
   H := Create ((Dev => Input));
   H := Create ((Dev => Output));
end Setup;
