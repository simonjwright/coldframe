--  Wait for a second before setting 'Initialized'.

separate (Initialization.Checking_Domain_Initialization)
procedure Initialize is
begin

   delay 1.0
   Initialized := True;

end Initialize;
