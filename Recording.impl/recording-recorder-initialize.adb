--  Sets up the Socket and the Buffer.

separate (Recording.Recorder)
procedure Initialize is
begin

   --  OK to initialize Sockets more than once, not OK not to
   --  initialize at all!
   GNAT.Sockets.Initialize;

   --  Need to create the Socket.
   --  GNAT.Create_Socket ()

   --  The Buffer's capacity would probably be adaptable via a runtime
   --  parameter.
   This.Buff.Initialize;

end Initialize;
