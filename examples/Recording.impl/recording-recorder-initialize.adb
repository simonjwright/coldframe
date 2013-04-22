--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  Part of the Recording demonstration.

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

--  Sets up the Socket and the Buffer.

separate (Recording.Recorder)
procedure Initialize is
   Capacity : constant Positive := 16;  --  would be adaptable
begin

   --  OK to initialize Sockets more than once, not OK not to
   --  initialize at all!
   GNAT.Sockets.Initialize;

   --  Need to create the Socket.
   --  GNAT.Create_Socket (This.Socket)

   This.Buff.Initialize (Capacity => Capacity);

end Initialize;
