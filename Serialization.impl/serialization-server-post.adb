--  $Id: serialization-server-post.adb,v fbe698e22b63 2003/01/24 06:25:15 simon $

--  This instance event handler posts its parameter's value to the
--  output stream. It then frees the value.

with BC.Support.Memory_Streams;

separate (Serialization.Server)
procedure Post
  (This : Handle;
   V : Buffer) is
begin

   BC.Support.Memory_Streams.Write_Contents (To => This.Channel, Stream => V);

end Post;
