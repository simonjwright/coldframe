--  $Id: serialization-server-post.adb,v 7b585abe1423 2003/01/22 22:36:20 simon $

--  This instance event handler posts its parameter's value to the
--  output stream. It then frees the value.

with Serialization_Support;

separate (Serialization.Server)
procedure Post
  (This : Handle;
   V : Serializable_Access) is
begin

   Serializable'Output (This.Channel, V.all);

   --  Use a local block to allow us to Free the in parameter V.
   declare
      Local : Serializable_Access := V;
   begin
      Serialization_Support.Free (Local);
   end;

end Post;
