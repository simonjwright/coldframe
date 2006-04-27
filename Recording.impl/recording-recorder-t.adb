with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

separate (Recording.Recorder)
task body T is
begin
   accept Start;
   loop
      declare
         Str : Stream;
      begin
         This.Buff.Fetch_Stream (Str);
         begin
            --  output the contents of Str to This.Socket
            null;
         exception
            when E : others =>
               Put_Line (Ada.Exceptions.Exception_Information (E));
               delay 1.0;  -- avoid tight loop
         end;
         This.Buff.Done;
      end;
   end loop;
end T;
