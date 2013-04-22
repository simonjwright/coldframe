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

with Ada.Unchecked_Deallocation;

package body Recording_Support is

   procedure Delete
   is new Ada.Unchecked_Deallocation (BC.Support.Memory_Streams.Stream_Type,
                                      Stream);

   function Image (R : Serializable) return String is
      pragma Unreferenced (R);
   begin
      return "";
   end Image;

   procedure Finalize (Obj : in out Controlled_Stream) is
   begin
      if Obj.Str /= null then
         Delete (Obj.Str);
      end if;
   end Finalize;

end Recording_Support;
