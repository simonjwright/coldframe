--  Copyright (C) Simon Wright <simon@pushface.org>

--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  Part of the Recording demonstration.

--  $RCSfile: recording_support.ads,v $
--  $Revision: ef76ca9d66ef $
--  $Date: 2006/05/03 22:07:21 $
--  $Author: simonjwright $

with Ada.Finalization;
with BC.Containers.Queues.Unmanaged;
with BC.Support.Memory_Streams;

package Recording_Support is

   type Serializable is abstract tagged private;
   function Image (R : Serializable) return String;

   subtype Recordable is Serializable'Class;

   type Stream is access BC.Support.Memory_Streams.Stream_Type;

   type Controlled_Stream is new Ada.Finalization.Controlled with record
      Str : Stream;
   end record;
   procedure Finalize (Obj : in out Controlled_Stream);

   package Abstract_Controlled_Stream_Containers
   is new BC.Containers (Controlled_Stream);
   package Abstract_Controlled_Stream_Queues
   is new Abstract_Controlled_Stream_Containers.Queues;
   package Queues is new Abstract_Controlled_Stream_Queues.Unmanaged;

   subtype Queue is Queues.Queue;

private

   type Serializable is abstract tagged null record;

end Recording_Support;
