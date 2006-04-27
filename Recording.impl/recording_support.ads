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
