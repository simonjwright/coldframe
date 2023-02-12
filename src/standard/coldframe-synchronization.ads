--  Copyright Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License.  This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

--  This unit has been copied from the Booch Components, where it was
--  BC.Support.Synchronization.

private with Ada.Finalization;
private with Ada.Task_Identification;

package ColdFrame.Synchronization is

   pragma Elaborate_Body;

   --  Semaphores provide for mutual exclusion.
   type Semaphore_Base is abstract tagged limited private;
   procedure Seize (The_Semaphore : in out Semaphore_Base) is abstract;
   procedure Release (The_Semaphore : in out Semaphore_Base) is abstract;
   function None_Pending (On_The_Semaphore : Semaphore_Base) return Boolean
      is abstract;


   --  A Semaphore is like a standard POSIX mutex.
   type Semaphore is new Semaphore_Base with private;
   procedure Seize (The_Semaphore : in out Semaphore);
   procedure Release (The_Semaphore : in out Semaphore);
   function None_Pending (On_The_Semaphore : Semaphore) return Boolean;


   --  A Recursive_Semaphore is like a POSIX recursive mutex; once
   --  Seized by a task, that task can Seize again; other tasks are
   --  blocked until the owning task has Released the semaphore as
   --  many times as it Seized it.
   type Recursive_Semaphore is new Semaphore_Base with private;
   procedure Seize (The_Semaphore : in out Recursive_Semaphore);
   procedure Release (The_Semaphore : in out Recursive_Semaphore);
   function None_Pending
     (On_The_Semaphore : Recursive_Semaphore) return Boolean;


   --  Monitors support Locks.
   type Monitor_Base is abstract tagged limited private;
   procedure Seize_For_Reading (The_Monitor : in out Monitor_Base)
      is abstract;
   procedure Seize_For_Writing (The_Monitor : in out Monitor_Base)
      is abstract;
   procedure Release_From_Reading (The_Monitor : in out Monitor_Base)
      is abstract;
   procedure Release_From_Writing (The_Monitor : in out Monitor_Base)
      is abstract;


   --  Single_Monitors allow one task at a time to have access, be it
   --  for reading or writing.
   type Single_Monitor is new Monitor_Base with private;
   procedure Seize_For_Reading (The_Monitor : in out Single_Monitor);
   procedure Seize_For_Writing (The_Monitor : in out Single_Monitor);
   procedure Release_From_Reading (The_Monitor : in out Single_Monitor);
   procedure Release_From_Writing (The_Monitor : in out Single_Monitor);


   --  Multiple_Monitors allow multiple readers; however, when a
   --  writer owns the monitor it has exclusive access.
   type Multiple_Monitor is new Monitor_Base with private;
   procedure Seize_For_Reading (The_Monitor : in out Multiple_Monitor);
   procedure Seize_For_Writing (The_Monitor : in out Multiple_Monitor);
   procedure Release_From_Reading (The_Monitor : in out Multiple_Monitor);
   procedure Release_From_Writing (The_Monitor : in out Multiple_Monitor);


   --  A Lock is designed to provide "locking by declaration".
   --    declare
   --      L : Lock (Some_Monitor'Access);
   --    begin
   --      -- the monitor is locked
   --    end;
   --    -- the monitor is unlocked as L is finalized, even if an exception
   --    -- occurs

   type Lock_Base is abstract tagged limited private;

   --  A simple Lock provides mutual exclusion
   type Lock (Using : access Semaphore_Base'Class)
   is new Lock_Base with private;

   --  Read_ and Write_ Locks support multiple reader/single writer
   --  access provided the given Monitor supports it; otherwise they
   --  merely provide mutual exclusion.
   type Read_Lock (Using : access Monitor_Base'Class)
   is new Lock_Base with private;

   type Write_Lock (Using : access Monitor_Base'Class)
   is new Lock_Base with private;

private

   type Semaphore_Base is abstract tagged limited null record;

   protected type Semaphore_Type is
      entry Seize;
      procedure Release;
      function None_Pending return Boolean;
   private
      Seized : Boolean := False;
   end Semaphore_Type;

   type Semaphore is new Semaphore_Base with record
      S : Semaphore_Type;
   end record;

   protected type Recursive_Semaphore_Type is
      entry Seize;
      procedure Release;
      function None_Pending return Boolean;
   private
      entry Waiting;
      Owner : Ada.Task_Identification.Task_Id;
      Count : Natural := 0;
   end Recursive_Semaphore_Type;

   type Recursive_Semaphore is new Semaphore_Base with record
      S : Recursive_Semaphore_Type;
   end record;

   type Monitor_Base is abstract tagged limited null record;

   type Single_Monitor is new Monitor_Base with record
      The_Semaphore : Recursive_Semaphore;
   end record;

   --  Monitor_Type is due to Matthew Heaney <matthew_heaney@acm.org>.
   --  The Booch C++ version was inoperative, at least in sjw's translation.

   type Seize_Kind is (For_Reading, For_Writing);

   protected type Monitor_Type is
      entry Seize (Kind : Seize_Kind);
      procedure Release_From_Reading;
      procedure Release_From_Writing;
   private
      entry Waiting_To_Write;
      Reader_Count : Natural := 0;
      Writing : Boolean := False;
   end Monitor_Type;

   type Multiple_Monitor is new Monitor_Base with record
      M : Monitor_Type;
   end record;

   type Lock_Base
   is abstract new Ada.Finalization.Limited_Controlled with record
      Finalized : Boolean := False;
   end record;

   type Lock (Using : access Semaphore_Base'Class)
   is new Lock_Base with null record;
   procedure Initialize (The_Lock : in out Lock);
   procedure Finalize (The_Lock : in out Lock);

   type Read_Lock (Using : access Monitor_Base'Class)
   is new Lock_Base with null record;
   procedure Initialize (The_Lock : in out Read_Lock);
   procedure Finalize (The_Lock : in out Read_Lock);

   type Write_Lock (Using : access Monitor_Base'Class)
   is new Lock_Base with null record;
   procedure Initialize (The_Lock : in out Write_Lock);
   procedure Finalize (The_Lock : in out Write_Lock);

end ColdFrame.Synchronization;
