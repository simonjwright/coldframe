with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;
with AUnit.Assertions; use AUnit.Assertions;

with Event_Test.Events;
with Event_Test.Events.Tear_Down;

with ColdFrame.Project.Events;

package body Event_Test.Test_Engine is

   --------------------
   --  Test globals  --
   --------------------

   Waiting : Boolean := False;
   Result : Integer := 0;


   -------------------
   --  Test events  --
   -------------------

   --  This event delays for the duration requested in its Payload.
   type Wait is new ColdFrame.Project.Events.Event_Base with record
      Payload : Duration;
   end record;

   procedure Handler (For_The_Event : Wait);

   procedure Handler (For_The_Event : Wait) is
   begin
      Waiting := True;
      delay For_The_Event.Payload;
      Waiting := False;
   end Handler;


   --  This event stores its Payload in Result.
   type Store is new ColdFrame.Project.Events.Event_Base with record
      Payload : Integer;
   end record;

   procedure Handler (For_The_Event : Store);

   procedure Handler (For_The_Event : Store) is
   begin
      Result := For_The_Event.Payload;
   end Handler;


   --  This event waits for Interval, then posts a Store with Payload
   --  equal to its own, as a normal event or as an event-to-self
   --  depending on To_Self.
   type Post is new ColdFrame.Project.Events.Event_Base with record
      Payload : Integer;
      To_Self : Boolean;
      Interval : Duration;
   end record;

   procedure Handler (For_The_Event : Post);

   procedure Handler (For_The_Event : Post) is
      Ev : constant ColdFrame.Project.Events.Event_P
        := new Store;
      S : Store renames Store (Ev.all);
   begin
      delay For_The_Event.Interval;
      S.Payload := For_The_Event.Payload;
      if For_The_Event.To_Self then
         ColdFrame.Project.Events.Post_To_Self
           (Ev,
            On => Events.Dispatcher);
      else
         ColdFrame.Project.Events.Post
           (Ev,
            On => Events.Dispatcher);
      end if;
   end Handler;


   --  This event takes a lock on its own Event Queue
   type Nest is new ColdFrame.Project.Events.Event_Base with null record;

   procedure Handler (For_The_Event : Nest);

   procedure Handler (For_The_Event : Nest) is
      L : ColdFrame.Project.Events.Lock (Events.Dispatcher);
      pragma Warnings (Off, For_The_Event);
      pragma Warnings (Off, L);
   begin
      null;
   end Handler;


   -----------------------
   --  Test procedures  --
   -----------------------

   --  One task can take out nested locks.
   procedure Lock_Vs_Self
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Lock_Vs_Self
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
   begin
      ColdFrame.Project.Events.Start (Events.Dispatcher);
      declare
         L : ColdFrame.Project.Events.Lock (Events.Dispatcher);
         pragma Warnings (Off, L);
      begin
         declare
            L2 : ColdFrame.Project.Events.Lock (Events.Dispatcher);
            pragma Warnings (Off, L2);
         begin
            --  Of course, the real check is that the procedure
            --  manages to pass this point!
            Assert (True,
                    "lock wasn't achieved");
         end;
      end;
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);
   end Lock_Vs_Self;


   --  Locks respect other locks.
   procedure Lock_Vs_Lock
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Lock_Vs_Lock
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
      task T;
      Sentinel : Boolean := False;
      task body T is
         L : ColdFrame.Project.Events.Lock (Events.Dispatcher);
         pragma Warnings (Off, L);
      begin
         delay 0.1;
         Sentinel := True;
      end T;
   begin
      ColdFrame.Project.Events.Start (Events.Dispatcher);
      delay 0.01;
      declare
         L : ColdFrame.Project.Events.Lock (Events.Dispatcher);
         pragma Warnings (Off, L);
      begin
         null;
      end;
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);
      Assert (Sentinel,
              "lock wasn't respected");
   end Lock_Vs_Lock;


   --  Locks respect events.
   procedure Lock_Vs_Event
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Lock_Vs_Event
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
      Ev : constant ColdFrame.Project.Events.Event_P
        := new Wait;
      W : Wait renames Wait (Ev.all);
   begin
      --  Ev will fire immediately. Its handler waits for 0.1 seonds, as
      --  instructed.
      W.Payload := 0.1;
      ColdFrame.Project.Events.Post (Ev,
                                     On => Events.Dispatcher);
      ColdFrame.Project.Events.Start (Events.Dispatcher);
      delay 0.01;
      Assert (Waiting,
              "event is not being handled");
      declare
         L : ColdFrame.Project.Events.Lock (Events.Dispatcher);
         pragma Warnings (Off, L);
      begin
         Assert (not Waiting,
                 "event is still being handled");
      end;
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);
   end Lock_Vs_Event;


   --  Normal events have equal priority with Locks. There is no
   --  guarantee on relative timing.
   --  Self events have higher priority than Locks.
   procedure Lock_Vs_Self_Event
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Lock_Vs_Self_Event
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
      Ev : constant ColdFrame.Project.Events.Event_P
        := new Post;
      P : Post renames Post (Ev.all);
   begin
      P.Payload := 17;
      P.To_Self := True;
      P.Interval := 0.1;
      ColdFrame.Project.Events.Post (Ev,
                                     On => Events.Dispatcher);
      ColdFrame.Project.Events.Start (Events.Dispatcher);
      delay 0.01;
      declare
         L : ColdFrame.Project.Events.Lock (Events.Dispatcher);
         pragma Warnings (Off, L);
      begin
         Assert (Result = 17,
                 "event has fired");
      end;
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);
   end Lock_Vs_Self_Event;


   --  An event handler can take out a lock on its own Event Queue.
   procedure Event_Takes_Lock
     (R : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Event_Takes_Lock
     (R : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Warnings (Off, R);
      Ev : constant ColdFrame.Project.Events.Event_P
        := new Nest;
   begin
      ColdFrame.Project.Events.Post (Ev,
                                     On => Events.Dispatcher);
      ColdFrame.Project.Events.Start (Events.Dispatcher);
      ColdFrame.Project.Events.Wait_Until_Idle (Events.Dispatcher);
      --  Of course, the real check is that the procedure
      --  manages to pass this point!
      Assert (True,
              "lock wasn't achieved");
   end Event_Takes_Lock;


   ---------------
   --  Harness  --
   ---------------

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine
        (T, Lock_Vs_Self'Access, "Nested lock");
      Register_Routine
        (T, Lock_Vs_Lock'Access, "Lock against lock");
      Register_Routine
        (T, Lock_Vs_Event'Access, "Lock against event");
      Register_Routine
        (T, Lock_Vs_Self_Event'Access, "Lock against self event");
      Register_Routine
        (T, Event_Takes_Lock'Access, "Event handler takes nested lock");
   end Register_Tests;

   function Name (T : Test_Case) return String_Access is
      pragma Warnings (Off, T);
   begin
      return new String'("Event engine");
   end Name;

   procedure Set_Up (T : in out Test_Case) is
      pragma Warnings (Off, T);
   begin
      Events.Initialize;
      Waiting := False;
      Result := 0;
   end Set_Up;

   procedure Tear_Down (T :  in out Test_Case) is
      pragma Warnings (Off, T);
   begin
      Events.Tear_Down;
   end Tear_Down;

end Event_Test.Test_Engine;
