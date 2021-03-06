with Performance.Initialize;
with Performance.Tear_Down;
with Performance.Person.All_Instances;
with Performance.Owner.Inheritance;
with Performance.Pet;
with Performance.Cat.Inheritance;
with Performance.A1;
with Performance.A2.All_Instances;
with Performance.Event_Timing;

with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;
with ColdFrame.Instances;
with ColdFrame.Project.Events;
with ColdFrame.Exceptions.Traceback;
pragma Warnings (Off, ColdFrame.Exceptions.Traceback);

with ColdFrame.Project.High_Resolution_Time;
use ColdFrame.Project.High_Resolution_Time;

procedure Performance.Harness is
   subtype CIH is ColdFrame.Instances.Handle;
   T : Time;
   D : Duration;
   use type Ada.Containers.Count_Type;
begin

   T := Clock;
   for I in 1 .. 25 loop
      D := Clock - T;
   end loop;
   Put_Line ("no-op:" & Duration'Image (D / 25));

   declare
      Pr : Person.Handle;
      pragma Warnings (Off, Pr);
   begin

      Performance.Initialize;

      T := Clock;
      for I in Owners loop
         Pr := Person.Create ((Name => I));
      end loop;
      D := Clock - T;
      Put_Line ("average creation time (identifier, enum):"
                  & Duration'Image (D / (Owners'Pos (Owners'Last) + 1)));

      T := Clock;
      for I in Owners loop
         Pr := Person.Find ((Name => I));
      end loop;
      D := Clock - T;
      Put_Line ("average find time (identifier, enum):"
                  & Duration'Image (D / (Owners'Pos (Owners'Last) + 1)));

      T := Clock;
      for I in Owners loop
         Person.Delete ((Name => I));
      end loop;
      D := Clock - T;
      Put_Line ("average deletion time (identifier, enum):"
                  & Duration'Image (D / (Owners'Pos (Owners'Last) + 1)));

      Performance.Tear_Down;

   end;

   declare
      Ps : array (Owners) of Person.Handle;
   begin

      Performance.Initialize;

      for P in Ps'Range loop
         Ps (P) := Person.Create ((Name => P));
      end loop;
      T := Clock;
      for P in Owners loop
         Person.Delete (Ps (P));
      end loop;
      D := Clock - T;
      Put_Line ("average deletion time (handle):"
                  & Duration'Image (D / (Owners'Pos (Owners'Last) + 1)));

      Performance.Tear_Down;

   end;

   declare
      Pr : Person.Handle;
      pragma Warnings (Off, Pr);
      Prc : Person.Vectors.Vector
        (Capacity => Owners'Pos (Owners'Last) + 1);
   begin

      Performance.Initialize;

      for P in Owners'First .. Owners'Last loop
         Pr := Person.Create ((Name => P));
      end loop;
      T := Clock;
      Prc := Person.All_Instances;
      D := Clock - T;
      Put_Line
        ("obtain all instances (per instance):"
           & Duration'Image (D / Natural (Person.Vectors.Length (Prc))));

      Performance.Tear_Down;

   end;

   declare
      Pt : Pet.Handle;
   begin

      Performance.Initialize;

      T := Clock;
      for I in Pets loop
         Pt := Pet.Create;
         Pet.Set_Name (Pt, To => I);
      end loop;
      D := Clock - T;
      Put_Line ("average creation time (auto, set 1 attr):"
                  & Duration'Image (D / (Pets'Pos (Pets'Last) + 1)));

      Performance.Tear_Down;

   end;

   declare
      Prs : array (Owners) of Person.Handle;
      Os : array (Owners) of Owner.Handle;
      Pts : array (Pets) of Pet.Handle;
      O : Owner.Handle;
      pragma Warnings (Off, O);
   begin

      Performance.Initialize;

      --  The association is set up with only one object on the "many"
      --  side. This should give a worst-case result, since the whole
      --  extent will have to be searched for just one result (clearly
      --  more results would take longer, but the average time would
      --  be less, depending on the relative costs of scanning the
      --  input/inserting matches in the output).

      for I in Os'Range loop
         Prs (I) := Person.Create ((Name => I));
         Os (I) := Owner.Inheritance.Create_Tree
           (A_Person => CIH (Prs (I)));
      end loop;
      for J in Pts'Range loop
         Pts (J) := Pet.Create;
      end loop;
      T := Clock;
      for I in Owners loop
         A1.Link (Is_Owned_By => Os (I),
                  Owns => Pts (Pets'Val (Owners'Pos (I))));
      end loop;
      D := Clock - T;
      Put_Line ("average link time (simple):"
                  & Duration'Image (D / (Owners'Pos (Owners'Last) + 1)));

      T := Clock;
      for I in Pts'Range loop
         O := A1.Owns (Pts (I));
      end loop;
      D := Clock - T;
      Put_Line ("average navigation time (simple, easy):"
                  & Duration'Image (D / (Pets'Pos (Pets'Last) + 1)));

      declare
         Ptc : Pet.Vectors.Vector;
         pragma Unreferenced (Ptc);
      begin
         T := Clock;
         for I in Os'Range loop
            Ptc := A1.Is_Owned_By (Os (I));
         end loop;
         D := Clock - T;
         Put_Line ("average navigation time (simple, hard):"
                     & Duration'Image (D / (Owners'Pos (Owners'Last) + 1)));
      end;

      T := Clock;
      for I in Owners loop
         A1.Unlink (Is_Owned_By => Os (I),
                    Owns => Pts (Pets'Val (Owners'Pos (I))));
      end loop;
      D := Clock - T;
      Put_Line ("average unlink time (simple):"
                  & Duration'Image (D / (Owners'Pos (Owners'Last) + 1)));

      Performance.Tear_Down;

   end;

   declare
      Prs : array (Owners) of Person.Handle;
      Pts : array (Pets) of Pet.Handle;
      H : A2.Handle;
      pragma Warnings (Off, H);
   begin

      Performance.Initialize;

      --  The association is set up with only one object on the "many"
      --  side. This should give a worst-case result, since the whole
      --  extent will have to be searched for just one result (clearly
      --  more results would take longer, but the average time would
      --  be less, depending on the relative costs of scanning the
      --  input/inserting matches in the output).

      for I in Prs'Range loop
         Prs (I) := Person.Create ((Name => I));
      end loop;
      for J in Pts'Range loop
         Pts (J) := Pet.Create;
      end loop;
      T := Clock;
      for I in Owners loop
            H := A2.Link (Lives_With => Prs (I),
                          Feeds => Pts (Pets'Val (Owners'Pos (I))));
      end loop;
      D := Clock - T;
      Put_Line ("average link time (associative):"
                  & Duration'Image (D / (Owners'Pos (Owners'Last) + 1)));

      declare
         Hc : A2.Vectors.Vector;
         pragma Unreferenced (Hc);
      begin
         T := Clock;
         for I in Pts'Range loop
            Hc := A2.Feeds (Pts (I));
         end loop;
         D := Clock - T;
         Put_Line ("average navigation time (associative, to assoc):"
                     & Duration'Image (D / (Pets'Pos (Pets'Last) + 1)));
      end;

      declare
         Prc : Person.Vectors.Vector
           (Capacity => Owners'Pos (Owners'Last) + 1);
         pragma Unreferenced (Prc);
      begin
         T := Clock;
         for I in Pts'Range loop
            Prc := A2.Feeds (Pts (I));
         end loop;
         D := Clock - T;
         Put_Line ("average navigation time (associative, to other):"
                     & Duration'Image (D / (Pets'Pos (Pets'Last) + 1)));
      end;

      declare
         type Houses is array (Positive range <>) of A2.Handle;
         Hc : constant A2.Vectors.Vector := A2.All_Instances;
         Hs : Houses (1 .. Natural (A2.Vectors.Length (Hc)));
      begin

         for H in Hs'Range loop
            Hs (H) := A2.Vectors.Element (Hc, H);
         end loop;
         T := Clock;
         for H in Hs'Range loop
            A2.Unlink (A2_Handle => Hs (H));
         end loop;
         D := Clock - T;
         Put_Line ("average unlink time (associative):"
                     & Duration'Image (D / Hs'Length));
      end;

      Performance.Tear_Down;

   end;

   declare
      Ps : array (Pets) of Pet.Handle;
      Cs : array (Pets) of Cat.Handle;
   begin

      Performance.Initialize;

      T := Clock;
      for I in Cs'Range loop
         Cs (I) := Cat.Inheritance.Create_Tree (A_Pet => null);
      end loop;
      D := Clock - T;
      Put_Line ("average create_tree time:"
                  & Duration'Image (D / (Pets'Pos (Pets'Last) + 1)));

      T := Clock;
      for I in Cs'Range loop
         Ps (I) := Cat.Inheritance.Find_Pet_Parent (Cs (I));
      end loop;
      D := Clock - T;
      Put_Line ("average find_parent time:"
                  & Duration'Image (D / (Pets'Pos (Pets'Last) + 1)));

      T := Clock;
      for I in Ps'Range loop
         Pet.Speak (Ps (I));
      end loop;
      D := Clock - T;
      Put_Line ("average dispatching call time:"
                  & Duration'Image (D / (Pets'Pos (Pets'Last) + 1)));


      T := Clock;
      for I in Cs'Range loop
         Cat.Eat (Cs (I));
      end loop;
      D := Clock - T;
      Put_Line ("average inherited call time:"
                  & Duration'Image (D / (Pets'Pos (Pets'Last) + 1)));

      Performance.Tear_Down;

   end;

   begin

      T := Clock;

      for I in 1 .. 10 loop
         declare
            L : ColdFrame.Project.Events.Lock (Event_Timing.Dispatcher_A);
            pragma Warnings (Off, L);
         begin
            null;
         end;
      end loop;
      D := Clock - T;
      Put_Line ("average lock claim time:"
                  & Duration'Image (D / 10));

   end;

   begin

      T := Clock;
      ColdFrame.Project.Events.Post (new Event_Timing.Repost,
                                     On => Event_Timing.Dispatcher_A);
      delay 1.0;
      D := Event_Timing.Done_At - T;
      Put_Line ("average event dispatch (same domain):"
                  & Duration'Image (D / Event_Timing.Loops));

   end;

   begin

      T := Clock;
      ColdFrame.Project.Events.Post (new Event_Timing.Ping,
                                     On => Event_Timing.Dispatcher_A);
      delay 1.0;
      D := Event_Timing.Done_At - T;
      Put_Line ("average event dispatch (other domain):"
                  & Duration'Image (D / Event_Timing.Loops));

   end;

   begin

      T := Clock;

      declare
         Ev : constant ColdFrame.Project.Events.Event_P
           := new Event_Timing.Timing;
         P : Event_Timing.Timing renames Event_Timing.Timing (Ev.all);
      begin
         P.Count := 0;
         ColdFrame.Project.Events.Set (Event_Timing.Timer,
                                       On => Event_Timing.Dispatcher_A,
                                       To_Fire => Ev,
                                       After => 0.0);
      end;

      delay 2.0;
      D := Event_Timing.Done_At - T;
      Put_Line ("timer firing (same dispatcher):"
                  & Duration'Image (D / Event_Timing.Loops));

   end;

   ColdFrame.Project.Events.Stop (Event_Timing.Dispatcher_A);
   ColdFrame.Project.Events.Tear_Down (Event_Timing.Dispatcher_A);
   ColdFrame.Project.Events.Stop (Event_Timing.Dispatcher_B);
   ColdFrame.Project.Events.Tear_Down (Event_Timing.Dispatcher_B);

--     declare
--        package LE renames ColdFrame.Logging_Event_Basis;
--        package DC
--          renames ColdFrame.Logging_Event_Basis.Abstract_Datum_Containers;
--        package MS renames ColdFrame.Memory_Streams;
--        package ST renames BC.Support.Statistics;
--        S : aliased MS.Stream_Type (10_000);
--     begin
--        DC.Container'Class'Output (S'Access, LE.Results);
--        Put_Line ("stream size is" & MS.Length (S)'Img);
--        declare
--           Data : constant DC.Container'Class
--             := DC.Container'Class'Input (S'Access);
--           It : DC.Iterator'Class
--             := DC.New_Iterator (Data);
--        begin
--           while not DC.Is_Done (It) loop
--              declare
--                 D : constant LE.Datum := DC.Current_Item (It);
--              begin
--                 Put (To_String (D.Event));
--                 Put (',');
--                 Put (Integer'Image (ST.Count (D.Queueing)));
--                 Put (',');
--                 Put (Duration'Image (Duration (ST.Mean (D.Queueing))));
--                 Put (',');
--                 Put (Duration'Image (Duration (ST.Min (D.Queueing))));
--                 Put (',');
--                 Put (Duration'Image (Duration (ST.Max (D.Queueing))));
--                 Put (',');
--                 Put (Duration'Image (Duration (ST.Sigma (D.Queueing))));
--                 Put (',');
--                 Put (Duration'Image (Duration (ST.Mean (D.Executing))));
--                 Put (',');
--                 Put (Duration'Image (Duration (ST.Min (D.Executing))));
--                 Put (',');
--                 Put (Duration'Image (Duration (ST.Max (D.Executing))));
--                 Put (',');
--                 Put (Duration'Image (Duration (ST.Sigma (D.Executing))));
--                 New_Line;
--              end;
--              DC.Next (It);
--           end loop;
--        end;
--     end;

end Performance.Harness;
