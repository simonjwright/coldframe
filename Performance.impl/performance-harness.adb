with GNAT.IO; use GNAT.IO;
with Performance.Initialize;
with Performance.Tear_Down;
with Performance.Owner;
with Performance.Pet.Collections;
with Performance.Cat.Inheritance;
with Performance.A1;
with Seawolf_High_Resolution_Time; use Seawolf_High_Resolution_Time;

procedure Performance.Harness is
   O : Owner.Handle;
   P : Pet.Handle;
   T : Time;
   D : Duration;
begin

   Performance.Initialize;
   T := Clock;
   for I in Owners loop
      O := Owner.Create ((Name => I));
   end loop;
   D := Clock - T;
   Put_Line ("average creation time (identifier, enum):"
               & Duration'Image (D / (Owners'Pos (Owners'Last) + 1)));

   T := Clock;
   for I in Owners loop
      O := Owner.Find ((Name => I));
   end loop;
   D := Clock - T;
   Put_Line ("average find time (identifier, enum):"
               & Duration'Image (D / (Owners'Pos (Owners'Last) + 1)));

   T := Clock;
   for I in Owners loop
      Owner.Delete ((Name => I));
   end loop;
   D := Clock - T;
   Put_Line ("average deletion time (identifier, enum):"
               & Duration'Image (D / (Owners'Pos (Owners'Last) + 1)));
   Performance.Tear_Down;

   Performance.Initialize;
   declare
      Os : array (Owners) of Owner.Handle;
   begin
      for O in Os'Range loop
         Os (O) := Owner.Create ((Name => O));
      end loop;
      T := Clock;
      for O in Owners loop
         Owner.Delete (Os (O));
      end loop;
      D := Clock - T;
      Put_Line ("average deletion time (handle):"
                  & Duration'Image (D / (Owners'Pos (Owners'Last) + 1)));
   end;
   Performance.Tear_Down;

   Performance.Initialize;
   T := Clock;
   for I in Pets loop
      P := Pet.Create;
      Pet.Set_Name (P, To => I);
   end loop;
   D := Clock - T;
   Put_Line ("average creation time (auto, set 1 attr):"
               & Duration'Image (D / (Pets'Pos (Pets'Last) + 1)));
   Performance.Tear_Down;

   Performance.Initialize;
   declare
      Os : array (Owners) of Owner.Handle;
      Ps : array (Pets) of Pet.Handle;
   begin

      for I in Os'Range loop
         Os (I) := Owner.Create ((Name => I));
      end loop;
      for J in Ps'Range loop
         Ps (J) := Pet.Create;
      end loop;
      T := Clock;
      for I in Owners loop
         A1.Link (Is_Owned_By => Os (I),
                  Owns => Ps (Pets'Val (Owners'Pos (I))));
      end loop;
      D := Clock - T;
      Put_Line ("average link time (simple):"
                  & Duration'Image (D / (Owners'Pos (Owners'Last) + 1)));

      T := Clock;
      for I in Ps'Range loop
         O := A1.Owns (Ps (I));
      end loop;
      D := Clock - T;
      Put_Line ("average navigation time (simple, easy):"
                  & Duration'Image (D / (Pets'Pos (Pets'Last) + 1)));

      declare
         Pc : Pet.Collections.Collection;
      begin
         T := Clock;
         for I in Os'Range loop
            Pc := A1.Is_Owned_By (Os (I));
         end loop;
         D := Clock - T;
         Put_Line ("average navigation time (simple, hard):"
                     & Duration'Image (D / (Owners'Pos (Owners'Last) + 1)));
      end;

      T := Clock;
      for I in Owners loop
         A1.Unlink (Is_Owned_By => Os (I),
                  Owns => Ps (Pets'Val (Owners'Pos (I))));
      end loop;
      D := Clock - T;
      Put_Line ("average unlink time (simple):"
                  & Duration'Image (D / (Owners'Pos (Owners'Last) + 1)));

   end;
   Performance.Tear_Down;

   Performance.Initialize;
   declare
      Ps : array (Pets) of Pet.Handle;
      Cs : array (Pets) of Cat.Handle;
   begin

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


   end;
   Performance.Tear_Down;

end Performance.Harness;
