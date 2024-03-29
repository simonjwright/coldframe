--  Copyright (C) Simon Wright <simon@pushface.org>

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

with Ada.Strings.Fixed;
with DOM.Core.Nodes;
with Normalize_XMI.Identifiers;
with Normalize_XMI.Messages;
with XIA;

package body Normalize_XMI.Model.State_Machines is

   --  The Effect for a Transition or a State entry is a
   --  semicolon-separated list of operation names. Split_Effect
   --  splits at the semicolons and normalizes the resulting
   --  identifiers.
   --
   --  For_Element is required to make useful error messages in case
   --  of invalid identifiers.
   function Split_Effect (For_Element : Element'Class;
                          Effect : String) return String_Vectors.Vector;

   --  Output the elements of Effect to To as <action/> elements.
   procedure Put_Actions (To     : Ada.Text_IO.File_Type;
                          Effect : String_Vectors.Vector);

   ------------------------------------------------------------------------

   type Transition_Element is new Element with record
      Trigger : Element_P;               --  An Event, or null for completion
      Effect  : String_Vectors.Vector;   --  Operation names
      Source  : Element_P;               --  A State
      Target  : Element_P;               --  A State
   end record;
   overriding
   procedure Resolve (T : in out Transition_Element);
   overriding
   procedure Output (T : Transition_Element; To : Ada.Text_IO.File_Type);

   function Read_Transition (From   : not null DOM.Core.Node;
                             Parent : not null Element_P) return Element_P;

   ------------------------------------------------------------------------

   type State_Kind is (Initial, Normal, Final);
   type State_Element is new Element with record
      Kind : State_Kind := Normal;
      Entry_Effect  : String_Vectors.Vector;   --  Operation names
   end record;
   overriding
   procedure Resolve (S : in out State_Element);
   overriding
   procedure Output (S : State_Element; To : Ada.Text_IO.File_Type);

   function Read_State (From   : not null DOM.Core.Node;
                        Parent : not null Element_P) return Element_P;

   ------------------------------------------------------------------------

   type Event_Element is new Element with record
      --  We aren't interested in the parameter name (it will be
      --  generated as "Payload").
      Class : Boolean;
      Parameter_Type : Ada.Strings.Unbounded.Unbounded_String;
      Effect  : String_Vectors.Vector;   --  Op'n names (only for <<class>>)
   end record;
   overriding
   procedure Resolve (E : in out Event_Element);
   overriding
   procedure Output (E : Event_Element; To : Ada.Text_IO.File_Type);

   function Read_Event (From   : not null DOM.Core.Node;
                        Parent : not null Element_P) return Element_P;

   ------------------------------------------------------------------------

   overriding
   procedure Resolve (T : in out Transition_Element)
   is
      pragma Unreferenced (T);
   begin
      Messages.Trace ("......... checking transition");
   end Resolve;

   overriding
   procedure Output (T : Transition_Element; To : Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;
   begin
      Put (To, "<transition");
      if T.Has_Stereotype ("ignore") then
         Put (To, " ignore='true'");
      end if;
      --  self?
      Put_Line (To, ">");
      if T.Trigger /= null then
         Put_Line (To, "<event>" & (+T.Trigger.Name) & "</event>");
      end if;
      Put_Actions (To, T.Effect);
      Put_Line (To, "<source>" & (+T.Source.Name) & "</source>");
      Put_Line (To, "<target>" & (+T.Target.Name) & "</target>");
      T.Output_Documentation (To);
      Put_Line (To, "</transition>");
   end Output;

   function Read_Transition (From   : not null DOM.Core.Node;
                             Parent : not null Element_P) return Element_P
   is
      --  Convenience function to read Source or Target state.
      function Read_State (With_Xmi_Id : String) return Element_P;

      S : State_Machine_Element renames State_Machine_Element (Parent.all);

      function Read_State (With_Xmi_Id : String) return Element_P
      is
      begin
         if S.States.Contains (With_Xmi_Id) then
            return S.States.Element (With_Xmi_Id);
         else
            declare
               Nodes : constant DOM.Core.Node_List :=
                 XIA.XPath_Query
                 (S.Node,
                  "UML:StateMachine.top/descendant::*[@xmi.id='"
                    & With_Xmi_Id
                    & "']");
               St : constant Element_P :=
                 Read_State (DOM.Core.Nodes.Item (Nodes, 0),
                             Parent => Parent);
            begin
               S.States.Insert (Key => With_Xmi_Id,
                                New_Item => St);
               return St;
            end;
         end if;
      end Read_State;

      N : constant Element_P := new Transition_Element (Parent);
      T : Transition_Element renames Transition_Element (N.all);
   begin
      T.Populate (From => From, Needs_Name => False);

      --  Trigger
      declare
         Nodes : constant DOM.Core.Node_List := XIA.XPath_Query
           (From, "UML:Transition.trigger/*");
      begin
         if DOM.Core.Nodes.Length (Nodes) > 0 then
            declare
               Xmi_Idref : constant String :=
                 Read_Attribute
                 ("xmi.idref",
                  From_Element => DOM.Core.Nodes.Item (Nodes, 0));
            begin
               if not S.Events.Contains (Xmi_Idref) then
                  declare
                     Nodes : constant DOM.Core.Node_List :=
                       XIA.XPath_Query
                       (S.Node,
                        "../*[@xmi.id='"
                          & Xmi_Idref
                          & "']");
                     E : constant Element_P :=
                       Read_Event (DOM.Core.Nodes.Item (Nodes, 0),
                                   Parent => Parent);
                  begin
                     S.Events.Insert (Key => Xmi_Idref,
                                      New_Item => E);
                     T.Trigger := E;
                  end;
               else
                  T.Trigger := S.Events.Element (Xmi_Idref);
               end if;
            end;
         end if;
      end;

      --  Source
      declare
         Nodes : constant DOM.Core.Node_List := XIA.XPath_Query
           (From, "UML:Transition.source/*");
         Xmi_Idref : constant String :=
           Read_Attribute
           ("xmi.idref",
            From_Element => DOM.Core.Nodes.Item (Nodes, 0));
      begin
         T.Source := Read_State (With_Xmi_Id => Xmi_Idref);
      end;

      --  Target
      declare
         Nodes : constant DOM.Core.Node_List := XIA.XPath_Query
           (From, "UML:Transition.target/*");
         Xmi_Idref : constant String :=
           Read_Attribute
           ("xmi.idref",
            From_Element => DOM.Core.Nodes.Item (Nodes, 0));
      begin
         T.Target := Read_State (With_Xmi_Id => Xmi_Idref);
      end;

      --  Actions (XXX what will the translation do if there are > 1?)
      declare
         Nodes : constant DOM.Core.Node_List := XIA.XPath_Query
           (From, "descendant::UML:ActionExpression");
      begin
         if DOM.Core.Nodes.Length (Nodes) > 0 then
            T.Effect := Split_Effect
              (Element'Class (T),
               Read_Attribute
                 ("body",
                  From_Element => DOM.Core.Nodes.Item (Nodes, 0)));
         end if;
      end;

      return N;
   end Read_Transition;

   ------------------------------------------------------------------------

   overriding
   procedure Resolve (S : in out State_Element)
   is
   begin
      Messages.Trace ("......... checking state " & (+S.Name));
      if S.Has_Stereotype ("final") then
         case S.Kind is
            when Initial =>
               Messages.Error
                 ("Initial state "
                    & S.Fully_Qualified_Name
                    & " is marked <<final>>");
            when Normal =>
               S.Kind := Final;
            when Final =>
               null;
         end case;
      end if;
   end Resolve;

   overriding
   procedure Output (S : State_Element; To : Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;
   begin
      Put (To, "<state");
      case S.Kind is
         when Initial =>
            Put (To, " initial='true'");
         when Normal =>
            null;
         when Final =>
            Put (To, " final='true'");
      end case;
      Put_Line (To, ">");
      Put_Line (To, "<name>" & (+S.Name) & "</name>");
      Put_Actions (To, S.Entry_Effect);
      S.Output_Documentation (To);
      Put_Line (To, "</state>");
   end Output;

   function Read_State (From   : not null DOM.Core.Node;
                        Parent : not null Element_P) return Element_P
   is
      N : constant Element_P := new State_Element (Parent);
      S : State_Element renames State_Element (N.all);
      Node_Name : constant String := DOM.Core.Nodes.Node_Name (From);
   begin
      S.Populate (From => From, Needs_Name => False);

      if Node_Name = "UML:Pseudostate" then
         S.Kind := Initial;
         if +S.Name = "" then
            S.Name := +"Initial";
         end if;
      elsif Node_Name = "UML:FinalState" then
         S.Kind := Final;
         if +S.Name = "" then
            S.Name := +"Final";
         end if;
      elsif Node_Name /= "UML:SimpleState" then
         Messages.Error
           ("Don't recognise "
              & S.Fully_Qualified_Name
              & "'s kind: "
              & Node_Name);
      elsif +S.Name = "" then
         S.Name := +"{unnamed}";
         Messages.Error
           ("Unnamed state " & S.Fully_Qualified_Name);
      end if;

      --  Entry actions
      declare
         Nodes : constant DOM.Core.Node_List := XIA.XPath_Query
           (From, "descendant::UML:ActionExpression");
      begin
         if DOM.Core.Nodes.Length (Nodes) > 0 then
            S.Entry_Effect := Split_Effect
              (Element'Class (S),
               Read_Attribute
                 ("body",
                  From_Element => DOM.Core.Nodes.Item (Nodes, 0)));
         end if;
      end;

      return N;
   end Read_State;

   ------------------------------------------------------------------------

   overriding
   procedure Resolve (E : in out Event_Element)
   is
   begin
      Messages.Trace ("......... checking event " & (+E.Name));

      --  If this is a class event, copy the Effect from the
      --  Transition which it triggers.
      if E.Has_Stereotype ("class") then
         declare
            procedure Copy_Effect (Pos : Element_Vectors.Cursor);
            procedure Copy_Effect (Pos : Element_Vectors.Cursor)
            is
               T : Transition_Element
                 renames Transition_Element
                 (Element_Vectors.Element (Pos).all);
            begin
               if T.Trigger = E'Unchecked_Access then
                  E.Effect := T.Effect;  -- XXX check for multiples?
               end if;
            end Copy_Effect;
            S : State_Machine_Element
              renames State_Machine_Element (E.Parent.all);
         begin
            --  XXX check for <<class>> state machine? (but SMs don't
            --  look for event actions).
            S.Transitions.Iterate (Copy_Effect'Access);
         end;
      end if;
   end Resolve;

   overriding
   procedure Output (E : Event_Element; To : Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;
   begin
      Put (To, "<event");
      if E.Has_Stereotype ("class") then
         Put (To, " class='true'");
      end if;
      Put_Line (To, ">");
      Put_Line (To, "<name>" & (+E.Name) & "</name>");
      if +E.Parameter_Type /= "" then
         Put_Line (To, "<type>" & (+E.Parameter_Type) & "</type>");
      end if;
      Put_Actions (To, E.Effect);
      E.Output_Documentation (To);
      Put_Line (To, "</event>");
   end Output;

   function Read_Event (From   : not null DOM.Core.Node;
                        Parent : not null Element_P) return Element_P
   is
      N : constant Element_P := new Event_Element (Parent);
      E : Event_Element renames Event_Element (N.all);
   begin
      E.Populate (From => From);

      --  Override the standard Name handling.
      --
      --  We don't use any qualifiers present in the element's name,
      --  because they're only there to disambiguate events with the
      --  same name but directed to different classes.
      declare
         Name : constant String :=
           Read_Attribute ("name", From_Element => From);
         Dot : constant Natural
           := Ada.Strings.Fixed.Index (Source => Name,
                                       Pattern => ".",
                                       From => Name'Last,
                                       Going => Ada.Strings.Backward);
         Candidate_Name : constant String
           := Name (Positive'Max (Dot + 1, Name'First) .. Name'Last);
      begin
         if Identifiers.Is_Valid (Candidate_Name) then
            E.Name := +Identifiers.Normalize
              (Name (Positive'Max (Dot + 1, Name'First) .. Name'Last));
         else
            null;
            --  The error will already have been reported via
            --  Populate() above.
         end if;
      end;

      --  Parameter Type. We aren't interested in the parameter name,
      --  because ColdFrame only supports a single Event parameter,
      --  and it's translated as a type extension with component name
      --  "Payload".
      declare
         Nodes : constant DOM.Core.Node_List := XIA.XPath_Query
           (From, "UML:Event.parameter/UML:Parameter/UML:Parameter.type/*");
      begin
         if DOM.Core.Nodes.Length (Nodes) > 1 then
            Messages.Error
              ("Event "
                 & E.Fully_Qualified_Name
                 & " is not permitted to have more than one parameter");
         elsif DOM.Core.Nodes.Length (Nodes) = 1 then
            declare
               Name : constant String :=
                 Read_Attribute ("name", DOM.Core.Nodes.Item (Nodes, 0));
            begin
               if Identifiers.Is_Valid (Name) then
                  E.Parameter_Type := +Identifiers.Normalize (Name);
               else
                  Messages.Error
                    ("Event "
                       & E.Fully_Qualified_Name
                       & " has invalid parameter type """
                       & Name
                       & """");
               end if;
            end;
         end if;
      end;

      return N;
   end Read_Event;

   ------------------------------------------------------------------------

   overriding
   procedure Resolve (S : in out State_Machine_Element)
   is
      procedure Resolve_M (Pos : Element_Maps.Cursor);
      procedure Resolve_V (Pos : Element_Vectors.Cursor);
      procedure Resolve_M (Pos : Element_Maps.Cursor)
      is
      begin
         Element_Maps.Element (Pos).Resolve;
      end Resolve_M;
      procedure Resolve_V (Pos : Element_Vectors.Cursor)
      is
      begin
         Element_Vectors.Element (Pos).Resolve;
      end Resolve_V;

      --  Checking that there's only one of each State
      package State_Counters is new Ada.Containers.Indefinite_Ordered_Maps
        (Element_Type => Positive,
         Key_Type => String);
      procedure Add_State (Pos : Element_Maps.Cursor);
      State_Counter : State_Counters.Map;
      procedure Add_State (Pos : Element_Maps.Cursor)
      is
         Inserted_At : State_Counters.Cursor;
         Inserted : Boolean;
      begin
         State_Counter.Insert
           (Key => +Element_Maps.Element (Pos).Name,
            New_Item => 1,
            Position => Inserted_At,
            Inserted => Inserted);
         if not Inserted then
            State_Counter.Replace_Element
              (Position => Inserted_At,
               New_Item => State_Counters.Element (Inserted_At) + 1);
         end if;
      end Add_State;
      procedure Check_State (Pos : State_Counters.Cursor);
      procedure Check_State (Pos : State_Counters.Cursor) is
      begin
         if State_Counters.Element (Pos) > 1 then
            Messages.Error (S.Parent.Fully_Qualified_Name
                              & " has"
                              & State_Counters.Element (Pos)'Img
                              & " events named "
                              & State_Counters.Key (Pos));
         end if;
      end Check_State;
   begin
      Messages.Trace ("...... checking state_machine " & (+S.Name));
      S.States.Iterate (Resolve_M'Access);
      S.Events.Iterate (Resolve_M'Access);
      S.Transitions.Iterate (Resolve_V'Access);

      S.States.Iterate (Add_State'Access);
      State_Counter.Iterate (Check_State'Access);
   end Resolve;

   overriding
   procedure Output (S : State_Machine_Element; To : Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;
      procedure Output_M (Pos : Element_Maps.Cursor);
      procedure Output_V (Pos : Element_Vectors.Cursor);
      procedure Output_M (Pos : Element_Maps.Cursor)
      is
      begin
         Element_Maps.Element (Pos).Output (To);
      end Output_M;
      procedure Output_V (Pos : Element_Vectors.Cursor)
      is
      begin
         Element_Vectors.Element (Pos).Output (To);
      end Output_V;
   begin
      --  Output the Events
      S.Events.Iterate (Output_M'Access);
      if not S.Has_Stereotype ("class") then
         Put_Line (To, "<statemachine>");
         S.Output_Documentation (To);
         S.States.Iterate (Output_M'Access);

         --  Not clear why the schema has <(sm-)event/> here? Anyway,
         --  the irrelevant child elements output here won't harm, all
         --  we need is event/name.
         S.Events.Iterate (Output_M'Access);

         S.Transitions.Iterate (Output_V'Access);
         Put_Line (To, "</statemachine>");
      end if;
   end Output;

   function Read_State_Machine (From   : not null DOM.Core.Node;
                                Parent : not null Element_P) return Element_P
   is
      N : constant Element_P := new State_Machine_Element (Parent);
      S : State_Machine_Element renames State_Machine_Element (N.all);
   begin
      S.Populate (From => From);

      --  We only want Events for Transitions between States in this
      --  StateMachine, so we start at the top and work down.
      --
      --  XXX no check for unused Events, States.
      declare
         Nodes : constant DOM.Core.Node_List := XIA.XPath_Query
           (From,
            "UML:StateMachine.transitions/UML:Transition");
      begin
         for J in 0 .. DOM.Core.Nodes.Length (Nodes) - 1 loop
            declare
               T : constant Element_P
                 := Read_Transition (DOM.Core.Nodes.Item (Nodes, J),
                                     Parent => N);
            begin
               S.Transitions.Append (T);
            end;
         end loop;
      end;

      return N;
   end Read_State_Machine;

   ------------------------------------------------------------------------

   function Split_Effect (For_Element : Element'Class;
                          Effect : String) return String_Vectors.Vector
   is
      Spans : constant Identifiers.Spans
        := Identifiers.Find_Spans (Effect, Splitting_At => ';');
   begin
      return Result : String_Vectors.Vector do
         for J in Spans'Range loop
            if Spans (J).U >= Spans (J).L then
               declare
                  Action : constant String :=
                    Ada.Strings.Fixed.Trim
                      (Effect (Spans (J).L .. Spans (J).U),
                       Ada.Strings.Both);
               begin
                  if Identifiers.Is_Valid (Action) then
                     Result.Append (Identifiers.Normalize (Action));
                  else
                     Messages.Error
                       ("Invalid action """
                          & Action
                          & """ in "
                          & For_Element.Fully_Qualified_Name);
                  end if;
               end;
            end if;
         end loop;
      end return;
   end Split_Effect;

   procedure Put_Actions (To     : Ada.Text_IO.File_Type;
                          Effect : String_Vectors.Vector)
   is
      procedure Put (Pos : String_Vectors.Cursor);
      procedure Put (Pos : String_Vectors.Cursor)
      is
      begin
         Ada.Text_IO.Put_Line
           (To, "<action>" & String_Vectors.Element (Pos) & "</action>");
      end Put;
   begin
      Effect.Iterate (Put'Access);
   end Put_Actions;

end Normalize_XMI.Model.State_Machines;
