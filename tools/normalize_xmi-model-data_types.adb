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

--  $RCSfile: normalize_xmi-model-data_types.adb,v $
--  $Revision: 7790302b4adb $
--  $Date: 2013/04/22 15:37:22 $
--  $Author: simonjwright $

with DOM.Core.Nodes;
with McKae.XML.XPath.XIA;
with Normalize_XMI.Messages;
with Normalize_XMI.Model.Operations;

package body Normalize_XMI.Model.Data_Types is


   function Read_Data_Type (From   : not null DOM.Core.Node;
                            Parent : not null Element_P) return Element_P
   is
      N : constant Element_P := new Data_Type_Element;
      T : Data_Type_Element renames Data_Type_Element (N.all);
   begin
      T.Parent := Parent;
      T.Name := +Read_Name (From_Element => From);
      Messages.Trace ("... reading data type " & (+T.Name));
      T.Populate (From => From);

      --  Attributes
      declare
         Nodes : constant DOM.Core.Node_List := McKae.XML.XPath.XIA.XPath_Query
           (From, "UML:Classifier.feature/UML:Attribute");
      begin
         if DOM.Core.Nodes.Length (Nodes) > 0 then
            Messages.Error
              ("DataType "
                 & (+T.Name)
                 & " is not permitted to have attributes.");
         end if;
      end;

      --  Operations
      declare
         Nodes : constant DOM.Core.Node_List := McKae.XML.XPath.XIA.XPath_Query
           (From, "UML:Classifier.feature/UML:Operation");
      begin
         for J in 0 .. DOM.Core.Nodes.Length (Nodes) - 1 loop
            declare
               O : constant Element_P :=
                 Operations.Read_Operation (DOM.Core.Nodes.Item (Nodes, J),
                                            Parent => N);
               Name : constant String := +O.Name;
            begin
               if T.Operations.Contains (Name) then
                  Messages.Error
                    ("Type " & (+T.Name) & " has duplicate operation " & Name);
               else
                  T.Operations.Insert (Key => Name, New_Item => O);
               end if;
            end;
         end loop;
      end;

      return N;
   end Read_Data_Type;


   overriding
   procedure Resolve (T : in out Data_Type_Element)
   is
      procedure Resolve (Pos : Element_Maps.Cursor);
      procedure Resolve (Pos : Element_Maps.Cursor)
      is
      begin
         Element_Maps.Element (Pos).Resolve;
      end Resolve;
      use type Ada.Containers.Count_Type;
   begin
      Messages.Trace ("... checking data type " & (+T.Name));
      if T.Has_Stereotype ("access") then
         if not T.Has_Tag ("access-to-type") then
            Messages.Error
              ("Type "
                 & (+T.Name)
                 & " has <<access>> but not {access-to-type}.");
         else
            declare
               Target_Type_Name : constant String
                 := T.Tag_As_Name ("access-to-type");
               Target_Type : constant Element_P
                 := T.Find_Type (Target_Type_Name);
            begin
               if Target_Type = null then
                  Messages.Error
                    ("Type '"
                       & Target_Type_Name
                       & "' designated by <<access>> type "
                       & (+T.Name)
                       & " not found.");
               elsif not (Target_Type.all in Data_Type_Element'Class) then
                  Messages.Error
                    ("Type '"
                       & Target_Type_Name
                       & "' designated by <<access>> type "
                       & (+T.Name)
                       & " is not a data type.");
               else
                  Types.Type_Element (Target_Type.all).Accessor
                    := T'Unchecked_Access;
               end if;
            end;
         end if;
      end if;
      if T.Has_Stereotype ("access-to-operation") then
         if T.Operations.Length /= 1 then
            Messages.Error
              ("Type "
                 & (+T.Name)
                 & " is marked <<access-to-operation>> but has"
                 & T.Operations.Length'Img
                 & " operations.");
         else
            declare
               --  Workround for gcc-4.4 limitation in Debian 6.
               Operation : constant Element_P := T.Operations.First_Element;
               Operation_Name : constant String := +Operation.Name;
            begin
               if Operation_Name /= +T.Name then
                  Messages.Warning
                    ("Operation "
                       & (+T.Name)
                       & "."
                       & Operation_Name
                       & " renamed to "
                       & (+T.Name)
                       & ".");
                  Operation.Name := T.Name;
               end if;
            end;
         end if;
      end if;
      --  There are all sorts of complicated illegal possibilities
      --  here!
      if T.Has_Tag ("imported") and T.Has_Tag ("renames") then
         Messages.Error
           ("Type "
              & (+T.Name)
              & " has both {imported} and {renames} specified.");
      end if;
      if T.Has_Stereotype ("bounded-string")
        and not T.Has_Tag ("length") then
         Messages.Error
           ("Type "
              & (+T.Name)
              & " has <<bounded-string>> but not {length}.");
      end if;
      if T.Has_Stereotype ("constraint") then
         if T.Has_Tag ("constrains") then
            declare
               Constrained_Type_Name : constant String
                 := T.Tag_As_Name ("constrains");
               Constrained_Type : constant Element_P
                 := T.Find_Type (Constrained_Type_Name);
            begin
               if Constrained_Type = null then
                  Messages.Error
                    ("Type '"
                       & Constrained_Type_Name
                       & "', to be constrained by "
                       & (+T.Name)
                       & ", not found.");
               elsif
                 not (Constrained_Type.all in Data_Type_Element'Class)
                 and not (Constrained_Type.all in Standard_Type_Element'Class)
               then
                  Messages.Error
                    ("Type '"
                       & Constrained_Type_Name
                       & "', to be constrained by "
                       & (+T.Name)
                       & ", is not a data type.");
               end if;
            end;
         else
            Messages.Error
              ("Type "
                 & (+T.Name)
                 & " has <<constraint>> but not {constrains}.");
         end if;
         if not (T.Has_Tag ("lower") or T.Has_Tag ("upper")) then
            Messages.Error
              ("Type "
                 & (+T.Name)
                 & " has <<constraint>> but neither {lower} nor {upper}.");
         end if;
      end if;
      if T.Has_Stereotype ("fixed-string")
        and not T.Has_Tag ("length") then
         Messages.Error
           ("Type "
              & (+T.Name)
              & " has <<fixed-string>> but not {length}.");
      end if;
      if T.Has_Stereotype ("imported") and not T.Has_Tag ("imported") then
         Messages.Error
           ("Type "
              & (+T.Name)
              & " has <<imported>> but not {imported}.");
      end if;
      if T.Has_Stereotype ("renaming") and not T.Has_Tag ("renames") then
         Messages.Error
           ("Type "
              & (+T.Name)
              & " has <<renaming>> but not {renames}.");
      end if;
      T.Operations.Iterate (Resolve'Access);
   end Resolve;


   overriding
   procedure Output (T : Data_Type_Element; To : Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;
      procedure Output (Pos : Element_Maps.Cursor);
      procedure Output (Pos : Element_Maps.Cursor)
      is
      begin
         Element_Maps.Element (Pos).Output (To);
      end Output;
   begin
      if T.Has_Stereotype ("access") then
         --  Suppress the output of the <<access>> datatype; code
         --  generation outputs the access type immediately after the
         --  target type when it finds "access='access-type-name'".
         return;
      end if;
      Put (To, "<type");
      if T.Accessor /= null then
         Put (To, " access='" & (+T.Accessor.Name) & "'");
      end if;
      if T.Has_Stereotype ("access-to-operation") then
         Put (To, " access-to-operation='true'");
      end if;
      if T.Has_Stereotype ("callback") then
         Put (To, " callback='true'");
      end if;
      if T.Has_Stereotype ("null") then
         Put (To, " null='true'");
      end if;
      declare
         Visibility : constant String
           := Read_Attribute ("visibility", From_Element => T.Node);
      begin
         if Visibility = "" or Visibility = "package" then
            Put (To, " visibility='private'");
         else
            Put (To, " visibility='" & Visibility & "'");
         end if;
      end;
      Put_Line (To, ">");
      Put_Line (To, "<name>" & (+T.Name) & "</name>");
      T.Output_Documentation (To);
      if T.Has_Stereotype ("counterpart") then
         Put_Line (To, "<counterpart/>");
      end if;
      if T.Has_Stereotype ("bounded-string") then
         Put_Line (To,
                   "<string><max>"
                     & T.Tag_As_Value ("length")
                     & "</max></string>");
      end if;
      if T.Has_Stereotype ("constraint") then
         Put (To,
              "<subtype constrains="""
                & T.Tag_As_Value ("constrains")
                & """>");
         if T.Has_Tag ("lower") then
            Put (To, "<lower>" & T.Tag_As_Value ("lower") & "</lower>");
         end if;
         if T.Has_Tag ("upper") then
            Put (To, "<upper>" & T.Tag_As_Value ("upper") & "</upper>");
         end if;
         Put_Line (To, "</subtype>");
      end if;
      if T.Has_Stereotype ("fixed-string") then
         Put_Line (To,
                   "<string><fixed>"
                     & T.Tag_As_Value ("length")
                     & "</fixed></string>");
      end if;
      if T.Has_Tag ("imported") then
         Put_Line (To,
                   "<imported>" & T.Tag_As_Name ("imported") & "</imported>");
      end if;
      if T.Has_Tag ("renames") then
         Put_Line (To,
                   "<renames>" & T.Tag_As_Name ("renames") & "</renames>");
      end if;
      T.Operations.Iterate (Output'Access);
      Put_Line (To, "</type>");
   end Output;


end Normalize_XMI.Model.Data_Types;
