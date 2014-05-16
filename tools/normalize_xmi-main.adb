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

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with DOM.Core.Nodes;
with DOM.Readers;
with GNAT.Command_Line;
with Input_Sources.File;
with McKae.XML.XPath.XIA;
with Normalize_XMI.Identifiers;
with Normalize_XMI.Messages;
with Normalize_XMI.Model;

procedure Normalize_XMI.Main is

   procedure Usage;

   Arguments_Processed : Natural := 0;

   procedure Usage
   is
   begin
      Put_Line (Standard_Error, "No help available yet.");
   end Usage;

begin

   loop
      case GNAT.Command_Line.Getopt ("c: h v") is
         when ASCII.NUL => exit;
         when 'c' =>
            Identifiers.Read_Case_Exceptions
              (From => GNAT.Command_Line.Parameter);
         when 'h' =>
            Usage;
            return;
         when 'v' =>
            Messages.Set_Verbosity (To => True);
         when others =>
            Usage;
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
            return;
      end case;
   end loop;

   loop
      declare

         Arg : constant String := GNAT.Command_Line.Get_Argument;

         XML_Source_Reader : DOM.Readers.Tree_Reader;

         Doc : DOM.Core.Document;
         Domains : DOM.Core.Node_List;

         File_Source : Input_Sources.File.File_Input;

      begin

         if Arg'Length = 0 then
            if Arguments_Processed = 0 then
               Usage;
               Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
            end if;
            exit;
         end if;

         Messages.Information ("Processing " & Arg);

         begin
            Input_Sources.File.Open (Arg, File_Source);
         exception
            when Ada.IO_Exceptions.Name_Error =>
               Put_Line (Standard_Error, "Unable to open """ & Arg & """");
               Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
               return;
         end;
         begin
            DOM.Readers.Parse (XML_Source_Reader, File_Source);
         exception
            when E : others =>
               Put_Line (Standard_Error,
                         Ada.Exceptions.Exception_Message (E));
               Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
               return;
         end;
         Input_Sources.File.Close (File_Source);

         Doc := DOM.Readers.Get_Tree (XML_Source_Reader);

         declare
            Ignored_Packages : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
              (Doc,
               "//UML:Package[UML:ModelElement.stereotype/UML:Stereotype/"
                 & "@name='ignore']");
         begin
            --  We're processing in document order, so each node
            --  precedes its children. Processing in reverse order
            --  means that we hit the children first, so no risk of
            --  pruning already-pruned nodes.
            for J in reverse
              0 .. DOM.Core.Nodes.Length (Ignored_Packages) - 1 loop
               declare
                  Node_To_Prune : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item (Ignored_Packages, J);
                  Name : constant DOM.Core.Node_List
                    := McKae.XML.XPath.XIA.XPath_Query
                    (Node_To_Prune, "@name");
                  Parent : constant DOM.Core.Node
                    := DOM.Core.Nodes.Parent_Node (Node_To_Prune);
                  Pruned_Node : DOM.Core.Node;
                  use type DOM.Core.Node;
               begin
                  if DOM.Core.Nodes.Length (Name) > 0 then
                     Messages.Trace
                       ("discarding <<ignore>>d package "
                          & DOM.Core.Nodes.Node_Value
                          (DOM.Core.Nodes.Item (Name, 0)));
                  else
                     Messages.Trace ("discarding unnamed <<ignore>>d package");
                  end if;
                  Pruned_Node :=
                    DOM.Core.Nodes.Remove_Child (Parent,
                                                 Old_Child => Node_To_Prune);
                  pragma Assert (Pruned_Node = Node_To_Prune);
                  DOM.Core.Nodes.Free (Pruned_Node);
               end;
            end loop;
         end;

         Domains := McKae.XML.XPath.XIA.XPath_Query
           (Doc,
            "//UML:Package[UML:ModelElement.stereotype/UML:Stereotype/"
              & "@name='domain']");

         Messages.Trace
           ("number of domains:"
              & Natural'Image (DOM.Core.Nodes.Length (Domains)));

         for J in 0 .. DOM.Core.Nodes.Length (Domains) - 1 loop
            Model.Process_Domain (DOM.Core.Nodes.Item (Domains, J),
                                  In_File => Arg);
         end loop;

         Domains := McKae.XML.XPath.XIA.XPath_Query
           (Doc,
            "//UML:Package[UML:ModelElement.stereotype/UML:Stereotype/"
              & "@name='domain-interface']");

         Messages.Trace
           ("number of domain interfaces:"
              & Natural'Image (DOM.Core.Nodes.Length (Domains)));

         for J in 0 .. DOM.Core.Nodes.Length (Domains) - 1 loop
            Model.Process_Domain (DOM.Core.Nodes.Item (Domains, J),
                                  In_File => Arg);
         end loop;

      end;

      Arguments_Processed := Arguments_Processed + 1;

   end loop;

   case Messages.Number_Of_Warnings is
      when 0 => null;
      when 1 =>
         Put_Line (Standard_Error, "One warning.");
      when others =>
         Put_Line (Standard_Error,
                   Natural'Image (Messages.Number_Of_Warnings) & " warnings.");
   end case;

   case Messages.Number_Of_Errors is
      when 0 => null;
      when 1 =>
         Put_Line (Standard_Error, "One error.");
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      when others =>
         Put_Line (Standard_Error,
                   Natural'Image (Messages.Number_Of_Errors) & " errors.");
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end case;

exception
   when E : others =>
      Put_Line (Ada.Exceptions.Exception_Information (E));
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end Normalize_XMI.Main;
