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

--  $RCSfile: normalize_xmi-main.adb,v $
--  $Revision: fd881b1b7e39 $
--  $Date: 2011/12/11 15:20:54 $
--  $Author: simonjwright $

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
      case GNAT.Command_Line.Getopt ("c: h") is
         when ASCII.NUL => exit;
         when 'c' =>
            Identifiers.Read_Case_Exceptions
              (From => GNAT.Command_Line.Parameter);
         when 'h' =>
            Usage;
            return;
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
            return;
         end if;

         Put_Line (Standard_Error, "... processing " & Arg);

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
         Domains := McKae.XML.XPath.XIA.XPath_Query
           (Doc,
            "//UML:Package[UML:ModelElement.stereotype/@name='domain']");

         Put_Line
           (Standard_Error,
            "... number of domains:"
              & Natural'Image (DOM.Core.Nodes.Length (Domains)));

         for J in 0 .. DOM.Core.Nodes.Length (Domains) - 1 loop
            Model.Process_Domain (DOM.Core.Nodes.Item (Domains, J),
                                  In_File => Arg);
         end loop;

      end;

      Arguments_Processed := Arguments_Processed + 1;

   end loop;

exception
   when E : others =>
      Put_Line ("Exception " & Ada.Exceptions.Exception_Information (E));
end Normalize_XMI.Main;
