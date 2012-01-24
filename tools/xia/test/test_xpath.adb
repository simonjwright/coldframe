------------------------------------------------------------------------
--                                                                    --
--                     McKae Software Utilities                       --
--                                                                    --
--           Copyright (C) 2004 McKae Technologies                    --
--                                                                    --
-- The  McKae   software  utilities   are  free  software;   you  can --
-- redistribute it  and/or modify it  under terms of the  GNU General --
-- Public  License  as published  by  the  Free Software  Foundation; --
-- either version  2, or (at  your option) any later  version.  McKae --
-- Software Utilities are  distributed in the hope that  they will be --
-- useful,  but  WITHOUT  ANY  WARRANTY;  without  even  the  implied --
-- warranty of  MERCHANTABILITY or FITNESS FOR  A PARTICULAR PURPOSE. --
-- See the GNU  General Public License for more  details.  You should --
-- have received a copy of the GNU General Public License distributed --
-- with DTraq; see file COPYING.   If not, write to the Free Software --
-- Foundation, 59  Temple Place -  Suite 330, Boston,  MA 02111-1307, --
-- USA.                                                               --
--                                                                    --
-- As a  special exception, if other files  instantiate generics from --
-- this unit,  or you link this  unit with other files  to produce an --
-- executable,  this unit  does  not by  itself  cause the  resulting --
-- executable to be covered by  the GNU General Public License.  This --
-- exception does  not however invalidate  any other reasons  why the --
-- executable file might be covered by the GNU Public License.        --
--                                                                    --
-- The McKae Software Utilities  are maintained by McKae Technologies --
-- (http://www.mckae.com).                                            --
------------------------------------------------------------------------

with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Dom.Core.Nodes;
with Mckae.XML.XPath.XIA;
with Dom.Readers;
with Unicode.CES;

--  with Dtraq.Client_Transport;
--  use  Dtraq.Client_Transport;

with Mckae.Xml.Xpath.Locations;

with Input_Sources.File;

with Text_IO; use Text_IO;

procedure Test_Xpath is

   use Dom;
   use Dom.Core;
   use Input_Sources;
   use Mckae.Xml.XPath.Locations;
   use Mckae.Xml.XPath.XIA;
   use Ada.Strings.Fixed;
   use Ada.Strings.Maps;


   XML_Source_Reader : Dom.Readers.Tree_Reader;
   Queried_Nodes : Node_List;
   N             : Node;
   Child         : Node;
   Children      : Node_List;

   Children_Printed : Boolean := False;

   File_Source : File.File_Input;

   S : String(1..255);
   L : Natural;

   procedure Print_Text_Node (T      : Text;
                              Indent : Boolean := False) is
      White_Space     : constant Xpath_String := ' ' & Ascii.LF & Ascii.CR & ASCII.HT;
      White_Space_Set : constant Character_Set := To_Set(White_Space);

      S : Unicode.CES.Byte_Sequence := Trim(Nodes.Node_Value(T),
                                            White_Space_Set,
                                            White_Space_Set);

   begin
      if S'Length > 0 then
         if Indent then
            Put("  ");
         end if;
         Put_Line(S);
      end if;
   end Print_Text_Node;

begin
   Put("Enter XML file name: ");
   Get_Line(S, L);

   File.Open(S(1..L), File_Source);

   Readers.Parse(XML_Source_Reader, File_Source);
   File.Close(File_Source);

-- Connect_To_Server;

   loop
      Put("Enter XPath query: ");
      Get_Line(S, L);
      if S(1) /= '#' then -- Skip comment
         exit when L = 0;
         New_Line(2);
         Put_Line("Evaluating: " & S(1..L));
         New_Line;
         begin
            Queried_Nodes := Xpath_Query(Readers.Get_Tree(XML_Source_Reader), S(1..L));
            Put_Line("Number of nodes:" & Natural'Image(Dom.Core.Nodes.Length(Queried_Nodes)));

            for I in 0 .. Nodes.Length(Queried_Nodes) - 1 loop
               N := Dom.Core.Nodes.Item(Queried_Nodes, I);

               if N.Node_Type = Element_Node then
                  Put("<");
                  Put(Nodes.Node_Name(N));
                  Put(">");

                  Children := Nodes.Child_Nodes(N);
                  Children_Printed := False;
                  for J in 0 .. Nodes.Length(Children) - 1 loop
                     Child := Nodes.Item(Children, J);
                     if Child.Node_Type = Element_Node then
                        if not Children_Printed then
                           New_Line;
                           Children_Printed := True;
                        end if;
                        Put("  <");
                        Put(Nodes.Node_Name(Child));
                        Put_Line(">");
                     elsif Child.Node_Type = Text_Node then
                        if not Children_Printed then
                           New_Line;
                           Children_Printed := True;
                        end if;
                        Print_Text_Node(Child, Indent => True);
                     end if;
                  end loop;

                  Put("</");
                  Put(Nodes.Node_Name(N));
                  Put_Line(">");

               elsif N.Node_Type = Attribute_Node then
                  Put(Nodes.Node_Name(N) & "=""");
                  Put(Nodes.Node_Value (N));
                  Put_Line("""");

               elsif N.Node_Type = Text_Node then
                  Print_Text_Node(N);
               else
                  Put(Nodes.Node_Value(N));
               end if;
            end loop;

         exception
            when Malformed_XPath =>
               Put_Line("Malformed query");
         end;
      end if;
   end loop;

-- Disconnect_From_Server;

end Test_Xpath;
