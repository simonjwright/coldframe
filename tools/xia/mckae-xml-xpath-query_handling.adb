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

with Mckae.XML.XPath.Locations;

with Xia_Parser_Model;
with Xia_Parser_Parser;

package body Mckae.XML.XPath.Query_Handling is


   -- Decode the string representation of the location path into its
   --  components parts
   function Pathify(
                    Xpath_Query : Locations.Xpath_String
                    -- The XPath query to decode
                   ) return Locations.Location_Paths is

      Parse_Tree : Xia_Parser_model.Parseable_ptr;

      Path : Locations.Location_Paths;

   begin
      xia_parser_parser.run(Xpath_Query);
      parse_tree := xia_parser_parser.get_parse_tree;
      Locations.Reset_For_Parsing;
      Xia_Parser_Model.Pathify(parse_tree.all);

      Path := Locations.Get_Path;

      Path.Absolute := Xpath_Query(1) = '/';
      Path.Path(Path.Steps).Output_Step := True;

      return Path;

   exception
      when Xia_Parser_Parser.Syntax_Error =>
        raise Malformed_Query;
   end Pathify;

   -- Release the resources used by the construction of a location path
   procedure Free(Location_Path : in out Locations.Location_Paths) is
   begin
      Locations.Free(Location_Path);
   end Free;

end Mckae.XML.XPath.Query_Handling;
