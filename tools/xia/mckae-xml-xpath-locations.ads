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

with Ada.Strings.Unbounded;
with Mckae.XML.XPath.Predicates;
with Unicode.CES;

package Mckae.XML.XPath.Locations is

   use Ada.Strings.Unbounded;
   use McKae.XML.XPath;

   -- XPath axes
   type Axes is
     (Child_Axis,
      Parent_Axis,
      Self_Axis,
      Preceding_Sibling_Axis,
      Following_Sibling_Axis,
      Ancestor_Axis,
      Ancestor_Or_Self_Axis,
      Namespace_Axis,
      Attribute_Axis,

      Descendant_Or_Self_Axis,
      Descendant_Axis,
      Following_Axis,

      Preceding_Axis);

   -- No explicit axis implies the child axis
   No_Axis : constant Axes := Child_Axis;

   -- Axes whose traversal consists of immediate, direct traversals
   subtype Immediate_Axes is Axes range Child_Axis .. Attribute_Axis;

   -- Axes whose traversal requires the traversal of branchings
   subtype Branched_Axes is Axes range Descendant_Or_Self_Axis .. Preceding_Axis;

   type Axes_Directionality is array (Axes) of Boolean;

   Reverse_Axis : constant Axes_Directionality :=
     (Ancestor_Axis | Ancestor_Or_Self_Axis | Preceding_Axis | Preceding_Sibling_Axis => True,
      others => False);
   Forward_Axis : constant Axes_Directionality := not Reverse_Axis;

   -- Kinds of node tests
   type Node_Tests is
     (No_Node_Test,
      QName_Node_Test,
      NCName_Node_Test,
      Text_Node_Test,
      Comment_Node_Test,
      Processing_Instruction_Node_Test,
      Node_Node_Test);

   ----------------------------------------------------------------

   -- Specification of the node test portion of a location step
   type Node_Test_Specification
     (Node_Test : Node_Tests := No_Node_Test) is
      record
         Name : Unbounded_String;
         case Node_Test is
            when QName_Node_Test =>
               Prefix : Unbounded_String;
            when No_Node_Test   |
              NCName_Node_Test   |
              Text_Node_Test    |
              Comment_Node_Test |
              Processing_Instruction_Node_Test |
              Node_Node_Test =>
               null;
         end case;
      end record;

   -- Location step, the part in between '/'s
   type Location_Steps is record
      Axis                : Axes := No_Axis;
      Node_Test           : Node_Test_Specification;
      Location_Predicates : Predicates.Predicate_Handles;
      Output_Step         : Boolean := False;
   end record;

   -- A sequence of location steps
   type Location_Path_Steps is array (Natural range <>) of Location_Steps;
   type Location_Path_Steps_Handle is access Location_Path_Steps;

   type Location_Paths is
      record
         Absolute : Boolean;
         Steps    : Natural := 0;
         Path     : Location_Path_Steps_Handle := new Location_Path_Steps(1 .. 10);
      end record;

   -- Redefinition of strings to be compatible with the DOM interface
   subtype Xpath_String is Unicode.CES.Byte_Sequence;


   -- Add a newly defined location step to the path
   procedure Add (
                  Location_Step : in     Location_Steps
                  -- Newly defined location step
                 );

   -- Return the location path structure generated by parsing the
   --  Xpath query.  IMPORTANT!  The Location_Path returned by this
   --  function must be subsequently freed using the corresponding
   --  Free procedure.
   function Get_Path return Location_Paths;

   -- Release the resources used by the construction of a location path
   procedure Free(Location_Path : in out Location_Paths);

   -- Reset the location path processing for a new query.  This must
   --  be called before invoking the Xia_Parser_Model.Pathify
   --  procedure.
   procedure Reset_For_Parsing;

end Mckae.XML.XPath.Locations;
