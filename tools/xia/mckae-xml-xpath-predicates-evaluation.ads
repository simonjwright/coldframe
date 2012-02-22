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

with Mckae.XML.XPath.Node_Sets;
with Mckae.XML.XPath.Locations;

package McKae.XML.XPath.Predicates.Evaluation is

   ----------------------------------------------------------------------

   procedure Evaluate_Predicate (Nodes            : in out Node_Sets.Set;
                                 -- Node set to filter (these were attracted via the
                                 -- axis and node test

                                 Handle           : in     Predicate_Handles;
                                 -- The list of predicate filters to run the node
                                 -- set through

                                 Originating_Axis : in     Locations.Axes
                                 -- The axis via which the node set was selected
                                );
   -- Filter the node set through the given predicate(s), with the
   --  result being only those nodes that pass the filter.  The
   --  Originating_Axis parameter governs whether forward or reverse
   --  document will be followed when indexing nodes.

end McKae.XML.XPath.Predicates.Evaluation;

