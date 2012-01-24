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

with BC.Containers.Collections.Ordered.Unbounded;
with Bc.Support.Standard_Storage;
with XIA_Parser_Model;

package McKae.XML.XPath.Predicates is

   -- Handle to the predicate content
   type Predicate_Handles is private;

   -- Add the pointer to the root node of a parse subtree that was
   --  created for a predicate
   procedure Add_Predicate_Parse
     (Handle : in out Predicate_Handles;
      T      : in     Xia_Parser_Model.Parseable_Ptr);

   -- Null instances of a predicate definition
   Null_Predicate : constant Predicate_Handles;

   -- Release the contents of a predicate handle (which may consists
   --  of one or more individual predicate definitions).  Not that
   --  this does _not_ release the associated parse subtree associated
   --  with each predicate instance.
   procedure Release(Handle : in out Predicate_Handles);

   Malformed_Predicate : exception;
   -- Raised when a supplied query predicate does not conform to
   --  predicate syntax

private
   function "="(L, R : Xia_Parser_Model.Parseable_Ptr) return Boolean
     renames Xia_Parser_Model."=";

   -- This is a "filler" less-than function.  There is no intrinsic
   --  ordering of token pointer instances.  The Ordered variation of
   --  the collection is being used to simply ensure that the contents
   --  of the list are maintained in the order in which they're
   --  inserted.
   function "<"(L, R : Xia_Parser_Model.Parseable_Ptr) return Boolean;

   package Predicate_Containers is
      new Bc.Containers(Item => Xia_Parser_Model.Parseable_Ptr);
   package Predicate_Collections is
      new Predicate_Containers.Collections;
   package Ordered_Predicate_Collections is
      new Predicate_Collections.Ordered;
   package Predicate_Handle_Pkg is new Ordered_Predicate_Collections.Unbounded
     (Storage => BC.Support.Standard_Storage.Pool);

   type Predicate_Handles is record
      Predicate_List : Predicate_Handle_Pkg.Collection;
   end record;

   Null_Predicate : constant Predicate_Handles
     := (Predicate_List => Predicate_Handle_Pkg.Null_Container);

end McKae.XML.XPath.Predicates;
