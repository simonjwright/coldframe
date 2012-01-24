----------------------------------------------------------------------
--                                                                  --
--     XIA - XPath In Ada - An XPath Implementation for Ada 95      --
--                                                                  --
--           Copyright (C) 2004 McKae Technologies                  --
--                                                                  --
-- This library  is free software;  you can redistribute  it and/or --
--  modify  it under  terms of  the GNU  General Public  License as --
--  published by the Free Software Foundation; either version 2, or --
--  (at your option) any later  ver- sion.  DTraq is distributed in --
--  the  hope that  it will  be useful,  but WITHOUT  ANY WARRANTY; --
--  without even the implied warranty of MERCHANTABILITY or FITNESS --
--  FOR A  PARTICULAR PURPOSE.  See the GNU  General Public License --
--  for more details.   You should have received a  copy of the GNU --
--  General  Public  License   distributed  with  DTraq;  see  file --
--  COPYING.   If not, write  to the  Free Software  Foundation, 59 --
--  Temple Place  - Suite  330, Boston, MA  02111-1307, USA.   As a --
--  special  exception, if  other files  instantiate  generics from --
--  this unit, or you link this unit with other files to produce an --
--  executable, this  unit does not  by itself cause  the resulting --
--  executable  to be covered  by the  GNU General  Public License. --
--  This exception  does not  however invalidate any  other reasons --
--  why  the executable  file might  be covered  by the  GNU Public --
--  License.    DTraq   is   maintained   by   McKae   Technologies --
--  (http://www.dtraq.com).                                         --
----------------------------------------------------------------------
-- XIA (XPathInAda) is a native Ada implementation for querying XML
--  documents using the XPath 1.0 query language.
----------------------------------------------------------------------
--
-- Known limitations:
--
-- The core library function "id" is not yet implemented, as the
--  XMLAda function Get_Element_By_ID is not yet implemented.
--  Therfore invoking "id" will always result in an empty node-set.
--
----------------------------------------------------------------------


with Dom.Core;

package Mckae.XML.XPath.XIA is

   function XPath_Query
     (N     : Dom.Core.Node;
      -- A node in the XML tree against which the XPath query is
      --  submitted.

      XPath : String
      -- The XPath query.
     ) return Dom.Core.Node_List;
   -- Apply an XPath query, starting with the given node, in the case
   --  of a relative XPath query, or the document that contains the
   --  given node, otherwise.  Returns a list of nodes, which may be
   --  empty, that meet the XPath specification.  If the query does
   --  not follow XPath syntax, the Malformed_XPath exception is
   --  raised.

   Malformed_XPath : exception;
   -- Raised when the XPath query does not conform to XPath syntax.

   Inappropriate_Node : exception;
   -- Raised when the starting node does not correspond to the type of query

end Mckae.XML.XPath.XIA;
