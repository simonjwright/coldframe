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

--  $RCSfile: normalize_xmi-model-events.ads,v $
--  $Revision: de3f2d1bb719 $
--  $Date: 2012/01/19 17:12:05 $
--  $Author: simonjwright $

private package Normalize_XMI.Model.Events is

   --  Note, an Event_Element can correspond to a CallEvent or a
   --  SignalEvent; ColdFrame doesn't distinguish.

   --  Note, ColdFrame requires Event elements in the .norm file to be
   --  contained in the Class element to which they are directed,
   --  while ArgoUML places then in the outermost level of the package
   --  in which they're declared. This means we're going to have to be
   --  clever about matching them up (can't use the name, never mind
   --  the problem of normalized vs unnormalized names, because there
   --  may be multiple domains in one .uml file, and multiple packages
   --  in one domain); will need to use 'xmi.id' attributes.

   --  Used to read all the events in the domain.
   --
   --  It accumulates the results in Accumulating_In, which is a map
   --  of Event_Elements keyed by "xmi.id" (certainly unique within
   --  the .uml file).
   procedure Read_Event (From : DOM.Core.Node;
                         Parent : not null Element_P;
                         Accumulating_In : in out Element_Maps.Map);

   type Event_Element is new Element with private;

   overriding
   procedure Resolve (E : in out Event_Element);

   overriding
   procedure Output (E : Event_Element; To : Ada.Text_IO.File_Type);

   not overriding
   function Parameter_Type (Of_Event : Event_Element) return String;

private

   type Event_Element is new Element with record
      --  We aren't interested in the parameter name.
      Parameter_Type : Ada.Strings.Unbounded.Unbounded_String;
   end record;

end Normalize_XMI.Model.Events;
