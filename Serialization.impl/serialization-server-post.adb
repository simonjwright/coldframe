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

--  $RCSfile: serialization-server-post.adb,v $
--  $Revision: 5fa63567e054 $
--  $Date: 2003/08/13 19:42:20 $
--  $Author: simon $

--  This entry action posts its parameter's payload to the output
--  stream.

with BC.Support.Memory_Streams;

separate (Serialization.Server)
procedure Post
  (This : Handle;
   V : Buffer) is
begin

   BC.Support.Memory_Streams.Write_Contents (To => This.Channel, Stream => V);

end Post;
