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
--  $Revision: 141dc30870c4 $
--  $Date: 2003/02/20 20:58:51 $
--  $Author: simon $

--  This instance event handler posts its parameter's value to the
--  output stream. It then frees the value.

with BC.Support.Memory_Streams;

separate (Serialization.Server)
procedure Post
  (This : Handle;
   V : Buffer) is
begin

   BC.Support.Memory_Streams.Write_Contents (To => This.Channel, Stream => V);

end Post;
