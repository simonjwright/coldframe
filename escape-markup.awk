#! /usr/bin/awk -f

# $Id: escape-markup.awk,v c98aee53a2b4 2001/04/27 19:05:02 simon $

# Escapes XML-significant characters in the <documentation> element of an
# XML Domain Definition file, generated from Rose by ddf.ebs.

# Copyright (C) Simon Wright <simon@pushface.org>

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
# 
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
# USA.

BEGIN {
    RS = "</documentation>"
}

{
    parts[2] = "";
    split($0, parts, "<documentation>");
    if (length(parts[2]) > 0) {
	gsub("&", "\\&amp;", parts[2]);
	gsub("<", "\\&lt;", parts[2]);
	gsub(">", "\\&gt;", parts[2]);
	print parts[1] "<documentation>" parts[2] "</documentation>";
    } else {
	print parts[1];
    }
}
