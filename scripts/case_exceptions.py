#!/usr/bin/python

# $Id$

# Converts a GLIDE or ColdFrame case exception file into GPS (XML) format.

import re
import sys
import xml.dom.minidom


def addElement(match, element, doc, top):
    
    """Extract the 'casex' property from 'match' and add it to 'doc'
    under 'top' as the text component of an 'element' node. If there's
    a 'comment' property in 'match', add that as a child of the new
    node."""

    comp = top.appendChild(doc.createElement(element))
    comp.appendChild(doc.createTextNode(match.group('cases')))
    if match.group('comment'):
        comment = comp.appendChild(doc.createElement("comment"))
        comment.appendChild(doc.createCDATASection(match.group('comment')))


def parseCaseExceptionLines(f):

    """'f' is an open case exception file. Returns a DOM object
    containing the XML version of the data."""

    doc = xml.dom.minidom.getDOMImplementation().createDocument\
          (None,
           "project",
           None)
    domain = doc.documentElement
    top = doc.createElement("case_exceptions")
    domain.appendChild(top)
    substringPattern = re.compile(r'^\*(?P<cases>\S+)(\s+(?P<comment>.*))?$')
    wordPattern = re.compile(r'^(?P<cases>[^*]\S+)(\s+(?P<comment>.*))?$')
    for l in f.readlines():
        s = substringPattern.match(l)
        if s:
            addElement(s, "substring", doc, top)
        w = wordPattern.match(l)
        if w:
            addElement(w, "word", doc, top)
    return doc

                
if __name__ == '__main__':
    if len(sys.argv) == 1:
        doc = parseCaseExceptionLines(sys.stdin)
    else:
        f = open(sys.argv[1], 'r')
        doc = parseCaseExceptionLines(f)
        f.close()

    if len(sys.argv) < 3:
        doc.writexml(sys.stdout)
    else:
        f = open(sys.argv[2], "w")
        doc.writexml(f)
        f.close()
