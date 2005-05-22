#!/usr/bin/python

#  Copyright (C) Simon Wright <simon@pushface.org>

#  This package is free software; you can redistribute it and/or
#  modify it under terms of the GNU General Public License as
#  published by the Free Software Foundation; either version 2, or
#  (at your option) any later version. This package is distributed in
#  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
#  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
#  PARTICULAR PURPOSE. See the GNU General Public License for more
#  details. You should have received a copy of the GNU General Public
#  License distributed with this package; see file COPYING.  If not,
#  write to the Free Software Foundation, 59 Temple Place - Suite
#  330, Boston, MA 02111-1307, USA.

# $Id: cat2raw.py,v 94a2118e476b 2005/05/22 14:36:09 simonjwright $

# Reads a Rose .cat file and converts it to ColdFrame .raw format.

# Uses PLY.

import lex, yacc
import sys

#-----------------------------------------------------------------------
# Object model
#-----------------------------------------------------------------------

class Base:
    def __init__(self):
	self.qualifiers = ()
	self.attributes = {}
	pass
    def __getattr__(self, n):
	'''Attribute lookup.

	Not sure why using plain 'name' for the object name results in a
	lookup of the attribute __repr__ which is expected to be
	callable, but that's what happens!

	To get the object name, use 'object_name'.'''
	if n == 'object_name':
	    return self.qualifiers[0]
	elif n in self.attributes:
	    return self.attributes[n]
	else:
	    return None
    def init(self):
	if self.__class__ != Base: Base.__init__(self)
    def add_qualifiers(self, list): self.qualifiers = list
    def add_attributes(self, dict): self.attributes = dict
    def element(self): return ''
    def emit_documentation(self, to):
	if 'documentation' in self.attributes:
	    to.write('<documentation><![CDATA[ %s ]]></documentation>\n' %
		     self.attributes['documentation'])
    def emit_contents(self, to): pass
    def emit_element(self, element, value, to):
	to.write('<%s>%s</%s>\n' % (element, value, element))
    def emit_nested_attribute_list(self, list, attribute, to):
        to.write('<%s>\n' % list)
	self.emit_attribute_list(attribute, to)
	to.write('</%s>\n' % list)
    def emit_attribute_list(self, attribute, to):
	if attribute in self.attributes:
	    for i in self.attributes[attribute]:
		i.emit(to)
    def emit(self, to=sys.stdout):
	if len(self.element()) > 0:
	    if self.stereotype:
		to.write('<%s stereotype="%s">\n' %
			 (self.element(), self.stereotype))
	    else:
		to.write('<%s>\n' % self.element())
	    self.emit_element('name', self.object_name, to)
	    self.emit_documentation(to)
	    self.emit_contents(to)
	    to.write('</%s>\n' % self.element())

recognizedID = {}


# association: role(2), associative
class Association(Base):
    def __init__(self):
	self.init()
    def element(self): return 'association'
    def emit_contents(self, to):
	# errk, don't like this 'end' stuff!
	end = 0
	for r in self.roles:
	    end += 1
	    r.end = end
	    r.emit(to)
	if self.AssociationClass:
	    self.emit_element('associative',
			      object_name(self.AssociationClass),
			      to)

recognizedID['Association'] = Association


class Attribute(Base):
    def __init__(self):
	self.init()
    def element(self): return 'attribute'
    def emit_contents(self, to):
	# visibility
	# static
	to.write('<type>%s</type>\n' % self.attributes['type'])
	if self.initv:
	    self.emit_element('initial', self.initv, to)

recognizedID['ClassAttribute'] = Attribute


class Class(Base):
    def __init__(self):
	self.init()
    def element(self): return 'class'
    def name(self): return self.qualifiers[0]
    def emit_contents(self, to):
	# visibility
	# kind
	# abstract
	# cardinality
	# concurrency
	self.emit_nested_attribute_list('attributes', 'class_attributes', to)
	self.emit_nested_attribute_list('operations', 'operations', to)
	self.emit_nested_attribute_list('events', 'events', to)
	# statemachine

recognizedID['Class'] = Class


class Domain(Base):
    def __init__(self):
	self.init()
    def element(self): return 'domain'
    def emit_recursive(self, op, to):
	op(self, to)
	for c in filter(lambda o:
			(o.__class__ == Domain 
			 and (o.stereotype == 'generate' 
			      or o.stereotype == 'include')),
			self.logical_models):
	    c.emit_recursive(op, to)
    def emit_contents(self, to):
	self.emit_element('extractor', 'cat2raw.py', to)
	to.write('<date>\n')
	self.emit_element('year', 1970, to)
	self.emit_element('month', 1, to)
	self.emit_element('day', 1, to)
	self.emit_element('time', '00:00', to)
	to.write('</date>\n')
	to.write('<classes>\n')
	self.emit_recursive(self.emit_classes, to)
	to.write('</classes>\n')
	to.write('<relationships>\n')
	self.emit_recursive(self.emit_associations, to)
	to.write('</relationships>\n')
    def emit_classes(self, to):
	for i in self.logical_models:
	    if i.__class__ == Class:
		i.emit(to)
    emit_classes = staticmethod(emit_classes)
    def emit_associations(self, to):
	for i in self.logical_models:
	    if i.__class__ == Association:
		i.emit(to)
    emit_associations = staticmethod(emit_associations)

recognizedID['Class_Category'] = Domain


# entryaction: 
# event:
# inheritance: child, parent


class Operation(Base):
    def __init__(self):
	self.init()
    def element(self): return 'operation'
    def emit_contents(self, to):
	# abstract
	self.emit_element('visibility', self.opExportControl, to)
	self.emit_nested_attribute_list('parameters', 'parameters', to)
	if self.result:
	    self.emit_element('return', self.result, to)

recognizedID['Operation'] = Operation


class Parameter(Base):
    def __init__(self):
	self.init()
    def element(self): return 'parameter'
    def emit_contents(self, to):
	to.write('<type>%s</type>\n' % self.type)
	if self.initv:
	    self.emit_element('initial', self.initv, to)

recognizedID['Parameter'] = Parameter


class Role(Base):
    def __init__(self):
	self.init()
	self.end = 0
    def element(self): return 'role'
    def emit_contents(self, to):
	self.emit_element('end', self.end, to)
	self.emit_element('classname',
			  object_name(self.supplier),
			  to)
	self.emit_element('cardinality', self.client_cardinality, to)

recognizedID['Role'] = Role


# statemachine: states, transitions
# state: entryactions, initial, final
# transition: source, target, event/name, transitionaction/name


#-----------------------------------------------------------------------
# Utilities
#-----------------------------------------------------------------------

def create_object(id):
    '''The factory for creating objects of the class corresponding to the id.'''
    if id in recognizedID:
	return recognizedID[id]()
    else:
	sys.stderr.write("didn't recognise object ID %s.\n" % id)
	return Base()

def object_name(fqn):
    '''Strips the leading model path from a fully qualified name.'''
    last_sep = fqn.rfind('::')
    if last_sep >= 0:
	return fqn[last_sep + 2:]
    else:
	return fqn

#-----------------------------------------------------------------------
# Parser
#-----------------------------------------------------------------------

def p_cat_file(p):
    'cat_file : object object'
    #print "design: %s" % p[1]
    #print p[2]
    p[0] = p[2]
    pass

def p_object(p):
    'object : OBJECT ID qualifiers reference attributes RPAREN'
    #print "object %s, qualifiers %s, attributes %s" % (p[2], p[3], p[5])
    p[0] = create_object(p[2])
    p[0].add_qualifiers(p[3])
    p[0].add_attributes(p[5])
    pass

def p_qualifiers(p):
    '''qualifiers : qualifier qualifiers
                  | empty'''
    if len(p) == 3:
	p[0] = (p[1],) + p[2]
    else:
	p[0] = ()
    pass

def p_qualifier(p):
    'qualifier : QSTRING'
    p[0] = p[1]
    pass

def p_reference(p):
    '''reference : AMP INTNUM
                 | empty'''
    if len(p) == 3:
	p[0] = p[2]
    pass

def p_attributes(p):
    '''attributes : attribute attributes
                  | empty'''
    if len(p) == 3:
	p[0] = p[2]
	p[0][p[1][0]] = p[1][1]
    else:
	p[0] = {}
    pass

def p_attribute(p):
    'attribute : ID value'
    p[0] = (p[1], p[2])
    pass

def p_value(p):
    '''value : QSTRING
             | FLONUM
	     | INTNUM
	     | ID
	     | location
	     | list
	     | value_list
	     | reference
	     | doclines
	     | object'''
    p[0] = p[1]
    pass

def p_location(p):
    'location : LPAREN INTNUM COMMA INTNUM RPAREN'
    p[0] = (p[2], p[4])
    pass

def p_list(p):
    'list : LIST ID list_members RPAREN'
    p[0] = p[3]
    pass

def p_value_list(p):
    'value_list : VALUE ID value RPAREN'
    p[0] = p[3]
    pass

def p_doclines(p):
    '''doclines : DOCLINE doclines
                | DOCLINE'''
    if len(p) == 3:
	p[0] = p[1] + '\n' + p[2]
    else:
	p[0] = p[1]
    pass

def p_list_members(p):
    '''list_members : objects
                    | locations
		    | qstrings
		    | empty'''
    if p[1]:
	p[0] = p[1]
    else:
	p[0] = ()
    pass

def p_objects(p):
    '''objects : object objects
               | object'''
    if len(p) == 3:
	p[0] = (p[1],) + p[2]
    else:
	p[0] = (p[1],)
    pass

def p_locations(p):
    '''locations : location locations
                 | location'''
    if len(p) == 3:
	p[0] = (p[1],) + p[2]
    else:
	p[0] = (p[1],)
    pass

def p_qstrings(p):
    '''qstrings : QSTRING qstrings
                | QSTRING'''
    if len(p) == 3:
	p[0] = (p[1],) + p[2]
    else:
	p[0] = (p[1],)
    pass

# Empty productions
def p_empty(p):
    'empty :'
    pass

# Panic mode recovery
def p_error(p):
    if not p:
	print "That seems to be it."
	return None
    print "Syntax error at %s on line %d" % (p.type, p.lineno)
    # Read ahead looking for a terminating ")"
    while 1:
        tok = yacc.token()             # Get the next token
        if not tok or tok.type == 'RPAREN': break
    yacc.errok()
    # Return the token (or nil) to the parser as the next lookahead
    # token
    return tok

#-----------------------------------------------------------------------
# Lexer
#-----------------------------------------------------------------------

tokens = (
    'OBJECT',
    'LIST',
    'LPAREN',
    'RPAREN',
    'AMP',
    'ID',
    'INTNUM',
    'FLONUM',
    'QSTRING',
    'DOCLINE',
    'COMMA',
    'VALUE'
)

t_OBJECT  = r'\(object'
t_LIST    = r'\(list'
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_AMP     = r'@'
t_ID      = r'[a-zA-Z_]+'
t_INTNUM  = r'[+-]?\d+'
t_FLONUM  = r'[+-]?\d+\.\d+'
t_COMMA   = r','
t_VALUE   = r'\(value'

# A quoted string has its quotes removed.
def t_QSTRING(t):
    r'"[^"]*"'
    t.value = t.value[1:-1]
    return t

# A DOCLINE has its leading pipe removed.
def t_DOCLINE(t):
    r'\|.*'
    if t.value[len(t.value) - 1] == '\r':
	t.value = t.value[1:-1]
    else:
	t.value = t.value[1:]
    return t

# Define a rule so we can track skipped line numbers as well as
# significant ones
def t_newline(t):
    r'\n+'
    t.lineno += len(t.value)

# A string containing ignored characters (space, tab and CR)
t_ignore  = ' \t\r'

# Error handling rule
def t_error(t):
    print "Illegal character '%s', line %d" % (t.value[0], t.lineno)
    t.skip(1)

#-----------------------------------------------------------------------
# Main, test
#-----------------------------------------------------------------------

# Build the lexer
l = lex.lex()

# Some test input
data = '''\
(object Foo "foos name" "check foo" @41

)

(object Bar "bars name" @42
  fred 123)
'''

l.input(sys.stdin.read())

p = yacc.yacc()

p.parse(lexer = l).emit()
