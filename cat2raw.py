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

# $Id: cat2raw.py,v 6da234232e07 2005/05/22 22:04:35 simonjwright $

# Reads a Rose .cat file and converts it to ColdFrame .raw format.

# Uses PLY.

import lex, yacc
import datetime, sys

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
	    try:
		return self.attributes[n]
	    except:
		sys.stderr.write('oops! looking up attribute %s\n' % n)
		return None
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


class Action_Time(Base):
    def __init__(self):
	self.init()
    # Emitted as part of State.

recognizedID['ActionTime'] = Action_Time


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
	# visibility???
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
	self.emit_element('visibility', self.opExportControl, to)
	self.emit_kind(to)
	# abstract
	if self.cardinality:
	    self.emit_element('cardinality', self.cardinality, to)
	# concurrency
	self.emit_nested_attribute_list('attributes', 'class_attributes', to)
	self.emit_nested_attribute_list('operations', 'operations', to)
	self.emit_nested_attribute_list('events', 'events', to)
	print `self.state_machine`
	if self.state_machine:
	    self.state_machine.emit(to)
	    sys.stderr.write('found state mchine\n')
	    pass
    def emit_kind(self, to):
	# Overridden in children
	self.emit_element('kind', 'NormalClass', to)
    def emit_inheritance(self, to):
	for p in self.superclasses:
	    to.write('<inheritance>\n')
	    p.emit_element('name', p.label, to)
	    p.emit_documentation(to)
	    p.emit_element('parent',
			   object_name(p.supplier),
			   to)
	    self.emit_element('child', self.object_name, to)
	    to.write('</inheritance>\n')

recognizedID['Class'] = Class


class Class_Utility(Class):
    def __init__(self):
	self.init()
    def emit_kind(self, to):
	self.emit_element('kind', 'Utility', to)

recognizedID['Class_Utility'] = Class_Utility


class Domain(Base):
    def __init__(self):
	self.init()
    def element(self): return 'domain'
    def emit_recursive(self, op, to):
	op(self, to)
	for c in filter(lambda o:
			(o.__class__ == Domain 
			 and (o.stereotype.lower() == 'generate' 
			      or o.stereotype.lower() == 'include')),
			self.logical_models):
	    sys.stderr.write('entering %s ..\n' % c.object_name)
	    c.emit_recursive(op, to)
	    sys.stderr.write('.. leaving %s\n' % c.object_name)
    def emit_contents(self, to):
	t = datetime.datetime.today()
	self.emit_element('extractor', 'cat2raw.py $Revision: 6da234232e07 $', to)
	to.write('<date>\n')
	self.emit_element('year', t.year, to)
	self.emit_element('month', t.month, to)
	self.emit_element('day', t.day, to)
	self.emit_element('time', '%02d:%02d' % (t.hour, t.minute), to)
	to.write('</date>\n')
	to.write('<classes>\n')
	self.emit_recursive(self.emit_classes, to)
	to.write('</classes>\n')
	to.write('<relationships>\n')
	self.emit_recursive(self.emit_associations, to)
	self.emit_recursive(self.emit_inheritances, to)
	to.write('</relationships>\n')
    def emit_classes(self, to):
	for i in self.logical_models:
	    if isinstance(i, Class):
		i.emit(to)
    emit_classes = staticmethod(emit_classes)
    def emit_associations(self, to):
	for i in self.logical_models:
	    if isinstance(i, Association):
		i.emit(to)
    emit_associations = staticmethod(emit_associations)
    def emit_inheritances(self, to):
	for i in self.logical_models:
	    if i.superclasses:
		i.emit_inheritance(to)
    emit_inheritances = staticmethod(emit_inheritances)

recognizedID['Class_Category'] = Domain


# entryaction: 
# event:


class Inheritance(Base):
    def __init__(self):
	self.init()
    # All the work for Inheritance is done in Class (because part of
    # the output is the parent, and that's not readily available).

recognizedID['Inheritance_Relationship'] = Inheritance


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
    def element(self): return 'role'
    def emit_contents(self, to):
	self.emit_element('end', self.end, to)
	self.emit_element('classname',
			  object_name(self.supplier),
			  to)
	self.emit_element('cardinality', self.client_cardinality, to)

recognizedID['Role'] = Role


# statemachine: states, transitions
class State_Machine(Base):
    def __init__(self):
	self.init()
    def element(self): return 'statemachine'
    def emit_contents(self, to):
	pass

recognizedID['State_Machine'] = State_Machine


# state: entryactions, initial, final
class State(Base):
    def __init__(self):
	self.init()
    def element(self): return 'state'
    def emit_contents(self, to):
	pass

recognizedID['State'] = State


# transition: source, target, event/name, transitionaction/name
class Transition(Base):
    def __init__(self):
	self.init()
    def element(self): return 'transition'
    def emit_contents(self, to):
	pass

recognizedID['State_Transition'] = Transition


# Objects we don't care about
for o in (
    'AssocAttachView',
    'AssociationViewNew',
    'AttachView',
    'CategoryView',
    'ClassDiagram',
    'ClassView',
    'Compartment',
    'Destruction_Marker',
    'Focus_Of_Control',
    'Font',
    'InheritView',
    'InterMessView',
    'InterObjView',
    'InteractionDiagram',
    'ItemLabel',
    'Label',
    'Link',
    'Mechanism',
    'Message',
    'NoteView',
    'Object',
    'Petal',
    'RoleView',
    'SegLabel',
    'SelfTransView',
    'StateView',
    'State_Diagram',
    'TransView',
    'UsesView',
    'Uses_Relationship',
    'Visibility_Relationship',
    ):
    recognizedID[o] = Base

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

