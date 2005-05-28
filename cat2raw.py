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

# $Id: cat2raw.py,v ed03aa6ace00 2005/05/28 05:17:11 simonjwright $

# Reads a Rose .cat file and converts it to ColdFrame .raw format.

# Uses PLY (http://savannah.nongnu.org/projects/ply/).

import lex, yacc
import datetime, re, sys

#-----------------------------------------------------------------------
# Object model
#-----------------------------------------------------------------------

class Base:
    '''The base class for all Objects retrieved from the Petal file.'''
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
            if len(self.qualifiers):
	        return self.qualifiers[0]
            else:
                return None;
	elif n in self.attributes:
	    return self.attributes[n]
	else:
	    return None
    def init(self):
	'''Equivalent of super.__init__.'''
	if self.__class__ != Base: Base.__init__(self)
    def add_qualifiers(self, list):
	'''The qualifiers appear on the head line of the Petal object:
	(object kind q1 q2 q3
	Usually the first one is the object's name.'''
	self.qualifiers = list
    def add_attributes(self, dict):
	self.attributes = dict
    def element_tag(self):
	'''The string that appears in the XML element: <tag/>.'''
	return ''
    def emit_element_start(self, to):
	'''Outputs <element attr="value"> or just <element>, as required.
	The only attr supported here is stereotype.'''
	if self.stereotype:
	    to.write('<%s stereotype="%s">\n' %
		     (self.element_tag(), self.stereotype))
	else:
	    to.write('<%s>\n' % self.element_tag())
    def emit_element_end(self, to):
	'''Outputs the trailing </element>.'''
	to.write('</%s>\n' % self.element_tag())
    def emit_documentation(self, to):
	if 'documentation' in self.attributes:
	    to.write('<documentation><![CDATA[ %s ]]></documentation>\n' %
		     self.attributes['documentation'])
    def emit_contents(self, to):
	'''Outputs the contents of the XML element (excluding <name/> and
	<documentation/>).'''
	pass
    def emit_single_element(self, element, value, to):
	'''Outputs a single XML element.'''
	to.write('<%s>%s</%s>\n' % (element, value, element))
    def emit_name(self, to):
	'''Outputs the <name/> of the XML element.'''
	self.emit_single_element('name', self.object_name, to)
    def emit_visibility(self, to):
	'''Outputs the <visibility/> of the XMLelement.'''
	if self.opExportControl:
	    self.emit_single_element('visibility', self.opExportControl, to)
	else:
	    self.emit_single_element('visibility', 'PublicAccess', to)
    def emit_nested_attribute_list(self, list, attribute, to):
	'''Outputs all the contained <attribute/> XML elements, contained
	in a <list/> element.'''
        to.write('<%s>\n' % list)
	self.emit_attribute_list(attribute, to)
	to.write('</%s>\n' % list)
    def emit_attribute_list(self, attribute, to):
	'''Outputs all the <attribute/> XMLelements.'''
	if attribute in self.attributes:
	    for i in self.attributes[attribute]:
		i.emit(to)
    def emit(self, to=sys.stdout):
	'''Outputs the whole XML element.'''
	if len(self.element_tag()) > 0:
	    self.emit_element_start(to)
	    self.emit_name(to)
	    self.emit_documentation(to)
	    self.emit_contents(to)
	    self.emit_element_end(to)

recognizedID = {}


class Action(Base):
    '''Part of State. The only Actions we care about are entry actions.''' 
    def __init__(self):
	self.init()
    def element_tag(self): return 'entryaction'
    def is_entry_action(self):
	return (self.ActionTime.when == 'Entry')
    def emit(self, to):
	if self.is_entry_action(): Base.emit(self, to)

recognizedID['action'] = Action


class Action_Time(Base):
    '''Part of State. Indicates when the action occurs; the only ones we
    care about are entry actions.''' 
    def __init__(self):
	self.init()

recognizedID['ActionTime'] = Action_Time


class Association(Base):
    def __init__(self):
	self.init()
    def element_tag(self): return 'association'
    def emit_contents(self, to):
	# errk, don't like this 'end' stuff!
	end = 0
	for r in self.roles:
	    end += 1
	    r.end = end
	    r.emit(to)
	if self.AssociationClass:
	    self.emit_single_element('associative',
				     object_name(self.AssociationClass),
				     to)

recognizedID['Association'] = Association


class Attribute(Base):
    def __init__(self):
	self.init()
    def element_tag(self): return 'attribute'
    def emit_contents(self, to):
	# visibility???
	# static
	to.write('<type>%s</type>\n' % self.attributes['type'])
	if self.initv:
	    self.emit_single_element('initial', self.initv, to)

recognizedID['ClassAttribute'] = Attribute


class Class(Base):
    def __init__(self):
	self.init()
    def element_tag(self): return 'class'
    def name(self): return self.qualifiers[0]
    def emit_contents(self, to):
	self.emit_visibility(to)
	self.emit_kind(to)
	# abstract
	if self.cardinality:
	    self.emit_single_element('cardinality', self.cardinality, to)
	# concurrency
	self.emit_nested_attribute_list('attributes', 'class_attributes', to)
	self.emit_nested_attribute_list('operations', 'operations', to)
	self.emit_nested_attribute_list('events', 'used_nodes', to)
	if self.statemachine:
	    sm = self.statemachine[0]
	    if sm.stereotype is not None \
		    and sm.stereotype.lower() == 'generate':
		sm.emit(to)
	    else:
		sys.stderr.write('found state machine but skipped it!\n')
	    pass
    def emit_kind(self, to):
	# Overridden in children
	self.emit_single_element('kind', 'NormalClass', to)
    def emit_inheritance(self, to):
	for p in self.superclasses:
	    to.write('<inheritance>\n')
	    p.emit_single_element('name', p.label, to)
	    p.emit_documentation(to)
	    p.emit_single_element('parent',
				  object_name(p.supplier),
				  to)
	    self.emit_single_element('child', self.object_name, to)
	    to.write('</inheritance>\n')

recognizedID['Class'] = Class


class Class_Utility(Class):
    def __init__(self):
	self.init()
    def emit_kind(self, to):
	self.emit_single_element('kind', 'Utility', to)

recognizedID['Class_Utility'] = Class_Utility


class Domain(Base):
    def __init__(self):
	self.init()
    def element_tag(self): return 'domain'
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
	self.emit_single_element('extractor',
				 'cat2raw.py $Revision: ed03aa6ace00 $',
				 to)
	to.write('<date>\n')
	self.emit_single_element('year', t.year, to)
	self.emit_single_element('month', t.month, to)
	self.emit_single_element('day', t.day, to)
	self.emit_single_element('time', '%02d:%02d' % (t.hour, t.minute), to)
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


class Event(Base):
    def __init__(self):
	self.init()
    # I expect all the output will be managed elsewhere ...

recognizedID['Event'] = Event


class Inheritance(Base):
    def __init__(self):
	self.init()
    # All the work for Inheritance is done in Class (because part of
    # the output is the parent, and that's not readily available).

recognizedID['Inheritance_Relationship'] = Inheritance


class Operation(Base):
    def __init__(self):
	self.init()
    def element_tag(self): return 'operation'
    def emit_contents(self, to):
	# abstract
	self.emit_visibility(to)
	self.emit_nested_attribute_list('parameters', 'parameters', to)
	if self.result:
	    self.emit_single_element('return', self.result, to)

recognizedID['Operation'] = Operation


class Parameter(Base):
    def __init__(self):
	self.init()
    def element_tag(self): return 'parameter'
    def emit_contents(self, to):
	to.write('<type>%s</type>\n' % self.type)
	if self.initv:
	    self.emit_single_element('initial', self.initv, to)

recognizedID['Parameter'] = Parameter


class Role(Base):
    def __init__(self):
	self.init()
    def element_tag(self): return 'role'
    def emit_contents(self, to):
	self.emit_single_element('end', self.end, to)
	self.emit_single_element('classname',
				 object_name(self.supplier),
				 to)
	self.emit_single_element('cardinality', self.client_cardinality, to)

recognizedID['Role'] = Role


# statemachine: states, transitions
class State_Machine(Base):
    def __init__(self):
	self.init()
    def element_tag(self): return 'statemachine'
    def emit_name(self, to):
	if not re.search(r'/', self.object_name):
	    self.emit_single_element('name', self.object_name, to)
    def emit(self, to):
	if self.stereotype and self.stereotype.lower() == 'generate':
	    Base.emit(self, to)
    def emit_contents(self, to):
	self.emit_nested_attribute_list('states', 'states', to)
	self.emit_transitions(to)
    def emit_transitions(self, to):
	to.write('<transitions>\n')
	transitions = []
        for s in self.states:
            if s.transitions:
                for t in s.transitions:
                    # A bit ooky here; t needs to know the
                    # statemachine and the source state.
                    t.statemachine = self
                    t.source = s
                    t.emit(to)
	to.write('</transitions>\n')
    def state_named(self, n):
        '''Returns the state named 'n'.'''
        for s in self.states:
            if s.object_name == n:
                return s
        return None

recognizedID['State_Machine'] = State_Machine


# state: entryactions, initial, final
class State(Base):
    def __init__(self):
	self.init()
    def element_tag(self): return 'state'
    def name(self):
        if not self.object_name or re.search(r'UNNAMED', self.object_name):
            if self.type == 'StartState':
                return 'initial'
            elif  self.type == 'EndState':
                return 'final'
            else:
                return 'unnamed'
        else:
            return self.object_name
    def emit_element_start(self, to):
	if self.type == 'StartState':
	    to.write('<state initial="yes">\n')
	elif self.type == 'EndState':
	    to.write('<state final="yes">\n')
	else:
	    to.write('<state>\n')
    def emit_name(self, to):
        n = self.name()
        if n != 'anonymous':
	    self.emit_single_element('name', n, to)
    def emit_contents(self, to):
	self.emit_nested_attribute_list('entryactions', 'actions', to)
	pass

recognizedID['State'] = State


# transition: source, target, event/name, transitionaction/name
class Transition(Base):
    def __init__(self):
	self.init()
    def element_tag(self): return 'transition'
    def emit_name(self, to):
        '''Transitions dont have names.'''
        pass
    def emit_contents(self, to):
	self.emit_single_element('source', self.source.name(), to)
        self.emit_single_element\
            ('target',
             self.statemachine.state_named(self.supplier).name(),
             to)
        if self.Event:
            to.write('<event>\n')
            self.emit_single_element('name', self.Event.object_name, to)
            to.write('</event>\n')
        if self.action:
            self.emit_single_element('transitionaction',
                                     self.action.object_name,
                                     to)

recognizedID['State_Transition'] = Transition


class Uses_Relationship(Base):
    # Used for typed events only.
    def __init__(self):
	self.init()
    def element_tag(self): return 'event'
    def emit_name(self, to):
	self.emit_single_element('name', self.label, to)
    def emit(self, to):
	st = self.stereotype.lower()
	if st and re.search(r'event', st, re.I):
	    Base.emit(self, to)
    def emit_contents(self, to):
	self.emit_single_element('type',
				 object_name(self.supplier),
				 to)

recognizedID['Uses_Relationship'] = Uses_Relationship


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
	n = p[1][0]
	if n == 'statemachine':
	    # There is something very odd here; mjj thought it might
	    # be a copy/deepcopy problem, but that seems to end up
	    # trying to deepcopy None -- just like the problem without
	    # the deepcopy! Converting this particular case to a tuple
	    # seems to work. Could maybe be generalised?
	    p[0][n] = (p[1][1],)
	else:
	    p[0][n] = p[1][1]
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

