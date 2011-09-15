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

# $Id: cat2raw.py,v 2e39c16b3edc 2011/07/03 10:58:46 simonjwright $

# Reads a Rose .cat file and converts it to ColdFrame .raw format.

# Uses PLY (http://www.dabeaz.com/ply/).

import ply.lex as lex, ply.yacc as yacc
import time, getopt, os, re, sys

class Callable:
    '''Jython doesn't have staticmethod(), so this acts as a replacement.
    See http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/52304'''
    def __init__(self, anycallable):
	self.__call__ = anycallable

# Should we revert to a previous standard of ColdFrame?
reversionary = 0

#-----------------------------------------------------------------------
# Object model
#-----------------------------------------------------------------------

class Base:
    """The base class for all Objects retrieved from the Petal file."""
    def __init__(self):
	self.qualifiers = ()
	"""The qualifiers appear on the head line of the Petal object:
	(object kind q1 q2 q3
	Usually the first one is the object's name."""
	self.attributes = {}
	"""The attributes are all the named features of the object."""
    def __getattr__(self, n):
	"""Attribute lookup.
	Not sure why using plain 'name' for the object name results in a
	lookup of the attribute __repr__ which is expected to be
	callable, but that's what happens!
	To get the object name, use 'object_name'."""
	if n == 'object_name':
            if len(self.qualifiers):
	        return self.qualifiers[0]
            else:
                return None;
	elif self.attributes.has_key(n):
	    return self.attributes[n]
	else:
	    return None
    def init(self):
	"""Equivalent of super.__init__."""
	if self.__class__ != Base: Base.__init__(self)
    def add_attribute(self, name, value):
	self.attributes[name] = value
    def element_tag(self):
	"""The string that appears in the XML element: <tag/>."""
	return ''
    def emit_element_start(self, to):
	"""Outputs <element attr="value"> or just <element>, as required.
	The only attr supported here is stereotype."""
	if self.stereotype:
	    to.write('<%s stereotype="%s">\n' %
		     (self.element_tag(), self.stereotype))
	else:
	    to.write('<%s>\n' % self.element_tag())
    def emit_element_end(self, to):
	"""Outputs the trailing </element>."""
	to.write('</%s>\n' % self.element_tag())
    def emit_documentation(self, to):
	if self.attributes.has_key('documentation'):
	    if not reversionary:
		to.write('<documentation><![CDATA[ %s ]]></documentation>\n' %
			 self.attributes['documentation'])
	    else:
		to.write('<documentation>%s</documentation>\n' %
			 self.attributes['documentation'])
	else:
	    # Feature in normalize-rose.tcl; -handleAnnotation is what
	    # processes tags, and it's only called if there was a
	    # <documentation/> element.
	    self.emit_single_element('documentation', '', to)
    def emit_contents(self, to):
	"""Outputs the contents of the XML element (excluding <name/> and
	<documentation/>)."""
	pass
    def emit_single_element(self, element, value, to):
	"""Outputs a single XML element."""
	to.write('<%s>%s</%s>\n' % (element, value, element))
    def emit_name(self, to):
	"""Outputs the <name/> of the XML element."""
	self.emit_single_element('name', self.object_name, to)
    visibility_lookup = {
	'public' : 'PublicAccess',
	'protected' : 'ProtectedAccess',
	'private' : 'PrivateAccess',
	'implementation' : 'ImplementationAccess'
	}
    def emit_visibility(self, to, default):
	"""Outputs the <visibility/> of the XMLelement."""
	if not reversionary:
	    if self.exportControl:
		# class
		v = self.exportControl
	    elif self.opExportControl:
		# operation
		v = self.opExportControl
	    else:
		v = default
	    self.emit_single_element('visibility',
				     Base.visibility_lookup[v.lower()],
				     to)
    def emit_nested_attribute_list(self, list, attribute, to):
	"""Outputs all the contained <attribute/> XML elements, contained
	in a <list/> element."""
        to.write('<%s>\n' % list)
	self.emit_attribute_list(attribute, to)
	to.write('</%s>\n' % list)
    def emit_attribute_list(self, attribute, to):
	"""Outputs all the <attribute/> XMLelements."""
	if self.attributes.has_key(attribute):
	    for i in self.attributes[attribute]:
		i.emit(to)
    def emit(self, to=sys.stdout):
	"""Outputs the whole XML element."""
	if len(self.element_tag()) > 0:
	    self.emit_element_start(to)
	    self.emit_name(to)
	    self.emit_documentation(to)
	    self.emit_contents(to)
	    self.emit_element_end(to)

# Maps the Petal file component to the class to be used here. Use Base
# if the component isn't interesting.
recognizedID = {}


class Action(Base):
    """Part of State. The only Actions we care about are entry actions."""
    def __init__(self):
	self.init()
    def element_tag(self): return 'entryaction'
    def is_entry_action(self):
	return (self.ActionTime.when == 'Entry')
    def emit(self, to):
	if self.is_entry_action(): Base.emit(self, to)

recognizedID['action'] = Action


class Action_Time(Base):
    """Part of State. Indicates when the action occurs; the only ones we
    care about are entry actions."""
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
	self.emit_visibility(to, default='private')
	if self.static == 'TRUE':
	    self.emit_single_element('static', '', to)
        # SF2819677. Ignore the undefined type error here,
        # normalize-rose will catch it.
        try:
            self.emit_single_element('type', self.attributes['type'], to)
        except (KeyError):
            self.emit_single_element('type', '', to)
	if self.initv:
	    self.emit_single_element('initial', self.initv, to)

recognizedID['ClassAttribute'] = Attribute


class Class(Base):
    def __init__(self):
	self.init()
    def element_tag(self): return 'class'
    def name(self): return self.qualifiers[0]
    def emit_contents(self, to):
	self.emit_visibility(to, default='public')
	# 'public' actually only applies to types.
	self.emit_kind(to)
	if self.abstract == 'TRUE':
	    self.emit_single_element('abstract', '', to)
	if self.cardinality:
	    self.emit_single_element('cardinality', self.cardinality, to)
	if self.concurrency:
	    self.emit_single_element('concurrency', self.concurrency, to)
	self.emit_nested_attribute_list('attributes', 'class_attributes', to)
	self.emit_nested_attribute_list('operations', 'operations', to)
	self.emit_nested_attribute_list('events', 'used_nodes', to)
	if self.statemachine:
	    sm = self.statemachine[0]
	    if sm.stereotype is not None \
		    and sm.stereotype.lower() == 'generate':
		sm.emit(to)
	    else:
		sys.stderr.write('found state machine in %s but skipped it!\n'
				 % self.object_name)
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
    def load(self, path):
	"""This is an unloaded Domain. 'path' is the filename of the parent
	CAT file; CURDIR has to be replaced by the directory part of
	path."""
	dirname = os.path.dirname(path)
	if not dirname:
	    dirname = '.'
	    """Rose doesn't require that CURDIR be all upper-case, so some folk
	    may mix the case."""
	filename = re.sub(re.compile(r'^\$CURDIR', re.IGNORECASE),
			  dirname,
			  self.file_name)
	filename = re.sub(r'\\', '/', filename)
	sys.stderr.write('  included file \"%s\"\n' % filename)
	lexer = lex.lex()
	try:
	    file = open(filename, 'r')
	except:
	    sys.stderr.write("couldn't open %s.\n" % filename)
	    sys.exit(1)
	lexer.input(file.read())
	file.close()
	parser = yacc.yacc()
	domain = parser.parse(lexer = lexer)
	domain.load_children(filename)
	self.attributes = domain.attributes
    def load_children(self, path):
	"""Loads all the child packages.
	'path' is the filename of the current .cat file (whose
	directory component is CURDIR)."""
	if self.logical_models != None:
	    for o in self.logical_models:
		if o.__class__ == Domain and o.is_loaded == 'FALSE':
		    #sys.stderr.write('loading %s.%s ..\n'
		    #    	     % (self.object_name, o.object_name))
		    o.load(path)
		    #sys.stderr.write('.. finished %s.%s.\n'
		    #   	     % (self.object_name, o.object_name))
    def emit_recursive(self, op, to):
	op(self, to)
	for c in filter(lambda o:
			(o.__class__ == Domain
			 and (o.is_loaded == None or o.is_loaded == 'TRUE')
			 and (o.stereotype
			      and o.stereotype.lower() in
			      ('generate',  'include'))),
			self.logical_models):
	    #sys.stderr.write('entering %s ..\n' % c.object_name)
	    c.emit_recursive(op, to)
	    #sys.stderr.write('.. leaving %s\n' % c.object_name)
    def emit_contents(self, to):
	yr, mo, dy, hr, mn, s, wd, yd, dst = time.localtime(time.time())
	self.emit_single_element('extractor',
				 'cat2raw.py: $Revision: 2e39c16b3edc $',
				 to)
	to.write('<date>\n')
	self.emit_single_element('year', yr, to)
	self.emit_single_element('month', mo, to)
	self.emit_single_element('day', dy, to)
	self.emit_single_element('time',
				 '%02d:%02d' % (hr, mn), to)
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
    emit_classes = Callable(emit_classes)
    def emit_associations(self, to):
	for i in self.logical_models:
	    if isinstance(i, Association):
		i.emit(to)
    emit_associations = Callable(emit_associations)
    def emit_inheritances(self, to):
	for i in self.logical_models:
	    if i.superclasses:
		i.emit_inheritance(to)
    emit_inheritances = Callable(emit_inheritances)

recognizedID['Class_Category'] = Domain


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
	self.emit_visibility(to, default='public')
	if self.abstract == 'TRUE':
	    self.emit_single_element('abstract', '', to)
	self.emit_nested_attribute_list('parameters', 'parameters', to)
	if self.result:
	    self.emit_single_element('return', self.result, to)

recognizedID['Operation'] = Operation


class Parameter(Base):
    def __init__(self):
	self.init()
    def element_tag(self): return 'parameter'
    def emit_name(self, to):
	"""Has to be a <parametername/> element to get special mode
        processing."""
	self.emit_single_element('parametername', self.object_name, to)
    def emit_contents(self, to):
	self.emit_single_element('type', self.type, to)
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


class State_Machine(Base):
    def __init__(self):
	self.init()
    def element_tag(self): return 'statemachine'
    def emit_element_start(self, to):
	"""There seems to be a bug in normalize-rose.tcl such that if
	a statemacdhine has a 'generate="yes"' attribute *and* a
	<name/> element, part of the XML gets output before the
	<domain> start tag. ddf.ebs (aka extractor.ebs) doesn't output
	the 'generate="yes"' attribute."""
        to.write('<%s>\n' % self.element_tag())
    def emit_name(self, to):
        """Only emit the name if it's not a Rose default
        ('State/Activity Model*')."""
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
        """Returns the state named 'n'.
        A name of the form ':#4' means the 4th state (counting from 0)."""
        m = re.match(r'^:#(\d+)$', n)
        if m:
            return self.states[int(m.group(1))]
        else:
            for s in self.states:
                if s.object_name == n:
                    return s
            return None

recognizedID['State_Machine'] = State_Machine


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


class Transition(Base):
    def __init__(self):
	self.init()
    def element_tag(self): return 'transition'
    def emit_name(self, to):
        """Transitions don't have names."""
        pass
    def emit_contents(self, to):
	self.emit_single_element('source', self.source.name(), to)
        self.emit_single_element\
            ('target',
             self.statemachine.state_named(self.supplier).name(),
             to)
        if self.Event:
            to.write('<event>\n')
            self.emit_single_element('name', self.Event[0].object_name, to)
            to.write('</event>\n')
        elif reversionary:
            to.write('<event><name></name></event>\n')
        if self.action:
	    to.write('<transitionaction>\n')
            self.emit_single_element('name',
                                     self.action[0].object_name,
                                     to)
            to.write('</transitionaction>\n')
        elif reversionary:
            to.write('<transitionaction><name></name></transitionaction>\n')

recognizedID['State_Transition'] = Transition


class Uses_Relationship(Base):
    # Used for typed events only.
    def __init__(self):
	self.init()
    def element_tag(self): return 'event'
    def emit_name(self, to):
	self.emit_single_element('name', self.label, to)
    def emit(self, to):
	if self.stereotype and \
		re.search(r'event', self.stereotype.lower(), re.I):
	    Base.emit(self, to)
    def emit_contents(self, to):
	self.emit_single_element('type',
				 object_name(self.supplier),
				 to)

recognizedID['Uses_Relationship'] = Uses_Relationship


# Objects we don't care about
for o in (
    'ActivityDiagram',
    'AssocAttachView',
    'AssocConstraintView',
    'AssociationViewNew',
    'AttachView',
    'CategoryView',
    'ClassDiagram',
    'ClassView',
    'Compartment',
    'DependencyView',
    'Dependency_Relationship',
    'Destruction_Marker',
    'Focus_Of_Control',
    'Font',
    'ImportView',
    'InheritTreeView',
    'InheritView',
    'InstantiateView',
    'Instantiated_Class',
    'Instantiation_Relationship',
    'InterMessView',
    'InterObjView',
    'InteractionDiagram',
    'ItemLabel',
    'Label',
    'Link',
    'LinkView',
    'Mechanism',
    'MessView',
    'Message',
    'NoteView',
    'Object',
    'ObjectDiagram',
    'ObjectView',
    'Parameterized_Class',
    'Petal',
    'RoleView',
    'SegLabel',
    'SelfMessView',
    'SelfTransView',
    'Semantic_Info',
    'StateView',
    'State_Diagram',
    'Swimlane',
    'TransView',
    'UseCase',
    'UseCaseDiagram',
    'UseCaseView',
    'UsesView',
    'Visibility_Relationship',
    'sendEvent',
    ):
    recognizedID[o] = Base

#-----------------------------------------------------------------------
# Utilities
#-----------------------------------------------------------------------

def create_object(id):
    """The factory for creating objects of the class corresponding to the id."""
    if not recognizedID.has_key(id):
	sys.stderr.write\
           ("cat2raw.py: info: didn't recognise object ID %s.\n" % id)
	recognizedID[id] = Base
    return recognizedID[id]()

def object_name(fqn):
    """Strips the leading model path from a fully qualified name."""
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
    p[0] = p[2]

def p_object(p):
    'object : OBJECT ID qualifiers reference attributes RPAREN'
    p[0] = create_object(p[2])
    p[0].qualifiers = p[3]
    p[0].attributes = p[5]

def p_qualifiers(p):
    """qualifiers : qualifier qualifiers
                  | empty"""
    if len(p) == 3:
	p[0] = (p[1],) + p[2]
    else:
	p[0] = ()

def p_qualifier(p):
    'qualifier : QSTRING'
    p[0] = p[1]

def p_reference(p):
    """reference : AMP INTNUM
                 | empty"""
    if len(p) == 3:
	p[0] = p[2]
    else:
	p[0] = None

def p_attributes(p):
    """attributes : attribute attributes
                  | empty"""
    if len(p) == 3:
	p[0] = p[2]
	n = p[1][0]
	if n == 'statemachine' or n == 'Event' or n == 'action':
	    # There is something very odd here; mjj thought it might
	    # be a copy/deepcopy problem, but that seems to end up
	    # trying to deepcopy None -- just like the problem without
	    # the deepcopy! Converting these particular cases to a
	    # tuple seems to work. Could maybe be generalised?
	    p[0][n] = (p[1][1],)
	else:
	    p[0][n] = p[1][1]
    else:
	p[0] = {}

def p_attribute(p):
    'attribute : ID value'
    p[0] = (p[1], p[2])

def p_value(p):
    """value : QSTRING
             | FLONUM
	     | INTNUM
	     | ID
	     | location
	     | list
	     | value_list
	     | reference
	     | doclines
	     | object"""
    p[0] = p[1]

def p_location(p):
    'location : LPAREN INTNUM COMMA INTNUM RPAREN'
    p[0] = (p[2], p[4])

def p_list(p):
    'list : LIST ID list_members RPAREN'
    p[0] = p[3]

def p_value_list(p):
    'value_list : VALUE ID value RPAREN'
    p[0] = p[3]

def p_doclines(p):
    """doclines : DOCLINE doclines
                | DOCLINE"""
    if len(p) == 3:
	p[0] = p[1] + '\n' + p[2]
    else:
	p[0] = p[1]

def p_list_members(p):
    """list_members : objects
                    | locations
		    | qstrings
		    | empty"""
    if p[1]:
	p[0] = p[1]
    else:
	p[0] = ()

def p_objects(p):
    """objects : object objects
               | object"""
    if len(p) == 3:
	p[0] = (p[1],) + p[2]
    else:
	p[0] = (p[1],)

def p_locations(p):
    """locations : location locations
                 | location"""
    if len(p) == 3:
	p[0] = (p[1],) + p[2]
    else:
	p[0] = (p[1],)

def p_qstrings(p):
    """qstrings : QSTRING qstrings
                | QSTRING"""
    if len(p) == 3:
	p[0] = (p[1],) + p[2]
    else:
	p[0] = (p[1],)

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

# A quoted string has its quotes removed. Internal escaped quotes are
# retained. Escapes are removed (assuming only quotes and backslashes
# get escaped).
def t_QSTRING(t):
    r'"(\\.|[^"])*"'
    t.value = re.sub(r'\\(.)', r'\1', t.value[1:-1])
    return t

# A DOCLINE has its leading pipe removed. Escapes are removed
# (assuming only quotes and backslashes get escaped).
def t_DOCLINE(t):
    r'\|.*'
    if t.value[len(t.value) - 1] == '\r':
	t.value = t.value[1:-1]
    else:
	t.value = re.sub(r'\\(.)', r'\1', t.value[1:])
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
    t.lexer.skip(1)

#-----------------------------------------------------------------------
# Main, test
#-----------------------------------------------------------------------

def main():

    def usage():
	sys.stderr.write('%s $Revision: 2e39c16b3edc $\n' % sys.argv[0])
	sys.stderr.write('usage: cat2raw.py [flags] [input cat file]\n')
	sys.stderr.write('flags:\n')
	sys.stderr.write('-h, --help:              '
			 + 'output this message\n')
	sys.stderr.write('-o, --output=PATH:       '
			 + 'the output path/file '
			 + '(default is ./domain_name.raw)\n')
	sys.stderr.write('-r, --reversionary-mode: '
			 + 'output to previous standard (20040319)\n')

    try:
        opts, args = getopt.getopt\
	    (sys.argv[1:],
	     'ho:r',
	     ['help', 'output=', 'reversionary-mode'])
    except getopt.GetoptError:
        usage()
        sys.exit(1)

    global reversionary

    input = sys.stdin
    output = sys.stdout
    reversionary = 0
    path = '.'
    output_path = ''
    output_file = ''

    for o, v in opts:
	if o in ('-h', '--help'):
	    usage()
	    sys.exit()
	if o in ('-o', '--output'):
	    if os.path.isdir(v):
		output_path = os.path.abspath(v)
	    else:
		output_path = os.path.dirname(v)
		if not output_path: output_path = '.'
		output_file = os.path.basename(v)
	if o in ('-r', '--reversionary-mode'):
	    reversionary = 1

    if len(args) > 1:
        usage()
        sys.exit(1)
    if len(args) == 1:
	path = args[0]
	try:
	    input = open(path, 'r')
	except:
	    sys.stderr.write("couldn't open %s for input.\n" % path)
	    sys.exit(1)

    # create the lexer
    l = lex.lex()
    # connect it to the input cat file
    l.input(input.read())
    input.close()
    # create the parser
    p = yacc.yacc()
    # parse the input, creating a domain
    d = p.parse(lexer = l)
    # recursively load any child domains
    d.load_children(path)
    # output
    # by default, the output is to "the domain's normalized name".raw
    if output == sys.stdout:
	if not output_file:
	    output_file = re.sub(r'\s+', '_', d.object_name.strip()) + '.raw'
	n = os.path.join(output_path, output_file)
	try:
	    output = open(n, 'w')
	except:
	    sys.stderr.write("couldn't open %s for output.\n" % n)
	    sys.exit(1)

    d.emit(output)
    output.close()

if __name__ == '__main__':
    main()