#!/usr/bin/python
# -*- coding: utf-8 -*-

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

# Reads a TextUML file and converts it to an XMI-like format. The
# format is based on that used by ArgoUML, so may well have
# idiosyncracies; there is enough present to support ColdFrame's
# "normalize" utility.

# See http://abstratt.github.io/textuml/readme.html,
# https://github.com/abstratt/textuml.

# Uses PLY (http://www.dabeaz.com/ply/).

import ply.lex as lex
import ply.yacc as yacc
import time
import getopt
import os
import re
import sys

import xml.etree.ElementTree as ET


class Callable:
    # I think this was to support use via ant ...
    '''Jython doesn't have staticmethod(), so this acts as a replacement.
    See http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/52304'''
    def __init__(self, anycallable):
        self.__call__ = anycallable


# ----------------------------------------------------------------------
# Object model
# ----------------------------------------------------------------------


class Base:

    """The base class for all Objects retrieved from the .tuml file."""

    def __init__(self):
        """The aspects that every Object can be expected to have."""
        self.name = ''
        self.owner = None
        self.documentation = None
        self.annotations = ()
        self.modifiers = ()
        self.contents = ()

    def __str__(self):
        return "<%s %s/>" % (self.__class__.__name__, self.name)

    def __getattr__(self, n):
        """If the object doesn't have an attribute 'n', return None instead
        of run time error."""
        return None

    def path_name(self):
        """Returns the full path name of the object by following the
        self.owner link."""
        if self.owner is None:
            return ''
        elif self.owner.owner_name == '':
            return self.name
        else:
            return self.owner.path_name() + '.' + self.name

    def add_xml(self, to):
        """to is the ET.Element of the owner; add the necessary
        intermediate elements, then self."""
        pass

    def add_xml_path(self, to, elements):
        """Adds the path elements in 'elements' to 'to' and returns the
        final element. NB doesn't check whether the elements are already
        present."""
        result = to
        for e in elements:
            result = ET.SubElement(result, e)
        return result

    def add_xml_documentation(self, to):
        if self.documentation is not None:
            tv = self.add_xml_path(to, ('UML:ModelElement.taggedValue',
                                        'UML:TaggedValue'))
            tag_def = self.add_xml_path(tv, ('UML:TaggedValue.type',
                                             'UML:TagDefinition'))
            tag_def.set('name', 'documentation')
            data_value = self.add_xml_path(tv, ('UML:TaggedValue.dataValue',))
            data_value.text = self.documentation

    def add_xml_annotations(self, to):
        """Annotations are stereotypes and tagged values."""
        # self.annotations is a list,
        # ((stereotype-name, (tag-name, tag_value))))
        to.set('xmi.id', self.path_name())  # required by normalize_xmi - ?
        for a in self.annotations:
            self.add_xml_stereotype(to, a)

    def add_xml_stereotype(self, to, stereotype):
        """'stereotype' is a list (name, ((tag_name, tag_value)))."""
        st = self.add_xml_path(to, ('UML:ModelElement.stereotype',
                                    'UML:Stereotype'))
        st.set('name', stereotype[0])
        if len(stereotype[1]) > 0:
            tv = self.add_xml_path(to, ('UML:ModelElement.taggedValue',
                                        'UML:TaggedValue'))
            for t in stereotype[1]:
                typ = self.add_xml_path(tv, ('UML:TaggedValue.type',
                                             'UML:TagDefinition'))
                typ.set('name', t[0])
                dv = self.add_xml_path(tv, ('UML:TaggedValue.dataValue',))
                dv.text = t[1]

    def add_top_level_element_xml(self, to):
        """Adds the XML components common to all top level elements to the
        ET.Element 'to'."""
        to.set('name', self.name)
        self.add_xml_documentation(to)
        self.add_xml_annotations(to)

        # The modifiers for top level elements are visibility, abstract,
        # external, role.
        possible_visibilities = {'public', 'private', 'package', 'protected'}
        mods = set(self.modifiers)
        visibility_mods = possible_visibilities & mods
        if not visibility_mods:
            to.set('visibility', 'private')
        elif len(visibility_mods) == 1:
            for v in visibility_mods:
                if v == 'package':
                    to.set('visibility', 'public')
                else:
                    to.set('visibility', v)
        if to.get('visibility') == 'public':
            pub = self.add_xml_path(to, ('UML:ModelElement.stereotype',
                                          'UML:Stereotype'))
            pub.set('name', 'public')
        mods = mods - visibility_mods
        if 'abstract' in mods:
            to.set('isAbstract', 'true')
        else:
            to.set('isAbstract', 'false')
        mods.discard('abstract')
        if len(mods) > 0:
            for m in mods:
                warning("unsupported modifier %s in %s\n"
                        % (m, self.path_name()))
            sys.exit(1)


class Action(Base):
    """Part of State. The only Actions we care about are entry actions."""
    pass


class Action_Time(Base):
    """Part of State. Indicates when the action occurs; the only ones we
    care about are entry actions."""
    pass


class Association(Base):

    def add_xml(self, to):
        ass = self.add_xml_path(to, ('UML:Association',))
        self.add_top_level_element_xml(ass)
        self.add_xml_association(ass)

    def add_xml_association(self, to):
        """adds the UML:Association.connection, with the two
        UML:AssociationEnd's to the ET.Element in to."""
        con = self.add_xml_path(to, ('UML:Association.connection',))
        for role in self.roles:
            role.owner = self
            role.add_xml(con)


class Association_Class(Association):

    def add_xml(self, to):
        ass = self.add_xml_path(to, ('UML:AssociationClass',))
        self.add_top_level_element_xml(ass)
        self.add_xml_association(ass)
        ass.set('isActive', 'false')  # XXX
        cnt = self.add_xml_path(ass, ('UML:Classifier.feature',))
        for c in self.contents:
            c.owner = self
            c.add_xml(cnt)


class Attribute(Base):

    def add_xml(self, to):
        att = self.add_xml_path(to, ('UML:Attribute',))
        att.set('name', self.name)
        self.add_xml_documentation(att)
        self.add_xml_annotations(att)
        # The modifiers for attributes are visibility, static,
        # abstract, derived, readonly, id (clearly not all apply to
        # attributes!)
        possible_visibilities = {'public', 'private', 'package', 'protected'}
        mods = set(self.modifiers)
        visibility_mods = possible_visibilities & mods
        if not visibility_mods:
            att.set('visibility', 'public')
        elif len(visibility_mods) == 1:
            for v in visibility_mods:
                if v == 'package':
                    att.set('visibility', 'public')
                else:
                    att.set('visibility', v)
        mods = mods - visibility_mods
        if 'id' in mods:
            ident = self.add_xml_path(att, ('UML:ModelElement.stereotype',
                                            'UML:Stereotype'))
            ident.set('name', 'id')
        mods.discard('id')
        if 'static' in mods:
            att.set('ownerScope', 'classifier')
        else:
            att.set('ownerScope', 'instance')
        mods.discard('static')
        if len(mods) > 0:
            for m in mods:
                warning("unsupported modifier %s in %s\n"
                        % (m, self.path_name()))
            sys.exit(1)
        typ = self.add_xml_path(att, ('UML:StructuralFeature.type',
                                      'UML:DataType'))
        typ.set('name', self.type)
        if self.initial_value is not None:
            iv = self.add_xml_path(att, ('UML:Attribute.initialValue',
                                         'UML:Expression'))
            iv.set('body', self.initial_value)


class Class(Base):

    def add_xml(self, to):
        cls = self.add_xml_path(to, ('UML:Class',))
        cls.set('name', self.name)
        self.add_top_level_element_xml(cls)
        cls.set('isActive', 'false')  # XXX
        cnt = self.add_xml_path(cls, ('UML:Classifier.feature',))
        for c in self.contents:
            c.owner = self
            c.add_xml(cnt)


class Class_Utility(Class):
    pass


class Datatype(Base):

    def add_xml(self, to):
        dtp = self.add_xml_path(to, ('UML:DataType',))
        self.add_top_level_element_xml(dtp)


class Enumeration(Base):
    """self.literals is a tuple containing the literals."""

    def add_xml(self, to):
        en = self.add_xml_path(to, ('UML:Enumeration',))
        self.add_top_level_element_xml(en)
        lits = self.add_xml_path(en, ('UML:Enumeration.literal',))
        for e in self.literals:
            lit = self.add_xml_path(lits, ('UML:EnumerationLiteral',))
            lit.set('name', e[1])


class Event(Base):
    # I expect all the output will be managed elsewhere ...
    pass


class Inheritance(Base):
    # All the work for Inheritance is done in Class (because part of
    # the output is the parent, and that's not readily available).
    pass


class Model(Base):

    def add_xml(self, to):
        model = self.add_xml_path(to, ('UML:Model',))
        model.set('name', self.name)
        self.add_xml_documentation(model)
        contents = self.add_xml_path(model, ('UML:Namespace.ownedElement',))
        for c in self.contents:
            c.owner = self
            c.add_xml(contents)

    def path_name(self):
        '''At the root of the tree, so no owner; so the path name is
        just the name.'''
        return self.name


class Operation(Base):

    def add_xml(self, to):
        opn = self.add_xml_path(to, ('UML:Operation',))
        opn.set('name', self.name)
        self.add_xml_documentation(opn)
        self.add_xml_annotations(opn)
        # The modifiers for operations are visibility, static,
        # abstract, derived, readonly, id (clearly the last 3 only
        # apply to attributes!)
        possible_visibilities = {'public', 'private', 'package', 'protected'}
        mods = set(self.modifiers)
        visibility_mods = possible_visibilities & mods
        if not visibility_mods:
            opn.set('visibility', 'public')
        elif len(visibility_mods) == 1:
            for v in visibility_mods:
                if v == 'package':
                    opn.set('visibility', 'public')
                else:
                    opn.set('visibility', v)
        mods = mods - visibility_mods
        if 'abstract' in mods:
            opn.set('isAbstract', 'true')
        else:
            opn.set('isAbstract', 'false')
        mods.discard('abstract')
        if 'static' in mods:
            opn.set('ownerScope', 'classifier')
        else:
            opn.set('ownerScope', 'instance')
        mods.discard('static')
        if len(mods) > 0:
            for m in mods:
                sys.stderr.write("unsupported modifier %s in %s\n"
                                 % (m, self.path_name()))
            sys.exit(1)
        pars = self.add_xml_path(opn, ('UML:BehavioralFeature.parameter',))
        for p in self.parameters:
            p.owner = self
            p.add_xml(pars)
        if self.rtn is not None:
            rtn = self.add_xml_path(pars, ('UML:Parameter',))
            rtn.set('name', 'return')
            rtn.set('kind', 'return')
            typ = self.add_xml_path(rtn, ('UML:Parameter.type',
                                          'UML:DataType'))
            typ.set('name', self.rtn)


class Package(Base):

    def add_xml(self, to):
        pkg = self.add_xml_path(to, ('UML:Package',))
        self.add_top_level_element_xml(pkg)
        cnt = self.add_xml_path(pkg, ('UML:Namespace.ownedElement',))
        for c in self.contents:
            c.owner = self
            c.add_xml(cnt)


class Parameter(Base):

    def add_xml(self, to):
        par = self.add_xml_path(to, ('UML:Parameter',))
        par.set('name', self.name)
        self.add_xml_documentation(par)
        self.add_xml_annotations(par)
        # The modifiers for parameters are in, out, inout, read,
        # create, update, delete. Only the first three matter here.
        possible_mode_mods = {'in', 'out', 'inout'}
        mods = set(self.modifiers)
        mode_mods = possible_mode_mods & mods
        if not mode_mods:
            par.set('kind', 'in')
        elif len(mode_mods) == 1:
            for m in mode_mods:
                par.set('kind', m)
        else:
            error("too many modifiers %s on parameter %s"
                  % (mode_mods, self.path_name()))
        mods = mode_mods - possible_mode_mods
        if len(mods) > 0:
            for m in mods:
                warning("unsupported modifier %s in %s\n"
                        % (m, self.path_name))
            sys.exit(1)
        typ = self.add_xml_path(par, ('UML:Parameter.type', 'UML:DataType'))
        typ.set('name', self.type)
        if self.default_value is not None:
            dv = self.add_xml_path(par, ('UML:Parameter.defaultValue',
                                         'UML:Expression'))
            dv.set('body', self.default_value)


class Role(Base):

    def add_xml(self, to):
        asnd = self.add_xml_path(to, ('UML:AssociationEnd',))
        asnd.set('name', self.name)
        self.add_xml_documentation(asnd)
        self.add_xml_annotations(asnd)
        part = self.add_xml_path(asnd, ('UML:AssociationEnd.participant',
                                        'UML:Class'))
        part.set('name', self.target)
        mult = self.add_xml_path(asnd, ('UML:AssociationEnd.multiplicity',
                                        'UML:Multiplicity',
                                        'UML:Multiplicity.range',
                                        'UML:MultiplicityRange'))
        if isinstance(self.multiplicity, tuple):
            mult.set('lower', self.multiplicity[0])
            upper = self.multiplicity[1]
            if upper == '*' or upper == 'n':
                mult.set('upper', '-1')
            else:
                mult.set('upper', upper)
        elif self.multiplicity == '*' or self.multiplicity == 'n':
            mult.set('lower', '0')
            mult.set('upper', '-1')
        else:
            mult.set('lower', self.multiplicity)
            mult.set('upper', self.multiplicity)


class State_Machine(Base):
    pass


class State(Base):
    pass


class Transition(Base):
    pass


class Uses_Relationship(Base):
    # Used for typed events only.
    pass


# ----------------------------------------------------------------------
# Utilities
# ----------------------------------------------------------------------


def object_name(fqn):
    """Strips the leading model path from a fully qualified name."""
    last_sep = fqn.rfind('::')
    if last_sep >= 0:
        return fqn[last_sep + 2:]
    else:
        return fqn


# ----------------------------------------------------------------------
# Parser
# ----------------------------------------------------------------------

def p_start(p):
    '''
    start : \
        model_comment_opt annotations_opt package_heading \
            global_directive_section_opt namespace_contents END DOT
    '''
    if p[3][0] == 'model':
        p[0] = Model()
        p[0].name = p[3][1]
        p[0].documentation = p[1]
        p[0].annotations = p[2]
        p[0].contents = p[5]
    else:
        error("ERROR! expecting 'model', got '%s'" % p[3][0])


def p_package_heading(p):
    '''
    package_heading : package_type qualified_identifier SEMICOLON
    '''
    # (type, name)
    p[0] = (p[1], p[2])


def p_package_type(p):
    '''
    package_type \
        : MODEL
        | PACKAGE
        | PROFILE
    '''
    p[0] = p[1]


def p_qualified_identifier_list(p):
    '''
    qualified_identifier_list \
        : qualified_identifier COMMA qualified_identifier_list
        | qualified_identifier
    '''
    if len(p) == 4:
        p[0] = (p[1],) + p[3]
    else:
        p[0] = (p[1],)


def p_qualified_identifier(p):
    '''
    qualified_identifier \
        : IDENTIFIER NAMESPACE_SEPARATOR qualified_identifier
        | IDENTIFIER
    '''
    if len(p) == 4:
        p[0] = p[1] + '::' + p[3]
    else:
        p[0] = p[1]


def p_global_directive_section_opt(p):
    '''
    global_directive_section_opt \
        : global_directive_section
        | empty
    '''
    # XXX
    pass


def p_global_directive_section(p):
    '''
    global_directive_section \
        : global_directive global_directive_section
        | global_directive
    '''
    if len(p) == 3:
        p[0] = (p[1],) + p[2]
    else:
        p[0] = (p[1],)


def p_global_directive(p):
    '''
    global_directive \
        : load_decl
        | apply_profile_decl
        | import_decl
    '''
    # XXX
    pass


def p_load_decl(p):
    '''
    load_decl : LOAD
    '''
    # XXX
    pass


def p_apply_profile_decl(p):
    '''
    apply_profile_decl : APPLY
    '''
    # XXX
    pass


def p_import_decl(p):
    '''
    import_decl : IMPORT
    '''
    # XXX
    pass


def p_optional_alias(p):
    '''
    optional_alias : ALIAS IDENTIFIER
                   | empty
    '''
    # XXX
    pass


def p_namespace_contents(p):
    '''
    namespace_contents \
        : top_level_element namespace_contents
        | top_level_element
    '''
    if len(p) == 3:
        p[0] = (p[1],) + p[2]
    else:
        p[0] = (p[1],)


def p_sub_namespace(p):
    '''
    sub_namespace : package_heading namespace_contents END SEMICOLON
    '''
    # (package_heading, namespace_contents)
    if p[1][0] == 'package':
        p[0] = Package()
        p[0].name = p[1][1]
        p[0].contents = p[2]
    else:
        p[0] = (p[1], p[2])


# Top-level elements


def p_top_level_element(p):
    '''
    top_level_element \
        : model_comment_opt annotations_opt top_level_element_choice
    '''
    if isinstance(p[3], Base):
        p[0] = p[3]
        p[0].documentation = p[1]
        # Add any annotations to the end of existing ones (e.g., at
        # the time of writing, a textuml datatype implemented as a
        # Class stereotyped <<datatype>>).
        p[0].annotations += p[2]
    else:
        # (model_comment_opt, annotations_opt, top_level_element)
        p[0] = (p[1], p[2], p[3])


def p_top_level_element_choice(p):
    '''
    top_level_element_choice \
        : class_def
        | association_def
        | association_class_def
        | enumeration_def
        | primitive_def
        | sub_namespace
    '''
    # XXX textuml.scc has enumeration as a class_type. I suppose this
    # would allow you to specify operations - but how would you
    # specify the literals? Ugh. Not documented. Not clear!
    # association_def stereotype_def detached_operation_def
    # function_decl
    p[0] = p[1]


# Type identifiers


def p_minimal_type_identifier(p):
    '''
    minimal_type_identifier : qualified_identifier
    '''
    p[0] = p[1]


def p_single_type_identifier(p):
    '''
    single_type_identifier \
        : minimal_type_identifier
        | ANY
        | tuple_type
    '''
    # XXX template_binding?
    p[0] = p[1]


def p_minimal_type_identifier_list(p):
    '''
    minimal_type_identifier_list \
        : minimal_type_identifier COMMA minimal_type_identifier_list
        | minimal_type_identifier
    '''
    if len(p) == 4:
        p[0] = (p[1],) + p[3]
    else:
        p[0] = (p[1],)


def p_type_identifier(p):
    '''
    type_identifier \
        : single_type_identifier optional_multiplicity
        | function_signature optional_multiplicity
    '''
    p[0] = (p[1], p[2])


def p_optional_multiplicity(p):
    '''
    optional_multiplicity \
        : L_BRACKET multiplicity_spec R_BRACKET multiplicity_constraints
        | L_BRACKET multiplicity_spec R_BRACKET
        | empty
    '''
    if len(p) == 5:
        p[0] = (p[2], p[4])
    elif len(p) == 4:
        p[0] = (p[2],)


def p_multiplicity_spec(p):
    '''
    multiplicity_spec \
        : multiplicity_value COMMA multiplicity_value
        | multiplicity_value
    '''
    # If we return a list, => (lower_bound, upper_bound)
    if len(p) == 4:
        p[0] = (p[1], p[3])
    else:
        p[0] = p[1]


def p_multiplicity_constraints(p):
    '''
    multiplicity_constraints \
        : L_CURLY_BRACKET multiplicity_constraint_list R_CURLY_BRACKET
    '''
    p[0] = p[2]


def p_multiplicity_constraint_list(p):
    '''
    multiplicity_constraint_list \
        : multiplicity_constraint COMMA multiplicity_constraint_list
        | multiplicity_constraint
    '''
    if len(p) == 4:
        p[0] = (p[1],) + p[3]
    else:
        p[0] = (p[1],)


def p_multiplicity_constraint(p):
    '''
    multiplicity_constraint \
        : ORDERED
        | UNORDERED
        | UNIQUE
        | NONUNIQUE
    '''
    p[0] = p[1]


# Associations


def p_association_def(p):
    '''
    association_def \
        : annotations_opt ASSOCIATION IDENTIFIER association_role_decl_list \
            END SEMICOLON
    '''
    # No aggregation, composition
    p[0] = Association()
    p[0].name = p[3]
    p[0].annotations = p[1]
    p[0].roles = p[4]


def p_association_class_def(p):
    '''
    association_class_def \
        : annotations_opt ASSOCIATION_CLASS IDENTIFIER \
            association_role_decl_list feature_decl_list \
            END SEMICOLON
        | annotations_opt ASSOCIATION_CLASS IDENTIFIER \
            association_role_decl_list \
            END SEMICOLON
    '''
    # No aggregation, composition
    p[0] = Association_Class()
    p[0].name = p[3]
    p[0].annotations = p[1]
    p[0].roles = p[4]
    if len(p) == 8:
        p[0].contents = p[5]
    else:
        p[0].contents = ()


def p_association_role_decl_list(p):
    '''
    association_role_decl_list \
        : association_role_decl association_role_decl
    '''
    # must be two
    p[0] = (p[1], p[2])


def p_association_role_decl(p):
    '''
    association_role_decl \
        : model_comment_opt annotations_opt \
            IDENTIFIER IDENTIFIER IDENTIFIER association_multiplicity SEMICOLON
    '''
    # The first identifier is the originating class, the second the
    # verb phrase, the third is the target class.
    # e.g. window is_displayed_on screen[0, 1]
    # returns (cmt, anno, orig, verb, targ, mult)
    p[0] = Role()
    p[0].documentation = p[1]
    p[0].annotation = p[2]
    p[0].origin = p[3]
    p[0].name = p[4]
    p[0].target = p[5]
    p[0].multiplicity = p[6]


def p_association_multiplicity(p):
    '''
    association_multiplicity \
        : L_BRACKET multiplicity_spec R_BRACKET
    '''
    # returns either a single string (e.g. "1", "*") or a tuple (lb, ub)
    p[0] = p[2]


# Classes and interfaces


def p_class_def(p):
    '''
    class_def : class_header feature_decl_list END SEMICOLON
    '''
    p[0] = Class()
    p[0].name = p[1][2]
    p[0].modifiers = p[1][0]
    p[0].contents = p[2]
    # ColdFrame, because of ArgoUML features, represents a data type
    # with attributes to be implemented as a class but with the
    # stereotype <<datatype>>.
    if p[1][1] == 'datatype':
        p[0].annotations = (('datatype', ()),)


def p_class_header(p):
    '''
    class_header : class_modifiers class_type IDENTIFIER
    '''
    # XXX optional_formal_template_parameters
    # class_specializes_section class_implements_section
    p[0] = (p[1], p[2], p[3])


def p_class_modifiers(p):
    '''
    class_modifiers \
        : class_modifier_list
        | empty
    '''
    if p[1] is None:
        p[0] = ()
    else:
        p[0] = p[1]


def p_class_modifier_list(p):
    '''
    class_modifier_list \
        : class_modifier class_modifier_list
        | class_modifier
    '''
    if len(p) == 3:
        p[0] = (p[1],) + p[2]
    else:
        p[0] = (p[1],)


def p_class_modifier(p):
    '''
    class_modifier \
        : visibility_modifier
        | ABSTRACT
        | EXTERNAL
        | ROLE
    '''
    p[0] = p[1]


def p_class_type(p):
    '''
    class_type \
        : CLASS
        | INTERFACE
        | DATATYPE
        | ACTOR
        | SIGNAL
        | COMPONENT
        | ENUMERATION
    '''
    p[0] = p[1]


# Class features


def p_feature_decl_list(p):
    '''
    feature_decl_list \
        : feature_decl feature_decl_list
        | feature_decl
    '''
    if len(p) == 3:
        p[0] = (p[1],) + p[2]
    else:
        p[0] = (p[1],)


def p_feature_decl(p):
    '''
    feature_decl \
        : model_comment_opt annotations_opt modifiers_opt feature_type
    '''
    p[0] = p[4]
    p[0].documentation = p[1]
    p[0].annotations = p[2]
    p[0].modifiers = p[3]


def p_modifiers_opt(p):
    '''
    modifiers_opt \
        : modifier_list
        | empty
    '''
    if p[1] is None:
        p[0] = ()
    else:
        p[0] = p[1]


def p_modifier_list(p):
    '''
    modifier_list \
        : modifier modifier_list
        | modifier
    '''
    if len(p) == 3:
        p[0] = (p[1],) + p[2]
    else:
        p[0] = (p[1],)


def p_modifier(p):
    '''
    modifier \
        : visibility_modifier
        | STATIC
        | ABSTRACT
        | DERIVED
        | READONLY
        | ID
    '''
    p[0] = p[1]


def p_visibility_modifier(p):
    '''
    visibility_modifier \
        : PUBLIC
        | PRIVATE
        | PACKAGE
        | PROTECTED
    '''
    p[0] = p[1]


def p_feature_type(p):
    '''
    feature_type \
        : operation_decl
        | attribute_decl
    '''
    # XXX lots missing!
    p[0] = p[1]


def p_operation_decl(p):
    '''
    operation_decl : operation_header SEMICOLON
    '''
    # XXX operation_constraint* optional_behavioral_feature_body
    p[0] = Operation()
    p[0].name = p[1][1]
    p[0].parameters = p[1][2][0]
    # p[0].rtn = p[1][2][1]
    # rtn is (annotations_opt, (single_type_identifier optional_multiplicity))
    if p[1][2][1] is not None:
        p[0].rtn = p[1][2][1][1][0]


def p_operation_header(p):
    '''
    operation_header : operation_keyword IDENTIFIER signature
    '''
    # XXX QUERY wildcard_types_opt
    p[0] = (p[1], p[2], p[3])


def p_operation_keyword(p):
    '''
    operation_keyword : OPERATION
    '''
    # XXX QUERY
    p[0] = p[1]


def p_attribute_decl(p):
    '''
    attribute_decl \
        : ATTRIBUTE IDENTIFIER COLON type_identifier \
            initialization_expression_opt SEMICOLON
    '''
    # XXX optional_subsetting attribute_invariant
    p[0] = Attribute()
    p[0].name = p[2]
    p[0].type = p[4][0]   # omit multiplicity - really?
    p[0].initial_value = p[5]


def p_initialization_expression_opt(p):
    '''
    initialization_expression_opt \
        : initialization_expression
        | empty
    '''
    p[0] = p[1]


def p_initialization_expression(p):
    '''
    initialization_expression : ASSIGNOP simple_initialization
    '''
    # XXX expression_block
    p[0] = p[2]


def p_simple_initialization(p):
    '''
    simple_initialization : literal_or_identifier
    '''
    p[0] = p[1]


# Tuple types


def p_tuple_type(p):
    '''
    tuple_type \
        : L_CURLY_BRACKET tuple_type_slots R_CURLY_BRACKET
    '''
    p[0] = p[2]


def p_tuple_type_slots(p):
    '''
    tuple_type_slots \
    : tuple_type_slot COMMA tuple_type_slots
    | tuple_type_slot
    '''
    if len(p) == 4:
        p[0] = (p[1],) + p[3]
    else:
        p[0] = (p[1],)


def p_tuple_type_slot(p):
    '''
    tuple_type_slot \
        : IDENTIFIER COLON type_identifier
        | COLON type_identifier
    '''
    if len(p) == 4:
        p[0] = (p[1], p[3])
    else:
        p[0] = ('', p[2])


# Signatures


def p_function_signature(p):
    '''
    function_signature : L_CURLY_BRACKET simple_signature R_CURLY_BRACKET
    '''
    p[0] = p[2]


def p_signature(p):
    '''
    signature : L_PAREN param_decl_list R_PAREN optional_return_type
    '''
    # XXX optional_raises_section
    # (param_decl_list, optional_return_type)
    p[0] = (p[2], p[4])


def p_simple_signature(p):
    '''
    simple_signature \
        : L_PAREN simple_param_decl_list R_PAREN simple_optional_return_type
        | L_PAREN simple_param_decl_list R_PAREN
    '''
    if len(p) == 5:
        p[0] = (p[2], p[4])
    else:
        p[0] = (p[2], None)


def p_optional_return_type(p):
    '''
    optional_return_type \
        : annotations_opt simple_optional_return_type
        | empty
    '''
    if len(p) == 3:
        p[0] = (p[1], p[2])
    else:
        p[0] = None


def p_simple_optional_return_type(p):
    '''
    simple_optional_return_type : COLON type_identifier
    '''
    p[0] = p[2]


def p_param_decl_list(p):
    '''
    param_decl_list \
        : param_decl COMMA param_decl_list
        | param_decl
        | empty
    '''
    if p[1] is None:
        p[0] = ()
    elif len(p) == 2:
        p[0] = (p[1],)
    else:
        p[0] = (p[1],) + p[3]


def p_simple_param_decl_list(p):
    '''
    simple_param_decl_list \
        : simple_param_decl COMMA simple_param_decl_list
        | simple_param_decl
        | empty
    '''
    if p[1] is None:
        p[0] = ()
    elif len(p) == 2:
        p[0] = (p[1],)
    else:
        p[0] = (p[1],) + p[3]


def p_param_decl(p):
    '''
    param_decl : annotations_opt parameter_modifiers simple_param_decl
    '''
    p[0] = p[3]
    p[0].annotations = p[1]
    p[0].modifiers = p[2]


def p_simple_param_decl(p):
    '''
    simple_param_decl \
        : optional_parameter_name COLON type_identifier \
            initialization_expression_opt
    '''
    p[0] = Parameter()
    p[0].name = p[1]
    p[0].type = p[3][0]  # XXX omits multiplicity
    p[0].default_value = p[4]


def p_optional_parameter_name(p):
    '''
    optional_parameter_name \
        : IDENTIFIER
        | empty
    '''
    p[0] = p[1]


def p_parameter_modifiers(p):
    '''
    parameter_modifiers \
        : parameter_modifier parameter_modifiers
        | empty
    '''
    if p[1] is None:
        p[0] = ()
    else:
        p[0] = (p[1],) + p[2]


def p_parameter_modifier(p):
    '''
    parameter_modifier \
        : IN
        | OUT
        | INOUT
        | READ
        | CREATE
        | UPDATE
        | DELETE
    '''
    p[0] = p[1]


# Annotations


def p_annotations_opt(p):
    '''
    annotations_opt \
        : annotations
        | empty
    '''
    if p[1] is None:
        p[0] = ()
    else:
        p[0] = p[1]


def p_annotations(p):
    '''
    annotations : L_BRACKET annotation_list R_BRACKET
    '''
    p[0] = p[2]


def p_annotation_list(p):
    '''
    annotation_list \
        : annotation COMMA annotation_list
        | annotation
    '''
    if len(p) == 4:
        p[0] = (p[1],) + p[3]
    else:
        p[0] = (p[1],)


def p_annotation(p):
    '''
    annotation \
        : qualified_identifier annotation_optional_value_specs
        | qualified_identifier
    '''
    if len(p) == 3:
        p[0] = (p[1], p[2])  # NB the second element is a tuple
    else:
        p[0] = (p[1], ())


def p_annotation_optional_value_specs(p):
    '''
    annotation_optional_value_specs \
        : L_PAREN annotation_value_spec_list R_PAREN
    '''
    p[0] = p[2]


def p_annotation_value_spec_list(p):
    '''
    annotation_value_spec_list \
    : annotation_value_spec COMMA annotation_value_spec_list
    | annotation_value_spec
    '''
    if len(p) == 4:
        p[0] = (p[1],) + p[3]
    else:
        p[0] = (p[1],)


def p_annotation_value_spec(p):
    '''
    annotation_value_spec : IDENTIFIER EQUALS annotation_value
    '''
    p[0] = (p[1], p[3])


def p_annotation_value(p):
    '''
    annotation_value \
        : literal
        | IDENTIFIER
    '''
    p[0] = p[1]


# Enumeration definition

# XXX I had a lot of grief with reduce/reduce conflicts when I added
# enumeration_def; I was using data_type_modifiers then. Hence the
# explicit restriction to one visibility_modifier; now I just have a
# shift/reduce conflict, which is resolved correctly. Still ...

def p_enumeration_def(p):
    '''
    enumeration_def \
        : visibility_modifier ENUMERATION IDENTIFIER \
            enumeration_literal_decl_list END SEMICOLON
        | ENUMERATION IDENTIFIER \
            enumeration_literal_decl_list END SEMICOLON
    '''
    p[0] = Enumeration()
    if len(p) == 7:
        p[0].name = p[3]
        p[0].modifiers = (p[1],)
        p[0].literals = p[4]
    else:
        p[0].name = p[2]
        p[0].literals = p[3]  # elements are (documentation, name)


def p_enumeration_literal_decl_list(p):
    '''
    enumeration_literal_decl_list \
        : enumeration_literal_decl enumeration_literal_decl_list_tail
    '''
    p[0] = (p[1],) + p[2]


def p_enumeration_literal_decl(p):
    '''
    enumeration_literal_decl : model_comment_opt IDENTIFIER
    '''
    p[0] = (p[1], p[2])


def p_enumeration_literal_decl_list_tail(p):
    '''
    enumeration_literal_decl_list_tail \
        : COMMA enumeration_literal_decl_list
        | empty
    '''
    if len(p) == 2:
        p[0] = ()
    else:
        p[0] = p[2]


# Primitive definition


def p_primitive_def(p):
    '''
    primitive_def \
        : visibility_modifier PRIMITIVE IDENTIFIER SEMICOLON
        | PRIMITIVE IDENTIFIER SEMICOLON
    '''
    # removed the textuml.scc annotations? - added in p_top_level_element(p)
    # NB textuml doesn't allow modifiers on primitives! (fixed - issue #120)
    if len(p) == 5:
        p[0] = Datatype()
        p[0].name = p[3]
        p[0].modifiers = (p[1],)
    else:
        p[0] = Datatype()
        p[0].name = p[2]


def p_data_type_modifiers(p):
    '''
    data_type_modifiers \
        : data_type_modifier_list
        | empty
    '''
    if p[1] is None:
        p[0] = ()
    else:
        p[0] = p[1]


def p_data_type_modifier_list(p):
    '''
    data_type_modifier_list \
        : data_type_modifier data_type_modifier_list
        | data_type_modifier
    '''
    if len(p) == 3:
        p[0] = (p[1],) + p[2]
    else:
        p[0] = (p[1],)


def p_data_type_modifier(p):
    '''
    data_type_modifier \
        : visibility_modifier
    '''
    # XXX EXTERNAL
    p[0] = p[1]


# Closures

# Expressions

# Miscellaneous stuff


def p_model_comment_opt(p):
    '''
    model_comment_opt \
        : MODEL_COMMENT
        | empty
    '''
    p[0] = p[1]


def p_literal(p):
    '''
    literal \
        : boolean
        | number
        | STRING
        | NULL
    '''
    p[0] = p[1]


def p_literal_or_identifier(p):
    '''
    literal_or_identifier \
        : literal
        | IDENTIFIER
    '''
    p[0] = p[1]


def p_boolean(p):
    '''
    boolean \
        : TRUE
        | FALSE
    '''
    p[0] = p[1]


def p_number(p):
    '''
    number \
        : INTEGER
        | REAL
    '''
    p[0] = p[1]


def p_multiplicity_value(p):
    '''
    multiplicity_value \
    : INTEGER
    | MULT
    '''
    p[0] = p[1]


# Empty productions


def p_empty(p):
    'empty :'
    pass


def p_error(p):
    '''Panic mode recovery.'''
    if not p:
        warning("That seems to be it.")
        return None
    text = lexer.lexdata
    last_cr = text.rfind('\n', 0, p.lexpos)
    if last_cr < 0:
        last_cr = 0
    print last_cr
    column = (p.lexpos - last_cr) - 1
    error("Syntax error at %s on line %d:%d" % (p.type, p.lineno, column))
    # # Read ahead looking for a terminating ";" or "."
    # while 1:
    #     tok = parser.token()             # Get the next token
    #     if not tok:
    #         break
    #     if tok.type == 'SEMICOLON' or tok.type == 'DOT':
    #         tok = parser.token()         # Skip it
    #         break
    # parser.errok()
    # # Return the token (or nil) to the parser as the next lookahead
    # # token
    # return tok


# ----------------------------------------------------------------------
# Lexer
# ----------------------------------------------------------------------


reserved = {
    'abstract': 'ABSTRACT',
    'access': 'ACCESS',
    'actor': 'ACTOR',
    'aggregation': 'AGGREGATION',
    'alias': 'ALIAS',
    'allow': 'ALLOW',
    'all': 'ALL',
    'and': 'AND',
    'any': 'ANY',
    'anyone': 'ANYONE',
    'apply': 'APPLY',
    'association': 'ASSOCIATION',
    'association_class': 'ASSOCIATION_CLASS',
    'as': 'AS',
    'attribute': 'ATTRIBUTE',
    'begin': 'BEGIN',
    'broadcast': 'BROADCAST',
    'by': 'BY',
    'call': 'CALL',
    'catch': 'CATCH',
    'class': 'CLASS',
    'component': 'COMPONENT',
    'composition': 'COMPOSITION',
    'connector': 'CONNECTOR',
    'create': 'CREATE',
    'datatype': 'DATATYPE',
    'delete': 'DELETE',
    'deny': 'DENY',
    'dependency': 'DEPENDENCY',
    'derived': 'DERIVED',
    'destroy': 'DESTROY',
    'do': 'DO',
    'else': 'ELSE',
    'elseif': 'ELSEIF',
    'end': 'END',
    'entry': 'ENTRY',
    'enumeration': 'ENUMERATION',
    'exit': 'EXIT',
    'extends': 'EXTENDS',
    'extent': 'EXTENT',
    'external': 'EXTERNAL',
    'false': 'FALSE',
    'final': 'FINAL',
    'finally': 'FINALLY',
    'function': 'FUNCTION',
    'id': 'ID',
    'if': 'IF',
    'implements': 'IMPLEMENTS',
    'import': 'IMPORT',
    'in': 'IN',
    'initial': 'INITIAL',
    'inout': 'INOUT',
    'interface': 'INTERFACE',
    'invariant': 'INVARIANT',
    'is': 'IS',
    'link': 'LINK',
    'literal': 'ENUMERATION_LITERAL',
    'load': 'LOAD',
    'model': 'MODEL',
    'navigable': 'NAVIGABLE',
    'new': 'NEW',
    'none': 'NONE',
    'nonunique': 'NONUNIQUE',
    'not': 'NOT',
    'null': 'NULL',
    'on': 'ON',
    'operation': 'OPERATION',
    'opposite': 'OPPOSITE',
    'or': 'OR',
    'ordered': 'ORDERED',
    'out': 'OUT',
    'package': 'PACKAGE',
    'port': 'PORT',
    'postcondition': 'POSTCONDITION',
    'precondition': 'PRECONDITION',
    'primitive': 'PRIMITIVE',
    'private': 'PRIVATE',
    'profile': 'PROFILE',
    'property': 'PROPERTY',
    'protected': 'PROTECTED',
    'provided': 'PROVIDED',
    'public': 'PUBLIC',
    'query': 'QUERY',
    'raise': 'RAISE',
    'raises': 'RAISES',
    'read': 'READ',
    'readonly': 'READONLY',
    'reception': 'RECEPTION',
    'reference': 'REFERENCE',
    'repeat': 'REPEAT',
    'required': 'REQUIRED',
    'return': 'RETURN',
    'role': 'ROLE',
    'self': 'SELF',
    'send': 'SEND',
    'signal': 'SIGNAL',
    'specializes': 'SPECIALIZES',
    'state': 'STATE',
    'statemachine': 'STATEMACHINE',
    'static': 'STATIC',
    'stereotype': 'STEREOTYPE',
    'subsets': 'SUBSETS',
    'terminate': 'TERMINATE',
    'then': 'THEN',
    'to': 'TO',
    'transition': 'TRANSITION',
    'true': 'TRUE',
    'try': 'TRY',
    'type': 'TYPE',
    'unique': 'UNIQUE',
    'unlink': 'UNLINK',
    'unordered': 'UNORDERED',
    'until': 'UNTIL',
    'update': 'UPDATE',
    'var': 'VAR',
    'when': 'WHEN',
    'where': 'WHERE',
    'while': 'WHILE'
}


tokens = (
    # arithmetic symbols
    'PLUS',
    'MINUS',
    'MULT',
    'DIV',
    'ASSIGNOP',

    # relational symbols
    'EQUALS',
    'EQUALS_EQUALS',
    'LT',
    'LE',
    'GT',
    'GE',

    # separator symbols
    'COMMA',
    'COLON',
    'SEMICOLON',
    'DOT',
    'NAMESPACE_SEPARATOR',
    'HASH',
    'L_PAREN',
    'R_PAREN',
    'L_BRACKET',
    'R_BRACKET',
    'L_CURLY_BRACKET',
    'R_CURLY_BRACKET',
    'LEFT_ARROW',
    'RIGHT_ARROW',
    'L_GUILLEMET',
    'R_GUILLEMET',
    'NOT_NULL',

    # identifiers
    'IDENTIFIER',

    # numbers
    'INTEGER',
    'REAL',

    # strings
    'STRING',

    # comments
    'COMMENT',
    'MODEL_COMMENT',
) + tuple(reserved.values())

t_PLUS = r'\+'
t_MINUS = r'-'
t_MULT = r'\*'
t_DIV = r'/'
t_ASSIGNOP = r':='

t_EQUALS = r'='
t_EQUALS_EQUALS = r'=='
t_LT = r'<'
t_LE = r'<='
t_GT = r'>'
t_GE = r'>='

t_COMMA = r','
t_COLON = r':'
t_SEMICOLON = r';'
t_DOT = r'.'
t_NAMESPACE_SEPARATOR = r'::'
t_HASH = r'\#'
t_L_PAREN = r'\('
t_R_PAREN = r'\)'
t_L_BRACKET = r'\['
t_R_BRACKET = r']'
t_L_CURLY_BRACKET = r'{'
t_R_CURLY_BRACKET = r'}'
t_LEFT_ARROW = r'<-'
t_RIGHT_ARROW = r'->'
t_L_GUILLEMET = r'«'
t_R_GUILLEMET = r'»'
t_NOT_NULL = r'\?'

t_INTEGER = r'[+-]?\d+'

t_REAL = r'[+-]?\d+\.\d+'


def t_ignore_COMMENT(t):
    # This lexeme may be multiline; need to update the line number.
    r'/\*([^*]|\*[^/])*\*/'
    for c in t.value:
        if c == '\n':
            t.lexer.lineno += 1


def t_MODEL_COMMENT(t):
    # This lexeme may be multiline; need to update the line number.
    r'\(\*([^*]|\*[^)])*\*\)'
    for c in t.value:
        if c == '\n':
            t.lexer.lineno += 1
    t.value = t.value[2:-2].strip()
    return t


def t_IDENTIFIER(t):
    r'[a-zA-Z][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value, 'IDENTIFIER')  # Check for reserved words
    return t


def t_STRING(t):
    # A quoted string has its quotes removed. Internal escaped quotes are
    # retained. Escapes are removed (assuming only quotes and
    # backslashes get escaped).
    r'"(\\.|[^"])*"'
    t.value = re.sub(r'\\(.)', r'\1', t.value[1:-1])
    return t


# # remnant of cat2raw ...
# # A DOCLINE has its leading pipe removed. Escapes are removed
# # (assuming only quotes and backslashes get escaped).
# def t_DOCLINE(t):
#     r'\|.*'
#     if t.value[len(t.value) - 1] == '\r':
#         t.value = t.value[1:-1]
#     else:
#         t.value = re.sub(r'\\(.)', r'\1', t.value[1:])
#     return t


def t_newline(t):
    # Define a rule so we can track skipped line numbers as well as
    # significant ones.
    r'\n+'
    t.lexer.lineno += len(t.value)


# A string containing ignored characters (space, tab and CR)
t_ignore = ' \t\r'


def t_error(t):
    '''Error handling.'''
    print "Illegal character '%s', line %d" % (t.value, t.lexer.lineno)
    t.lexer.skip(1)


# ----------------------------------------------------------------------
# Main, test
# ----------------------------------------------------------------------


def warning(msg):
    sys.stderr.write("%s\n" % msg)


def error(msg):
    sys.stderr.write("%s\n" % msg)
    sys.exit(1)


def main():

    def usage():
        sys.stderr.write('usage: tuml2xmi.py [flags] [input .tuml file]\n')
        sys.stderr.write('flags:\n')
        sys.stderr.write('-h, --help:              '
                         + 'output this message\n')
        sys.stderr.write('-o, --output=PATH:       '
                         + 'the output path/file '
                         + '(default is ./domain_name.xmi)\n')
        sys.stderr.write('-v, --verbose:              '
                         + 'detailed progress reporting\n')

    try:
        opts, args = getopt.getopt(
            sys.argv[1:],
            'ho:v',
            ['help', 'output=', 'verbose'])
    except getopt.GetoptError:
        usage()
        sys.exit(1)

    input = sys.stdin
    output_file = ''
    verbosity = False

    for o, v in opts:
        if o in ('-h', '--help'):
            usage()
            sys.exit()
        if o in ('-o', '--output'):
            output_file = v
        if o in ('-v', '--verbose'):
            verbosity = True

    if len(args) > 1:
        usage()
        sys.exit(1)
    if len(args) == 1:
        input_file = args[0]
        try:
            input = open(input_file, 'r')
        except:
            error("couldn't open %s for input.\n" % input_file)

    # create the lexer
    global lexer
    lexer = lex.lex()
    # connect it to the input tuml file
    lexer.input(input.read())
    input.close()
    # create the parser (global, for p_error())
    global parser
    parser = yacc.yacc()
    # parse the input, creating a Model
    m = parser.parse(lexer=lexer, debug=verbosity)
    # output
    # by default, the output for a domain is to <domain>.xmi
    if output_file == '-':
        output = sys.stdout
    elif output_file == '' and len(args) == 0:
        output = sys.stdout
    else:
        if output_file == '':
            root, ext = os.path.splitext(input_file)
            output_file = root + '.xmi'
        try:
            output = open(output_file, 'w')
        except:
            sys.stderr.write("couldn't open %s for output.\n" % output_file)
            sys.exit(1)

    xmi_document = ET.ElementTree(ET.Element('uml'))
    model = xmi_document.getroot()
    model = ET.SubElement(model, 'XMI')
    model.set('xmlns:UML', 'http://pushface.org/coldframe/textuml')

    m.add_xml(model)

    xmi_document.write(output, encoding='utf-8', xml_declaration=True)

    output.close()


if __name__ == '__main__':
    main()
