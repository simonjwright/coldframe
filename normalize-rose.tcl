#!/bin/sh
# the next line restarts using itclsh \
exec itclsh "$0" "$@"

# $Id: normalize-rose.tcl,v d50183a491fb 2001/04/27 19:00:12 simon $

# Converts an XML Domain Definition file, generated from Rose by
# ddf.ebs, into normalized XML.

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

package require xml
package require Itcl

#############
# Utilities #
#############

# Given a string,
# - trims leading and trailing white space
# - capitalizes the first letter of each word
# - replaces each run of white space by a single underscore
# and returns the result
proc normalize {s} {
    set tmp [string trim $s]
    if [string match "\"*" $tmp] {return $tmp}
    set tmp [string tolower $tmp]
    regsub -all {_} "$tmp" " " tmp
    set tmp [split $tmp]
    set res ""
    foreach w $tmp {
	set res \
	    "$res [string toupper [string index $w 0]][string range $w 1 end]"
    }
    regsub -all {[ \t]+} "[string trim $res]" "_" res
    return $res
}

# Output a simple XML element.
# 'tag' may be either just a tag or {tag {attribute value} ...}
proc putElement {tag content} {
    set t [lindex $tag 0]
    puts -nonewline "<$t"
    for {set i 1} {$i < [llength $tag]} {incr i} {
	set a [lindex $tag $i]
	puts -nonewline " [lindex $a 0]=\"[lindex $a 1]\""
    }
    puts ">$content</$t>"
}

itcl::class Stack {
    variable s {}
    method -empty {} {return [expr [llength $s] <= 0]}
    method -push {elem} {set s [linsert $s 0 $elem]}
    method -top {} {return [lindex $s 0]}
    method -pop {} {
	set el [lindex $s 0]
	set s [lrange $s 1 end]
	return $el
    }
}

#######################
# XML utility classes #
#######################

# The Base class supports the XML parse.
itcl::class Base {

    # holds the tag of the XML element - eg, for <foo>bar</foo> it will be
    # "foo"
    variable tag

    # set the tag
    method -tag {t} {set tag $t}

    # get the tag
    method -getTag {} {return $tag}


    # holds the textual content of the XML element - eg, for the element
    # above it will be "bar"
    variable text ""

    # the textual content of the XML element may be received in chunks;
    # add another chunk
    method -addText {str} {
	set text "$text$str"
    }

    # set the textual content
    method -text {t} {set text $t}


    # an array, indexed by attribute name, containing attribute values
    variable xmlattributes
    
    # copies the attributes from the given array (created during the
    # parse) into the local store
    method -xmlattributes {attributes} {
	upvar $attributes a
	foreach i [array names a] {
	    set xmlattributes($i) "$a($i)"
	    if [expr [string compare "stereotype" $i] == 0] {
		$this -stereotype "$a($i)"
	    }
	}
    }

    # debug utility, returns a string representation of the attributes
    method -formatxmlattributes {} {
	set res ""
	if [info exists xmlattributes] {
	    foreach i [array names xmlattributes] {
		set res "$res<$i -> $xmlattributes($i)> "
	    }
	}
	return $res
    }


    # the immediately enclosing XML element
    variable owner

    method -owner {o} {set owner $o}
    method -getOwner {} {return $owner}


    # called when the closing </tag> is read to take any necessary
    # actions
    method -complete {} {}

    # called when the outermost closing <tag> is read to do the first
    # pass of processing
    method -evaluate {outermost} {}

    # called when the outermost closing </tag> is read to do the second
    # pass of processing
    method -generate {outermost} {error "CF: undefined $tag method -generate"}
}


# The base class for all XML elements whose interesting aspect is their
# textual content - eg, <name>foo</name>
itcl::class String {
    inherit Base

    # the object has already been popped off the stack; call the stack top's
    # method with the same name as this tag to store the value (so, given
    # the example above, the containing object needs to offer a -name method)
    method -complete {} {
	[stack -top] -$tag $text
    }
}


# The base class for all XML elements which represent identifiers; stores
# eg " hELLO   world " as "Hello_World"
itcl::class IdentifierString {
    inherit String

    # the object has already been popped off the stack; call the stack top's
    # method with the same name as this tag to store the value after
    # conversion to identifier form (so, given the example above, the
    # containing object needs to offer a -name method)
    method -complete {} {
	[stack -top] -$tag [normalize $text]
    }
}


# The base class for all non-trivial XML elements. They will certainly have
# an element name (eg, <foo> -> name "foo") and may contain:
# * a stereotype (eg, <foo stereotype="key=a, number=5">)
# * documentation
# * an annotation (any data in [[ ]] in the documentation text).
itcl::class Element {
    inherit Base

    variable annotation ""

    variable documentation ""

    variable name "unnamed"

    variable stereotype

    # called to process annotation information. The annotation info is
    # of the form
    #
    # annotation ::= annotation ';' parameter-setting
    #             | parameter-setting ;
    # parameter-setting ::= parameter-name [ ':' value ];
    #
    # For each parameter-setting, method -parameter-name is invoked with one
    # argument (value, or null).
    method -handleAnnotation {} {
	set a $annotation
	set pattern {^[ \t]*([-a-z0-9_]+)[ \t]*(:([^;]+))?[;]?}
	for {} {[regexp \
		-nocase \
		$pattern \
		$a wh attr dummy value]} {} {
	    if [catch {$this -[string tolower $attr] $value}] {
		puts stderr \
		    "annotation not handled, \"$name -[string tolower $attr] $value\""
	    }
	    regexp -nocase -indices $pattern $a wh
	    set a [string range $a \
		    [expr [lindex $wh 1] + 1] end]
	}
    }

    # called for a <documentation> element to extract annotation information,
    # if any, from the documentation string. The annotation info is contained
    # between double square brackets [[ ]].
    # The documentation content is left normalised (leading and trailing white
    # space trimmed, paragraphs separated by a single newline).
    method -documentation {d} {
	set documentation $d
	$this -handleDocumentation
    }
    # actually does the work; allows -documentation to be overridden
    method -handleDocumentation {} {
	set d $documentation
	regexp {\[\[(.*)\]\]} $d wh annotation
	$this -handleAnnotationWhenRead
	regsub -all {\[\[(.*)\]\]} $d "" tmp
	set tmp [string trim $tmp]
	regsub -all {([ \t]*\n)+} $tmp "\n" documentation
    }
    # normally the annotation can be processed as soon as it's seen;
    # override to avoid this
    method -handleAnnotationWhenRead {} {
	$this -handleAnnotation
    }

    method -generateDocumentation {} {
	if [string length $documentation] {
	    puts "<documentation>"
	    set pars [split $documentation "\n"]
	    foreach p $pars {
		regsub -all {&} $p {\&amp;} p
		regsub -all {>} $p {\&gt;} p
		regsub -all {<} $p {\&lt;} p
		regsub -all {\\\\\{} $p {<} p
		regsub -all {\\\\\}} $p {>} p
		puts "<par>"
		puts $p
		puts "</par>"
	    }
	    puts "</documentation>"
	}
    }

    method -name {n} {
	set name $n
	$this -handleStereotype
    }

    method -getName {} {return $name}

    # Because stereotypes are seen before the <name> element, if we process
    # the stereotype immediately we won't be able to report the name.
    # So we postpone handling the stereotype until the -name method
    # is called.
    # Just for completeness, and in case there was no <name>, we try
    # handling again on completion.
    method -stereotype {s} {
	set stereotype $s
    }

    # if a stereotype attribute has been seen, process it.
    # Given the example above, this results in the calls
    #   $this -key a
    #   $this -number 5
    method -handleStereotype {} {
	if [info exists stereotype] {
	    set p {[ \t]*([a-z0-9_]+)[ \t]*(=[ \t]*([a-z0-9_,]+))?[ \t]*}
	    set s $stereotype
	    for {} {[regexp -nocase $p $s wh n opt v]} {} {
		# n is the tag name, v the tag value if any
		if [catch {$this -[string tolower $n] "$v"}] {
		    puts stderr \
		        "stereotype not handled, \"$name -[string tolower $n] $v\""
		}
		regexp -nocase -indices $p $s wh
		set s [string range $s [expr [lindex $wh 1] + 1] end]
	    }
	    unset stereotype
	}
    }

    method -complete {} {
	$this -handleStereotype
	if [catch {[stack -top] -add $this} msg] {
	    puts "error adding a [$this -getTag] to a [[stack -top] -getTag]"
	    error $msg
	}
    }
}

# Lists are for containers constructed during the parse of the XML,
# where there may be many instances (eg, there are many instances
# of domain/classes/class or class/attributes/attribute)
itcl::class List {
    inherit Base

    variable members {}

    method -add {elem} {lappend members $elem}

    method -size {} {return [llength $members]}

    method -getMembers {} {return $members}

    method -complete {} {
	[stack -top] -$tag $this
    }

    method -evaluate {domain} {
	foreach el $members {$el -evaluate $domain}
    }

    method -generate {domain} {
	foreach el $members {$el -generate $domain}
    }
}

# Containers are for singleton (per-Domain, per-Class) containment,
# particularly where members need to be revisited.
# byName is indexed by name and holds the index number in byNumber.
# byNumber is indexed by number and holds the content.
itcl::class Container {
    inherit Base

    private variable byName

    private variable byNumber

    constructor {} {
	array set byName {}
	array set byNumber {}
	return $this
    }

    # Derived classes should override -className to return their own
    # name (in lower case)
    method -className {} {return "container"}

    # Return the number of contained elements
    method -size {} {return [array size byName]}

    # Indicate whether the name doesn't denote an object in the Container
    method -isMissing {name} {
	return [expr ![info exists byName($name)]]
    }

    # Indicate whether the name denotes an object in the Container
    method -isPresent {name} {
	return [info exists byName($name)]
    }

    # Add the given object to the container at entry "name"
    method -add {object name} {
	if [info exists byName($name)] {
	    error "CF: [$this -className] already holds an element named $name"
	}
	set newIndex [$this -size]
	set byName($name) $newIndex
	set byNumber($newIndex) $object
    }

    # Find the index of a named element in the Container
    method -index {name} {
	if [info exists byName($name)] {return $byName($name)}
	error "CF: [$this -className] item $name not found\
		([$this -size] entries, [array names byName])"
    }

    # Return the indexed element from the Container.
    method -atIndex {index} {
	if {$index < [$this -size]} {
	    return $byNumber($index)
	}
	error "CF: [$this -className] index $index out of range\
		([$this -size] entries)"
    }
 
    # Return the named element from the Container
    method -atName {name} {
	set index [$this -index $name]
	return [$this -atIndex $index]
    }

    method -complete {} {
	[stack -top] -[$this -className] $this
    }

    method -evaluate {domain} {
	set size [$this -size]
	for {set i 0} {$i < $size} {incr i 1} {
	    [$this -atIndex $i] -evaluate $domain
	}
    }

    method -generate {domain} {
	set size [$this -size]
	for {set i 0} {$i < $size} {incr i 1} {
	    [$this -atIndex $i] -generate $domain
	}
    }
}

########################################
# Our classes for storing the XML info #
########################################

# String classes

itcl::class Associative {
    inherit IdentifierString
}

itcl::class Cardinality {
    inherit String
}

itcl::class Child {
    inherit IdentifierString
}

itcl::class Classname {
    inherit IdentifierString
}

itcl::class End {
    inherit String
}

itcl::class ExportControl {
    inherit String
}

itcl::class Initial {
    inherit IdentifierString
}

itcl::class Name {
    inherit IdentifierString
}

itcl::class Parent {
    inherit IdentifierString
}

itcl::class Return {
    inherit IdentifierString
}

itcl::class Type {
    inherit IdentifierString
}

# Element classes

itcl::class Domain {
    inherit Element

    private common currentDomain

    variable classes

    variable relationships

    variable datatypes

    variable transitiontables

    variable terminators

    proc currentDomain {} {return $currentDomain}

    constructor {} {
	set currentDomain $this
	set classes [Classes ::#auto]
	set relationships [Relationships ::#auto]
	set datatypes [Datatypes ::#auto]
    }

    method -classes {l} {set classes $l}

    method -getClasses {} {return $classes}

    method -relationships {l} {set relationships $l}

    method -getRelationships {} {return $relationships}

    method -datatypes {l} {set datatypes $l}

    method -getDatatypes {} {return $datatypes}

    method -typesfiles {l} {set typesfiles $l}

    method -transitiontables {l} {set transitiontables $l}

    method -terminators {l} {set terminators $l}

    method -complete {} {
	$this -generate
    }

    method -generate {} {
	$classes -evaluate $this
	$relationships -evaluate $this
	$datatypes -evaluate $this

	puts "<domain>"
	putElement name "$name"
	putElement date "[exec date]"
	$this -generateDocumentation

	$classes -generate $this
	$relationships -generate $this
	$datatypes -generate $this

	puts "</domain>"
    }
}

itcl::class Class {
    inherit Element

    # an abbreviation of the name may be useful (eg, when making names
    # for referential attributes)
    variable abbreviation

    # contains the attributes
    variable attributes

    # contains the operations
    variable operations

    # specifies the maximum number of instances (optional)
    variable max

    # specifies if this is an interface class
    variable interface 0
    method -interface {dummy} {
	set interface 1
	set singleton 1
    }

    # true if there's one and only one instance of the class
    variable singleton 0
    method -singleton {dummy} {set singleton 1}

    method -abbreviation {a} {
	set abbreviation [string toupper [string trim $a]]
    }

    method -getAbbreviation {} {
	# if no abbreviation has been supplied, make one up from the
	# initial letters of the name (which will already have been
	# capitalised)
	if ![info exists abbreviation] {
	    set tmp [split $name "_"]
	    set abbreviation ""
	    foreach w $tmp {
		set abbreviation "$abbreviation[string index $w 0]"
	    }
	}
	return $abbreviation
    }

    method -operations {l} {set operations $l}

    method -max {s} {set max [string trim $s]}

    #
    # variables & methods related to <<control>> classes (XXX needed?)
    #

    variable isControl 0

    # called (via stereotype mechanism) to indicate that this is a
    # <<control>> class
    method -control {dummy} {set isControl 1}

    #
    # variables & methods related to <<type>> classes
    #

    variable isType 0

    variable typeInfo

    # called (via stereotype mechanism) to indicate that this is a
    # <<type>> class
    method -type {dummy} {set isType 1}

    method -attributes {l} {set attributes $l}

    method -hasIdentifier {} {
	foreach a [$attributes -getMembers] {
	    if [$a -getIdentifier] {
		return 1
	    }
	}
	return 0
    }

    # called to formalize a relationship.
    # this is the source of the formalization
    # obj is the class which holds the formalizing attribute(s)
    # relation is the relationship to be formalized
    # identifier is true if the formalizing attribute(s) are to
    #   be part of obj's identifier
    method -addFormalizingAttributesTo {obj relation identifier} {
	set attr [ReferentialAttribute ::#auto $this $relation $identifier]
	$obj -addReferentialAttribute $attr
    }

    method -addReferentialAttribute {a} {
	puts stderr "referential attribute [$a -getName] added to $name"
	$a -owner $attributes
	$attributes -add $a
    }

    # because different annotations (text in [[ ]] in documentation) are
    # appropriate in ordinary classes vs <type> classes, don't process
    # annotations if this is a type.
    method -handleAnnotationWhenRead {} {
	if {!$isType} {
	    $this -handleAnnotation
	}
    }

    method -complete {} {
	$this -handleStereotype
	if $isType {
	    set dts [[Domain::currentDomain] -getDatatypes]
	    if [$dts -isPresent $name] {
		set dt [$dts -atName $name]
	    } else {
		set dt [Datatype ::#auto $name]
		$dts -add $dt $name
	    }
	    # transfer the documentation and the (already-extracted) annotation
	    # to the new Datatype.
	    $dt -documentation $documentation
	    $dt -annotation $annotation
	} elseif $isControl {
	    puts stderr "<<control>> not yet handled properly"
	    [stack -top] -add $this $name
	} else {
	    [stack -top] -add $this $name
	}
    }

    method -evaluate {domain} {
	$attributes -evaluate $domain
    }

    method -generate {domain} {
	if [expr $singleton && [$this -hasIdentifier]] {
	    puts stderr "singleton [$this -getName] has identifier(s)"
	} elseif [expr !$singleton && ![$this -hasIdentifier]] {
	    puts stderr "[$this -getName] has no identifier(s)"
	}
	puts -nonewline "<class"
	if [info exists max] {puts -nonewline " max=\"$max\""}
	if $singleton {puts -nonewline " singleton=\"yes\""}
	if $interface {puts -nonewline " interface=\"yes\""}
	puts ">"
	putElement name "$name"
	putElement abbreviation [$this -getAbbreviation]
	$this -generateDocumentation
	$attributes -generate $domain
	$operations -generate $domain
	puts "</class>"
    }
}

itcl::class Operation {
    inherit Element

    # is this a class operation?
    variable cls 0

    # the return type, if any
    variable ret ""

    variable parameters

    # called via stereotype mechanism to indicate that this is a class
    # operation
    method -class {dummy} {set cls 1}

    method -return {r} {set ret $r}

    method -parameters {pl} {set parameters $pl}

    method -generate {domain}  {
	puts -nonewline "<operation"
	if $cls {puts -nonewline " class=\"yes\""}
	if {[string length $ret] > 0} {puts -nonewline " return=\"$ret\""}
	puts ">"
	putElement name $name
	$this -generateDocumentation
	$parameters -generate $domain
	puts "</operation>"
    }
}

itcl::class Parameter {
    inherit Element

    variable type

    variable initial ""

    variable modeInfo ""

    method -type {t} {set type $t}

    method -initial {i} {set initial $i}
    
    # called when documentation with a [[ mode : mode-setting ]] annotation
    # has been found. mode-setting can be 'in', 'inout' or 'in out', or 'out'.
    method -mode {value} {
	switch [string trim $value] {
	    in       {set modeInfo ""}
	    "in out" -
	    inout    {set modeInfo inout}
	    out      {set modeInfo out}
	    default  {
		error "unrecognised parameter mode $value"
	    }
	}
    }

    method -generate {domain} {
	puts -nonewline "<parameter"
	if {[string length $modeInfo] > 0} {
	    puts -nonewline " mode=\"$modeInfo\""
	}
	puts ">"
	putElement name $name
	putElement type $type
	if {[string length $initial] > 0} {putElement initial $initial}
	if {[string length $modeInfo] > 0} {putElement mode $modeInfo}
	$this -generateDocumentation
	puts "</parameter>"
    }
}

itcl::class Relationship {
    inherit Element

    variable formalized 0

    method -formalized {} {set formalized 1}

    method -needsFormalizing {} {return [expr !$formalized]}

}

itcl::class Association {
    inherit Relationship

    variable role1

    variable role2

    variable associative

    method -associative {a} {set associative $a}

    method -getAssociativeClassName {} {return $associative}

    method -isAssociative {} {return [info exists associative]}

    method -role {role} {
	set role[$role -getEnd] $role
    }

    method -relationshipType {} {
	set type "[$role1 -getCardinality]:[$role2 -getCardinality]"
	switch $type {
	    "1:1" -
	    "1:M" -
	    "M:M" {}
	    "M:1" {
		set type "1:M"
		set tmp $role1
		set role1 $role2
		set role2 $tmp
		$role1 -end 1
		$role2 -end 2
	    }
	}
	if [$this -isAssociative] {
	    set type "1-($type)"
	}
	# XXX could just check for ^M here?
	if [string match "M:M" $type] {
	    error "illegal M:M association [$this -getName]"
	}
	return $type
    }

    method -complete {} {
	# -relationshipType may swap the roles over! Must be called before
	# anything else is done.
	$this -relationshipType
	[stack -top] -add $this $name
    }

    method -evaluate {domain} {
	if [expr ![$this -needsFormalizing]] {
	    puts stderr "$name already formalized"
	    return
	}
	set os [$domain -getClasses]
	set type [$this -relationshipType]
	set cl1 [$os -atName [$role1 -getClassname]]
	set cl2 [$os -atName [$role2 -getClassname]]
	$this -formalized
	if [$this -isAssociative] {
	    set assoc [$os -atName $associative]
	}
	switch $type {
	    "1:1"     {
		# ensure that one and only one of the roles is marked
		# as the source end
		if [$role2 -getSourceEnd] {
		    if [$role1 -getSourceEnd] {
			error "both ends of $name are marked as source"
		    }
		    $cl2 -addFormalizingAttributesTo $cl1 $this 0
		} elseif [$role1 -getSourceEnd] {
		    $cl1 -addFormalizingAttributesTo $cl2 $this 0
		} elseif [$role1 -getConditionality] {
		    if [$role2 -getConditionality] {
			error "neither end of biconditional $name\
				is marked as source"
		    }
		    $role2 -setSourceEnd 1
		    $cl2 -addFormalizingAttributesTo $cl1 $this 0
		} elseif [$role2 -getConditionality] {
		    $role1 -setSourceEnd 1
		    $cl1 -addFormalizingAttributesTo $cl2 $this 0
		} else {
		    error "neither end of unconditional $name\
			    is marked as source"
		}
	    }
	    "1:M"     {
		# the identifying attributes in role 1 are used as 
		# referential attributes in role 2
		$role1 -setSourceEnd 1
		$role2 -setSourceEnd 0
		$cl1 -addFormalizingAttributesTo $cl2 $this 0
	    }
	    "1-(1:1)" {error "CF: oops! 1-(1:1) not yet implemented!"}
	    "1-(1:M)" {
		# The identifying attributes in both roles are used as
		# referential attributes in the associative class.
		# Only the referential attributes from role 2 become
		# identifiers in the associative class.
		$role1 -setSourceEnd 1
		$role2 -setSourceEnd 1
		$cl1 -addFormalizingAttributesTo $assoc $this 0
		$cl2 -addFormalizingAttributesTo $assoc $this 1
	    }
	    "1-(M:M)" {
		# The identifying attributes in both roles are used as
		# referential and identifying attributes in the associative
		# class.
		$role1 -setSourceEnd 1
		$role2 -setSourceEnd 1
		$cl1 -addFormalizingAttributesTo $assoc $this 1
		$cl2 -addFormalizingAttributesTo $assoc $this 1
	    }
	}
    }

    method -generate {domain} {
	if [$this -needsFormalizing] {
	    error "[$this -getName] is unformalized"
	}
	puts "<association>"
	putElement name $name
	$role1 -generate $domain
	$role2 -generate $domain
	if [info exists associative] {
	    set os [$domain -getClasses]
	    putElement associative [[$os -atName $associative] -getName]
	}
	$this -generateDocumentation
	puts "</association>"
    }
}

itcl::class Role {
    inherit Element
    # Element::name is the role verb phrase

    # 1 or M
    variable cardinality

    # The name of the class which is the object of the role; if we have
    # A "does something to" B, classname is B.
    variable classname

    # 1 if conditional, 0 if unconditional
    variable conditional

    # The end (A => 1, B => 2) at which the role is defined in the analysis.
    # May change if the relation is normalized (eg, M:1 -> 1:M)
    variable end

    # True if the class at this end provides the referential attributes
    # that formalize the relation. Normally we can work this out from the
    # multiplicity and conditionality, but for 1:1, 1c:1c, 1-(1:1) and
    # 1-(1c:1c) the analyst has to specify using the [[source]] annotation.
    variable sourceEnd 0

    method -cardinality {c} {
	switch $c {
	    "1"     -
	    "1..1"  {set conditional 0; set cardinality "1"}
	    "1..n"  -
	    "n"     {set conditional 0; set cardinality "M"}
	    "0..1"  {set conditional 1; set cardinality "1"}
	    "0..n"  {set conditional 1; set cardinality "M"}
	    default {error "unrecognised multiplicity $c"}
	}
    }

    method -getCardinality {} {return $cardinality}

    method -classname {n} {set classname $n}

    method -getClassname {} {return $classname}

    method -getConditionality {} {return $conditional}

    method -end {e} {set end $e}

    method -getEnd {} {return $end}

    method -source {dummy} {set sourceEnd 1}

    method -setSourceEnd {s} {set sourceEnd $s}
    method -getSourceEnd {} {return $sourceEnd}

    method -complete {} {
	[stack -top] -role $this
    }

    method -evaluate {domain} {
    }

    method -generate {domain} {
	puts -nonewline "<role"
	if $conditional {puts -nonewline " conditional=\"yes\""}
	if {$cardinality == "M"} {puts -nonewline " multiple=\"yes\""}
	if $sourceEnd {puts -nonewline " source=\"yes\""}
	puts ">"
	set os [$domain -getClasses]
	set cl [$os -atName $classname]
	putElement classname [$cl -getName]
	putElement name [$this -getName]
	puts "</role>"
    }
}

itcl::class Inheritance {
    inherit Relationship

    # parent is a string containing the name of the supertype class
    variable parent

    # child is the lazy way of not handing the list head ..
    variable child

    # children is a List of the names of the subtype classes
    variable children

    constructor {} {set children [List ::#auto]}

    method -parent {p} {set parent $p}

    method -child {c} {
	set child $c
	$children -add $c
    }

    method -addChild {c} {
	$children -add $c
    }

    method -relationshipType {} {return "Super/Sub"}

    # Override inherited method, to collect all the documentation
    # fragments together. No ordering as yet.
    method -documentation {d} {
	set documentation "$documentation\n\n$d"
    }

    method -complete {} {
	set inheritances [stack -top]
	if [$inheritances -isPresent $name] {
	    set extant [$inheritances -atName $name]
	    $extant -addChild $child
	    $extant -documentation $documentation
	} else {
	    $inheritances -add $this $name
	}
    }

    method -evaluate {domain} {
	$this -handleDocumentation
	if [expr ![$this -needsFormalizing]] {
	    puts stderr "$name already formalized"
	    return
	}
	set os [$domain -getClasses]
	set p [$os -atName $parent]
	if [expr ![$p -hasIdentifier]] {
	    puts stderr "[$p -getName] has no identifier(s)"
	    return
	}
	$this -formalized
	foreach ch [$children -getMembers] {
	    set c [$os -atName $ch]
	    $p -addFormalizingAttributesTo $c $this 1
	    set role [Role ::#auto]
	    $role -owner $this
	    $role -end 4
	    $role -name "is supertype of"
	    $role -classname $ch
	}
    }

    method -generate {domain} {
	if [$this -needsFormalizing] {
	    error "[$this -getName] is unformalized"
	}
	puts "<inheritance>"
	putElement name $name
	set os [$domain -getClasses]
	set p [$os -atName $parent]
	putElement parent [$p -getName]
	foreach ch [$children -getMembers] {
	    set c [$os -atName $ch]
	    putElement child "[$c -getName]"
	}
	$this -generateDocumentation
	puts "</inheritance>"
    }
}

itcl::class Datatype {
    inherit Element

    variable type

    variable dataType "standard"

    variable dataDetail

    constructor {name} {set type $name}

    # process annotation.
    method -annotation {a} {
	set annotation $a
	set pattern {^[ \t]*([-a-z0-9_]+)[ \t]*(:([^;]+))?[;]?}
	for {} {[regexp \
		-nocase \
		$pattern \
		$a wh attr dummy value]} {} {
	    $this -[string tolower $attr] $value
	    regexp -nocase -indices $pattern $a wh
	    set a [string range $a \
		    [expr [lindex $wh 1] + 1] end]
	}
    }

    method -className {} {return "datatype"}

    # called when the type is an enumeration. values is a list of the
    # comma-separated enumerals, which have not been normalized.
    method -enumeration {values} {
	set dataType "enumeration"
	set raw [split $values ","]
	foreach v $raw {
	    set vs [lappend vs [normalize $v]]
	}
	set dataDetail $vs
    }

    # called when the type is an integer. constraint is a set of key/value
    # pairs, which may be newline- or comma-separated.
    # Useful keys are lower, upper, size
    method -integer {constraint} {
	set dataType "integer"
	regsub -all {,[ \t]*} "[string trim $constraint]" "\n" dataDetail
    }

    # called when the type is a real. constraint is a set of key/value
    # pairs, which may be newline- or comma-separated.
    # Useful keys are delta, digits, lower, upper, size, small
    method -real {constraint} {
	set dataType "real"
	regsub -all {,[ \t]*} "[string trim $constraint]" "\n" dataDetail
    }

    # called when the type is a set of instances of a class. cls
    # is the name of the class, which has not been normalized.
    method -set {cls} {
	set dataType "set"
	set dataDetail [normalize $cls]
    }


    # called when the type is a string. constraint is a set of key/value
    # pairs, which may be newline- or comma-separated.
    # Useful key is max (max length).
    method -string {constraint} {
	set dataType "string"
	regsub -all {,[ \t]*} "[string trim $constraint]" "\n" dataDetail
    }

    method -complete {} {
	if [expr ![[stack -top] -isPresent $type]] {
	    [stack -top] -add $this $type
	}
    }

    method -evaluate {domain} {
    }

    method -generate {domain} {
	puts "<type>"
	putElement name "$type"
	$this -generateDocumentation
	switch $dataType {
	    enumeration {
		puts "<enumeration>"
		foreach d $dataDetail {putElement literal $d}
		puts "</enumeration>"
	    }
	    integer -
	    real -
	    string {
		puts "<$dataType>"
		foreach {key value} $dataDetail {
		    putElement $key $value
		}
		puts "</$dataType>"
	    }
	    set {
		putElement set $dataDetail
	    }
	    standard {
		putElement standard $type
	    }
	    default {
		error "CF: oops! dataType \"$dataType\""
	    }
	}
	puts "</type>"
    }
}

itcl::class Documentation {
    inherit String
}

itcl::class Typesfile {
    inherit Element
}

itcl::class Transitiontable {
    inherit Element
}

itcl::class Terminator {
    inherit Element
}

itcl::class Tag {
    inherit Element
}

itcl::class Attribute {
    inherit Element

    # the type name
    variable type

    # the initial value
    variable initial ""

    # indicates whether this attribute is an identifier
    variable identifier 0

    method -type {t} {set type $t}

    method -initial {i} {set initial $i}
    
    # used via stereotype processing to indicate this is an identifying
    # attribute
    method -id {dummy} {$this -identifier}

    method -identifier {} {set identifier 1}

    method -getIdentifier {} {return $identifier}

    method -getOwningClass {} {
	return [[$this -getOwner] -getOwner]
    }

    # indicates whether this attribute formalizes an association,
    # where the analyst needs to mark what would otherwise be a
    # non-identifying referential attribute
    variable formalizedAssociation

    # used via stereotype processing to indicate that this attribute
    # formalizes an association
    method -formalizes {assoc} {
	set formalizedAssociation [normalize $assoc]
    }

    method -evaluate {domain} {
	# extract and store data types
	set datatypes [$domain -getDatatypes]
	if [$datatypes -isMissing $type] {
	    set datatype [Datatype ::#auto $type]
	    $datatypes -add $datatype $type
	} else {
	    set datatype [$datatypes -atName $type]
	}
	if [info exists formalizedAssociation] {
	    # we rely on Relationships being evaluated after Classes
	    # (and Attributes are evaluated because they're in Classes).
	    set rels [$domain -getRelationships]
	    [$rels -atName $formalizedAssociation] -formalized
	}
    }

    method -generate {domain} {
	puts -nonewline "<attribute"
	if $identifier {puts -nonewline " identifier=\"yes\""}
	if [info exists formalizedAssociation] {
	    puts -nonewline " refers=\"$type\""
	    puts -nonewline " relation=\"$formalizedAssociation\""
	}
	puts ">"
	putElement name "$name"
	putElement type "$type"
	if {[string length $initial] > 0} {putElement initial $initial}
	$this -generateDocumentation
	puts "</attribute>"
    }
}

itcl::class ReferentialAttribute {
    inherit Base

    # holds the  relationship
    variable relation

    # holds the source class
    variable source

    # true if the attribute is to form part of the owning class'
    # identifier
    variable identifier

    constructor {src rel id} {
	set source $src
	set relation $rel
	set identifier $id
    }

    method -getName {} {
	return "[$relation -getName].[$source -getName]"
    }

    method -getIdentifier {} {return $identifier}

    method -generate {domain} {
	puts -nonewline "<attribute"
	puts -nonewline " refers=\"[$source -getName]\""
	puts -nonewline " relation=\"[$relation -getName]\""
	if $identifier {puts -nonewline " identifier=\"yes\""}
	puts "/>"
    }
}

# Aggregate classes

itcl::class TaggedList {
    inherit List

    # when generating, generates its contents within a container element
    # (same name as the list tag).
    method -generate {domain} {
	puts "<$tag>"
	$this List::-generate $domain
	puts "</$tag>"
    }
}

itcl::class Attributes {
    inherit List
}

itcl::class Associations {
    inherit Container
    method -className {} {return "associations"}
}

itcl::class Datatypes {
    inherit Container

    constructor {} {
	# insert standard (provided) types.
	$this -add [Datatype ::#auto Boolean] Boolean
	$this -add [Datatype ::#auto Real] Real
	$this -add [Datatype ::#auto Integer] Integer
	$this -add [Datatype ::#auto String] String
	$this -add [Datatype ::#auto Unbounded_String] Unbounded_String
    }

    method -className {} {return "datatypes"}

}

itcl::class Inheritances {
    inherit Container
    method -className {} {return "inheritances"}
}

itcl::class Classes {
    inherit Container
    method -className {} {return "classes"}
}

itcl::class Operations {
    inherit List
}

itcl::class Parameters {
    inherit List
}

itcl::class Relationships {
    inherit Container
    method -className {} {return "relationships"}
}

itcl::class Tags {
    inherit List
}

itcl::class Terminators {
    inherit List
}

itcl::class Transitiontables {
    inherit List
}

# Convert XML tag name to element of appropriate type

proc elementFactory {tag} {
    # XXX should this perhaps be an operation of Domain?
    switch $tag {
	attribute         {return [Attribute #auto]}
	attributes        {return [Attributes #auto]}
	association       {return [Association #auto]}
	associations      {return [Associations #auto]}
	associative       {return [Associative #auto]}
	cardinality       {return [Cardinality #auto]}
	child             {return [Child #auto]}
	"class"           {return [Class #auto]}
	classes           {return [[Domain::currentDomain] -getClasses]}
	classname         {return [Classname #auto]}
	datatype          {return [Datatype #auto]}
	datatypes         {return [[Domain::currentDomain] -getDatatypes]}
	documentation     {return [Documentation #auto]}
	domain            {return [Domain #auto]}
	end               {return [End #auto]}
	exportcontrol     {return [ExportControl #auto]}
	inheritance       {return [Inheritance #auto]}
	inheritances      {return [Inheritances #auto]}
	initial           {return [Initial #auto]}
	name              {return [Name #auto]}
	operation         {return [Operation #auto]}
	operations        {return [Operations #auto]}
	parameter         {return [Parameter #auto]}
	parameters        {return [Parameters #auto]}
	parent            {return [Parent #auto]}
	relationship      {return [Relationship #auto]}
	relationships     {return [[Domain::currentDomain] -getRelationships]}
	"return"          {return [Return #auto]}
	role              {return [Role #auto]}
	terminator        {return [Terminator #auto]}
	terminators       {return [Terminators #auto]}
	transitiontable   {return [Transitiontable #auto]}
	transitiontables  {return [Transitiontables #auto]}
	type              {return [Type #auto]}
	default           {return [Element #auto]}
    }
}

#######################
# XML parse interface #
#######################

proc startTag {tag attrs} {
    # I tried doing these as one operation -- itcl got v confused
    set t [string tolower $tag]
    set el [elementFactory $t]
    array set attr $attrs
    $el -tag $t
    if [expr [array size attr] > 0] {$el -xmlattributes attr}
    stack -push $el
}

proc textInTag {str} {
    [stack -top] -addText $str
}

proc endTag {tag} {
    set el [stack -pop]
    $el -owner [stack -top]
    $el -complete
}

###########
# Globals #
###########

Stack stack

################
# Main program #
################

set parser [xml::parser]
$parser configure \
	-elementstartcommand startTag \
	-elementendcommand endTag \
	-characterdatacommand textInTag
#$parser parse [read stdin]
#exit 0
if [catch {$parser parse [read stdin]} msg] {
    puts stderr "error: $msg"
    exit 1
}

#;; for emacs:
#;; Local Variables:
#;; tcl-default-application: "itclsh"
#;; End:
