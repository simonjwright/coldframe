#!/bin/sh
# the next line restarts using itclsh \
exec itclsh "$0" "$@"

# $Id: normalize-rose.tcl,v 71c3fa04882f 2001/09/14 19:34:14 simon $

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

# Reads case exceptions from a file. The file format is basically
# that used by ACT's GLIDE, but we don't distinguish substring specs
# from whole-identifier ones.
proc setCaseExceptions {file} {
    global caseExceptions

    if [catch {open $file r} f] {
	error "can't open $file: $f"
    } else {
	while {[gets $f line] >= 0} {
	    regsub -all {[\*\t ]} $line "" res
	    set caseExceptions([string tolower $res]) $res
	}
	close $f
    }
}

# Given a string,
# - trims leading and trailing white space
# - capitalizes the first letter of each word
# - replaces each run of white space by a single underscore
# and returns the result
proc normalize {s} {
    global caseExceptions

    set tmp [string trim $s]
    if [string match "\"*" $tmp] {return $tmp}
    set tmp [string tolower $tmp]
    regsub -all {_} "$tmp" " " tmp
    set tmp [split $tmp]
    set und ""
    foreach w $tmp {
	if [info exists caseExceptions($w)] {
	    set und "$und $caseExceptions($w)"
	} else {
	    set und \
	      "$und [string toupper [string index $w 0]][string range $w 1 end]"
	}
    }
    regsub -all {[ \t]+} "[string trim $und]" "_" und
    regsub -all {\.} "$und" " " tmp
    set tmp [split $tmp]
    set res ""
    foreach w $tmp {
	set res \
	    "$res [string toupper [string index $w 0]][string range $w 1 end]"
    }
    regsub -all {[ \t]+} "[string trim $res]" "." res
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
	    if [catch {$this -$i "$a($i)"}] {
		puts stderr \
			"XML attribute <$tag $i=\"$a($i)\"> not handled"
	    }
#	    if [expr [string compare "stereotype" $i] == 0] {
#		$this -stereotype "$a($i)"
#	    }
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

itcl::class Arguments {
    inherit String
}

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

itcl::class Concurrency {
    inherit String
}

itcl::class Day {
    inherit String
}

itcl::class End {
    inherit String
}

itcl::class ExportControl {
    inherit String
}

itcl::class Extractor {
    inherit String
}

itcl::class Initial {
    inherit IdentifierString
}

itcl::class Month {
    inherit String
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

itcl::class Revision {
    inherit String
}

itcl::class Source {
    inherit IdentifierString
}

itcl::class Target {
    inherit IdentifierString
}

itcl::class Time {
    inherit String
}

itcl::class Type {
    inherit IdentifierString
}

itcl::class Year {
    inherit String
}

# Date classes

proc monthName {m} {
    return [lindex {Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec} [incr m -1]]
}

itcl::class Date {
    inherit Base

    variable day
    variable month
    variable year
    variable time

    method -day {d} {set day $d}
    method -month {m} {set month $m}
    method -year {y} {set year $y}
    method -time {t} {set time $t}

    method -complete {} {
	$owner -date $this
    }

    method -generate {dom} {
	puts "<date>"
	putElement year $year
	putElement month [monthName $month]
	putElement day $day
	putElement time $time
	puts "</date>"
    }

}

# Element classes

itcl::class Domain {
    inherit Element

    private common currentDomain

    variable extractor

    variable date
    variable revision

    variable classes

    variable relationships

    variable datatypes

#    variable transitiontables

    proc currentDomain {} {return $currentDomain}

    constructor {} {
	set currentDomain $this
	set classes [Classes ::#auto]
	set relationships [Relationships ::#auto]
	set datatypes [Datatypes ::#auto]
    }

    method -extractor {e} {set extractor $e}

    method -date {d} {set date $d}
    method -revision {r} {set revision [string trim $r]}

    method -classes {l} {set classes $l}

    method -getClasses {} {return $classes}

    method -relationships {l} {set relationships $l}

    method -getRelationships {} {return $relationships}

    method -datatypes {l} {set datatypes $l}

    method -getDatatypes {} {return $datatypes}

#    method -transitiontables {l} {set transitiontables $l}

    method -complete {} {
	$this -generate
    }

    method -generate {} {
	global coldFrameVersion

	$classes -evaluate $this
	$relationships -evaluate $this
	$datatypes -evaluate $this

	puts "<domain>"
	putElement name "$name"

	putElement extractor $extractor

	$date -generate $this
	if [info exists revision] {
	    putElement revision $revision
	}

	if [info exists coldFrameVersion] {
	    putElement normalizer $coldFrameVersion
	}

	$this -generateDocumentation

	$classes -generate $this
	$relationships -generate $this
	$datatypes -generate $this

	puts "</domain>"
    }
}

itcl::class Class {
    inherit Element

    # contains the attributes
    variable attributes

    method -attributes {l} {set attributes $l}

    # contains the operations
    variable operations

    method -operations {l} {set operations $l}

    # specifies the maximum number of instances (optional)
    variable max

    method -max {s} {set max [string trim $s]}

    method -cardinality {c} {
	set c [string trim $c]
	switch $c {
	    "n" -
	    "0..n" -
	    "1..n" {}
	    "0..0" -
	    "0..1" {
		puts stderr "cardinality $c ignored for $name"
	    }
	    "1..1" {$this -singleton dummy}
	    default {
		if ![regexp {^[0-9]+$} $c] {
		    error "bad cardinality $c for $name"
		}
		$this -max $c
	    }
	}
    }

    # specifies if this is an active class
    variable active 0

    method -concurrency {conc} {
	set c [string trim $conc]
	switch $c {
	    Active  {set active 1}
	    default {}
	}
    }

    # specifies if this is a public class
    variable public 0

    method -public {dummy} {
	set public 1
	set singleton 1
    }

    method -interface {dummy} {
	$this -public $dummy
    }

    # true if there's one and only one instance of the class
    variable singleton 0

    method -singleton {dummy} {set singleton 1}

    # an abbreviation of the name may be useful (eg, when making names
    # for referential attributes)
    variable abbreviation

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

    #
    # variables & methods related to <<control>> classes (XXX needed?)
    #

    variable isControl 0

    method -control {dummy} {set isControl 1}

    #
    # variables & methods related to <<type>> classes
    #

    variable isType 0

    variable typeInfo

    variable callback

    # called (via stereotype mechanism) to indicate that this is a
    # <<type>> class
    method -type {dummy} {set isType 1}

    # called (via stereotype mechanism) to indicate that this is used
    # in a callback
    method -callback {size} {set callback [string trim $size]}

    # state machine, if specified
    variable statemachine
    method -statemachine {s} {set statemachine $s}

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
    # role is the role that this (XXX???) plays
    # identifier is true if the formalizing attribute(s) are to
    #   be part of obj's identifier
    method -addFormalizingAttributesTo {obj relation role identifier} {
	set attr [ReferentialAttribute ::#auto $this $relation $role $identifier]
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
	if [expr $isType && [$attributes -size] == 0] {
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
	    if $isType {
		# must be a record type
		$this -handleAnnotation
		set dts [[Domain::currentDomain] -getDatatypes]
		if [$dts -isPresent $name] {
		    set dt [$dts -atName $name]
		} else {
		    set dt [Datatype ::#auto $name]
		    $dts -add $dt $name
		}
		$dt -record
	    }
	    [stack -top] -add $this $name
	}
    }

    method -evaluate {domain} {
	$attributes -evaluate $domain
	if [info exists statemachine] {
	    $statemachine -evaluate $domain
	}
    }

    method -generate {domain} {
	if [expr $isType && [$this -hasIdentifier]] {
	    puts stderr "type [$this -getName] has identifier"
	} elseif [expr $singleton && [$this -hasIdentifier]] {
	    puts stderr "singleton [$this -getName] has identifier"
	} elseif [expr !($singleton || $isType) && ![$this -hasIdentifier]] {
	    puts stderr "[$this -getName] has no identifier"
	}
	if $isType {
	    puts -nonewline "<type"
	    if [info exists callback] {
		puts -nonewline " callback=\"$callback\""
	    }
	    puts ">"
	    putElement name "$name"
	    $this -generateDocumentation
	    $attributes -generate $domain
	    $operations -generate $domain
	    puts "</type>"
	} else {
	    puts -nonewline "<class"
	    if $active {puts -nonewline " active=\"yes\""}
	    if [info exists max] {puts -nonewline " max=\"$max\""}
	    if $singleton {puts -nonewline " singleton=\"yes\""}
	    if $public {puts -nonewline " public=\"yes\""}
	    puts ">"
	    putElement name "$name"
	    putElement abbreviation [$this -getAbbreviation]
	    $this -generateDocumentation
	    $attributes -generate $domain
	    $operations -generate $domain
	    if [info exists statemachine] {
		$statemachine -generate $domain
	    }
	    puts "</class>"
	}
    }
}

itcl::class Operation {
    inherit Element

    # is this abstract?
    variable abstr 0

    # is this an access-to-operation?
    variable acc 0

    # is this a class operation?
    variable cls 0

    # is this an initialize operation?
    variable init 0

    # is this a finalize operation?
    variable final 0

    # the return type, if any
    variable ret ""

    variable parameters

    # called via stereotype mechanism to indicate that this is an
    # abstract operation
    method -abstract {summy} {
	set abstr 1
    }

    # called via stereotype mechanism to indicate that this is an
    # access-to-operation (and also a class operation; how would
    # we know which instance to invoke?)
    method -access {dummy} {
	set acc 1
	set cls 1
    }

    # called via stereotype mechanism to indicate that this is a class
    # operation
    method -class {dummy} {set cls 1}

    # called via stereotype mechanism to indicate that this is an
    # initialization operation (and also a class operation).
    method -init {dummy} {
	set cls 1
	set init 1
    }

    # called via stereotype mechanism to indicate that this is an
    # instance finalization operation.
    method -finalize {dummy} {
	set final 1
    }

    method -return {r} {set ret $r}

    method -parameters {pl} {set parameters $pl}

    method -generate {domain}  {
	puts -nonewline "<operation"
	if $abstr {puts -nonewline " abstract=\"yes\""}
	if $acc {puts -nonewline " access=\"yes\""}
	if $cls {puts -nonewline " class=\"yes\""}
	if $init {puts -nonewline " initialize=\"yes\""}
	if $final {puts -nonewline " finalize=\"yes\""}
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
	if {[string length $name] == 0} {
	    error "unnamed association"
	}
	if {[string length [$role1 -getName]] == 0} {
	    error "unnamed role in association $name"
	}
	if {[string length [$role2 -getName]] == 0} {
	    error "unnamed role in association $name"
	}
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
		    $cl2 -addFormalizingAttributesTo $cl1 $this $role2 0
		} elseif [$role1 -getSourceEnd] {
		    $cl1 -addFormalizingAttributesTo $cl2 $this $role1 0
		} elseif [$role1 -getConditionality] {
		    if [$role2 -getConditionality] {
			error "neither end of biconditional $name\
				is marked as source"
		    }
		    $role2 -setSourceEnd 1
		    $cl2 -addFormalizingAttributesTo $cl1 $this $role2 0
		} elseif [$role2 -getConditionality] {
		    $role1 -setSourceEnd 1
		    $cl1 -addFormalizingAttributesTo $cl2 $this $role1 0
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
		$cl1 -addFormalizingAttributesTo $cl2 $this $role1 0
	    }
	    "1-(1:1)" {
		# ensure that one and only one of the roles is marked
		# as the source end
		if [$role2 -getSourceEnd] {
		    if [$role1 -getSourceEnd] {
			error "both ends of $name are marked as source"
		    }
		    $cl1 -addFormalizingAttributesTo $assoc $this $role1 0
		    $cl2 -addFormalizingAttributesTo $assoc $this $role2 1
		} elseif [$role1 -getSourceEnd] {
		    $cl1 -addFormalizingAttributesTo $assoc $this $role1 1
		    $cl2 -addFormalizingAttributesTo $assoc $this $role2 0
		} elseif [$role1 -getConditionality] {
		    if [$role2 -getConditionality] {
			error "neither end of biconditional $name\
				is marked as source"
		    }
		    $role2 -setSourceEnd 1
		    $cl1 -addFormalizingAttributesTo $assoc $this $role1 0
		    $cl2 -addFormalizingAttributesTo $assoc $this $role2 1
		} elseif [$role2 -getConditionality] {
		    $role1 -setSourceEnd 1
		    $cl1 -addFormalizingAttributesTo $assoc $this $role1 1
		    $cl2 -addFormalizingAttributesTo $assoc $this $role2 0
		} else {
		    error "neither end of unconditional $name\
			    is marked as source"
		}
	    }
	    "1-(1:M)" {
		# The identifying attributes in both roles are used as
		# referential attributes in the associative class.
		# Only the referential attributes from role 2 become
		# identifiers in the associative class.
		$role1 -setSourceEnd 1
		$role2 -setSourceEnd 1
		$cl1 -addFormalizingAttributesTo $assoc $this $role1 0
		$cl2 -addFormalizingAttributesTo $assoc $this $role2 1
	    }
	    "1-(M:M)" {
		# The identifying attributes in both roles are used as
		# referential and identifying attributes in the associative
		# class.
		$role1 -setSourceEnd 1
		$role2 -setSourceEnd 1
		$cl1 -addFormalizingAttributesTo $assoc $this $role1 1
		$cl2 -addFormalizingAttributesTo $assoc $this $role2 1
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
	    default {error "unrecognised multiplicity \"$c\" in role \"$name\""}
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
	if {[string length $name] == 0} {
	    error "unnamed inheritance"
	}
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
	    puts stderr "[$p -getName] has no identifier"
	    return
	}
	$this -formalized
	foreach ch [$children -getMembers] {
	    set c [$os -atName $ch]
	    set role [Role ::#auto]
	    $role -owner $this
	    $role -end 4
	    $role -name "Parent"
	    $role -classname $ch
	    $p -addFormalizingAttributesTo $c $this $role 1
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

itcl::class Action {
    inherit Element

    variable arguments
    method -arguments {a} {set arguments $a}

    variable args {}

    method -complete {} {
	if [info exists arguments] {
	    foreach arg [split $arguments ","] {
		lappend args [Argument::parse $arg]
	    }
	}
	[stack -top] -add $this
    }

    method -evaluate {domain} {
	foreach arg $args {
	    $arg -evaluate $domain
	}
    }

    method -generate {domain} {
	puts "<action>"
	putElement name $name
	foreach arg $args {
	    $arg -generate $domain
	}
	puts "</action>"
    }

}

itcl::class Argument {
    variable name
    variable type

    # arg is of the form "name : type"
    proc parse {arg} {
	set spl [split $arg ":"]
	if [expr [llength $spl] != 2] {
	    error "invalid argument \"$arg\""
	}
	set n [normalize [lindex $spl 0]]
	set t [normalize [lindex $spl 1]]
	return [Argument ::#auto $n $t]
    }

    constructor {n t} {
	set name $n
	set type $t
	return $this
    }

    method -evaluate {domain} {
	# cf Attribute
	set datatypes [$domain -getDatatypes]
	if [$datatypes -isMissing $type] {
	    set datatype [Datatype ::#auto $type]
	    $datatypes -add $datatype $type
	} else {
#	    set datatype [$datatypes -atName $type]
	}
    }

    method -generate {domain} {
	puts "<argument>"
	putElement name $name
	putElement type $type
	puts "</argument>"
    }

}

itcl::class Event {
    inherit Element

    variable arguments
    method -arguments {a} {set arguments $a}

    variable args {}

    method -complete {} {
	if [info exists arguments] {
	    foreach arg [split $arguments ","] {
		lappend args [Argument::parse $arg]
	    }
	}
	[stack -top] -event $this
    }

    method -evaluate {domain} {
	foreach arg $args {
	    $arg -evaluate $domain
	}
    }

    method -generate {domain} {
	puts "<event>"
	putElement name $name
	foreach arg $args {
	    $arg -generate $domain
	}
	puts "</event>"
    }

}

itcl::class State {
    inherit Element

    variable entryactions
    method -entryactions {e} {set entryactions $e}

    variable final 0
    method -final {dummy} {set final 1}

    variable initial 0
    method -initial {dummy} {set initial 1}

    method -evaluate {domain} {
#	if [expr [string length $name] == 0] {
#	    if $initial {
#		set name "Initial"
#	    } elseif $final {
#		set name "Final"
#	    } else {
#		# how can I identify this more clearly?
#		error "unnamed state"
#	    }
#	}
	if [info exists entryactions] {
	    $entryactions -evaluate $domain
	}
    }

    method -generate {domain} {
	puts -nonewline "<state"
	if $initial {puts -nonewline " initial=\"yes\""}
	if $final {puts -nonewline " final=\"yes\""}
	puts ">"
	putElement name $name
	if [info exists entryactions] {
	    $entryactions -generate $domain
	}
	puts "</state>"
    }

}

itcl::class StateMachine {
    inherit Element

    variable states
    method -states {s} {set states $s}

    variable transitions
    method -transitions {t} {set transitions $t}

    method -complete {} {
	[stack -top] -statemachine $this
    }

    method -evaluate {domain} {
	$states -evaluate $domain
	$transitions -evaluate $domain
    }

    method -generate {domain} {
	puts "<statemachine>"
	$states -generate $domain
	$transitions -generate $domain
	puts "</statemachine>"
    }

}

itcl::class Transition {
    inherit Element

    variable event
    method -event {e} {set event $e}

    variable source
    method -source {s} {set source $s}

    variable target
    method -target {t} {set target $t}

    method -evaluate {domain} {
	$event -evaluate $domain
    }

    method -generate {domain} {
	puts "<transition>"
	putElement source $source
	putElement target $target
	$event -generate $domain
	puts "</transition>"
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

    # called when the type is imported from some other domain.
    method -imported {domain} {
	set dataType "imported"
	set dataDetail [normalize $domain]
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

    # called when the type is actually a record (a Class with isType set)
    method -record {} {set dataType "record"}

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
	if {$dataType == "record"} {return}
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
	    imported {
		putElement imported $dataDetail
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

itcl::class Transitiontable {
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

#    method -getOwningClass {} {
#	return [[$this -getOwner] -getOwner]
#    }

    # indicates whether this attribute formalizes an association,
    # where the analyst needs to mark what would otherwise be a
    # non-identifying referential attribute
    variable formalizedAssociation

    # used via stereotype processing to indicate that this analyst-defined
    # attribute formalizes an association
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
#	    set datatype [$datatypes -atName $type]
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

    # holds the role
    variable role

    # true if the attribute is to form part of the owning class'
    # identifier
    variable identifier

    constructor {src rel rol id} {
	set source $src
	set relation $rel
	set role $rol
	set identifier $id
    }

    method -getName {} {
	return "[$relation -getName].[$role -getName].[$source -getName]"
    }

    method -getIdentifier {} {return $identifier}

    method -generate {domain} {
	puts -nonewline "<attribute"
	puts -nonewline " refers=\"[$source -getName]\""
	puts -nonewline " relation=\"[$relation -getName]\""
	puts -nonewline " role=\"[$role -getName]\""
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

itcl::class Classes {
    inherit Container
    method -className {} {return "classes"}
}

itcl::class Datatypes {
    inherit Container

    constructor {} {
	# insert standard (provided) types.
	$this -add [Datatype ::#auto Autonumber] Autonumber
	$this -add [Datatype ::#auto Boolean] Boolean
	$this -add [Datatype ::#auto Date] Date
	$this -add [Datatype ::#auto Float] Float
	$this -add [Datatype ::#auto Integer] Integer
	$this -add [Datatype ::#auto Real] Real
	$this -add [Datatype ::#auto String] String
	$this -add [Datatype ::#auto Time] Time
	$this -add [Datatype ::#auto Unbounded_String] Unbounded_String
    }

    method -className {} {return "datatypes"}

}

itcl::class EntryActions {
    inherit List
}

itcl::class Inheritances {
    inherit Container
    method -className {} {return "inheritances"}
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

itcl::class States {
    inherit List
}

itcl::class Transitions {
    inherit List
}

# Convert XML tag name to element of appropriate type

proc elementFactory {tag} {
    # XXX should this perhaps be an operation of Domain?
    switch $tag {
	action            {return [Action #auto]}
	arguments         {return [Arguments #auto]}
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
	concurrency       {return [Concurrency #auto]}
	datatype          {return [Datatype #auto]}
	datatypes         {return [[Domain::currentDomain] -getDatatypes]}
	date              {return [Date #auto]}
	day               {return [Day #auto]}
	documentation     {return [Documentation #auto]}
	domain            {return [Domain #auto]}
	end               {return [End #auto]}
	entryactions      {return [EntryActions #auto]}
	event             {return [Event #auto]}
	exportcontrol     {return [ExportControl #auto]}
	extractor         {return [Extractor #auto]}
	inheritance       {return [Inheritance #auto]}
	inheritances      {return [Inheritances #auto]}
	initial           {return [Initial #auto]}
	month             {return [Month #auto]}
	name              {return [Name #auto]}
	operation         {return [Operation #auto]}
	operations        {return [Operations #auto]}
	parameter         {return [Parameter #auto]}
	parameters        {return [Parameters #auto]}
	parent            {return [Parent #auto]}
	relationship      {return [Relationship #auto]}
	relationships     {return [[Domain::currentDomain] -getRelationships]}
	"return"          {return [Return #auto]}
	revision          {return [Revision #auto]}
	role              {return [Role #auto]}
	source            {return [Source #auto]}
	state             {return [State #auto]}
	statemachine      {return [StateMachine #auto]}
	states            {return [States #auto]}
	target            {return [Target #auto]}
	time              {return [Time #auto]}
	transition        {return [Transition #auto]}
	transitions       {return [Transitions #auto]}
	type              {return [Type #auto]}
	year              {return [Year #auto]}
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

# process command line:
# flags
#   --version cf-20010607
#   --casing filename

set argState expectingFlag
foreach arg $argv {
    switch -- $argState {
	expectingFlag {
	    switch -- $arg {
		--version {set argState expectingVersion}
		--casing  {set argState expectingCaseExceptionFile}
		default   {error "unknown flag $arg"}
	    }
	}
	expectingVersion {
	    set coldFrameVersion $arg
	    set argState expectingFlag
	}
	expectingCaseExceptionFile {
	    setCaseExceptions $arg
	    set argState expectingFlag
	}
    }
}

set parser [xml::parser]
$parser configure \
	-elementstartcommand startTag \
	-elementendcommand endTag \
	-characterdatacommand textInTag

$parser parse [read stdin]
exit 0

if [catch {$parser parse [read stdin]} msg] {
    puts stderr "error: $msg"
    exit 1
}

#;; for emacs:
#;; Local Variables:
#;; tcl-default-application: "itclsh"
#;; End:
