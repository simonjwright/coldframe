#!/bin/sh
# the next line restarts using itclsh \
exec itclsh "$0" "$@"

# $Id: normalize-rose.tcl,v a7593cf67403 2000/12/16 14:53:20 simon $

# Converts an XML Domain Definition file, generated from Rose by
# ddf.ebs, into normalized XML.

# Copyright (C) Simon Wright <simon@pushface.org>

lappend auto_path ~/TclXML-1.2
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
    method -generate {outermost} {error "Undefined $tag method -generate"}
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
# an element name (eg, <foo> -> name "foo") and may contain a particular
# attribute, the stereotype (eg, <foo stereotype="key=a, number=5">).
itcl::class Element {
    inherit Base

    variable name "unnamed"

    variable stereotype

    method -name {n} {set name $n}

    method -getName {} {return $name}

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
		# should maybe catch errors here!
		$this -[string tolower $n] "$v"
		regexp -nocase -indices $p $s wh
		set s [string range $s [expr [lindex $wh 1] + 1] end]
	    }
	}
    }

    method -complete {} {
	$this -handleStereotype
	if [catch {[stack -top] -add $this} msg] {
	    puts "error adding a [$this -getTag] to a [[stack -top] -getTag]"
	    error $msg
	}
    }

    method -report {} {
	puts "$tag $name [$this -formatxmlattributes]"
    }
}

# Lists are for containers constructed during the parse of the XML,
# where there may be many instances (eg, there are many instances
# of domain/objects/object or object/attributes/attribute)
itcl::class List {
    inherit Base

    variable members {}

    method -add {elem} {lappend members $elem}

    method -size {} {return [llength $members]}

    method -getMembers {} {return $members}

    method -complete {} {
	[stack -top] -$tag $this
    }

    method -report {} {
	puts "list $tag [$this -formatxmlattributes]"
	foreach el $members {$el -report}
    }

    method -evaluate {domain} {
	foreach el $members {$el -evaluate $domain}
    }

    method -generate {domain} {
	foreach el $members {$el -generate $domain}
    }
}

# Containers are for singleton (per-Domain, per-Object) containment,
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
	    error "[$this -className] already holds an element named $name"
	}
	set newIndex [$this -size]
	set byName($name) $newIndex
	set byNumber($newIndex) $object
    }

    # Find the index of a named element in the Container
    method -index {name} {
	if [info exists byName($name)] {return $byName($name)}
	error "[$this -className] item $name not found\
		([$this -size] entries, [array names byName])"
    }

    # Return the indexed element from the Container.
    method -atIndex {index} {
	if {$index < [$this -size]} {
	    return $byNumber($index)
	}
	error "[$this -className] index $index out of range\
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

    method -report {} {
	puts "[$this -className]"
	set size [$this -size]
	puts "$size"
	for {set i 0} {$i < $size} {incr i 1} {
	    [$this -atIndex $i] -report
	}
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

# XXX
itcl::class Content {
    # Concrete classes need constructor {name}

    method -className {} {return "content"}

    method -evaluate {domain} {}

    method -generate {domain} {
	error "[$this -className] -generate not overridden"
    }
}

########################################
# DDF classes for storing the XML info #
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

itcl::class Key {
    inherit String
}

itcl::class Name {
    inherit IdentifierString
}

itcl::class Number {
    inherit String
}

itcl::class Parent {
    inherit IdentifierString
}

itcl::class Protection {
    inherit String

    method -complete {} {
	switch $text {
	    PublicAccess {[stack -top] -identifier}
	    default {}
	}
    }
}

itcl::class Return {
    inherit IdentifierString
}

itcl::class Type {
    inherit IdentifierString
}

itcl::class Version {
    inherit String
}

# Element classes

itcl::class Domain {
    inherit Element

    private common currentDomain

    variable key

    variable number

    variable version

    variable objects

    variable relationships

    variable datatypes

    variable typesfiles

    variable transitiontables

    variable terminators

    proc currentDomain {} {return $currentDomain}

    constructor {} {
	set currentDomain $this
	set objects [Objects ::#auto]
	set relationships [Relationships ::#auto]
	set datatypes [Datatypes ::#auto]
    }

    method -key {k} {set key [string toupper [string trim $k]]}

    method -getKey {} {return $key}

    method -number {n} {set number $n}

    method -getNumber {} {return $number}

    method -version {n} {set version $n}

    method -objects {l} {set objects $l}

    method -getObjects {} {return $objects}

    method -relationships {l} {set relationships $l}

    method -getRelationships {} {return $relationships}

    method -datatypes {l} {set datatypes $l}

    method -getDatatypes {} {return $datatypes}

    method -typesfiles {l} {set typesfiles $l}

    method -transitiontables {l} {set transitiontables $l}

    method -terminators {l} {set terminators $l}

    method -complete {} {
	$this -handleStereotype
	$this -generate
    }

    method -report {} {
	$this Element::-report
	if [info exists objects] {$objects -report}
	if [info exists relationships] {$relationships -report}
	if [info exists datatypes] {$datatypes -report}
	if [info exists typesfiles] {$typesfiles -report}
	if [info exists transitiontables] {$transitiontables -report}
	if [info exists terminators] {$terminators -report}
    }

    method -generate {} {
	puts "<domain>"
	putElement name "$name"
	putElement key "$key"
	putElement number "$number"
	putElement version "$version"
	putElement date "[exec /bin/date]"
	$objects -evaluate $this
	$relationships -evaluate $this
	$datatypes -evaluate $this
	$typesfiles -evaluate $this
	$transitiontables -evaluate $this
	$terminators -evaluate $this
	$objects -generate $this
	$relationships -generate $this
	$datatypes -generate $this
	#$typesfiles -generate $this
	#$transitiontables -generate $this
	#$terminators -generate $this
	puts "</domain>"
    }
}

itcl::class Object {
    inherit Element

    variable key

    variable number

    variable relations {}

    variable tags

    variable attributes

    variable operations

    method -key {k} {set key [string toupper [string trim $k]]}

    method -getKey {} {return $key}

    method -number {n} {set number $n}

    method -getNumber {} {return $number}

    method -addRelation {n} {
	if [expr [lsearch $relations $n] < 0] {
	    lappend relations $n
	}
    }

    method -tags {l} {set tags $l}

    method -operations {l} {set operations $l}

    #
    # variables & methods related to <<control>> objects
    #

    variable isControl 0

    # called (via stereotype mechanism) to indicate that this is a
    # <<control>> object
    method -control {dummy} {set isControl 1}

    #
    # variables & methods related to <<type>> objects
    #

    variable isType 0

    variable typeInfo

    # called (via stereotype mechanism) to indicate that this is a
    # <<type>> object
    method -type {dummy} {set isType 1}

    # called for a <documentation> element to extract the type information
    # from the documentation string. The type info is contained between
    # double square brackets [[ ]]
    method -documentation {d} {
	if [expr $isType && ![regexp {\[\[(.*)\]\]} $d wh typeInfo]] {
	     error "User data type $name has no type information"
	}
    }

    # called when the type is an enumeration. Returns a list of the
    # comma-separated enumerals, which have been normalized (underscores,
    # mixed case)
    private method -enumeration {values} {
	set raw [split $values ","]
	foreach v $raw {
	    set vs [lappend vs [normalize $v]]
	}
	return $vs
    }

    # called when the type is a real. constraint is a set of key/value
    # pairs, which may be newline- or comma-separated.
    private method -real {constraint} {
	return $constraint
    }

    # called when the type is a string. constraint is a set of
    # key/value # pairs, which may be newline- or comma-separated.
    private method -string {constraint} {
	return $constraint
    }


    method -attributes {l} {set attributes $l}

    method -hasIdentifier {} {
	foreach a [$attributes -getMembers] {
	    if [$a -getIdentifier] {
		return 1
	    }
	}
	return 0
    }

    method -addFormalizingAttributesTo {obj relation identifier} {
	foreach a [$attributes -getMembers] {
	    if [$a -getIdentifier] {
		set relName [$relation -getName]
		set attr [$a -makeReferentialClone $key $relName]
		if $identifier {
		    $attr -identifier
		}
		$obj -addReferentialAttribute $attr
	    }
	}
    }

    method -addReferentialAttribute {a} {
	puts stderr "referential attribute [$a -getName] added to $name"
	$a -owner $attributes
	$attributes -add $a
    }

    method -sourceOfRelation {role} {
	foreach a [$attributes -getMembers] {
	    if [$a -getIdentifier] {
		puts stderr \
		    "$name.[$a -getName] is source for [[$role -getOwner] -getName]"
		$a -usedInRole $role
	    }
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
	    if [expr ![regexp {(^.*):(.*$)} $typeInfo wh kind values]] {
		error "bad user type definition \"$typeInfo\""
	    }
	    # need to ensure $kind is trimmed, tolower will do this
	    switch [string tolower $kind] {
		enumeration {$dt -enumeration [$this -enumeration $values]}
		real        {$dt -real [$this -real $values]}
		string      {$dt -string [$this -string $values]}
		default     {error "unrecognised user type definition $kind"}
	    }
	} elseif $isControl {
	    puts stderr "<<control>> not yet handled properly"
	    [stack -top] -add $this $name
	} else {
	    [stack -top] -add $this $name
	}
    }

    method -report {} {
	$this Element::-report
	#$objectrelations -report
	$tags -report
	$attributes -report
    }

    method -evaluate {domain} {
#	$tags -evaluate $domain
	$attributes -evaluate $domain
    }

    method -generate {domain} {
	puts "<object>"
	putElement name "$name"
	putElement key "$key"
	putElement number "$number"
	$attributes -generate $domain
	$operations -generate $domain
	puts "</object>"
    }
}

itcl::class Operation {
    inherit Element

    variable ret ""

    variable parameters

    method -return {r} {set ret $r}

    method -parameters {pl} {set parameters $pl}

    method -generate {domain}  {
	puts -nonewline "<operation"
	if {[string length $ret] > 0} {puts -nonewline " return=\"$ret\""}
	puts ">"
	putElement name $name
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

    # called for a <documentation> element to extract the mode information,
    # if any, from the documentation string. The mode info is contained
    # between double square brackets [[ ]]
    method -documentation {d} {
	if [regexp {\[\[(.*)\]\]} $d wh modeStr] {
	    if [expr ![regexp {(^.*):(.*$)} $modeStr wh kind value]] {
		error "bad parameter mode definition \"$d\""
	    }
	    switch [string trim $kind] {
		mode    {
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
		default {
		    error "bad parameter mode definition \"$kind: $value\""
		}
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
	puts "</parameter>"
    }
}

itcl::class Relationship {
    inherit Element

    variable formalized 0

    method -formalized {} {set formalized 1}

    method -needsFormalizing {} {return [expr !$formalized]}

    method -getNumber {} {
	set name [$this -getName]
	if [expr [regexp -nocase {^r([0-9]+)} $name wh number] == 1] {
	    return $number
	} else {
	    error "bad relationship name \"$name\""
	}
    }
}

itcl::class Association {
    inherit Relationship

    # XXX I'm not at all sure I understand role1, role2 vs KC_A_End etc.

    variable role1

    variable role2

    variable associative

    method -associative {a} {set associative $a}

    method -getAssociativeObjectName {} {return $associative}

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

    method -report {} {
	$this Element::-report
    }

    method -evaluate {domain} {
	if [expr ![$this -needsFormalizing]] {
	    puts stderr "$name already formalized"
	    return
	}
	set n [$this -getNumber]
	set os [$domain -getObjects]
	set type [$this -relationshipType]
	set cl1 [$os -atName [$role1 -getClassname]]
	if [expr ![$cl1 -hasIdentifier]] {
	    puts stderr "[$cl1 -getName] has no identifier(s)"
	    return
	}
	set cl2 [$os -atName [$role2 -getClassname]]
	if [expr ![$cl2 -hasIdentifier]] {
	    puts stderr "[$cl2 -getName] has no identifier(s)"
	    return
	}
	$this -formalized
	$cl1 -addRelation $n
	$cl2 -addRelation $n
	if [$this -isAssociative] {
	    set assoc [$os -atName $associative]
	    $assoc -addRelation $n
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
		    $cl2 -sourceOfRelation $role1
		} elseif [$role1 -getSourceEnd] {
		    $cl1 -addFormalizingAttributesTo $cl2 $this 0
		    $cl1 -sourceOfRelation $role2
		} elseif [$role1 -getConditionality] {
		    if [$role2 -getConditionality] {
			error "neither end of biconditional $name\
				is marked as source"
		    }
		    $cl2 -addFormalizingAttributesTo $cl1 $this 0
		    $cl2 -sourceOfRelation $role1
		} elseif [$role2 -getConditionality] {
		    $cl1 -addFormalizingAttributesTo $cl2 $this 0
		    $cl1 -sourceOfRelation $role2
		} else {
		    error "neither end of unconditional $name\
			    is marked as source"
		}
	    }
	    "1:M"     {
		# the identifying attributes in role 1 are used as 
		# referential attributes in role 2
		$cl1 -addFormalizingAttributesTo $cl2 $this 0
		$cl1 -sourceOfRelation $role2
	    }
	    "1-(1:1)" {error "oops! not yet implemented!"}
	    "1-(1:M)" {
		# The identifying attributes in both roles are used as
		# referential attributes in the associative object.
		# Only the referential attributes from role 2 become
		# identifiers in the associative object.
		$cl1 -addFormalizingAttributesTo $assoc $this 0
		$cl1 -sourceOfRelation $role2
		$cl2 -addFormalizingAttributesTo $assoc $this 1
		$cl2 -sourceOfRelation $role1
	    }
	    "1-(M:M)" {
		# The identifying attributes in both roles are used as
		# referential and identifying attributes in the associative
		# object.
		$cl1 -addFormalizingAttributesTo $assoc $this 1
		$cl1 -sourceOfRelation $role2
		$cl2 -addFormalizingAttributesTo $assoc $this 1
		$cl2 -sourceOfRelation $role1
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
	    set os [$domain -getObjects]
	    putElement associative [[$os -atName $associative] -getName]
	}
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

    # True if the object at this end provides the referential attributes
    # that formalize the relation. Normally we can work this out from the
    # multiplicity and conditionality, but for 1:1, 1c:1c, 1-(1:1) and
    # 1-(1c:1c) the analyst has to specify.
    variable sourceEnd 0

    method -cardinality {c} {
	switch $c {
	    "1"     {set conditional 0; set cardinality "1"}
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

    method -exportcontrol {e} {
	switch $e {
	    "PublicAccess"         -
	    "PrivateAccess"        -
	    "ImplementationAccess" {set sourceEnd 0}
	    "ProtectedAccess"      {set sourceEnd 1}
	    default                {error "unrecognised exportcontrol $e"}
	}
	set exportcontrol $e
    }

    method -getSourceEnd {} {return $sourceEnd}

    method -complete {} {
	[stack -top] -role $this
    }

    method -report {} {
	$this Element::-report
    }

    method -evaluate {domain} {
    }

    method -generate {domain} {
	puts -nonewline "<role"
	if $conditional {puts -nonewline " conditional=\"yes\""}
	if {$cardinality == "M"} {puts -nonewline " multiple=\"yes\""}
	puts ">"
	set os [$domain -getObjects]
	set cl [$os -atName $classname]
	putElement classname [$cl -getName]
	putElement name [$this -getName]
	puts "</role>"
    }
}

itcl::class Inheritance {
    inherit Relationship

    # parent is a string containing the name of the supertype object
    variable parent

    # child is the lazy way of not handing the list head ..
    variable child

    # children is a List of the names of the subtype objects
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

    method -report {} {
	$this Element::-report
    }

    method -complete {} {
	set inheritances [stack -top]
	if [$inheritances -isPresent $name] {
	    set extant [$inheritances -atName $name]
	    $extant -addChild $child
	} else {
	    $inheritances -add $this $name
	}
    }

    method -evaluate {domain} {
	if [expr ![$this -needsFormalizing]] {
	    puts stderr "$name already formalized"
	    return
	}
	set os [$domain -getObjects]
	set p [$os -atName $parent]
	if [expr ![$p -hasIdentifier]] {
	    puts stderr "[$p -getName] has no identifier(s)"
	    return
	}
	$this -formalized
	$p -addRelation [$this -getNumber]
	foreach ch [$children -getMembers] {
	    set c [$os -atName $ch]
	    $c -addRelation [$this -getNumber]
	    $p -addFormalizingAttributesTo $c $this 1
	    set role [Role ::#auto]
	    $role -owner $this
	    $role -end 4
	    $role -name "is supertype of"
	    $role -classname $ch
	    # XXX more here!
	    $p -sourceOfRelation $role
	}
    }

    method -generate {domain} {
	if [$this -needsFormalizing] {
	    error "[$this -getName] is unformalized"
	}
	puts "<inheritance>"
	putElement name $name
	set os [$domain -getObjects]
	set p [$os -atName $parent]
	putElement parent [$p -getName]
	foreach ch [$children -getMembers] {
	    set c [$os -atName $ch]
	    putElement child "[$c -getName]"
	}
	puts "</inheritance>"
    }
}

itcl::class Datatype {
    inherit Content

    variable type

    variable package

    variable constraint

    variable objectUsers {}

    variable relationshipUsers {}

    variable dataType "standard"

    variable dataDetail

    constructor {name} {set type $name}

    method -className {} {return "datatype"}

    method -addObjectUser {obj} {
	set name [$obj -getName]
	set objs [$obj -getOwner]
	set index [$objs -index $name]
    }

    # called when the type is an enumeration. values is a list of the
    # comma-separated enumerals, which have been normalized (underscores,
    # mixed case)
    method -enumeration {values} {
	set dataType "enumeration"
	set dataDetail $values
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

    method -generate {domain} {
	puts "<type>"
	putElement name "$type"
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
	    standard {
		putElement standard $type
	    }
	    default {
		error "oops! dataType '$dataType'"
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

    method -report {} {
	$this Element::-report
    }
}

itcl::class Transitiontable {
    inherit Element

    method -report {} {
	$this Element::-report
    }
}

itcl::class Terminator {
    inherit Element

    method -report {} {
	$this Element::-report
    }
}

itcl::class Tag {
    inherit Element

    method -report {} {
	$this Element::-report
    }
}

itcl::class Attribute {
    inherit Element

    # the type name
    variable type

    # indicates whether this attribute is an identifier
    variable identifier 0

    # indicates whether this attribute is defined referentially
    variable referential 0

    # a list of the roles (relationship ends) for which this attribute
    # is the source for a referential attribute at the other end
    variable roles {}

    method -type {t} {set type $t}

    method -identifier {} {set identifier 1}

    method -getIdentifier {} {return $identifier}

    method -getOwningObject {} {
	return [[$this -getOwner] -getOwner]
    }

    private method -referential {} {set referential 1}

    # called from an object which needs to formalize a relationship by
    # use of a matching attribute
    method -makeReferentialClone {key relName} {
	set res [Attribute ::#auto]
	$res -name $key\_$name\_$relName
	$res -type $type
	# XXX probably need to do something about datatype section
	$res -referential
	return $res
    }

    method -usedInRole {r} {
	lappend roles $r
    }

    method -report {} {
	if $identifier {puts -nonewline "* "} else {puts -nonewline "  "}
	puts "$tag $name : $type [$this -formatxmlattributes]"
    }

    method -evaluate {domain} {
	# extract and store data types
	set datatypes [$domain -getDatatypes]
	if [$datatypes -isMissing $type] {
	    # XXX need to create more carefully. How does it get constructed?
	    set datatype [Datatype ::#auto $type]
	    $datatype -addObjectUser [$this -getOwningObject]
	    $datatypes -add $datatype $type
	} else {
	    set datatype [$datatypes -atName $type]
	    $datatype -addObjectUser [$this -getOwningObject]
	}
    }

    method -generate {domain} {
	puts -nonewline "<attribute"
	if $identifier {puts -nonewline " identifier=\"yes\""}
	if $referential {puts -nonewline " referential=\"yes\""}
	puts ">"
	putElement name "$name"
	# puts "$identifier"
	putElement type "$type"
	puts "</attribute>"
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

itcl::class Objects {
    inherit Container

    method -className {} {return "objects"}
}

itcl::class Operations {
    inherit List
}

itcl::class Parameters {
    inherit List
}

itcl::class Relationships {
    inherit Container

    # sorted holds a list of relationship numbers in-order, as an optimisation
    # while outputting objects (which need the index of the relation in this
    # list, starting from 0, rather than the relationship number).
    variable sorted

    variable sortedNumbers

    method -className {} {return "relationships"}

    method -indexOf {rn} {
	set res [lsearch $sortedNumbers $rn]
	if [expr $res < 0] {error "relation r$rn not found in $sorted"}
	return $res
    }

    method -evaluate {domain} {
	set size [$this -size]
	set sorted {}
	for {set i 0} {$i < $size} {incr i 1} {
	    lappend sorted [$this -atIndex $i]
	}
	proc cmp {r1 r2} {return [expr [$r1 -getNumber] - [$r2 -getNumber]]}
	set sorted [lsort -command cmp $sorted]
	set sortedNumbers {}
	foreach r $sorted {
	    lappend sortedNumbers [$r -getNumber]
	}
	$this Container::-evaluate $domain
	$this Container::-evaluate $domain
    }

    method -generateX {domain} {
	puts "relationships"
	puts "[$this -size]"
	# for reasons that ain't obvious, we need to dump the relations in
	# relation number order so a bsearch can be done later
	foreach r $sorted {$r -generate $domain}
    }
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

itcl::class Typesfiles {
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
	key               {return [Key #auto]}
	name              {return [Name #auto]}
	number            {return [Number #auto]}
	object            {return [Object #auto]}
	objects           {return [[Domain::currentDomain] -getObjects]}
	operation         {return [Operation #auto]}
	operations        {return [Operations #auto]}
	parameter         {return [Parameter #auto]}
	parameters        {return [Parameters #auto]}
	parent            {return [Parent #auto]}
	protection        {return [Protection #auto]}
	relationship      {return [Relationship #auto]}
	relationships     {return [[Domain::currentDomain] -getRelationships]}
	"return"          {return [Return #auto]}
	role              {return [Role #auto]}
	tag               {return [Tag #auto]}
	tags              {return [Tags #auto]}
	terminator        {return [Terminator #auto]}
	terminators       {return [Terminators #auto]}
	transitiontable   {return [Transitiontable #auto]}
	transitiontables  {return [Transitiontables #auto]}
	type              {return [Type #auto]}
	typesfile         {return [Typesfile #auto]}
	typesfiles        {return [Typesfiles #auto]}
	version           {return [Version #auto]}
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
$parser parse [read stdin]

#;; for emacs:
#;; Local Variables:
#;; tcl-default-application: "itclsh"
#;; End:
