#!/bin/sh
# the next line restarts using itclsh \
exec itclsh "$0" "$@"

# ddf.tcl
# $Id: ddf.tcl,v cb246d39e310 2000/04/29 15:19:57 simon $

# Converts an XML Domain Definition file, generated from Rose by
# ddf.ebs, into the form expected by the Object Oriented Model
# Compiler.

lappend auto_path ~/TclXML-1.2
package require xml
package require Itcl

#############
# Utilities #
#############

proc normalize {s} {
    set tmp [string tolower [string trim $s]]
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

itcl::class Base {

    variable text ""

    variable tag "untagged"

    variable xmlattributes

    variable owner

    method -addText {str} {
	if {$str != "\n"} {set text "$text$str"}
    }

    method -text {t} {set text $t}

    method -tag {t} {set tag $t}

    method -complete {} {}

    method -xmlattributes {arrayname} {
	upvar $arrayname a
	foreach i [array names a] {
	    set xmlattributes($i) "$a($i)"
	    if [expr [string compare "stereotype" $i] == 0] {
		$this -stereotype "$a($i)"
	    }
	}
    }

    method -formatxmlattributes {} {
	set res ""
	if [info exists xmlattributes] {
	    foreach i [array names xmlattributes] {
		set res "$res<$i -> $xmlattributes($i)> "
	    }
	}
	return $res
    }

    method -owner {o} {set owner $o}

    method -getOwner {} {return $owner}

    method -report {} {puts "aBase"}

    method -evaluate {domain} {}

    method -generate {domain} {error "Undefined $tag method -generate"}
}

itcl::class String {
    inherit Base

    method -complete {} {
	[stack -top] -$tag $text
    }

    method -report {} {puts "$tag $text"}
}

itcl::class IdentifierString {
    inherit String

    method -complete {} {
	[stack -top] -$tag [normalize $text]
    }
}

itcl::class Element {
    inherit Base

    variable name "unnamed"

    variable stereotype

    method -name {n} {set name $n}

    method -getName {} {return $name}

    method -stereotype {s} {
	set stereotype $s
    }

    method -stereotypePattern {} {return ""}

    method -handleStereotype {} {
	if [info exists stereotype] {
	    set p [$this -stereotypePattern]
	    if {[string length $p] > 0} {
		set s $stereotype
		for {} {[regexp -nocase $p $s wh n v]} {} {
		    # n is the tag name, v the tag value
		    # should maybe catch errors here!
		    $this -$n "$v"
		    regexp -nocase -indices $p $s wh
		    set s [string range $s [expr [lindex $wh 1] + 1] end]
		}
	    }
	}
    }

    method -complete {} {
	$this -handleStereotype
	[stack -top] -add $this
    }

    method -report {} {
	puts "$tag $name [$this -formatxmlattributes]"
    }
}

itcl::class List {
    inherit Base

    # Lists are for containers constructed during the parse of the XML,
    # where there may be many instances [OK, Simon, give an example!]

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

itcl::class Container {
    inherit Base

    # Containers are for singleton (per-Domain, per-Object) containment,
    # particularly where members need to be revisited.
    # byName is indexed by name and holds the index number in byNumber
    # byNumber is indexed by number and holds the content.

    private variable byName

    private variable byNumber

    constructor {} {
	array set byName {}
	array set byNumber {}
	return $this
    }

    # Derived classes should override -className to return their own
    # name (in lower case).
    method -className {} {return "container"}

    # Return the number of contained elements.
    method -size {} {return [array size byName]}

    # Indicate whether the name doesn't denote an object in the Container.
    method -isMissing {name} {
	return [expr ![info exists byName($name)]]
    }

    # Indicate whether the name denotes an object in the Container.
    method -isPresent {name} {
	return [info exists byName($name)]
    }

    # Add the given object to the container at entry "name".
    method -add {object name} {
	if [info exists byName($name)] {
	    error "[$this -className] already holds an element named $name"
	}
	set newIndex [$this -size]
	set byName($name) $newIndex
	set byNumber($newIndex) $object
    }

    # Find the index of a named element in the Container.
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
 
    # Return the named element from the Container.
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

itcl::class Content {
    # Concrete classes need constructor {name}

    method -className {} {return "Content"}

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

    method -stereotypePattern {} {
	return {[ \t]*([a-z0-9_]+)[ \t]*=[ \t]*([a-z0-9_,]+)[ \t]*}
    }

    method -key {k} {set key $k}

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
	puts "$name"
	puts "$key"
	puts "$number"
	puts "$version"
	puts "today"
	$objects -evaluate $this
	$relationships -evaluate $this
	$datatypes -evaluate $this
	$typesfiles -evaluate $this
	$transitiontables -evaluate $this
	$terminators -evaluate $this
	$objects -generate $this
	$relationships -generate $this
	$datatypes -generate $this
	$typesfiles -generate $this
	$transitiontables -generate $this
	$terminators -generate $this
    }
}

itcl::class Object {
    inherit Element

    variable key

    variable number

    variable relations {}

    variable tags

    variable attributes

    method -stereotypePattern {} {
	return {[ \t]*([a-z0-9_]+)[ \t]*=[ \t]*([a-z0-9_,]+)[ \t]*}
    }

    method -key {k} {set key $k}

    method -getKey {} {return $key}

    method -number {n} {set number $n}

    method -getNumber {} {return $number}

    method -addRelation {n} {
	if [expr [lsearch $relations $n] < 0] {
	    lappend relations $n
	}
    }

    method -tags {l} {set tags $l}

    method -attributes {l} {set attributes $l}

    method -addFormalizingAttributesTo {obj role identifier} {
	foreach a [$attributes -getMembers] {
	    if [$a -getIdentifier] {
		set relName [[$role -getOwner] -getName]
		puts stderr \
		    "[normalize $key\_[$a -getName]\_$relName] added to [$obj -getName],\
		    [$role -getName]"
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
	[stack -top] -add $this $name
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
	puts "$name"
	puts "$key"
	puts "$number"
	puts "[llength $relations]"
	set rels [$domain -getRelationships]
	foreach r $relations {
	    puts "[$rels -indexOf $r]"
	}
	# withs (kinds of relationships used)
	set withs 0
	foreach r $relations {
	    set rel [$rels -atName R$r]
	    set type [$rel -relationshipType]
	    switch $type {
		"1:1"        {set withs [expr $withs | 1]}
		"1:M"        {set withs [expr $withs | 2]}
		"1-(1:1)"    {set withs [expr $withs | 4]}
		"1-(1:M)"    {set withs [expr $withs | 8]}
		"1-(M:M)"    {set withs [expr $withs | 16]}
		"Super/Sub"  {set withs [expr $withs | 32]}
		default      {error "unrecognised relation kind $type"}
	    }
	}
	puts $withs
	# stt index
	puts -1
	$tags -generate $domain
	# will need to take account of extra Status attribute
	$attributes -generate $domain
    }
}

itcl::class Relationship {
    inherit Element

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
	set n [$this -getNumber]
	set os [$domain -getObjects]
	set type [$this -relationshipType]
	set cl1 [$os -atName [$role1 -getClassname]]
	$cl1 -addRelation $n
	set cl2 [$os -atName [$role2 -getClassname]]
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
		    $cl2 -addFormalizingAttributesTo $cl1 $role2 0
		    $cl2 -sourceOfRelation $role1
		} elseif [$role1 -getSourceEnd] {
		    $cl1 -addFormalizingAttributesTo $cl2 $role1 0
		    $cl1 -sourceOfRelation $role2
		} elseif [$role1 -getConditionality] {
		    if [$role2 -getConditionality] {
			error "neither end of biconditional $name\
				is marked as source"
		    }
		    $cl2 -addFormalizingAttributesTo $cl1 $role2 0
		    $cl2 -sourceOfRelation $role1
		} elseif [$role2 -getConditionality] {
		    $cl1 -addFormalizingAttributesTo $cl2 $role1 0
		    $cl1 -sourceOfRelation $role2
		} else {
		    error "neither end of unconditional $name\
			    is marked as source"
		}
	    }
	    "1:M"     {
		# the identifying attributes in role 1 are used as 
		# referential attributes in role 2
		$cl1 -addFormalizingAttributesTo $cl2 $role1 0
		$cl1 -sourceOfRelation $role2
	    }
	    "1-(1:1)" {error "oops! not yet implemented!"}
	    "1-(1:M)" {
		# The identifying attributes in both roles are used as
		# referential attributes in the associative object.
		# Only the referential attributes from role 2 become
		# identifiers in the associative object.
		# XXX role 2 ????????
		$cl1 -addFormalizingAttributesTo $assoc $role1 0
		$cl1 -sourceOfRelation $role2
		$cl2 -addFormalizingAttributesTo $assoc $role2 1
		$cl2 -sourceOfRelation $role1
	    }
	    "1-(M:M)" {
		# The identifying attributes in both roles are used as
		# referential and identifying attributes in the associative
		# object.
                $cl1 -addFormalizingAttributesTo $assoc $role1 1
		$cl1 -sourceOfRelation $role2
		$cl2 -addFormalizingAttributesTo $assoc $role2 1
		$cl2 -sourceOfRelation $role1
	    }
	}
    }

    method -generate {domain} {
	puts "[$this -getNumber]"
	puts "[$this -relationshipType]"
	$role1 -generate $domain
	$role2 -generate $domain
	if [info exists associative] {
	    set os [$domain -getObjects]
	    set cl [$os -atName $associative]
	    puts "1"
	    puts "[$cl -getName]"
	    puts "[$cl -getNumber]"
	    # associative object's cardinality is single (no M-(M:M)'s)
	    puts "0"
	    # associative object is unconditional
	    puts "0"
	    # associative object has no role
	    puts "0"
	    # "end" is C
	    puts "2"
	} else {
	    puts "0"
	}
	# tag info
	puts "0"
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
	puts stderr "$name, exportcontrol $e"
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
	set os [$domain -getObjects]
	set cl [$os -atName $classname]
	puts "[$cl -getName]"
	puts "[$cl -getNumber]"
	switch $cardinality {
	    "1" {puts "0"}
	    "M" {puts "1"}
	}
	puts "$conditional"
	set r [normalize [$this -getName]]
	set l [string length $r]
	if [expr $l > 0] {
	    puts "1\n$r"
	} else {
	    puts "0"
	}
	puts "[expr $end - 1]"
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
	set os [$domain -getObjects]
	set p [$os -atName $parent]
	$p -addRelation [$this -getNumber]
	foreach ch [$children -getMembers] {
	    set c [$os -atName $ch]
	    $c -addRelation [$this -getNumber]
	}
    }

    method -generate {domain} {
	puts "[$this -getNumber]"
	puts "[$this -relationshipType]"
	set os [$domain -getObjects]
	set p [$os -atName $parent]
	puts "[$p -getNumber]"
	puts "[$p -getName]"
	puts "[$children -size]"
	foreach ch [$children -getMembers] {
	    set c [$os -atName $ch]
	    puts "[$c -getName]"
	    puts "[$c -getNumber]"
	}
	# tag info
	puts 0
    }
}

itcl::class Datatype {
    inherit Content

    variable type

    variable package

    variable constraint

    variable objectUsers {}

    variable relationshipUsers {}

    # dataType 0 -> Provided_Data (? XXX)
    variable dataType 0

    constructor {name} {set type $name}

    method -className {} {return "datatype"}

    method -addObjectUser {obj} {
	set name [$obj -getName]
	set objs [$obj -getOwner]
	set index [$objs -index $name]
	puts stderr "$type -addObjectUser: name $name, index $index"
    }

    method -complete {} {
	if [[stack -top] -isPresent $type] {
	    puts "$name already present"
	} else {
	    [stack -top] -add $this $$type
	}
    }

    method -generate {domain} {
	puts "$type"
	if [info exists package] {
	    puts "$package"
	} else {
	    puts "Types_d[$domain -getNumber]"
	}
	# constraint flag -- 0 -> no constraint
	puts 0
	# number of using objects (dummy, object index 1 I think)
	puts "1\n0"
	# relationship users -- fossil, I think
	puts 0
	# data type switch, 0 -> provided
	puts 0
	# event type
	puts -1
	# super type
	puts -1
	# number of tags
	puts 0
    }
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

    variable type

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

    method -makeReferentialClone {key relName} {
	set res [Attribute ::#auto]
	$res -name [normalize $key\_$name\_$relName]
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
	puts "$name"
	puts "$identifier"
	puts "$type"
	set datatypes [$domain -getDatatypes]
	set index [$datatypes -index $type]
	puts "$index"
	# referential users
	puts "[llength $roles]"
	set objects [$domain -getObjects]
	foreach r $roles {
	    set object [$this -getOwningObject]
	    puts -nonewline stderr "[$object -getName].$name "
	    puts -nonewline stderr "used in [[$r -getOwner] -getName] "
	    puts -nonewline stderr "end [$r -getEnd] "
	    puts -nonewline stderr "class [$r -getClassname] "
	    puts stderr ""
	    set rel [$r -getOwner]
	    puts "[$rel -getNumber]"
	    # If the relationship is associative, the using (formalising)
	    # end is the associative object
	    if [$rel -isAssociative] {
		puts "2"
		switch [$r -getEnd] {
		    1       {puts "1"}
		    2       {puts "0"}
		    default {error "oops!"}
		}
		set user [$objects -atName [$rel -getAssociativeObjectName]]
	    } else {
		switch [$r -getEnd] {
		    1       {puts "0\n1"}
		    2       {puts "1\n0"}
		    default {error "oops!"}
		}
		set user [$objects -atName [$r -getClassname]]
	    }
	    puts "[$user -getName]"
	    puts "[$user -getNumber]"
	    puts "[normalize [$object -getKey]\_$name\_[$rel -getName]]"
	}
	# defined-referentially flag
	if $referential {
	    puts stderr "referential attribute [[$this -getOwningObject] -getName].$name"
	}
	puts "$referential"
    }
}

# Aggregate classes

itcl::class CountedList {
    inherit List

    # when generating, outputs its size and then generates its contents.

    method -generate {domain} {
	puts "[$this -size]"
	$this List::-generate $domain
    }
}

itcl::class OuterList {
    inherit CountedList

    # like CountedList, but outputs its own name first

    method -generate {domain} {
	puts "$tag"
	$this CountedList::-generate $domain
    }
}

itcl::class Attributes {
    inherit CountedList
}

itcl::class Associations {
    inherit Container

    method -className {} {return "associations"}
}

itcl::class Datatypes {
    inherit Container

    method -className {} {return "datatypes"}

    method -generate {domain} {
	puts "datatypes"
	puts "[$this -size]"
	Container::-generate $domain
    }
}

itcl::class Inheritances {
    inherit Container

    method -className {} {return "inheritances"}
}

itcl::class Objects {
    inherit Container

    method -className {} {return "objects"}

    method -generate {domain} {
	puts "objects"
	puts "[$this -size]"
	# for reasons that ain't obvious, we need to dump the objects in
	# object number order so a bsearch can be done later
	set size [$this -size]
	set os {}
	for {set i 0} {$i < $size} {incr i 1} {
	    lappend os [$this -atIndex $i]
	}
	proc cmp {o1 o2} {return [expr [$o1 -getNumber] - [$o2 -getNumber]]}
	set os [lsort -command cmp $os]
	foreach o $os {$o -generate $domain}
    }
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
    }

    method -generate {domain} {
	puts "relationships"
	puts "[$this -size]"
	# for reasons that ain't obvious, we need to dump the relations in
	# relation number order so a bsearch can be done later
	foreach r $sorted {$r -generate $domain}
    }
}

itcl::class Tags {
    inherit CountedList
}

itcl::class Terminators {
    inherit OuterList
}

itcl::class Transitiontables {
    inherit OuterList
}

itcl::class Typesfiles {
    inherit OuterList

    method -generate {domain} {
	# XXX not sure about this, probably need only this sort
	puts "typesfiles"
	puts "1"
	puts "Types_d[$domain -getNumber]"
    }
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
	domain            {return [Domain #auto]}
	end               {return [End #auto]}
	exportcontrol     {return [ExportControl #auto]}
	inheritance       {return [Inheritance #auto]}
	inheritances      {return [Inheritances #auto]}
	key               {return [Key #auto]}
	name              {return [Name #auto]}
	number            {return [Number #auto]}
	object            {return [Object #auto]}
	objects           {return [[Domain::currentDomain] -getObjects]}
	parent            {return [Parent #auto]}
	protection        {return [Protection #auto]}
	relationship      {return [Relationship #auto]}
	relationships     {return [[Domain::currentDomain] -getRelationships]}
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
    # puts "start tag $tag"
    # I tried doing these as one operation -- itcl got v confused
    set el [elementFactory $tag]
    array set attr $attrs
    $el -tag $tag
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
