#!/bin/sh
# the next line restarts using itclsh \
exec itclsh "$0" "$@"

# ddf.tcl
# $Id: ddf.tcl,v 49a34b52c5ce 2000/04/03 16:54:21 simon $

# Converts an XML Domain Definition file, generated from Rose by
# ddf.ebs, into the form expected by the Object Oriented Model
# Compiler.

lappend auto_path ~/TclXML-1.2
package require xml
package require Itcl

###################
# Utility classes #
###################

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
    method -addText {str} {
	if {$str != "\n"} {set text "$text$str"}
    }
    method -text {t} {set text $t}
    method -tag {t} {set tag $t}
    method -complete {} {stack -pop}
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
    # I expect iterator, output stuff here
    method -report {} {puts "aBase"}
    method -evaluate {domain} {}
    method -generate {domain} {error "Undefined $tag method -generate"}
}

itcl::class String {
    inherit Base
    method -complete {} {
	stack -pop
	[stack -top] -$tag $text
    }
    method -report {} {puts "$tag $text"}
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
	stack -pop
	[stack -top] -add $this
    }
    method -report {} {
	puts "$tag $name [$this -formatxmlattributes]"
    }
}

itcl::class List {
    # Lists are for containers constructed during the parse of the XML,
    # where there may be many instances [OK, Simon, give an example!]
    inherit Base
    variable members {}
    method -add {elem} {lappend members $elem}
    method -size {} {return [llength $members]}
    method -complete {} {
	stack -pop
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

    # Containers are for singleton (per-Domain, per-Object) containment,
    # particularly where members need to be revisited.
    # byName is indexed by name and holds the index number in byNumber
    # byNumber is indexed by number and holds the content.

    inherit Base
    private variable byName
    private variable byNumber

    constructor {} {
	array set byName {}
	array set byNumber {}
	return $this
    }

    # Derived classes should override -className to return their own
    # name (in lower case).
    method -className {} {return "Container"}

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
		([$this -size] entries)"
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
	stack -pop
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

itcl::class Name {
    inherit String
}

itcl::class Type {
    inherit String
}

itcl::class Protection {
    inherit String
    method -complete {} {
	stack -pop
	switch $text {
	    PublicAccess {[stack -top] -identifier}
	    default {}
	}
    }
}

itcl::class Key {
    inherit String
}

itcl::class Number {
    inherit String
}

itcl::class Version {
    inherit String
}

itcl::class End {
    inherit String
}

itcl::class Associative {
    inherit String
}

itcl::class Classname {
    inherit String
}

itcl::class Cardinality {
    inherit String
}

itcl::class Parent {
    inherit String
}

itcl::class Child {
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
    method -number {n} {set number $n}
    method -version {n} {set version $n}
    method -objects {l} {set objects $l}
    method -relationships {l} {set relationships $l}
    method -datatypes {l} {set datatypes $l}
    method -getObjects {} {return $objects}
    method -getRelationships {} {return $relationships}
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
    variable objectrelations
    variable tags
    variable attributes
    method -stereotypePattern {} {
	return {[ \t]*([a-z0-9_]+)[ \t]*=[ \t]*([a-z0-9_,]+)[ \t]*}
    }
    method -key {k} {set key $k}
    method -number {n} {set number $n}
    method -relations {l} {set objectrelations $l}
    method -tags {l} {set tags $l}
    method -attributes {l} {set attributes $l}
    method -report {} {
	$this Element::-report
	$objectrelations -report
	$tags -report
	$attributes -report
    }
    method -evaluate {domain} {
#	$objectrelations -evaluate $domain
#	$tags -evaluate $domain
	$attributes -evaluate $domain
    }
    method -generate {domain} {
	puts "$name"
	puts "$key"
	puts "$number"
	# objectrelations
	#$objectrelations -generate $domain
	puts 0
	# withs
	puts 0
	# stt index
	puts -1
	$tags -generate $domain
	# will need to take account of extra Status attribute
	$attributes -generate $domain
    }
}

itcl::class Relationship {
    inherit Element Content
    method -report {} {
	$this Element::-report
    }
    method -complete {} {error oops}
    method -evaluate {domain} {
	puts "evaluating [$this -getName]"
    }
    method -generate {domain} {
	puts "generating [$this -getName]"
    }
}

itcl::class Association {
    inherit Element
    variable role1
    variable role2
    variable associative
    method -associative {a} {set associative $a}
    method -role {role} {
	set role[$role -getEnd] $role
	puts "adding role [$role -getName] to [$this -getName]"
    }
    method -complete {} {
	stack -pop
	[stack -top] -add $this $name
    }
    method -report {} {
	$this Element::-report
    }
    method -evaluate {domain} {
	puts "evaluating [$this -getName]"
    }
    method -generate {domain} {
	puts "generating association $name"
    }
}

itcl::class Role {
    inherit Element
    variable classname
    variable cardinality
    variable end
    method -classname {n} {set classname $n}
    method -cardinality {c} {set cardinality $c}
    method -end {e} {set end $e}
    method -getEnd {} {return $end}
    method -complete {} {
	puts "Role [$this -getName], class $classname, cardinality $cardinality, end $end"
	stack -pop
	[stack -top] -role $this
    }
    method -report {} {
	$this Element::-report
    }
    method -evaluate {domain} {
	puts "evaluating [$this -getName]"
    }
}

itcl::class Inheritance {
    inherit Element
    variable parent
    # child is the lazy way of not handing the list head ..
    variable child
    variable children
    constructor {} {set children [List ::#auto]}
    method -parent {p} {set parent $p}
    method -child {c} {
	set child $c
	$children -add $c
    }
    method -addChild {c} {
	$children -add $child
    }
    method -report {} {
	$this Element::-report
    }
    method -complete {} {
	puts "Inheritance [$this -getName], parent $parent, child $child"
	stack -pop
	set inheritances [stack -top]
	if [$inheritances -isPresent $name] {
	    puts "$name already present"
	    set extant [$inheritances -atName $name]
	    $extant -addChild $child
	} else {
	    $inheritances -add $this $name
	}
    }
    method -evaluate {domain} {
	puts "evaluating [$this -getName]"
    }
    method -generate {domain} {
	puts "generating inheritance $name"
    }
}

itcl::class Datatype {
    inherit Content
    variable type
    variable package
    variable constraint
    variable objectUsers
    variable relationshipUsers
    variable dataType
    variable eventType
    variable superType
    variable tags
    constructor {name} {set type $name}
    method -className {} {return "datatype"}
    method -complete {} {
	stack -pop
	if [[stack -top] -isPresent $type] {
	    puts "$name already present"
	} else {
	    [stack -top] -add $this $$type
	}
    }
    method -generate {domain} {
	puts "$type"
	puts "SomePackage"
	# constraint flag -- 0 -> no constraint
	puts 0
	# number of using objects
	puts 0
	# relationship users
	puts 0
	# data type switch
	puts -1
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

itcl::class ObjectRelation {
    inherit Element
    method -complete {} {
	$this -name $text
	$this Element::-complete
    }
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
    method -type {t} {set type $t}
    method -identifier {} {set identifier 1}
    method -report {} {
	if $identifier {puts -nonewline "* "} else {puts -nonewline "  "}
	puts "$tag $name : $type [$this -formatxmlattributes]"
    }
    method -evaluate {domain} {
	set datatypes [$domain -getDatatypes]
	if [$datatypes -isMissing $type] {
	    set datatype [Datatype ::#auto $type]
	    puts "new datatype of type $type"
	    $datatypes -add $datatype $type
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
	puts 0
	# defined-referentially flag
	puts 0
    }
}

# Aggregate classes

itcl::class CountedList {
    # when generating, outputs its size and then generates its contents.
    inherit List
    method -generate {domain} {
	puts "[$this -size]"
	$this List::-generate $domain
    }
}

itcl::class OuterList {
    # like CountedList, but outputs its own name first
    inherit CountedList
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

itcl::class ObjectRelations {
    inherit List
    method -generate {domain} {
	set size [$this -size]
	puts "$size"
	if {$size > 0} {	    
	    set rels [$domain -getRelationships]
	    foreach r $members {
		puts -nonewline "rel [$r -getName] at [$rels -index [$r -getName]] "
	    }
	    puts ""
	}
	# XXX fake the "withs" by selecting all possibilities
	puts "-1"
    }
}

itcl::class Objects {
    inherit OuterList
}

itcl::class Relationships {
    inherit Container
    variable associations
    variable inheritances
    method -className {} {return "relationships"}
    method -associations {a} {set associations $a}
    method -inheritances {r} {set inheritances $r}
    method -evaluate {domain} {
	$associations -evaluate $domain
	$inheritances -evaluate $domain
    }
    method -generate {domain} {
	puts "relationships"
	puts "[expr [$associations -size] + [$inheritances -size]]"
	$associations -generate $domain
	$inheritances -generate $domain
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
	inheritance       {return [Inheritance #auto]}
	inheritances      {return [Inheritances #auto]}
	key               {return [Key #auto]}
	name              {return [Name #auto]}
	number            {return [Number #auto]}
	object            {return [Object #auto]}
	objects           {return [[Domain::currentDomain] -getObjects]}
	parent            {return [Parent #auto]}
	protection        {return [Protection #auto]}
	relation          {return [ObjectRelation #auto]}
	relations         {return [ObjectRelations #auto]}
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
    [stack -top] -complete
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
