#!/bin/sh
# the next line restarts using itclsh \
exec tclsh "$0" "$@"

# ddf.tcl
# $Id: ddf.tcl,v 73f83b044ea3 2000/03/19 17:39:28 simon $

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

Stack stack

#######################
# XML utility classes #
#######################

itcl::class Base {
    variable text ""
    variable tag "untagged"
    variable xmlattributes
    #constructor {args} {eval configure $args}
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
    method -generate {domain} {error "Undefined $tag method -generate"}
}

itcl::class String {
    inherit Base
    #constructor {args} {eval configure $args}
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
    #constructor {args} {eval configure $args}
    method -name {n} {set name $n}
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
    inherit Base
    variable members {}
    method -add {elem} {lappend members $elem}
    method -size {} {return [llength $members]}
    method -members {} {return $members}
    method -complete {} {
	stack -pop
	[stack -top] -$tag $this
    }
    method -report {} {
	puts "list $tag [$this -formatxmlattributes]"
	foreach el $members {$el -report}
    }
    method -generate {domain} {
	foreach el $members {$el -generate $domain}
    }
}

########################################
# DDF classes for storing the XML info #
########################################

# Non-XML derived classes

itcl::class Datatypes {
    # byName is indexed by name and holds the index number in byNumber
    # byNumber is indexed by number and holds the Datatype
    private variable byName
    private variable byNumber
    constructor {} {
	array set byName {}
	array set byNumber {}
	return $this
    }
    method -size {} {return [array size byName]}
    method -index {name} {
	if [info exists byName($name)] {return $byName($name)}
	set newIndex [$this -size]
	set byName($name) $newIndex
	set byNumber($newIndex) [Datatype #auto $name]
	return $newIndex
    }
    method -atIndex {index} {
	if {$index < [$this -size]} {
	    return $byNumber($index)
	}
	error "Datatype index $index out of range ([$this -size] entries)"
    } 
    method -generate {domain} {
	puts "datatypes"
	set size [$this -size]
	puts "$size"
	for {set i 0} {$i < $size} {incr i 1} {
	    [$this -atIndex $i] -generate $domain
	}
    }
}

itcl::class Datatype {
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

# Element classes

itcl::class Domain {
    inherit Element
    variable key
    variable number
    variable version
    variable objects
    variable relationships
    variable datatypes
    variable typesfiles
    variable transitiontables
    variable terminators
    #constructor {args} {eval configure $args}
    method -stereotypePattern {} {
	return {[ \t]*([a-z0-9_]+)[ \t]*=[ \t]*([a-z0-9_,]+)[ \t]*}
    }
    method -key {k} {set key $k}
    method -number {n} {set number $n}
    method -version {n} {set version $n}
    method -objects {l} {set objects $l}
    method -relationships {l} {set relationships $l}
    method -datatypes {l} {set datatypes $l}
    method -getDatatypes {} {
	if [expr ! [info exists datatypes]] {
	    # XXX I don't understand _when_ it's necessary to put the name
	    # in the global scope.
	    set datatypes [Datatypes ::#auto]
	}
	return $datatypes
    }
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
    variable relations
    variable tags
    variable attributes
    method -stereotypePattern {} {
	return {[ \t]*([a-z0-9_]+)[ \t]*=[ \t]*([a-z0-9_,]+)[ \t]*}
    }
    method -key {k} {set key $k}
    method -number {n} {set number $n}
    method -relations {l} {set relations $l}
    method -tags {l} {set tags $l}
    method -attributes {l} {set attributes $l}
#    method -complete {} {
#	stack -pop
#	[stack -top] -add $this
#    }	
    method -report {} {
	$this Element::-report
	$relations -report
	$tags -report
	$attributes -report
    }
    method -generate {domain} {
	puts "$name"
	puts "$key"
	puts "$number"
	# relations
	$relations -generate $domain
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
    inherit Element
#    method -complete {} {
#	stack -pop
#	[stack -top] -add $this
#    }	
    method -report {} {
	$this Element::-report
    }
}

itcl::class Typesfile {
    inherit Element
    variable attributes [List #auto]
#      method -complete {} {
#  	stack -pop
#  	[stack -top] -add $this
#      }	
    method -report {} {
	$this Element::-report
    }
}

itcl::class Transitiontable {
    inherit Element
#      method -complete {} {
#  	stack -pop
#  	[stack -top] -add $this
#      }	
    method -report {} {
	$this Element::-report
    }
}

itcl::class Terminator {
    inherit Element
#      method -complete {} {
#  	stack -pop
#  	[stack -top] -add $this
#      }	
    method -report {} {
	$this Element::-report
    }
}

itcl::class Relation {
    inherit Element
    variable relations [List #auto]
#      method -complete {} {
#  	stack -pop
#  	[stack -top] -add $this
#      }	
    method -report {} {
	$this Element::-report
    }
}

itcl::class Tag {
    inherit Element
#      method -complete {} {
#  	stack -pop
#  	[stack -top] -add $this
#      }	
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
#      method -complete {} {
#  	stack -pop
#  	[stack -top] -add $this
#      }	
    method -report {} {
	if $identifier {puts -nonewline "* "} else {puts -nonewline "  "}
	puts "$tag $name : $type [$this -formatxmlattributes]"
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

# List classes

itcl::class CountedList {
    inherit List
    method -generate {domain} {
	puts "[$this -size]"
	$this List::-generate $domain
    }
}

itcl::class OuterList {
    inherit CountedList
    method -generate {domain} {
	puts "$tag"
	$this CountedList::-generate $domain
    }
}

itcl::class Objects {
    inherit OuterList
}

itcl::class Relationships {
    inherit OuterList
}

itcl::class Typesfiles {
    inherit OuterList
}

itcl::class Transitiontables {
    inherit OuterList
}

itcl::class Terminators {
    inherit OuterList
}

itcl::class Relations {
    inherit CountedList
}

itcl::class Tags {
    inherit CountedList
}

itcl::class Attributes {
    inherit CountedList
}

# Convert XML tag name to element of appropriate type

proc elementFactory {tag} {
    switch $tag {
	name              {return [Name #auto]}
	type              {return [Type #auto]}
	protection        {return [Protection #auto]}
	key               {return [Key #auto]}
	number            {return [Number #auto]}
	version           {return [Version #auto]}
	domain            {return [Domain #auto]}
	objects           {return [Objects #auto]}
	object            {return [Object #auto]}
	relationships     {return [Relationships #auto]}
	relationship      {return [Relationship #auto]}
	datatypes         {return [Datatypes #auto]}
	datatype          {return [Datatype #auto]}
	typesfiles        {return [Typesfiles #auto]}
	typesfile         {return [Typesfile #auto]}
	transitiontables  {return [Transitiontables #auto]}
	transitiontable   {return [Transitiontable #auto]}
	terminators       {return [Terminators #auto]}
	terminator        {return [Terminator #auto]}
	relations         {return [Relations #auto]}
	relation          {return [Relation #auto]}
	tags              {return [Tags #auto]}
	tag               {return [Tag #auto]}
	attributes        {return [Attributes #auto]}
	attribute         {return [Attribute #auto]}
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
#    puts "the top of the stack is a [[stack -top] cget -tag]"
#    foreach i [array names attr] {
#	puts "<$i -> $attr($i)>"
#    }
}

proc textInTag {str} {
    [stack -top] -addText $str
}

proc endTag {tag} {
    [stack -top] -complete
}

# Main program

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
