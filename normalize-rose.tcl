#!/bin/sh
# the next line restarts using itclsh \
exec itclsh "$0" "$@"

# $Id: normalize-rose.tcl,v 2b45e4b4b97e 2003/03/30 18:58:16 simon $

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

# Reads case exceptions from a file or colon-separated list of files.
# The file format is that used by ACT's GLIDE; each line contains a case
# exception. If preceded by *, any matching word in an identifier will be
# adjusted; otherwise, the whole identifier has to match, underscores
# included. 
proc setCaseExceptions {files} {
    global tcl_platform caseExceptions subCaseExceptions

    if {$tcl_platform(platform) == "windows"} {
	set sep ";"
    } else {
	set sep ":"
    }

    foreach file [split $files $sep] {
	if [catch {open $file r} f] {
	    Warning "can't open case exception file $file: $f"
	} else {
	    while {[gets $f line] >= 0} {
		regsub -all {[\*\t ]} $line "" res
		if {[string range $res 0 0] == "*"} {
		    set res [string range $res 1 end]
		    set subCaseExceptions([string tolower $res]) $res
		} else {
		    set caseExceptions([string tolower $res]) $res
		}
	    }
	    close $f
	}
    }
}


# Given a string,
# - split at dots into "components"
# - for each component, split at space or underscore into words
# - for each word, if it matches a substring case exception then apply
#   the exception, otherwise capitalise it
# - join the words up into components using underscore
# - if the component matches a whole string case exception then apply
#   the exception, otherwise capitalise it
# - join the components up using dots
# - return the result
proc normalize {s} {
    global caseExceptions subCaseExceptions

    set s [string trim $s]

    # check for illegal characters
    regsub -all {[A-Za-z0-9_ \t.]} $s "" illegal
    if {[string length $illegal] > 0} {
	set tag [[stack -top] -getXmlTag]
	if {$tag == "parameter"} {
	    # Special circuitry to report the most likely occurrence in
	    # a more helpful manner.
	    # XXX probably needs help from class Stack.
	    set p [stack -pop]
	    set ps [stack -pop]
	    set op [stack -pop]
	    set ops [stack -pop]
	    set cls [stack -top]
	    stack -push $ops
	    stack -push $op
	    stack -push $ps
	    stack -push $p
	    Error "illegal name \"$s\" in [$cls -getName].[$op -getName]"
	} else {
	    Error "illegal name \"$s\" in <$tag>"
	}
    }

    # check for reserved words
    if [isLanguageReservedWord $s] {
 	set tag [[stack -top] -getXmlTag]
	Error "reserved word \"$s\" in <$tag>"
    }

    # split at dots
    set components [split $s .]
    set processedComponents {}
    foreach c $components {
	# handle the words; convert runs of spaces/underscores to spaces
	set c [string trim $c]
	if {[string length $c] == 0} {
	    Warning "spare . in \"$s\""
	}
	regsub -all {[ _]+} $c " " c
	set words [split $c]
	set processedWords {}
	foreach w $words {
	    # handle substring case exceptions
	    if [info exists subCaseExceptions([string tolower $w])] {
		set w $subCaseExceptions([string tolower $w])
	    } else {
		set w \
		    [string toupper [string index $w 0]][string range $w 1 end]
	    }
	    lappend processedWords $w
	}
	set c [join $processedWords _]
	# handle string case exceptions
	if [info exists caseExceptions([string tolower $c])] {
	    set c $caseExceptions([string tolower $c])
	} else {
	    set c \
		[string toupper [string index $c 0]][string range $c 1 end]
	}
	lappend processedComponents $c
    }

    return [join $processedComponents .]
}


# Given a string,
# - trims leading and trailing white space
# - handles "strings" and 'c'haracters directly
# - handles signed entities by recursion
# - handles based literals directly
# - handles null directly
# otherwise,
# - handles as identifier.
proc normalizeValue {s} {
    set tmp [string trim $s]
    # handle strings
    if [regexp {^\".*\"$} $tmp] {return $tmp}
    # handle characters
    if [regexp {^\'.\'$} $tmp] {return $tmp}
    # allow signed entities
    if [regexp {[-+]} $tmp] {
	set sign [string index $tmp 0]
	return "$sign[normalizeValue [string range $tmp 1 end]]"
    }
    # allow based literals
    if [regexp -nocase {[0-9_]+\#[0-9a-f_.]*\#} $tmp] {return $tmp}
    switch [string tolower $tmp] {
	null       {return null}
	default    {return [normalize $tmp]}
    }
}


# check for Ada reserved words
proc isLanguageReservedWord {w} {
    global languageReservedWord
    return [info exists languageReservedWord([string tolower $w])]
}

foreach w {
    abort
    abs
    abstract
    accept
    access
    aliased
    all
    and
    array
    at
    begin
    body
    case
    constant
    declare
    delay
    delta
    digits
    do
    else
    elsif
    end
    entry
    exception
    exit
    for
    function
    generic
    goto
    if
    in
    is
    limited
    loop
    mod
    new
    not
    null
    of
    or
    others
    out
    package
    pragma
    private
    procedure
    protected
    raise
    range
    record
    rem
    renames
    requeue
    return
    reverse
    select
    separate
    subtype
    tagged
    task
    terminate
    then
    type
    until
    use
    when
    while
    with
    xor
} {
    set languageReservedWord($w) 1
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
    variable xmlTag

    # set the xmlTag
    method -xmlTag {t} {set xmlTag $t}

    # get the xmlTag
    method -getXmlTag {} {return $xmlTag}


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
		Warning "CF: XML attribute <$xmlTag $i=\"$a($i)\"> not handled"
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
    method -generate {outermost} {Error "CF: undefined $xmlTag method -generate"}
}


# The base class for all XML elements whose interesting aspect is their
# textual content - eg, <name>foo</name>
# Also handles tagged values; if there are any, the stack top needs to
# offer a "-tag tag value" method.
itcl::class String {
    inherit Base

    method -processTags {} {
	if [regexp "\[\{\}\]" $text] {
	    regsub -all "\}\[^{\]*\{" $text "," stripInner
	    regsub -all "(^.*\{)|(\}.*$)" $stripInner "" stripOuter
	    set taglist [split $stripOuter ","]
	    foreach t $taglist {
		regexp {([^=]*)(=(.*))?} $t wh tag eq val
		if {[string length $val] == 0} {
		    set val true
		}
		[stack -top] -tag [string trim $tag] [string trim $val]
	    }
	    # strip out any property lists
	    regsub -all "\{\[^}\]*\}" $text "" text
	}
    }

    # The object has already been popped off the stack; call the stack top's
    # method with the same name as this tag to store the value (so, given
    # the example above, the containing object needs to offer a -name method).
    method -complete {} {
	$this -processTags
	[stack -top] -$xmlTag $text
    }
}


# The base class for all XML elements which represent identifiers; stores
# eg " hELLO   world " as "Hello_World".
itcl::class IdentifierString {
    inherit String

    # the object has already been popped off the stack; call the stack top's
    # method with the same name as this xmlTag to store the value after
    # conversion to identifier form (so, given the example above, the
    # containing object needs to offer a -name method)
    method -complete {} {
	$this -processTags
	[stack -top] -$xmlTag [normalize $text]
    }
}


# The base class for all XML elements which represent values; stores
# eg " hello   world " as "Hello_World".
itcl::class ValueString {
    inherit String

    # the object has already been popped off the stack; call the stack top's
    # method with the same name as this xmlTag to store the value after
    # conversion to clean value form (so, given the example above, the
    # containing object needs to offer a -name method)
    method -complete {} {
#	$this -processTags
	[stack -top] -$xmlTag [normalizeValue $text]
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

    variable tags

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
	    set attr [string tolower $attr]
	    if {[string length $value] == 0} {set value "true"}
	    if [catch {$this -$attr $value}] {
		Warning \
		    "annotation not handled, \
		    \"$name \[\[$attr : [string trim $value]]]\""
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
	    set p {[ \t]*([-a-z0-9_ \t]+)(=[ \t]*([a-z0-9_,]+))?[ \t]*}
	    set s $stereotype
	    for {} {[regexp -nocase $p $s wh n opt v]} {} {
		# n is the tag name, v the tag value if any
		if [catch {$this -[join [split [string tolower $n]] "-"] "$v"}] {
		    Warning \
		        "stereotype not handled,\
			\"$name <<[string tolower $n]>>\""
		}
		regexp -nocase -indices $p $s wh
		set s [string range $s [expr [lindex $wh 1] + 1] end]
	    }
	    unset stereotype
	}
    }

    # Handle property lists of tagged values.
    method -tag {tag value} {
	set tags($tag) $value 
    }

    method -complete {} {
	if [info exists tags] {
	    foreach w [array names tags] {
		puts stderr "$xmlTag $name: $w=$tags($w)"
	    }
	}
	$this -handleStereotype
	if [catch {[stack -top] -add $this} msg] {
	    Error "CF: error \"$msg\" adding a [$this -getXmlTag]\
		    to a [[stack -top] -getXmlTag]"
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
	[stack -top] -$xmlTag $this
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
	    Error "CF: [$this -className] already holds an element named $name"
	}
	set newIndex [$this -size]
	set byName($name) $newIndex
	set byNumber($newIndex) $object
    }

    # Find the index of a named element in the Container
    method -index {name} {
	if [info exists byName($name)] {return $byName($name)}
	Error "CF: [$this -className] item $name not found\
		([$this -size] entries, [array names byName])"
    }

    # Return the indexed element from the Container.
    method -atIndex {index} {
	if {$index < [$this -size]} {
	    return $byNumber($index)
	}
	Error "CF: [$this -className] index $index out of range\
		([$this -size] entries)"
    }
 
    # Return the named element from the Container
    method -atName {name} {
	set index [$this -index $name]
	return [$this -atIndex $index]
    }

    # Return a List of the members
    method -getMembers {} {
	set res {}
	set size [$this -size]
	for {set i 0} {$i < $size} {incr i 1} {
	    lappend res [$this -atIndex $i]
	}
	return $res
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

itcl::class Abstract {
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
    inherit ValueString
}

itcl::class Month {
    inherit String
}

itcl::class Name {
    inherit IdentifierString
}

itcl::class ParameterName {
    inherit String

    # the object has already been popped off the stack; call the stack top's
    # method with the same name as this xmlTag to store the value after
    # handling any initial parameter mode and conversion to identifier form.
    method -complete {} {
	$this -processTags
	set words [split $text]
	set first [lindex $words 0]
	switch -exact $first {
	    in -
	    out -
	    inout   {
		[stack -top] -mode $first
		set text [lrange $words 1 end]
	    }
	    default {}
	}
	[stack -top] -$xmlTag [normalize $text]
    }
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
    
    variable datatypes
    
    variable exceptions
    
    variable relationships
    
    proc currentDomain {} {return $currentDomain}
    
    constructor {} {
	set currentDomain $this
	set classes [Classes ::\#auto]
	set datatypes [Datatypes ::\#auto]
	set exceptions [Exceptions ::\#auto]
	set relationships [Relationships ::\#auto]
    }

    method -extractor {e} {set extractor $e}

    method -date {d} {set date $d}
    method -revision {r} {set revision [string trim $r]}

    method -classes {l} {set classes $l}

    method -getClasses {} {return $classes}

    method -datatypes {l} {set datatypes $l}

    method -getDatatypes {} {return $datatypes}

    method -exceptions {l} {set exceptions $l}

    method -getExceptions {} {return $exceptions}

    method -relationships {l} {set relationships $l}

    method -getRelationships {} {return $relationships}

    method -complete {} {
	$this -generate
    }

    method -generate {} {
	global coldFrameVersion

	$classes -evaluate $this
	$datatypes -evaluate $this
	$exceptions -evaluate $this
	$relationships -evaluate $this

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
	$datatypes -generate $this
	$exceptions -generate $this
	$relationships -generate $this

	puts "</domain>"
    }
}

itcl::class Class {
    inherit Element

    constructor {} {set events [EventSet ::\#auto]}

    # contains the attributes
    variable attributes

    method -attributes {l} {set attributes $l}

    # contains the operations
    variable operations

    method -operations {l} {set operations $l}

    # contains any events
    variable events

    method -events {l} {
	foreach e [$l -getMembers] {
	    $events -add $e [$e -getName]
	}
    }

    # Interface to let a StateMachine tell us its events
    method -addEvent {e} {
	if [$events -isMissing [$e -getName]] {
	    $events -add $e [$e -getName]
	}
    }

    # specifies the maximum number of instances (optional)
    variable max

    method -max {s} {set max [string trim $s]}

    method -cardinality {c} {
	set c [string trim $c]
	if [regexp {^(([0-9]+) *\.\. *)?([1-9][0-9]*|n|\*)$} $c \
		whole b1 lower upper] {
	    if [string length $lower] {
		switch -exact $lower {
		    "0"     {
			switch -exact $upper {
			    "n"     -
			    "*"     {}
			    default {$this -max $upper}
			}
		    }
		    "1"     {
			switch -exact $upper {
			    "1"     {
				$this -singleton dummy
				$this -max 1
			    }
			    default {
				Error "illegal lower bound 1 in cardinality \
				\"$c\" for $name"
			    }
			}
		    }
		    default {
			Error "illegal lower bound $lower in cardinality \
			\"$c\" for $name"
		    }
		}
	    } else {
		switch -exact $upper {
		    "n"     -
		    "*"     {}
		    "1"     {$this -singleton dummy}
		    default {
			Error "must specify cardinality for $name as 0..$upper"
		    }
		}
	    }
	} else {
	    Error "unrecognised cardinality \"$c\" in $name"
	}
    }

    # specifies if this class is abstract
    variable abstr 0

    method -abstract {dummy} {set abstr 1}

    # specifies if this is an active class
    variable active 0

    method -active {dummy} { set active 1}

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
	set max 1
    }

    method -interface {dummy} {
	$this -public $dummy
    }

    # specifies if this is a visible class
    variable visible 0

    method -visible-for-test {dummy} {set visible 1}

    # true if there's one and only one instance of the class
    variable singleton 0

    method -singleton {dummy} {
	set singleton 1
	set max 1
    }

    # an abbreviation of the name may be useful (eg, when making names
    # for referential attributes)
    variable abbreviation

    method -abbreviation {a} {
	set abbreviation [string toupper [string trim $a]]
    }

    method -getAbbreviation {} {
	# if no abbreviation has been supplied,
	#   if the name consists of more than one word, make one up from
	#     the initial letters of the name (which will already have been
	#     capitalised)
	#   otherwise, prefix the name with A.
	if ![info exists abbreviation] {
	    set tmp [split $name "_"]
	    if {[llength $tmp] == 1} {
		if [string match {[AEIOU]} [string index $name 0]] {
		    set abbreviation "An_$name"
		} else {
		    set abbreviation "A_$name"
		}
	    } else {
		set abbreviation ""
		foreach w $tmp {
		    set abbreviation "$abbreviation[string index $w 0]"
		}
	    }
	}
	# check for reserved words
	if [isLanguageReservedWord $abbreviation] {
	    Error "abbreviation of $name is reserved word"
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

    variable callback

    variable discriminated 0

    variable extends

    variable serializable 0

    # called (via stereotype mechanism) to indicate that this is a
    # <<type>> class
    method -type {dummy} {set isType 1}

    # called (via annotation mechanism) to indicate that this is used
    # in a callback
    method -callback {size} {
	set callback [string trim $size]
    }

    # called (via annotation mechanism) to indicate that this is a
    # discriminated (record) type
    method -discriminated {dummy} {set discriminated 1}

    # called (via annotation mechanism) to indicate that this type
    # extends an (imported) base type
    method -extends {base} {set extends [normalize $base]}

    # called (via annotation mechanism) to indicate that this type
    # is serializable
    method -serializable {dummy} {set serializable 1}

    #
    # variables and methods related to <<exception>> classes
    #

    variable isException 0

    method -exception {dummy} {set isException 1}

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
	set attr [ReferentialAttribute ::\#auto $this $relation $role $identifier]
	$obj -addReferentialAttribute $attr
    }

    method -addReferentialAttribute {a} {
	Message "referential attribute [$a -getName] added to $name"
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
	if {$isType && [$attributes -size] == 0} {
	    if $discriminated {
		Error "discriminated type [$this -getName] has no attributes"
	    }
	    if [info exists extends] {
		Error "tried to extend [$this -getName],\
                    which has no attributes"
	    }
	    set dts [[Domain::currentDomain] -getDatatypes]
	    if [$dts -isPresent $name] {
		set dt [$dts -atName $name]
	    } else {
		set dt [Datatype ::\#auto $name]
		$dts -add $dt $name
	    }
	    # transfer the operations, the documentation and the
	    # already-extracted annotation to the new Datatype.
	    if [info exists operations] {
		$dt -operations $operations
	    }
	    $dt -documentation $documentation
	    $dt -annotation $annotation
	    if [info exists callback] {
		$dt -callback $callback
	    }
	    if $serializable {
		$dt -serializable 1
	    }
	} elseif $isControl {
	    Warning "<<control>> not yet handled properly"
	    [stack -top] -add $this $name
	} elseif $isException {
	    set exs [[Domain::currentDomain] -getExceptions]
	    set ex [Exception ::\#auto $name] 
	    $exs -add $ex $name
	    $ex -documentation $documentation
	} else {
	    if $isType {
		# must be a record type
		$this -handleAnnotation
		set dts [[Domain::currentDomain] -getDatatypes]
		if [$dts -isPresent $name] {
		    set dt [$dts -atName $name]
		} else {
		    set dt [Datatype ::\#auto $name]
		    $dts -add $dt $name
		}
		$dt -record
	    }
	    [stack -top] -add $this $name
	}
    }

    method -evaluate {domain} {
	$attributes -evaluate $domain
	$operations -evaluate $domain
	if [info exists statemachine] {
	    $statemachine -evaluate $domain
	}
    }

    method -generate {domain} {
	if [expr $isType && [$this -hasIdentifier]] {
	    Error "type [$this -getName] has identifier"
	} elseif [expr $singleton && [$this -hasIdentifier]] {
	    Error "singleton [$this -getName] has identifier"
	} elseif [expr !($singleton || $isType) && ![$this -hasIdentifier]] {
	    Error "[$this -getName] has no identifier"
	}
	if $isType {
	    puts -nonewline "<type"
	    if [info exists callback] {
		puts -nonewline " callback=\"$callback\""
	    }
	    if $discriminated {
		puts -nonewline " discriminated=\"yes\""
	    }
	    if [info exists extends] {
		puts -nonewline " extends=\"$extends\""
	    }
	    if $serializable {
		puts -nonewline " serializable=\"yes\""
	    }
	    puts ">"
	    putElement name "$name"
	    $this -generateDocumentation
	    $attributes -generate $domain
	    $operations -generate $domain
	    puts "</type>"
	} else {
	    puts -nonewline "<class"
	    if $abstr {puts -nonewline " abstract=\"yes\""}
	    if $active {puts -nonewline " active=\"yes\""}
	    if [info exists max] {puts -nonewline " max=\"$max\""}
	    if $singleton {puts -nonewline " singleton=\"yes\""}
	    if $public {puts -nonewline " public=\"yes\""}
	    if $visible {puts -nonewline " visible=\"yes\""}
	    puts ">"
	    putElement name "$name"
	    putElement abbreviation [$this -getAbbreviation]
	    $this -generateDocumentation
	    $attributes -generate $domain
	    $operations -generate $domain
	    $events -generate $domain
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
    # called via stereotype mechanism to indicate that this is an
    # abstract operation
    method -abstract {dummy} {set abstr 1}

    # is this an access-to-operation?
    variable acc 0
    # called via stereotype mechanism to indicate that this is an
    # access-to-operation (and also a class operation; how would
    # we know which instance to invoke?)
    method -access {dummy} {
	set acc 1
	set cls 1
    }

    # is this an accessor?
    variable accessor 0
    method -accessor {dummy} {set accessor 1}

    # is this a class operation?
    variable cls 0
    # called via stereotype mechanism to indicate that this is a class
    # operation
    method -class {dummy} {set cls 1}

    # does this have a calling convention?
    variable convention
    # called via stereotype mechanism to indicate that this has a
    # calling convention
    method -convention {conv} {set convention [normalize $conv]}

    # is this an initialize operation?
    variable init 0
    # called via stereotype mechanism to indicate that this is an
    # initialization operation (and also a class operation).
    method -init {dummy} {
	set cls 1
	set init 1
    }

    # is this a finalize operation?
    variable final 0
    # called via stereotype mechanism to indicate that this is an
    # instance finalization operation.
    method -finalize {dummy} {
	set final 1
    }

    # is this a teardown operation?
    variable teardown 0
    # called via stereotype mechanism to indicate that this is an
    # instance teardown operation.
    method -teardown {dummy} {
	set teardown 1
    }

    # is this a <<class event>> event handler:
    variable handler 0
    # called via stereotype mechanism to indicate that this is a
    # <<class event>> event handler (and also a class operation).
    method -handler {dummy} {
	set cls 1
	set handler 1
    }

    # is this a task entry?
    variable entry 0
    # called via stereotype mechanism to indicate that the operation
    # is a task entry
    method -entry {dummy} {set entry 1}

    # is this operation one that we expect to be generated?
    # may be unset, "framework", "navigation", "instantiation".
    variable suppressed
    # called via stereotype mechanism to indicate that this is
    # expected to be generated or otherwise omitted, and is only
    # included for completeness and to allow its inclusion in
    # sequence diagrams etc
    method -generated {dummy} {
	set suppressed "framework"
    }
    method -navigation {dummy} {
	set suppressed "navigation"
    }
    method -instantiation {dummy} {
	set suppressed "instantiation"
    }

    # the return type, if any
    variable ret ""
    method -return {r} {set ret $r}

    variable parameters
    method -parameters {pl} {set parameters $pl}

    method -evaluate {domain} {
	$parameters -evaluate {domain}
    }

    method -generate {domain}  {
	puts -nonewline "<operation"
	if $abstr {puts -nonewline " abstract=\"yes\""}
	if $acc {puts -nonewline " access=\"yes\""}
	if $accessor {puts -nonewline " accessor=\"yes\""}
	if $cls {puts -nonewline " class=\"yes\""}
	if [info exists convention] {
	    puts -nonewline " convention=\"$convention\""
	}
	if $entry {puts -nonewline " entry=\"yes\""}
	if $final {puts -nonewline " finalize=\"yes\""}
	if $teardown {puts -nonewline " teardown=\"yes\""}
	if $handler {puts -nonewline " handler=\"yes\""}
	if $init {puts -nonewline " initialize=\"yes\""}
	if [info exists suppressed] {
	    puts -nonewline " suppressed=\"$suppressed\""
	}
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

    # special name handling for mode
    method -parametername {n} {
	$this -name $n
    }

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
		Error "unrecognised parameter mode $value"
	    }
	}
    }

    method -evaluate {domain} {
 	# check name, type present
	if [expr [string length $name] == 0] {
	    set op [[$this -getOwner] -getOwner]
	    set cls [[$op -getOwner] -getOwner]
	    Error "missing parameter name in\
                    [$cls -getName].[$op -getName]"
	}
	if [expr [string length $type] == 0] {
	    set op [[$this -getOwner] -getOwner]
	    set cls [[$op -getOwner] -getOwner]
	    Error "missing parameter type for\
                    [$cls -getName].[$op -getName]($name)"
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

    # called from class cls during evaluation to indicate that the class
    # named src is used as the source
    method -formalizingSource {cls src} {
	set r1cls [$role1 -getClassname]
	set r2cls [$role2 -getClassname]
	if {$r1cls == $src && $r2cls == $cls} {
	    if [$role2 -getSourceEnd] {
		Error "$cls tried to set $src as source for $name\
                       when [$role2 -getName] already is"
	    } else {
		$role1 -setSourceEnd 1
	    }
	} elseif {$r2cls == $src && $r1cls == $cls} {
	    if [$role1 -getSourceEnd] {
		Error "$cls tried to set $src as source for $name\
                       when [$role1 -getName] already is"
	    } else {
		$role2 -setSourceEnd 1
	    }
	} else {
	    Error "$cls tried to set $src as source for $name"
	}
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
	    Error "illegal M:M association [$this -getName]"
	}
	return $type
    }

    method -complete {} {
	# -relationshipType may swap the roles over! Must be called before
	# anything else is done.
	if {[string length $name] == 0} {
	    Error "unnamed association between \"[$role1 -getClassname]\"\
		    and \"[$role2 -getClassname]\""
	}
	if {[string length [$role1 -getName]] == 0} {
	    Error "unnamed role in association $name"
	}
	if {[string length [$role2 -getName]] == 0} {
	    Error "unnamed role in association $name"
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
			Error "both ends of $name are marked as source"
		    }
		    $cl2 -addFormalizingAttributesTo $cl1 $this $role2 0
		} elseif [$role1 -getSourceEnd] {
		    $cl1 -addFormalizingAttributesTo $cl2 $this $role1 0
		} elseif [$role1 -getConditionality] {
		    if [$role2 -getConditionality] {
			Error "neither end of biconditional $name\
				is marked as source"
		    }
		    $role2 -setSourceEnd 1
		    $cl2 -addFormalizingAttributesTo $cl1 $this $role2 0
		} elseif [$role2 -getConditionality] {
		    $role1 -setSourceEnd 1
		    $cl1 -addFormalizingAttributesTo $cl2 $this $role1 0
		} else {
		    Error "neither end of unconditional $name\
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
			Error "both ends of $name are marked as source"
		    }
		    $cl1 -addFormalizingAttributesTo $assoc $this $role1 0
		    $cl2 -addFormalizingAttributesTo $assoc $this $role2 1
		} elseif [$role1 -getSourceEnd] {
		    $cl1 -addFormalizingAttributesTo $assoc $this $role1 1
		    $cl2 -addFormalizingAttributesTo $assoc $this $role2 0
		} elseif [$role1 -getConditionality] {
		    if [$role2 -getConditionality] {
			Error "neither end of biconditional $name\
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
		    Error "neither end of unconditional $name\
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
	    Error "CF: [$this -getName] is unformalized"
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
	proc badUpperMultiplicity {upper name} {
	    global stack
	    set this [stack -pop]
	    set rel [[stack -top] -getName]
	    stack -push $this
	    Warning "upper bound ($upper) of multiplicity in role $rel.$name\
                     treated as *"
	}
	proc badLowerMultiplicity {lower name} {
	    global stack
	    set this [stack -pop]
	    set rel [[stack -top] -getName]
	    stack -push $this
	    Error "illegal lower bound ($lower) of multiplicity in role\
                   $rel.$name"
	}
	set c [string trim $c]
	if [regexp {^(([0-9]+) *\.\. *)?([1-9][0-9]*|n|\*)$} $c \
		whole b1 lower upper] {
	    if [string length $lower] {
		switch -exact $lower {
		    "0"     {
			switch -exact $upper {
			    "1"     {set conditional 1; set cardinality "1"}
			    "n"     -
			    "*"     {set conditional 1; set cardinality "M"}
			    default {
				badUpperMultiplicity $upper $name
				set conditional 1; set cardinality "M"
			    }
			}
		    }
		    "1"     {
			switch -exact $upper {
			    "1"     {set conditional 0; set cardinality "1"}
			    "n"     -
			    "*"     {set conditional 0; set cardinality "M"}
			    default {
				badUpperMultiplicity $upper $name
				set conditional 0; set cardinality "M"
			    }
			}
		    }
		    default {
			badLowerMultiplicity $lower $name
			# set some values to allow further processing
			set conditional 0; set cardinality "1"
		    }
		}
	    } else {
		switch -exact $upper {
		    "1"     {set conditional 0; set cardinality "1"}
		    "n"     -
		    "*"     {set conditional 1; set cardinality "M"}
		    default {
			badUpperMultiplicity $upper $name
			set conditional 0; set cardinality "M"
		    }
		}
	    }
	} else {
	    global stack
	    set me [stack -pop]
	    set rel [[stack -top] -getName]
	    stack -push $me
	    Error "unrecognised multiplicity \"$c\" in role $rel.$name"
	    # set some values to allow further processing
	    set conditional 0; set cardinality "*"
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

    constructor {} {set children [List ::\#auto]}

    method -parent {p} {
	set parent $p
    }

    method -getParent {} {return $parent}

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
	    Error "unnamed inheritance of $parent by $child"
	}
	set inheritances [stack -top]
	if [$inheritances -isPresent $name] {
	    set extant [$inheritances -atName $name]
	    if {$parent != [$extant -getParent]} {
		if {[string length $name] != 0} {
		    Error "multiple parents $parent, [$extant -getParent]\
                           in $name"
		}
	    } else {
		$extant -addChild $child
		$extant -documentation $documentation
	    }
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
	$this -formalized
	foreach ch [$children -getMembers] {
	    set c [$os -atName $ch]
	    set role [Role ::\#auto]
	    $role -owner $this
	    $role -end 4
	    $role -name "Parent"
	    $role -classname $ch
	    $p -addFormalizingAttributesTo $c $this $role 1
	}
    }

    method -generate {domain} {
	if [$this -needsFormalizing] {
	    Error "CF: [$this -getName] is unformalized"
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

    method -generate {domain} {
	puts "<action>"
	putElement name $name
	puts "</action>"
    }

}

itcl::class Event {
    inherit Element

    variable type
    method -type {t} { set type $t}

    # Dependencies stereotyped <<event>> produce instance (state machine)
    # events. Dependencies stereotyped <<class event>> produce class events.
    variable cls 0
    method -class-event {dummy} {set cls 1}
    method -instance-event {dummy} {set cls 0}

    method -complete {} {
	[stack -top] -event $this
    }

    method -generate {domain} {
	puts -nonewline "<event"
	if $cls {puts -nonewline " class=\"yes\""}
	puts ">"
	putElement name $name
	if [info exists type] {
	    putElement type $type
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
	$this -generateDocumentation
	puts "</state>"
    }

}

itcl::class StateMachine {
    inherit Element

    constructor {} {set events [EventSet ::\#auto]}

    variable states
    method -states {s} {set states $s}

    variable transitions
    method -transitions {t} {set transitions $t}

    variable events
    # called by each transition during evaluation to add its event.
    method -addEvent {e} {
	if [$events -isMissing [$e -getName]] {
	    $events -add $e [$e -getName]
	}
    }

    method -complete {} {
	[stack -top] -statemachine $this
    }

    method -evaluate {domain} {
	$states -evaluate $domain
	$transitions -evaluate $domain
	set es [$events -getMembers]
	foreach e $es {
	    [$this -getOwner] -addEvent $e
	}
    }

    method -generate {domain} {
	puts "<statemachine>"
	putElement name "$name"
	$states -generate $domain
	# output the events here as well as in the class, so we can generate
	# the state machine-specific bits (might have some events not
	# involved in state machine).
	$events -generate $domain
	$transitions -generate $domain
	puts "</statemachine>"
    }

}

itcl::class Transition {
    inherit Element

    variable event
    method -event {e} {set event $e}

    variable ignore 0
    method -ignore {dummy} {set ignore 1}

    # is this a transition triggered by an event-to-self?
    variable self 0
    method -self {dummy} {set self 1}

    variable source
    method -source {s} {set source $s}

    variable target
    method -target {t} {set target $t}

    method -evaluate {domain} {
	# tell the statemachine about the event unless this is an unguarded
	# transition (XXX what's the word for that?)
	if {[string length [$event -getName]] > 0} {
	    $event -evaluate $domain
	    set mc [[$this -getOwner] -getOwner]
	    $mc -addEvent $event
	}
    }

    method -generate {domain} {
	puts -nonewline "<transition"
	if $ignore then {puts -nonewline " ignore=\"yes\""}
	if $self then {puts -nonewline " self=\"yes\""}
	puts ">"
	if {[string length [$event -getName]] > 0} {
	    putElement event [$event -getName]
	}
	putElement source $source
	putElement target $target
	$this -generateDocumentation
	puts "</transition>"
    }

}

itcl::class Datatype {
    inherit Element

    variable callback

    variable dataDetail

    variable dataType "standard"

    variable hash

    variable image

    variable operations

    variable serializable 0

    variable type

    constructor {name} {set type $name}

    # process annotation.
    method -annotation {a} {
	set annotation $a
	set pattern {^[ \t\n]*([-a-z0-9_]+)[ \t\n]*(:([^;]+))?[;]?}
	for {} {[regexp \
		-nocase \
		$pattern \
		$a wh attr dummy value]} {} {
	    set attr [string tolower $attr]
	    if {[string length $value] == 0} {set value "true"}
	    if [catch {$this -$attr [string trim $value]}] {
		Warning \
		    "annotation not handled, \
		    \"$type \[\[$attr : [string trim $value]]]\""
	    }
	    regexp -nocase -indices $pattern $a wh
	    set a [string range $a \
		    [expr [lindex $wh 1] + 1] end]
	}
    }

    method -className {} {return "datatype"}

    method -operations {ops} {
	set operations $ops
    }

    # called when the user has requested a callback.
    method -callback {max} {
	set callback [string trim $max]
    }

    # called when the user has tried to extend a non-record type.
    method -discriminated {dummy} {
	Error "discriminated type $type has no attributes"
    }

    # called when the user has tried to extend a non-record type.
    method -extends {dummy} {
	Error "tried to extend $type, which has no attributes"
    }

    # called when the user has marked a non-record type as
    # serializable.
    method -serializable {dummy} {set serializable 1}

    # Called when the type is a counterpart.
    method -counterpart {dummy} {
	set dataType "counterpart"
    }

    # called when the type is an enumeration. values is a list of the
    # comma-separated enumerals, which have not been normalized.
    method -enumeration {values} {
	set dataType "enumeration"
	set raw [split $values ",|"]
	foreach v $raw {
	    set vs [lappend vs [normalize $v]]
	}
	set dataDetail $vs
    }

    # called to specify an image function (for serialization)
    method -image {img} {set image [normalize $img]}

    # called when the type is imported from some other domain.
    method -imported {domain} {
	set dataType "imported"
	set dataDetail [normalize $domain]
    }

    # called when the type renames some other type.
    method -renames {other} {
	set dataType "renames"
	set dataDetail [normalize $other]
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

    # called when the type is a string. constraint is a set of key/value
    # pairs, which may be newline- or comma-separated.
    # Useful key is max (max length).
    method -string {constraint} {
	set dataType "string"
	regsub -all {,[ \t]*} "[string trim $constraint]" "\n" dataDetail
    }

    # called to specify the hash mechanism (probably only relevant
    # for imported/renamed types)
    method -hash {as} {set hash [string tolower $as]}

    method -complete {} {
	if [expr ![[stack -top] -isPresent $type]] {
	    [stack -top] -add $this $type
	}
    }

    method -generate {domain} {
	if {$dataType == "record"} {return}
	puts -nonewline "<type"
	if [info exists callback] {
	    puts -nonewline " callback=\"$callback\""
	}
	if [info exists image] {
	    puts -nonewline " image=\"$image\""
	}
	if $serializable {
	    puts -nonewline " serializable=\"yes\""
	}
	puts ">"
	putElement name "$type"
	$this -generateDocumentation
	switch $dataType {
	    counterpart {
		puts "<counterpart/>"
	    }
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
	    renames {
		putElement renames $dataDetail
	    }
	    standard {
		putElement standard $type
	    }
	    default {
		Error "CF: unhandled dataType \"$dataType\""
	    }
	}
	if [info exists hash] {
	    putElement hash $hash
	}
	if [info exists operations] {
	    $operations -generate $domain
	}
	puts "</type>"
    }
}

itcl::class Documentation {
    inherit String
}

itcl::class Exception {
    inherit Element

    constructor {n} {set name $n}

    method -generate {domain} {
	puts "<exception>"
	putElement name "$name"
	$this -generateDocumentation
	puts "</exception>"
    }
}

itcl::class Transitiontable {
    inherit Element
}

itcl::class XmlTag {
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

    # indicates whether this is a class attribute
    variable cls 0

    method -type {t} {set type $t}

    method -initial {i} {set initial $i}
    
    # used via stereotype processing to indicate this is an identifying
    # attribute
    method -id {dummy} {$this -identifier}

    method -identifier {} {set identifier 1}

    method -getIdentifier {} {return $identifier}

    # used via stereotype processing to indicate this is a class attribute
    method -class {dummy} {set cls 1}

    # indicates whether this attribute formalizes an association,
    # where the analyst needs the attribute to help form the identifier
    variable formalizedAssociation

    # used via stereotype processing to indicate that this analyst-defined
    # attribute formalizes an association. It's implicit that the class
    # at the other end will be the "source".
    method -formalizes {assoc} {
	set formalizedAssociation [normalize $assoc]
    }

    method -evaluate {domain} {
	# check id vs class
	if {$identifier && $cls} {
	    Error "attribute [[[$this -getOwner] -getOwner] -getName].$name\
		    marked as <<class>> and <<id>>"
	}
	# check name, type present (in fact Rose requires the name)
	if [expr [string length $name] == 0] {
	    Error "missing attribute name in\
                    [[[$this -getOwner] -getOwner] -getName]"
	}
	if [expr [string length $type] == 0] {
	    Error "missing attribute type for\
                    [[[$this -getOwner] -getOwner] -getName].$name"
	}
	# extract and store data types
	set datatypes [$domain -getDatatypes]
	if [$datatypes -isMissing $type] {
	    set datatype [Datatype ::\#auto $type]
	    $datatypes -add $datatype $type
	} else {
#	    set datatype [$datatypes -atName $type]
	}
	if [info exists formalizedAssociation] {
	    # we rely on Relationships being evaluated after Classes
	    # (and Attributes are evaluated because they're in Classes).
	    set rels [$domain -getRelationships]
	    if [$rels -isPresent $formalizedAssociation] {
		set rel [$rels -atName $formalizedAssociation]
		$rel -formalized
		$rel -formalizingSource \
		    [[[$this -getOwner] -getOwner] -getName] \
		    $type
	    } else {
		Error "attribute\
                       [[[$this -getOwner] -getOwner] -getName].$name \
                       used to formalize non-existent $formalizedAssociation"
	    }
	}
    }

    method -generate {domain} {
	puts -nonewline "<attribute"
	if $identifier {puts -nonewline " identifier=\"yes\""}
	if $cls {puts -nonewline " class=\"yes\""}
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

itcl::class XmlTaggedList {
    inherit List

    # when generating, generates its contents within a container element
    # (same name as the list xmlTag).
    method -generate {domain} {
	puts "<$xmlTag>"
	$this List::-generate $domain
	puts "</$xmlTag>"
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
	$this -add [Datatype ::\#auto Autonumber] Autonumber
	$this -add [Datatype ::\#auto Boolean] Boolean
	$this -add [Datatype ::\#auto Date] Date
	$this -add [Datatype ::\#auto Float] Float
	$this -add [Datatype ::\#auto Integer] Integer
	$this -add [Datatype ::\#auto Real] Real
	$this -add [Datatype ::\#auto String] String
	$this -add [Datatype ::\#auto Time] Time
	$this -add [Datatype ::\#auto Timer] Timer
	$this -add [Datatype ::\#auto Unbounded_String] Unbounded_String
    }

    method -className {} {return "datatypes"}

}

itcl::class EntryActions {
    inherit List
}

itcl::class EventSet {
    inherit Container
    method -className {} {return "eventset"}
}

# This is used because Event occurs in 2 contexts; (a) in <transition>,
# (b) in <events>.
itcl::class Events {
    inherit List
    method -event {e} {$this -add $e}
}

itcl::class Exceptions {
    inherit Container
    method -className {} {return "exceptions"}
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

proc elementFactory {xmlTag} {
    # XXX should this perhaps be an operation of Domain?
    switch $xmlTag {
	abstract          {return [Abstract #auto]}
	action            {return [Action #auto]}
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
	events            {return [Events #auto]}
	exception         {return [Exception #auto]}
	exceptions        {return [Exceptions #auto]}
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
	parametername     {return [ParameterName #auto]}
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
    $el -xmlTag $t
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
set verbose 0
set errors 0
set stackDump 0

#############
# Utilities #
#############

proc Message {str} {
    global verbose
    if $verbose {
	puts stderr $str
    }
}

proc Warning {str} {
    puts stderr "Warning: $str"
}

proc Error {str} {
    global errors
    puts stderr "Error: $str"
    incr errors
}

################
# Main program #
################

# process command line:
# flags
#   --casing filename
#   --stack-dump
#   --verbose
#   --version cf-20010607

set argState expectingFlag
foreach arg $argv {
    switch -- $argState {
	expectingFlag {
	    switch -- $arg {
		--casing  {set argState expectingCaseExceptionFile}
		--stack-dump {set stackDump 1}
		--verbose {set verbose 1}
		--version {set argState expectingVersion}
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

if $stackDump {
    $parser parse [read stdin]
} else {
    if [catch {$parser parse [read stdin]} msg] {
	puts stderr "CF: internal error: $msg"
	exit 1
    }
}

if $errors {
    if {$errors == 1} {
	puts stderr "One error detected."
    } else {
	puts stderr "$errors errors detected."
    }
    exit 1
}

#;; for emacs:
#;; Local Variables:
#;; tcl-default-application: "itclsh"
#;; End:
