#!/bin/sh
# the next line restarts using tclsh \
exec tclsh "$0" "$@"

# $Id: make-build.tcl,v 3499a5c7948d 2003/09/27 16:58:17 simon $

# A handy representation to hold the location-to-domain mapping

set locations {
    {Applications {
        House_Management
    }}
    {Common {
        Digital_IO
    }}
    {Facilities {
        Problem_Reporting
    }}
}

# Convert the representation to a lookup from domain to location
foreach l $locations {
    foreach d [lindex $l 1] {
        set lookup($d) [lindex $l 0]
    }
}

proc output {project main} {
    global lookup

    # Output the standard "with"ed Projects
    foreach p {Options ColdFrame BC Library XMLAda} {
        puts "with \"$p\";"
    }

    # Output the main Project
    puts "project $project is"

    # The main program
    puts "   for Main use (\"[string tolower $main]\");"

    # Where to put the executable
    puts "   for Exec_Dir use \".\";"

    if [catch {open $project.domains r} f] {
        Error "can't open $project.domains"
    } else {
        puts "   for Source_Dirs use"
        puts "      ("
        set continuation 0
        while {[gets $f line] >= 0} {
            set d [string trim $line]
            if [info exists lookup($d)] {
                set loc $lookup($d)/$d
                if $continuation {puts ","}
                puts "       \"$loc.impl\","
                puts -nonewline "       \"$loc.gen\""
                set continuation 1
            } else {
                Warning "domain $d not included"
            }
        }
        puts "\n      );"
        close $f
    }

    # The standard compiler settings
    puts "   for Object_Dir use Options'Object_Dir;"
    foreach c {Ide Builder Compiler Binder Linker} {
        puts "   package $c renames Options.$c;"
    }

    # Close the nain Project
    puts "end $project;"

}


###########
# Globals #
###########

set errors 0

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
#   --main main-program
#   --project project-file-name

set argState expectingFlag
foreach arg $argv {
    switch -- $argState {
        expectingFlag {
            switch -- $arg {
                --main  {set argState expectingMain}
                --project  {set argState expectingProject}
                 default   {error "unknown flag $arg"}
            }
        }
        expectingMain {
            set main $arg
            set argState expectingFlag
        }
        expectingProject {
            set project $arg
            set argState expectingFlag
        }
    }
}

if [info exists main] {
    output $project $main
} else {
    output $project $project
}
