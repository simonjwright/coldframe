#!/bin/sh
# the next line restarts using tclsh \
exec tclsh "$0" "$@"

# $Id$

proc readDomains {domainFile} {
    if [catch {open $domainFile r} f] {
        Error "can't open $domainFile"
    } else {
        global lookup generateOnly
        while {[gets $f line] >= 0} {
            regsub -all "," $line " " inf
            set lookup([lindex $inf 1]) [lindex $inf 0]
            switch -exact [lindex $inf 2] {
                g       {
                    set generateOnly([lindex $inf 1]) 1
                }
                default {}
            }
        }
        close $f
    }
}


proc outputGPR {project main base} {
    global env lookup generateOnly

    if [catch {open $project.gpr w} o] {
        Error "can't open $project.gpr"
    } else {

        # Output the standard "with"ed Projects
        foreach p {Options ColdFrame BC} {
            puts $o "with \"$p\";"
        }

        # Output the main Project
        puts $o "project $project is"

        # The main program
        puts $o "   for Main use (\"[string tolower $main]\");"

        # Where to put the executable
        puts $o "   for Exec_Dir use \".\";"

        if [catch {open $env(COLDFRAMEOUT)/$project.domains r} f] {
            Error "can't open $env(COLDFRAMEOUT)/$project.domains"
        } else {
            puts $o "   for Source_Dirs use"
            puts $o "      ("
            puts -nonewline $o "       \".\""
            while {[gets $f line] >= 0} {
                set d [string trim $line]
                if [info exists lookup($d)] {
                    set loc $base$lookup($d)/$d
                    puts $o ","
                    if ![info exists generateOnly($d)] {
                        puts $o "       \"$loc.impl\","
                    }
                    puts -nonewline $o "       \"$loc.gen\""
                    set continuation 1
                } else {
                    Warning "domain $d not included"
                }
            }
            puts $o "\n      );"
            close $f
        }

        # The standard compiler settings
        puts $o "   for Object_Dir use Options'Object_Dir & \"/main\";"
        foreach c {IDE Builder Compiler Binder Linker} {
            puts $o "   package $c renames Options.$c;"
        }

        # Close the nain Project
        puts $o "end $project;"

        # And finish

        close $o

    }
}


proc outputMake {project base} {
    global env lookup

    if [catch {open $project.mk w} o] {
        Error "can't open $project.mk"
    } else {

        if [catch {open $env(COLDFRAMEOUT)/$project.domains r} f] {
            Error "can't open $env(COLDFRAMEOUT)/$project.domains"
        } else {
            while {[gets $f line] >= 0} {
                set d [string trim $line]
                if [info exists lookup($d)] {
                    puts $o "cd $base$lookup($d)"
                    puts $o "make -f $env(TOP)/$env(COLDFRAME)/Makefile-winnt $d.gen"
                } else {
                    Warning "domain $d not included"
                }
            }
            close $f
        }

        # And finish

        close $o

    }
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

if [info exists env(BASE)] {
    set base $env(BASE)/
} else {
    set base ""
}

# process command line:
# flags
#   --base code-base-directory
#   --domains domain-mapping-file
#   --main main-program
#   --project project-file-name

set argState expectingFlag
foreach arg $argv {
    switch -- $argState {
        expectingFlag {
            switch -- $arg {
                --base     {set argState expectingBase}
                --domains  {set argState expectingDomains}
                --main     {set argState expectingMain}
                --project  {set argState expectingProject}
                 default   {error "unknown flag $arg"}
            }
        }
        expectingBase {
            set base $arg/
            set argState expectingFlag
        }
        expectingDomains {
            readDomains $arg
            set argState expectingFlag
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
    outputGPR $project $main $base
} else {
    outputGPR $project $project $base
}

outputMake $project $base
