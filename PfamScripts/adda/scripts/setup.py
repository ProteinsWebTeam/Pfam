################################################################################
#   Gene prediction pipeline 
#
#   $Id$
#
#   Copyright (C) 2004 Andreas Heger
#
#   This program is free software; you can redistribute it and/or
#   modify it under the terms of the GNU General Public License
#   as published by the Free Software Foundation; either version 2
#   of the License, or (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
#################################################################################
import os, sys, string, re, tempfile, optparse, time

"""install blast
"""

import Experiment

def AddOptions( outfile,  infile, target_directory, source_directory, options ):
    """add options from infile to outfile."""

    keep = False
    for line in infile:
        if re.search("Section parameters: start", line):
            keep = True
            continue
        elif re.search("Section parameters: end", line):
            keep = False
            continue
        elif re.match("DIR_ROOT=", line):
            line = "DIR_ROOT=%s/" % target_directory
        elif re.match("include", line):
            other_file = re.match("include\s+(\S+)", line).groups()[0]
            if re.search("\$\(DIR_SCRIPTS_SRC\)", line ):
                other_file = re.sub( "\$\(DIR_SCRIPTS_SRC\)", source_directory + "/", other_file)
                AddOptions( outfile, open(other_file, "r"),
                            target_directory, source_directory, options)
        elif re.match("DIR_SCRIPTS_SRC", line):
            line = "DIR_SCRIPTS_SRC=%s/\n" % source_directory
        if keep:
            outfile.write( re.sub("\?=", "=", line) )

    outfile.write("\n")

if __name__ == "__main__":

    parser = optparse.OptionParser( version = "%prog version: $Id$")

    parser.add_option("-f", "--force", dest="force", action="store_true",
                      help="force overwrite of existing Makefile." )
    parser.add_option("-d", "--dest", dest="destination", type="string",
                      help="destination directory." )
    parser.add_option("-p", "--project", dest="project", type="string",
                      help="name of the project." )
    parser.add_option("-t", "--tempdir", dest="temporary", type = "string",
                      help="temporary directory, used if local/remote are not set." )
    parser.add_option("-l", "--tempdir-local", dest="temporary_local", type="string",
                      help="pathname to local temporary directory." )
    parser.add_option("-r", "--tempdir-remote", dest="temporary_remote", type="string",
                      help="pathname to remote temporary directory." )

    parser.add_option("-y", "--python-libraries", dest="python_libraries", type="string",
                      help="pathname to directory with python libraries.",
                      action="append")
    
    parser.add_option("-i", "--ld-libraries", dest="ld_libraries", type="string",
                      help="pathname to directory with libaries (LD_LIBRARY_PATH).",
                      action="append")

    parser.add_option("-b", "--binaries", dest="binaries", type="string",
                      help="pathname with binaries.",
                      action="append")
    
    parser.add_option("-m", "--method", dest="method", type="string",
                      help="""method to install [gpipe|gpipe_export|blast|introns|orthology|structures|synteny|
                      orthology_pairwise|orthology_pairwise_multiple|orthology_multiple|orthology_malis|
                      codonbias|codonbias_multiple|analyze_codonbias_duplications].""" )

    parser.set_defaults(
        destination = ".",
        force = False,
        project = None,
        temporary = "/net/cpp-group/gpipe/tmp",
        temporary_local = None,        
        temporary_remote = None,
        method = None,
        binaries=["/production/bin",],
        ld_libraries=["/production/lib",],
        python_libraries=["/production/lib/python",],
        )

    (options, args) = Experiment.Start( parser )

    if not options.method:
        raise "please specify a method."
    
    if len(args) >= 1 and not options.destination:
        options.destination = args[0]
        del args[0]
        
    if not options.destination:
        raise "please specify a destination directory for installing the scripts."

    if not options.temporary_remote:
        options.temporary_remote = options.temporary_local
        
    source_directory = os.path.realpath(os.path.dirname(sys.argv[0]))
    options.python_libraries.append( source_directory )
    target_directory = os.path.abspath(options.destination)

    ## create working directories
    if not os.path.exists( target_directory ):
        os.makedirs( target_directory )
    if options.temporary_local:
        if not os.path.exists( options.temporary_local ):
            os.makedirs( options.temporary_local )
        if not os.path.exists( options.temporary_remote ):
            os.makedirs( options.temporary_remote )

    target_makefile = target_directory + "/Makefile"
    target_script = target_directory + "/setup"    

    if os.path.exists( target_makefile ) and not options.force:
        print "Makefile %s already exists. Use -f to overwrite"
        sys.exit(1)

    ## Create makefile
    source_makefile = source_directory + "/Makefile.%s" % options.method
    if not os.path.exists(source_makefile):
        raise "unknown method %s: %s not found" % (options.method, source_makefile)
    
    ## 1. write parameters
    keep = 0
    outfile = open( target_makefile, "w" )
    outfile.write("############################################\n")    
    outfile.write("# created from %s\n#\n" % source_makefile)
    outfile.write("# created at %s\n#\n" % time.asctime(time.localtime(time.time())))
    outfile.write("# python %s\n#\n" % " ".join(sys.argv))
    outfile.write("############################################\n")

    AddOptions( outfile, open(source_makefile, "r"),
                target_directory, source_directory, options )

    # check if a separate variables makefile is present
    if os.path.exists( source_makefile + "_variables" ):
        target_variables_makefile = target_directory + "/Makefile.variables"
        source_variables_makefile = source_makefile + "_variables"
        outfile_variables = open( target_variables_makefile, "w" )
        
        AddOptions( outfile_variables, open(source_variables_makefile, "r"),
                    target_directory, source_directory, options )

        outfile_variables.write( "############################################\n")
        outfile_variables.write( "# user specified options\n\n")
        if len(args) > 0:
            for arg in args:
                outfile_variables.write( arg + "\n\n")
        
        outfile_variables.close()
        
    else:
        ## 2. write user specified variables into main file
        outfile.write( "############################################\n")
        outfile.write( "# user specified options\n\n")
        if len(args) > 0:
            for arg in args:
                outfile.write( arg + "\n\n")
            
    ## 3. write paths
    outfile.write( "############################################\n")            
    outfile.write( "# include master makefile\n\n")
    outfile.write( "include %s\n\n" % source_makefile)    

    outfile.write( "############################################\n")
    outfile.close()

    ## 4. write setup script to source
    outfile = open( target_script, "w")
    
    outfile.write('''################################################################################
#
#   ADDA
#
#   $Id$
#
#   Copyright (C) 2004 Andreas Heger
#
#   This program is free software; you can redistribute it and/or
#   modify it under the terms of the GNU General Public License
#   as published by the Free Software Foundation; either version 2
#   of the License, or (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
#################################################################################

LD_LIBRARY_PATH=%s:${LD_LIBRARY_PATH}
export LD_LIBRARY_PATH

PATH=%s:${PATH}
export PATH

PYTHONPATH=%s:${PYTHONPATH}
export PYTHONPATH

#-----------------------------------
# helper functions for detecting errors in pipes
#-----------------------------------
detect_pipe_error_helper()
{
    while [ "$#" != 0 ] ; do
        # there was an error in at least one program of the pipe
        if [ "$1" != 0 ] ; then return 1 ; fi
        shift 1
    done
    return 0
}

detect_pipe_error()
{
    detect_pipe_error_helper "${PIPESTATUS[@]}"
    return $?
}

export detect_pipe_error_helper
export detect_pipe_error
''' % (\
        ":".join( options.ld_libraries ),
        ":".join( options.binaries ),
        ":".join( options.python_libraries ) ) )

    outfile.close()
    
    os.system( "chmod 775 %s" % target_script )

    print """----------------------------------------------------

This setup was created with the following command

    python %s

-------------------------------------------------------
""" % (" ".join(sys.argv))

    print "Setup of module '%s' complete.\n" % ( options.method )
    print "Working directory is %s.\n" % (os.path.realpath(target_directory))
    print "Run setup in the working directory before each usage.\n"

    Experiment.Stop()

