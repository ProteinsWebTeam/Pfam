import re, sys, os
import multiprocessing
import numpy

PLOT = True

try:
    import matplotlib, pylab
except ImportError:
    PLOT = False
 #--------------------------------------------------------------------------
def plotHistogram(bins, vals,
                  title = None, 
                  filename = None,
                  f = None,
                  xlabel = "residue",
                  ylabel = "relative cumulative frequency",
                  logscale = "" ):

    # do not plot if called in subprocess. The first time this
    # function is called in a subprocess, it is fine, but called
    # by another, the error
    #
    # adda.py: Fatal IO error 0 (Success) on X server :0.0.
    #
    # appears.
    #
    # The test is not pretty and maintainable, but I could not
    # find how best to test if within MainProcess or not.
    if not re.search( "MainProcess", str(multiprocessing.current_process())):
        return

    if not PLOT: return

    pylab.plot( bins, vals )
    if f: 
        xstart, xend = pylab.gca().get_xlim()
        increment = (xend - xstart) / 100.0
        xvals = numpy.arange( xstart, xend, increment )
        yvals = f( xvals )
        pylab.plot( xvals, yvals )

    if title: pylab.title( title )
    pylab.xlabel( xlabel )
    pylab.ylabel( ylabel )

    ax = pylab.gca()
    if "x" in logscale: ax.set_xscale('log')
    if "y" in logscale: ax.set_yscale('log')

    if filename:
        pylab.savefig( os.path.expanduser(filename) )
    else:
        pylab.show()

    pylab.clf()
    
