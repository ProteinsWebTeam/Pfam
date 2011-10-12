import unittest, os, glob, re, tempfile, fileinput

import SegmentedFile

class TestSegmentedFile(unittest.TestCase):
    mHasHeader = False
    def setUp(self):
        fd, self.mFilename = tempfile.mkstemp()
        os.close(fd)
        os.remove( self.mFilename )

    def create(self):
        outfile = SegmentedFile.openfile( self.mFilename, "w" )
        for x in range(10):
            outfile.write( "%i\n" % x )
        outfile.close()

    def checkContents( self ):
        infile = SegmentedFile.openfile( self.mFilename, "r" )
        data = [int(x) for x in infile ]
        self.assertEqual( data, range( 10 ) )
        
    def tearDown(self):
        if os.path.exists(self.mFilename):
            os.remove( self.mFilename )
        filenames = glob.glob( self.mFilename + ".*" )
        for f in filenames:
            if os.path.exists(f): os.remove( f )

    def checkToken(self, filename):
        lines = open( filename, "r" ).readlines()
        if len(lines) > 0: 
            self.assertEqual( lines[-1], SegmentedFile.TOKEN )
            self.assertEqual( len( [ x for x in lines if re.search( SegmentedFile.TOKEN, x ) ] ), 1, "more than one EOF token in %s: %i" % (filename, len(lines) ))

    def testOpen(self):
        self.create()

    def testMerge(self):
        self.create()
        SegmentedFile.merge( self.mFilename, has_header=self.mHasHeader )
        self.checkContents()

    def testRead( self ):
        self.create()
        self.checkContents()

    def testReOpen( self ):
        self.create()
        self.assertRaises(OSError, SegmentedFile.openfile, self.mFilename, "w" )
        
class TestSegmentedFileNoHeader(TestSegmentedFile):
    """create two files."""

    mHasHeader = False
    def create(self):
        fd, self.mFilename = tempfile.mkstemp()
        outfile = SegmentedFile.openfile( self.mFilename, "w", slice="00-10" )
        for x in range(10): outfile.write( "%i\n" % x )
        outfile.close()
        outfile = SegmentedFile.openfile( self.mFilename, "w", slice="10-20" )
        for x in range(10,20): outfile.write( "%i\n" % x )
        outfile.close()

    def checkContents( self ):
        self.checkToken( self.mFilename )
        infile = SegmentedFile.openfile( self.mFilename, "r", has_header = self.mHasHeader )
        data = [int(x) for x in infile ]
        self.assertEqual( data, range( 20 ) )

class TestSegmentedEmpty(TestSegmentedFile):
    """check if empty files work."""

    mHasHeader = True
    def create(self):
        fd, self.mFilename = tempfile.mkstemp()
        outfile = SegmentedFile.openfile( self.mFilename, "w", slice="00-10" )
        outfile.write("header\n")
        for x in range(10): outfile.write( "%i\n" % x )
        outfile.close()
        outfile = SegmentedFile.openfile( self.mFilename, "w", slice="10-20" )
        outfile.write("header\n")
        outfile.close()
        outfile = SegmentedFile.openfile( self.mFilename, "w", slice="20-30" )
        outfile.write("header\n")
        for x in range(10,20): outfile.write( "%i\n" % x )
        outfile.close()

    def checkContents( self ):
        self.checkToken( self.mFilename )
        infile = SegmentedFile.openfile( self.mFilename, "r", has_header = self.mHasHeader )
        data = [ x for x in infile ]
        self.assertEqual( data[0], "header\n" )
        self.assertEqual( [int(x) for x in data[1:]], range( 20 ) )

class TestSegmentedFileWithHeader(TestSegmentedFile):
    """create two files."""
    mHasHeader = True
    def create(self):
        fd, self.mFilename = tempfile.mkstemp()

        outfile = SegmentedFile.openfile( self.mFilename, "w", slice="00-10" )
        outfile.write("header\n")
        for x in range(10): outfile.write( "%i\n" % x )
        outfile.close()
        outfile = SegmentedFile.openfile( self.mFilename, "w", slice="10-20" )
        outfile.write("header\n")
        for x in range(10,20): outfile.write( "%i\n" % x )
        outfile.close()

    def checkContents( self ):
        self.checkToken( self.mFilename )
        infile = SegmentedFile.openfile( self.mFilename, "r" )
        data = [ x for x in infile ]
        self.assertEqual( data[0], "header\n" )
        self.assertEqual( [int(x) for x in data[1:]], range( 20 ) )

class TestSegmentedFileWithHeaderComments(TestSegmentedFile):
    """create two files."""
    def create(self):
        fd, self.mFilename = tempfile.mkstemp()
        outfile = SegmentedFile.openfile( self.mFilename, "w", slice="00-10" )
        outfile.write("#comment1\n")
        outfile.write("header1\n")
        for x in range(10): outfile.write( "%i\n" % x )
        outfile.close()
        outfile = SegmentedFile.openfile( self.mFilename, "w", slice="10-20" )
        outfile.write("#comment2\n")
        outfile.write("header1\n")
        for x in range(10,20): outfile.write( "%i\n" % x )
        outfile.close()

    def checkContents(self):
        self.create()
        self.assertEqual( SegmentedFile.merge( self.mFilename ), True )
        self.checkToken( self.mFilename )
        infile = SegmentedFile.openfile( self.mFilename, "r" )
        data = [ x for x in infile ]
        self.assertEqual( data[1], "header1\n" )
        self.assertEqual( data[0], "#comment1\n" )
        self.assertEqual( data[12], "#comment2\n" )
        self.assertEqual( [int(x) for x in data[2:12] + data[13:]], range( 20 ) )

# class TestSegmentedFileCompressed(TestSegmentedFile):
#     """create two files."""
#     def create(self):
#         fd, self.mFilename = tempfile.mkstemp( suffix = ".gz" )
#         outfile = SegmentedFile.openfile( self.mFilename, "w", 
#                                           slice="00-10", 
#                                           openhook = fileinput.hook_compressed )
#         outfile.write("#comment1\n")
#         outfile.write("header1\n")
#         for x in range(10): outfile.write( "%i\n" % x )
#         outfile.close()
#         outfile = SegmentedFile.openfile( self.mFilename, "w", slice="10-20",
#                                           openhook = fileinput.hook_compressed )
#         outfile.write("#comment2\n")
#         outfile.write("header1\n")
#         for x in range(10,20): outfile.write( "%i\n" % x )
#         outfile.close()

#     def checkContents(self):
#         # self.assertEqual( SegmentedFile.isComplete( self.mFilename), True )
#         print "flinemae=", self.mFilename
#         self.assertEqual( SegmentedFile.merge( self.mFilename, openhook = fileinput.hook_compressed ), True )
#         print "flinemae=", self.mFilename
#         self.checkToken( self.mFilename )
#         infile = SegmentedFile.openfile( self.mFilename, "r" )
#         data = [ x for x in infile ]
#         self.assertEqual( data[1], "header1\n" )
#         self.assertEqual( data[0], "#comment1\n" )
#         self.assertEqual( data[12], "#comment2\n" )
#         self.assertEqual( [int(x) for x in data[2:12] + data[13:]], range( 20 ) )

if __name__ == '__main__':
    unittest.main()
