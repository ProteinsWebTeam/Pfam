import unittest, os, glob, re, tempfile, gzip

import FileSlice

class TestFileSlice(unittest.TestCase):

    mNumLines = 2
    mNumRecords = 20
    mGzipFactor = None

    def setUp(self):
        fd, self.mFilename = tempfile.mkstemp()
        os.close(fd)
        outfile = open(self.mFilename, "w")
        l = 0
        for x in range(0,self.mNumRecords): 
            for y in range(0,self.mNumLines): 
                outfile.write( "%i\t%i\t%i\t%i\n" % (x,y,l,y) )
                l += 1
        outfile.close()
        
    def tearDown(self):
        if os.path.exists(self.mFilename):
            os.remove( self.mFilename )

    def checkComplete(self, nchunks ):

        all_data = []
        for chunk in range( 0, nchunks):
            iterator = FileSlice.Iterator( self.mFilename, 
                                           nchunks,
                                           chunk, 
                                           FileSlice.iterator,
                                           gzip_factor = self.mGzipFactor )
            
            data = []
            for d in iterator:
                d = d.split("\t")
                self.assertEqual( len(d), 4 )
                data.append(d)
            all_data += data

        self.assertEqual( len(all_data), self.mNumRecords * self.mNumLines )
        self.assertEqual( [int(x[2]) for x in all_data], range( 0, self.mNumRecords * self.mNumLines ) )

    def testComplete(self):
        self.checkComplete( self.mNumRecords )
        
    def testFraction(self):
        for x in range( 0, self.mNumRecords):
            self.checkComplete( self.mNumRecords - x )
        
    def testThird(self):
        self.checkComplete( self.mNumRecords // 2 )

    def testSingle(self):
        self.checkComplete( 1 )

    def testFull(self):
        self.checkComplete( self.mNumRecords * self.mNumLines )
        
    def testDouble(self):
       self.checkComplete( 2 * self.mNumRecords )
    
    def testPlus1(self):
       self.checkComplete( self.mNumRecords + 1)

class TestFileSliceGzipped(TestFileSlice):

    def setUp(self):
        fd, self.mFilename = tempfile.mkstemp()
        os.close(fd)
        self.mFilename += ".gz"
        outfile = gzip.open(self.mFilename, "w")
        l = 0
        for x in range(0,self.mNumRecords): 
            for y in range(0,self.mNumLines): 
                outfile.write( "%i\t%i\t%i\t%i\n" % (x,y,l,y) )
                l += 1
        outfile.close()

class TestFileSliceGzippedCat(TestFileSlice):
    """test if compression works for concatenated files.
    """
    # expected compression
    mGzipFactor = 0.30

    def setUp(self):
        fd, self.mFilename = tempfile.mkstemp()
        os.close(fd)
        self.mFilename += ".gz"
        outfile = gzip.open(self.mFilename, "w")
        l = 0
        for x in range(0,self.mNumRecords // 2): 
            for y in range(0,self.mNumLines): 
                outfile.write( "%i\t%i\t%i\t%i\n" % (x,y,l,y) )
                l += 1
        outfile.close()
        outfile = gzip.open(self.mFilename, "a")
        for x in range(self.mNumRecords // 2,self.mNumRecords): 
            for y in range(0,self.mNumLines): 
                outfile.write( "%i\t%i\t%i\t%i\n" % (x,y,l,y) )
                l += 1
        outfile.close()

class TestFileSliceGroupBy(TestFileSlice):

    def checkComplete(self, nchunks ):

        all_data = []
        for chunk in range( 0, nchunks):
            iterator = FileSlice.IteratorMultiline( self.mFilename, 
                                                    nchunks,
                                                    chunk, 
                                                    FileSlice.groupby,
                                                    key = lambda x: x[:x.index("\t")] )
            
            for d in iterator:
                n = 0
                for x in d:
                    dd = x.split("\t")
                    self.assertEqual( len(dd), 4 )
                    all_data.append( dd )
                    n += 1
                self.assertEqual( n, self.mNumLines )
                    
        self.assertEqual( len(all_data), self.mNumRecords * self.mNumLines )
        self.assertEqual( [int(x[2]) for x in all_data], range( 0, self.mNumRecords * self.mNumLines ) )


if __name__ == '__main__':
    unittest.main()
