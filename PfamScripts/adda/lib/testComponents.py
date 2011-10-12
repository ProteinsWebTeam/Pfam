import unittest, os, glob, re, tempfile, gzip

from Components import *

class MyTest:
    links = ( ( 1, 2),
              ( 1, 3),
              ( 2, 3),
              ( 3, 3),
              ( 4, 5),
              ( 5, 6),
              )

class TestIComponents(unittest.TestCase, MyTest):

    def testAdd( self ):

        c = IComponents()
        for a, b in self.links: c.add( a, b )
        components = [ c.get(x) for x in range(0,8) ]
        self.assertEqual( components, [0,1,1,1,4,4,4,0] )
        self.assertEqual( c.getNumNodes(), 6 )
        self.assertEqual( c.getComponents(), [[1, 2, 3], [4, 5, 6]] )

class TestSComponents(unittest.TestCase, MyTest):

    def testAdd( self ):

        c = SComponents()
        for a, b in self.links: c.add( str(a), str(b) )
        components = [ c.get(str(x)) for x in range(0,8) ]
        self.assertEqual( components, [0,1,1,1,4,4,4,0] )
        self.assertEqual( c.getNumNodes(), 6 )
        self.assertEqual( c.getComponents(), [["1", "2", "3"], ["4", "5", "6"]] )

if __name__ == '__main__':
    unittest.main()
