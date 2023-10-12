from ctypes import *
import numpy as np

from os.path import dirname,abspath,join
from inspect import getsourcefile

class api():
    _DATA_UNKNOWN = 0
    _DATA_INT0D   = 1
    _DATA_INT1D   = 2
    _DATA_INT2D   = 3
    _DATA_REAL0D  = 4
    _DATA_REAL1D  = 5
    _DATA_REAL2D  = 6

    def __init__(self):

        # path to this file
        mypath=dirname(abspath(getsourcefile(lambda:0)))
        # one directory up
        mypath=dirname(mypath)
        # by default, the lib should be there:
        path = join(mypath,"src_lib/libthat.so")

        # load lib
        self.lib = CDLL(path)

        self.lib.lib_get_dtyp.restype = c_int
        self.lib.lib_get_dtyp.argtypes = [c_void_p, c_char_p ]
        self.lib.lib_create.restype = c_void_p
        # pointer to data struc
        self.me = self.lib.lib_create()
        # print(self.me)
        return

    def close(self):
        self.lib.close.restype = None
        self.lib.close.argtypes = [ c_void_p ]
        self.lib.close( self.me )
        return

    def print(self):
        self.lib.lib_print.argtypes = [ c_void_p ]
        self.lib.lib_print.restype = None
        self.lib.lib_print( self.me )
        return

    def set( self, name, val ):

        cname = name.encode()
        dtyp = self.lib.lib_get_dtyp( self.me, cname )
        if dtyp == self._DATA_INT0D:
            cr = c_int( val )
            ctyp = POINTER(c_int)
        elif dtyp == self._DATA_REAL0D:
            # convert to c_double
            cr = c_double( val )
            ctyp = POINTER(c_double)
        else:
            print("set does not supper this datatype")
            return

        # call libset
        self.lib.lib_set.argtypes = [ c_void_p, c_char_p, ctyp ]
        self.lib.lib_set.restype = None
        self.lib.lib_set( self.me, cname, pointer(cr) )
        return

    def setarray( self, name, val ):
        self.lib.lib_setarray.restype = None
        cname = name.encode()
        s=np.array([np.size(val,1), np.size(val,0)])
        s=np.intc(s)
        csize = s.ctypes.data_as( POINTER(c_int) )
        val = np.float64( val )
        cdata = val.ctypes.data_as( POINTER(c_double) )
        cdata_typ = POINTER(POINTER(c_double))

        self.lib.lib_setarray.argtypes = [c_void_p, c_char_p, POINTER(POINTER(c_int)), cdata_typ ]
        self.lib.lib_setarray( self.me, cname, pointer(csize), pointer(cdata) )
        return

    def dump2shm( self ):
        self.lib.lib_dump2shm.argtypes = [ c_void_p ]
        self.lib.lib_dump2shm.restype = c_int

        ierr = self.lib.lib_dump2shm( self.me )
        if ierr != 0:
            print("err in dump2sh")
        return

    def unlink_lshm( self ):
        self.lib.lib_shm_unlink.argtypes = [ ]
        self.lib.lib_shm_unlink.restype = c_int

        ierr = self.lib.lib_shm_unlink( )
        if ierr != 0:
            print("err in shm_unlink")
        return




