import numpy as np
import interf

# load api -- make connection with lib
a = interf.api()

print("Setting data from python:")
# send some data to lib
arr=np.array([[1.1,1.2,1.3], [2.1,2.2,2.3]])
a.set( "a", 5 )
a.set( "r", 3.6 )
a.setarray("r2d", arr)

# print the content of library to screen
a.print()

# dump library to shm
a.dump2shm()

# call program that read from shm, as subprocess (no connection to opened lib)
import subprocess

cmd=[ "mpiexec", "-n", "2", "../src_fprog/fprog.x" ]

print("Now launching Fortran as subprocess")
subprocess.run( cmd )


# unlink the shm
a.unlink_lshm()
a.close()
