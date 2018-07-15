To install SPIDER with mpi:

0. Download latest SPIDER distribution.
1. Download and install PGI compiler from https://www.pgroup.com/products/community.htm. Do not forget to install openmpi component included into the distribution.
2. modify the following lines in _spider/src/Makefile_linux_mpi_opt64_
```
COMP    = /home/azazello/soft/pgilinux-2018-184/linux86-64/2018/mpi/openmpi-2.1.2/bin/mpif90 -I /home/azazello/soft/pgilinux-2018-184/linux86-64/2018/mpi/openmpi-2.1.2/include
FFTWLIBDIR = ../../../../lib
MPILIBDIR = /home/azazello/soft/pgilinux-2018-184/linux86-64/2018/mpi/openmpi-2.1.2/lib
```
Now it will use openmpi and the compiler from PGI, but fftw from scipion libs.

3. Add pgfortran and libs to env:
```
export LD_LIBRARY_PATH="/home/azazello/soft/pgilinux-2018-184/linux86-64/18.4/lib:${LD_LIBRARY_PATH}"
export PATH="/home/azazello/soft/pgilinux-2018-184/linux86-64/18.4/bin:${PATH}"
```
4. `make -f Makefile_linux_mpi_opt64` in src folder
5. Add libs to your bashrc:
```
export LD_LIBRARY_PATH="/home/azazello/soft/pgilinux-2018-184/linux86-64/18.4/lib:${LD_LIBRARY_PATH}"
export LD_LIBRARY_PATH="/home/azazello/soft/pgilinux-2018-184/linux86-64/2018/mpi/openmpi-2.1.2/lib:${LD_LIBRARY_PATH}"
```
6. Check if all libs are linked:
`ldd spider/bin/spider_linux_mpi_opt64`
7. Check out which commands support MPI: https://spider.wadsworth.org/spider_doc/spider/docs/mpi.html
