Basic TEC Code
------

Original url link: [`tecode.zip`](http://depts.washington.edu/control/LARRY/TE/tecode.zip)

Contents of the `tecode` directory, all in ASCII format:

- `teprob.f`: Fortran code provided by Tennessee Eastman

- `tecommon.inc`: An "include" file needed to compile `TEPROB.F`

- `te_mex.f`: Fortran code needed to generate the .MEX interface to Matlab.*FOR MATLAB 3.x ONLY!!*

- `te_mextc.fv4`: This modification of `te_mex.f` was written for Matlab 4.2c on a MacIntosh. It has not been tested on other machines. It includes PI control of the reactor temperature and separator temperature. There are also modifications to the output vector. See comments in the code for more information. The state vector for this version has 52 elements. The last 2 are the integrated errors for the 2 PI controllers. You must initialize these properly for the system to be at steady state.