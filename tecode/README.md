Basic TEC Code
--------------

## Change log

+ About files
    + Add `readme.md`
    + Add f90 version of `teprob.f`: `teprob.f90`
    + Remove `tecommon.inc`, Merge its content into `teprob.f`.
    + Add `te_mextc.diff`: diff between original `teprob.f`+`te_mex.f` and `te_mextc2.fv4`
        Note: The following files are in their original, unmodified versions.
        ```sh
        cat te_mex.f > te_mex.fv3
        cat teprob.f >> te_mex.fv3
        git diff --no-index -- te_mex.fv3 te_mextc.fv4 > te_mextc.diff
        ```

+ `teprob.f`: Some format adjust, keep the code untouched.
    + CRLF -> LF
    + Add more comments
    + Copy and insert `tecommon.inc`
    + Adjust indent, keep use f77 format

+ `teprob.f90`: Equivalent to the original f77 version (`teprob.f`), but more easier to read.
    + Add a lot of comments
    + Keep a line < 73 chars
    + Indent adjusted to make it easier to read


## Original notes

Original url link: [`tecode.zip`](http://depts.washington.edu/control/LARRY/TE/tecode.zip)

Contents of the `tecode` directory, all in ASCII format:

- `teprob.f`: Fortran code provided by Tennessee Eastman

- `tecommon.inc`: An "include" file needed to compile `TEPROB.F`

- `te_mex.f`: Fortran code needed to generate the `.MEX` interface to Matlab. **FOR MATLAB 3.x ONLY!!**

- `te_mextc.fv4`: This modification of `te_mex.f` was written for **Matlab 4.2c** on a MacIntosh.
    It has not been tested on other machines.
    It includes PI control of the reactor temperature and separator temperature.
    There are also modifications to the output vector.
    See comments in the code for more information.
    The state vector for this version has 52 elements.
    The last 2 are the integrated errors for the 2 PI controllers.
    You must initialize these properly for the system to be at steady state.
