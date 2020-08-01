Basic TEC Code
--------------

**TEC Code with good format: [`teprob.f90`](teprob.f90).**

Original zip link: [`tecode.zip`](http://depts.washington.edu/control/LARRY/TE/tecode.zip), Or view original files on github [TEC/tecode](https://github.com/camaramm/tennessee-eastman-challenge/tree/master/tecode).

ref: Downs, James J., and Ernest F. Vogel. "A plant-wide industrial process control problem." *Computers & chemical engineering* 17.3 (1993): 245-255.


## Files

+ [`te_mex.f`](te_mex.f): **Original ver., untouched**, see [Original notes](#original-notes)
+ [`te_mextc.fv4`](te_mextc.fv4): **Original ver., untouched**, see [Original notes](#original-notes)
+ [`te_mextc.diff`](te_mextc.diff): *Newly added*, diff between original `teprob.f`+`te_mex.f` and `te_mextc2.fv4`  
    Note: The following files are in their *original, unmodified versions*.
    ```sh
    cat te_mex.f > te_mex.fv3
    cat teprob.f >> te_mex.fv3
    git diff --no-index -- te_mex.fv3 te_mextc.fv4 > te_mextc.diff
    ```
+ [`teprob.f`](teprob.f): **modified**. see [Change log](#change-log)
+ [`teprob.f90`](teprob.f90): *Newly added*, see [Change log](#change-log)  
    A f90 version of `teprob.f`.


## Change log

+ file change
    + Add `readme.md`
    + Add f90 version of `teprob.f`: `teprob.f90`
    + Remove `tecommon.inc`, Merge its content into `teprob.f`.
    + Add `te_mextc.diff`

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

> Contents of the `tecode` directory, all in ASCII format:
> 
> - `teprob.f`: Fortran code provided by Tennessee Eastman
> 
> - `tecommon.inc`: An "include" file needed to compile `TEPROB.F`
> 
> - `te_mex.f`: Fortran code needed to generate the `.MEX` interface to Matlab. **FOR MATLAB 3.x ONLY!!**
> 
> - `te_mextc.fv4`: This modification of `te_mex.f` was written for **Matlab 4.2c** on a MacIntosh.
>     It has not been tested on other machines.
>     It includes PI control of the reactor temperature and separator temperature.
>     There are also modifications to the output vector.
>     See comments in the code for more information.
>     The state vector for this version has 52 elements.
>     The last 2 are the integrated errors for the 2 PI controllers.
>     You must initialize these properly for the system to be at steady state.
