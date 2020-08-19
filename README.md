# Tennessee Eastman Challenge

This a fork of [camaramm's tennessee-eastman-challenge][camaramm-tec].

Tennessee Eastman Challenge is a collection of codes which simulate a simple chemical industry  process.
By using those codes, we can collect many kinds of data, such as: temperature, pressure, flow rate, concentration and etc, from the chemical process like a distributed control system (DCS).
Also, we can add some faults such as temperature, pressure saltation, pump shutdown, concentration change, to the process and then observe the changes in the process. 

> It's a good idea to reformat legacy codes to make them easy to understand.
> And continuous integration is a great tool that allows us to ensure compatibility when refactoring code.
> Also, it's better not to put binary files in the repository.
>
> There are lots of binary files, such as: PDF, ZIP, MEX(dll) and etc, in camaramm's repository.
> But the original repository is a static archive of the "Tennessee Eastman Challenge". 
> We'd better keep all those files untouched. 
>
> So, I decided to open a new repository for those goals, rather than fork it explicitly, and we keep using the same MIT license.
> Then we could move folders from the original repository to this one case by case. 
> Reformat those old codes, add continuous integration and test sets.


## Useful links

+ [Tennessee Eastman Challenge Archive - Prof. N. L. Ricker | washington.edu][washington-teca]
+ [camaramm/tennessee-eastman-challenge | github][camaramm-tec]  
    TECA on github.
- [camaramm/tennessee-eastman-profBraatz | github][camaramm-te-braatz]  
    The Fortran 77 codes for the open-loop and the closed-loop simulations for the Tennessee Eastman process (TEP) as well as the training and testing data files used for evaluating the data-driven methods (PCA, PLS, FDA, and CVA).


[washington-teca]: http://depts.washington.edu/control/LARRY/TE/download.html
[camaramm-tec]: https://github.com/camaramm/tennessee-eastman-challenge#basic-tec-code
[camaramm-te-braatz]: https://github.com/camaramm/tennessee-eastman-profBraatz


## TEC Code Versions

1. [Basic TEC Code](tecode). This is probably the original version (Oldest version).  
    ref: Downs, James J., and Ernest F. Vogel. "A plant-wide industrial process control problem." *Computers & chemical engineering* 17.3 (1993): 245-255.
