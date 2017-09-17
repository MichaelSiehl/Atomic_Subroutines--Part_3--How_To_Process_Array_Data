# Atomic_Subroutines--Part_3--How_To_Process_Array_Data
Fortran 2008 coarray programming with unordered execution segments (user-defined ordering) - Atomic Subroutines - Part 3: How to process array data

# Overview
This GitHub repository contains an example program that processes array data remotely through Fortran 2008 atomic subroutines (atomic_define, atomic_ref). The example program is a modification and extension of the second part that can be found here: https://github.com/MichaelSiehl/Atomic_Subroutines--How_the_Parallel_Codes_may_look_like--Part_2. For this third part I did streamline the code largely and did remove many unnecessary codes, to make the code easier to grasp for others. 
You may take the example program as a first evidence that we can safely transmit array data remotely through Fortran 2008 atomic subroutines. The example program can process integer values up to 999999 due to a low choosen enumeration step width, but in principle we may process nearly full sized integers if we'd choose a greater step width for the enumerations. Further, the ability to process array data is also the key to process any other (primitive) data types. To do so will require simple data conversions to store such data into one or more integers. Such data conversions should then occur with purely local (non-coarray) variables.

