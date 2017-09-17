# Atomic_Subroutines--Part_3--How_To_Process_Array_Data
Fortran 2008 coarray programming with unordered execution segments (user-defined ordering) - Atomic Subroutines - Part 3: How to process array data

# Overview
This GitHub repository contains an example program that processes array data remotely through Fortran 2008 atomic subroutines (atomic_define, atomic_ref). The example program is a modification and extension of the second part that can be found here: https://github.com/MichaelSiehl/Atomic_Subroutines--How_the_Parallel_Codes_may_look_like--Part_2. For this third part I did streamline the code largely and did remove many unnecessary codes, to make the code easier to grasp for others.<br />
You may take the example program as a first evidence that we can safely transmit array data remotely through Fortran 2008 atomic subroutines. The example program can process reduced-size integer values up to 999999 due to a low choosen enumeration step width, but in principle we may process nearly full sized integers if we'd choose a greater step width for the enumerations. Further, the ability to process array data is also the key to process any other (primitive) data types. To do so will require simple data conversions to store such data into one or more integers. Such data conversions should then occur with purely local (non-coarray) variables.

# How it works
The relevant codes are in the Main.f90 and OOOPimsc_admImageStatus_CA.f90 source code files.<br />
The example program should be compiled and run with 4 coarray images using OpenCoarrays/GFortran.<br />
Basically, we fill a small integer array (vector) with five values (1,2,3,4,5) on coarray image 1 and then do transfer these values to coarray images 2, 3, and 4 from the Main program (internally, calls to atomic_define are used for the remote transfer):

```fortran
program Main
  use OOOGglob_Globals
  use OOOEerro_admError
  use OOOPimsc_admImageStatus_CA
  implicit none
  !
  integer(OOOGglob_kint) :: intNumberOfRemoteImages
  integer(OOOGglob_kint), dimension (1:3) :: intA_RemoteImageNumbers ! please compile and run the
                                                                     ! program with 4 coarray images
  integer(OOOGglob_kint) :: intSetFromImageNumber
  integer(OOOGglob_kint), dimension (1:5) :: intA_TestArrayForRemoteTransfer
  !*********************************
  if (this_image() == 1) then
    !
    intNumberOfRemoteImages = 3
    intA_RemoteImageNumbers = (/2,3,4/)
    intA_TestArrayForRemoteTransfer = (/1,2,3,4,5/)
    !
    ! This routine call is to synchronize and distribute the TestArray (using atomic subroutines)
    ! to the involved remote images (2,3,4):
    call OOOPimsc_SynchronizeAndDistributeTheTestArray_CA (OOOPimscImageStatus_CA_1, &
                    intNumberOfRemoteImages, intA_RemoteImageNumbers, intA_TestArrayForRemoteTransfer)
  !***********************************
  else ! this_image > 1
    intSetFromImageNumber = 1
    !
    ! This routine call is to synchronize and receive the TestArray (using atomic subroutines)
    ! on the involved images (2,3,4):
    call OOOPimsc_InitiateAndWaitForTestArrayTransfer_CA (OOOPimscImageStatus_CA_1, intSetFromImageNumber, &
                                                             intA_TestArrayForRemoteTransfer)
    write(*,*) 'remote array transfer done: on image / array data', this_image(), intA_TestArrayForRemoteTransfer
  !************************************
  end if
  !
  write (*,*) 'execution finsished on image ', this_image()
  !
end program Main
```
