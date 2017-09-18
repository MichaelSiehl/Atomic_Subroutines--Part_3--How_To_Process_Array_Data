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

# - The OOOPimsc_admImageStatus_CA module
The remote transfer of array data through atomic subroutines requires two kinds of synchronizations: firstly, we must synchronize the array transfer as a whole (at several times) and secondly, we must synchronize the remote transfer of each distinct array element resp. (which is non-blocking and less demanding than it may sound). Thus, we need two distinct synchronization primitives (i.e. two distinct customized synchronization procedures) here.<br />
The OOOPimsc_admImageStatus_CA.f90 source code file (module) does contain all the required codes to achieve the remote array transfer atomically in a safe way. The parallel logic codes are contained in two procedures near the end of the source code file (see below).<br />
<br />
The relevant codes are:<br />

- The OOOPimscEnum_ImageActivityFlag integer-based enumeration for synchronization of the array transfer as a whole (only the last few enumeration values are required with the example program yet):

```fortran
!***  ImageActivityFlag:
type, private :: OOOPimsc_DontUse1
  integer(kind=OOOGglob_kint) :: Enum_StepWidth ! = 1000000
  integer(kind=OOOGglob_kint) :: InitialWaiting ! = 2000000
  integer(kind=OOOGglob_kint) :: TeamManager ! = 3000000
  integer(kind=OOOGglob_kint) :: TeamMember ! = 4000000
  integer(kind=OOOGglob_kint) :: ExecutionFinished ! = 5000000
  integer(kind=OOOGglob_kint) :: InitiateTestArrayTransfer ! = 12000000
  integer(kind=OOOGglob_kint) :: WaitForTestArrayTransfer ! = 13000000
  integer(kind=OOOGglob_kint) :: ResetTheTestArray ! = 14000000
  integer(kind=OOOGglob_kint) :: LocalTestArrayResetDone ! = 15000000
  integer(kind=OOOGglob_kint) :: TestArrayRemoteTransferDone ! = 16000000
  integer(kind=OOOGglob_kint) :: Enum_MaxValue ! = 17000000
end type OOOPimsc_DontUse1
!
type (OOOPimsc_DontUse1), public, parameter :: OOOPimscEnum_ImageActivityFlag &
     = OOOPimsc_DontUse1 (1000000,2000000,3000000,4000000,5000000, &
                           12000000, 13000000, 14000000, 15000000, 16000000, 17000000)
!**************************
```

- The OOOPimscEnum_ArrayElementSyncStat integer-based enumeration for synchronization of each distinct array element resp.:

```fortran
!***  ArrayElementSyncStat:
type, private :: OOOPimsc_DontUse2
  integer(kind=OOOGglob_kint) :: Enum_StepWidth ! = 1000000
  integer(kind=OOOGglob_kint) :: ArrayElementNotSynchronizedYet ! = 2000000
  integer(kind=OOOGglob_kint) :: ArrayElementSynchronized ! = 3000000
  integer(kind=OOOGglob_kint) :: Enum_MaxValue ! = 4000000
end type OOOPimsc_DontUse2
!
type (OOOPimsc_DontUse2), public, parameter :: OOOPimscEnum_ArrayElementSyncStat &
     = OOOPimsc_DontUse2 (1000000,2000000,3000000, 4000000)
```

- The OOOPimsc_adtImageStatus_CA type definition with the mA_atomic_intImageActivityFlag99 member for synchronization of the array transfer as a whole, and the mA_atomic_intTestArray member for the array data transfer and synchronization of each distinct array element resp. (these atomic members are the remote communication channels):

```fortran
type, public :: OOOPimsc_adtImageStatus_CA
  private
  !*****
  integer(atomic_int_kind), dimension (1:OOOGglob_NumImages_UpperBound, 1:2) :: &
                  mA_atomic_intImageActivityFlag99 = OOOPimscEnum_ImageActivityFlag % InitialWaiting
  !*****
  integer(atomic_int_kind), dimension (1:OOOGglob_NumImages_UpperBound) :: mA_atomic_intImageSyncMemoryCount99 = 0
  !*****
  integer(atomic_int_kind), dimension (1:5) :: mA_atomic_intTestArray
  !*****
  type (OOOEerroc_colError) :: m_UUerrocError ! error collection
  !
end type OOOPimsc_adtImageStatus_CA
```

- The OOOPimscImageStatus_CA_1 coarray declaration to establish coarray correspondence (i.e. the remote communication channels) through USE association:

```fortran
type (OOOPimsc_adtImageStatus_CA), public, codimension[*], save :: OOOPimscImageStatus_CA_1
```

- Two service procedures for packing and unpacking an enum value with an additional value (OOOPimsc_PackEnumValue, OOOPimsc_UnPackEnumValue); The enum value is used for synchronization whereas the additional value is the value for remote data transfer -both together as one value atomically through atomic subroutines-:

```fortran
subroutine OOOPimsc_PackEnumValue (Object_CA, intEnumValue, intAdditionalValue, &
                                                intPackedEnumValue, intEnum_StepWidth)
  ! pack the both integer input arguments into a single integer scalar
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intEnumValue
  integer(OOOGglob_kint), intent (in) :: intAdditionalValue
  integer(OOOGglob_kint), intent (out) :: intPackedEnumValue
  integer(OOOGglob_kint), optional, intent(in) :: intEnum_StepWidth ! only for error checking
  integer(OOOGglob_kint) :: status
  !
                                                                call OOOGglob_subSetProcedures &
                                                              ("OOOPimsc_PackEnumValue")
  !
  if (present(intEnum_StepWidth)) then ! do error checking
                                                                ! check if intAdditionalValue argument is to large:
                                                                ! ToDo: check if it is negative
                                                                if (intAdditionalValue >= intEnum_StepWidth) then
                                                                  call IIimsc_ErrorHandler (Object_CA, &
                                                                       "intAdditionalValue is to large", &
                                                                    OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
                                                                !
  end if
  !
  intPackedEnumValue = intEnumValue + intAdditionalValue
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimsc_PackEnumValue
!
!**********
subroutine OOOPimsc_UnpackEnumValue (intPackedEnumValue, intEnum_StepWidth, &
                             intUnpackedEnumValue, intUnpackedAdditionalValue)
  ! unpack the integer enum value into two integer scalars
  integer(OOOGglob_kint), intent (in) :: intPackedEnumValue
  integer(OOOGglob_kint), intent (in) :: intEnum_StepWidth
  integer(OOOGglob_kint), intent (out) :: intUnpackedEnumValue
  integer(OOOGglob_kint), intent (out) :: intUnpackedAdditionalValue
  !
                                                                call OOOGglob_subSetProcedures &
                                                                   ("OOOPimsc_UnpackEnumValue")
  !
  intUnpackedAdditionalValue = mod(intPackedEnumValue, intEnum_StepWidth)
  !
  intUnpackedEnumValue = intPackedEnumValue - intUnpackedAdditionalValue
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimsc_UnpackEnumValue
```

- The setter and getter/checker routines for the  mA_atomic_intImageActivityFlag99 member (resp. remote communication channel) for accessing the atomic member through atomic subroutines (the setter is the only means for remote communication through remote write using atomic_define):

```fortran
subroutine OOOPimscSAElement_atomic_intImageActivityFlag99_CA (Object_CA, intImageActivityFlag, &
                                intImageNumber, intArrayIndex, logExecuteSyncMemory)
  ! Set an Array Element atomically
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intImageActivityFlag
  integer(OOOGglob_kint), intent (in) :: intImageNumber ! the (remote) image number
  integer(OOOGglob_kint), optional, intent (in) :: intArrayIndex
  integer(OOOGglob_kint) :: intArrIndex
  integer(OOOGglob_kint) :: intMaxVal
  logical(OOOGglob_klog), optional, intent (in) :: logExecuteSyncMemory
  logical(OOOGglob_klog) :: logSyncMemoryExecution
  integer(OOOGglob_kint) :: status = 0 ! error status
                                                                call OOOGglob_subSetProcedures &
                                                                 ("OOOPimscSAElement_atomic_intImageActivityFlag99_CA")
  !
  if (present(intArrayIndex)) then
                                                                ! check if intArrayIndex argument is to large:
                                                                ! ToDo: check if it is negative
                                                                intMaxVal = OOOGglob_NumImages_UpperBound
                                                                if (intArrayIndex .gt. intMaxVal) then
                                                                  call IIimsc_ErrorHandler (Object_CA, &
                                                                       "intArrayIndex is to large", &
                                                                    OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
                                                                !
    intArrIndex = intArrayIndex
  else ! default:
    intArrIndex = intImageNumber
  end if
  !
  !
  if (present(logExecuteSyncMemory)) then
    logSyncMemoryExecution = logExecuteSyncMemory
  else ! default:
    logSyncMemoryExecution = .true.
  end if
  !
  !
  if (intImageNumber == this_image()) then ! local atomic define
    ! don't execute sync memory for local atomic_define:
    call atomic_define(Object_CA % mA_atomic_intImageActivityFlag99(intArrIndex,1), intImageActivityFlag)
  !
  else ! remote atomic define
                                                                ! check if the image number is valid:
                                                                if (IIimsc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumber)) then
                                                                  call IIimsc_ErrorHandler (Object_CA, &
                                                                       "no valid image number", &
                                                                    OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
                                                                !
    ! execute sync memory for remote atomic_define:
    if (logSyncMemoryExecution) call OOOPimsc_subSyncMemory (Object_CA) ! execute sync memory

! the following generates 'error #8583: COARRAY argument of ATOMIC_DEFINE/ATOMIC_REF intrinsic subroutine shall be a coarray.'
! with ifort 18 beta:
    call atomic_define(Object_CA [intImageNumber] % mA_atomic_intImageActivityFlag99(intArrIndex,1), intImageActivityFlag)
! the following does not generate an error with ifort 18 beta:
! (thus, we may expect that upcomming versions of ifort will support this too)
!    call atomic_define(Object_CA % mA_atomic_intImageActivityFlag99(intArrIndex,1), intImageActivityFlag)
    !
  end if
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimscSAElement_atomic_intImageActivityFlag99_CA
!
!**********
!
logical(OOOGglob_klog) function OOOPimscGAElement_check_atomic_intImageActivityFlag99_CA (Object_CA, intCheckImageActivityFlag, &
                                                          intArrayIndex, intAdditionalAtomicValue, logExecuteSyncMemory)
  ! Get (check) an Array Element atomically:
  ! in order to hide the sync memory statement herein, this Getter does not allow
  ! to access the member directly, but instead does only allow to check the atomic member
  ! for specific values (this Getter is intented for synchronizations)
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intCheckImageActivityFlag
  integer(OOOGglob_kint), optional, intent (in) :: intArrayIndex
  integer(OOOGglob_kint) :: intArrIndex
  integer(OOOGglob_kint) :: intMaxVal
  integer(OOOGglob_kint), optional, intent (out) :: intAdditionalAtomicValue
  integer(OOOGglob_kint) :: intImageActivityFlag
  integer(OOOGglob_kint) :: intPackedEnumValue
  integer(OOOGglob_kint) :: intEnum_StepWidth
  integer(OOOGglob_kint) :: intUnpackedEnumValue
  integer(OOOGglob_kint) :: intUnpackedAdditionalValue
  logical(OOOGglob_klog), optional, intent (in) :: logExecuteSyncMemory
  logical(OOOGglob_klog) :: logSyncMemoryExecution
  integer(OOOGglob_kint) :: status = 0 ! error status
                                                                call OOOGglob_subSetProcedures &
                                                                 ("OOOPimscGAElement_check_atomic_intImageActivityFlag99_CA")
  !
  OOOPimscGAElement_check_atomic_intImageActivityFlag99_CA = .false.
  !
  if (present(intArrayIndex)) then
                                                                ! check if intArrayIndex argument is to large:
                                                                ! ToDo: check if it is negative
                                                                intMaxVal = OOOGglob_NumImages_UpperBound
                                                                if (intArrayIndex .gt. intMaxVal) then
                                                                  call IIimsc_ErrorHandler (Object_CA, &
                                                                       "intArrayIndex is to large", &
                                                                    OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
                                                                !
    intArrIndex = intArrayIndex
  else ! default:
    intArrIndex = this_image()
  end if
  !
  if (present(logExecuteSyncMemory)) then
    logSyncMemoryExecution = logExecuteSyncMemory
  else ! default:
    logSyncMemoryExecution = .true.
  end if
  !
  ! access an array element in local PGAS memory atomically:
  call atomic_ref(intImageActivityFlag, Object_CA % mA_atomic_intImageActivityFlag99(intArrIndex,1))
  ! unpack the intImageActivityFlag value:
  intPackedEnumValue = intImageActivityFlag
  intEnum_StepWidth = OOOPimscEnum_ImageActivityFlag % Enum_StepWidth
  call OOOPimsc_UnpackEnumValue (intPackedEnumValue, intEnum_StepWidth, &
                             intUnpackedEnumValue, intUnpackedAdditionalValue)
  intImageActivityFlag = intUnpackedEnumValue
  !
  if (intCheckImageActivityFlag == intImageActivityFlag) then
    if (logSyncMemoryExecution) call OOOPimsc_subSyncMemory (Object_CA) ! execute sync memory
    OOOPimscGAElement_check_atomic_intImageActivityFlag99_CA = .true.
  end if
  !
  if (present(intAdditionalAtomicValue)) then
    intAdditionalAtomicValue = intUnpackedAdditionalValue
  end if
  !
                                                                call OOOGglob_subResetProcedures
end function OOOPimscGAElement_check_atomic_intImageActivityFlag99_CA
```

