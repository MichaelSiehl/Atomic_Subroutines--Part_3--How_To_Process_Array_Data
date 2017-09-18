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
  .
  integer(kind=OOOGglob_kint) :: WaitForTestArrayTransfer ! = 13000000
  .
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

- The OOOPimsc_adtImageStatus_CA type definition with the mA_atomic_intImageActivityFlag99 member for synchronization of the array transfer as a whole, and the mA_atomic_intTestArray member for the array data transfer and synchronization of each distinct array element resp. (Each single array element of these atomic members acts as a distinct remote communication channel):

```fortran
type, public :: OOOPimsc_adtImageStatus_CA
  private
  !*****
  integer(atomic_int_kind), dimension (1:OOOGglob_NumImages_UpperBound, 1:2) :: mA_atomic_intImageActivityFlag99
  !*****
  ...
  !*****
  integer(atomic_int_kind), dimension (1:5) :: mA_atomic_intTestArray
  !*****
  ...
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

- The OOOPimscSpinWaitBulkSync_atomic_intImageActivityFlag99_CA customized synchronization procedure for synchronizations through the  mA_atomic_intImageActivityFlag99 member/remote communication channel (the procedure is currently strongly type/member-bound, but this is not necessarily required since the procedure does not directly access the member/remote communication channel -some further research is required here-):

```fortran
subroutine OOOPimscSpinWaitBulkSync_atomic_intImageActivityFlag99_CA (Object_CA, intCheckImageActivityFlag, &
                  intNumberOfImages, intA_RemoteImageNumbers, logArrayIndexIsThisImage, &
                  intA_RemoteImageAndItsAdditionalAtomicValue, logExecuteSyncMemory)
  ! This routine is for atomic bulk synchronization (among the executing image and one or more remote images)
  ! using a spin-wait loop synchronizaton. Thus, the procedure implements a customized synchronization
  ! routine using atomic subroutines and the sync memory statement. Ordered execution segments among the involved images
  ! are not required.
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intCheckImageActivityFlag
  integer(OOOGglob_kint), intent (in) :: intNumberOfImages ! these are the number of involved remote images
  integer(OOOGglob_kint), dimension (1:intNumberOfImages), intent (in) :: intA_RemoteImageNumbers
  logical(OOOGglob_klog), optional, intent (in) :: logArrayIndexIsThisImage
  logical(OOOGglob_klog) :: logArrIndexIsThisImage
  integer(OOOGglob_kint) :: intArrIndex
  integer(OOOGglob_kint), optional, dimension (1:intNumberOfImages, 1:2), intent (out) :: &
                                                       intA_RemoteImageAndItsAdditionalAtomicValue
  integer(OOOGglob_kint) :: intCount
  integer(OOOGglob_kint) :: intImageNumber
  logical(OOOGglob_klog), dimension (1:intNumberOfImages) :: logA_CheckImageStates
  integer(OOOGglob_kint) :: intAtomicValue = 0
  logical(OOOGglob_klog), optional, intent (in) :: logExecuteSyncMemory
  logical(OOOGglob_klog) :: logSyncMemoryExecution
  integer(OOOGglob_kint) :: intMaxVal
  integer(OOOGglob_kint) :: status = 0 ! error status
  !
                                                                call OOOGglob_subSetProcedures &
                                            ("OOOPimscSpinWaitBulkSync_atomic_intImageActivityFlag99_CA")
  !
  !**********************************************************************
  !****
  if (present(logArrayIndexIsThisImage)) then
    logArrIndexIsThisImage = logArrayIndexIsThisImage
  else ! default:
    logArrIndexIsThisImage = .false.
  end if
  !****
  if (present(logExecuteSyncMemory)) then
    logSyncMemoryExecution = logExecuteSyncMemory
  else ! default:
    logSyncMemoryExecution = .true.
  end if
  !****
  ! initialize the array elements with .false.:
  logA_CheckImageStates = .false.
  !
  !**********************************************************************
  ! wait until all the involved remote image(s) do signal that they are in state intCheckImageActivityFlag
  ! spin-wait loop synchronization:
  do
    do intCount = 1, intNumberOfImages
      !
      intImageNumber = intA_RemoteImageNumbers(intCount)
      intArrIndex = intImageNumber ! but:
        if (logArrIndexIsThisImage) intArrIndex = this_image()
      if (intImageNumber .ne. this_image()) then ! (synchronization is only required between distinct images)
        if (.not. logA_CheckImageStates(intCount)) then ! check is only required if the remote image is not already
                                                        ! in state intCheckImageActivityFlag:
          !
          if (OOOPimscGAElement_check_atomic_intImageActivityFlag99_CA (OOOPimscImageStatus_CA_1, &
                           intCheckImageActivityFlag, intArrayIndex = intArrIndex, &
                           intAdditionalAtomicValue = intAtomicValue, logExecuteSyncMemory = .false.)) then
            logA_CheckImageStates(intCount) = .true. ! the remote image is in state intCheckImageActivityFlag
            !
            if (present(intA_RemoteImageAndItsAdditionalAtomicValue)) then
            ! save the remote image number together with its sent AdditionalAtomicValue:
              intA_RemoteImageAndItsAdditionalAtomicValue(intCount,1) = intImageNumber
              intA_RemoteImageAndItsAdditionalAtomicValue(intCount,2) = intAtomicValue
            end if
          end if
        end if
      else ! (intImageNumber .eq. this_image())
        ! raise an error:
                                                                call IIimsc_ErrorHandler (Object_CA, &
                                                            "the executing image can't synchronize with itself yet", &
                                                                  OOOGglob_error, status)
                                                                !
        logA_CheckImageStates(intCount) = .true. ! otherwise the outer do loop would turn into an endless loop
      end if
    end do
    !
    if (all(logA_CheckImageStates)) then ! all involved remote images are in state intCheckImageActivityFlag
      if (logSyncMemoryExecution) call OOOPimsc_subSyncMemory (Object_CA) ! execute sync memory
      exit ! exit the do loop if all involved remote images are in state
                                         ! intCheckImageActivityFlag
    end if
  end do
  !
  !**********************************************************************
  !
                                                                call OOOGglob_subResetProcedures
  !
end subroutine OOOPimscSpinWaitBulkSync_atomic_intImageActivityFlag99_CA
```

- The setter and getter/checker routines for the mA_atomic_intTestArray member (resp. remote communication channel) for accessing the atomic member through atomic subroutines (the setter is the only means for remote communication through remote write using atomic_define):

```fortran
subroutine OOOPimscSAElement_atomic_intTestArray_CA (Object_CA, intArrayElementSyncStat, &
                                intImageNumber, intArrayIndex, logExecuteSyncMemory)
  ! Set an Array Element atomically
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intArrayElementSyncStat
  integer(OOOGglob_kint), intent (in) :: intImageNumber ! the (remote) image number
  integer(OOOGglob_kint), intent (in) :: intArrayIndex
  integer(OOOGglob_kint), dimension(1) :: intA_MaxVal
  logical(OOOGglob_klog), optional, intent (in) :: logExecuteSyncMemory
  logical(OOOGglob_klog) :: logSyncMemoryExecution
  integer(OOOGglob_kint) :: status = 0 ! error status
                                                                call OOOGglob_subSetProcedures &
                                                                 ("OOOPimscSAElement_atomic_intTestArray_CA")
  !
                                                                ! check if intArrayIndex argument is to large:
                                                                ! ToDo: check if it is negative
                                                                intA_MaxVal(1) = ubound(Object_CA % &
                                                                              mA_atomic_intTestArray,1)
                                                                if (intArrayIndex .gt. intA_MaxVal(1)) then
                                                                  call IIimsc_ErrorHandler (Object_CA, &
                                                                       "intArrayIndex is to large", &
                                                                    OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
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
    call atomic_define(Object_CA % mA_atomic_intTestArray(intArrayIndex), intArrayElementSyncStat)
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
    call atomic_define(Object_CA [intImageNumber] % mA_atomic_intTestArray(intArrayIndex), intArrayElementSyncStat)
! the following does not generate an error with ifort 18 beta:
! (thus, we may expect that upcomming versions of ifort will support this too)
!    call atomic_define(Object_CA % mA_atomic_intTestArray(intArrayIndex), intArrayElementSyncStat)

    !
  end if
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimscSAElement_atomic_intTestArray_CA
!
!**********
logical(OOOGglob_klog) function OOOPimscGAElement_check_atomic_intTestArray_CA (Object_CA, intCheckArrayElementSyncStat, &
                                                          intArrayIndex, intAdditionalAtomicValue, logExecuteSyncMemory)
  ! Get (check) an Array Element atomically:
  ! in order to hide the sync memory statement herein, this Getter does not allow
  ! to access the member directly, but instead does only allow to check the atomic member
  ! for specific values (this Getter is intented for synchronizations)
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intCheckArrayElementSyncStat
  integer(OOOGglob_kint), intent (in) :: intArrayIndex
  integer(OOOGglob_kint), dimension(1) :: intA_MaxVal
  integer(OOOGglob_kint), optional, intent (out) :: intAdditionalAtomicValue
  integer(OOOGglob_kint) :: intArrayElementSyncStat
  integer(OOOGglob_kint) :: intPackedEnumValue
  integer(OOOGglob_kint) :: intEnum_StepWidth
  integer(OOOGglob_kint) :: intUnpackedEnumValue
  integer(OOOGglob_kint) :: intUnpackedAdditionalValue
  logical(OOOGglob_klog), optional, intent (in) :: logExecuteSyncMemory
  logical(OOOGglob_klog) :: logSyncMemoryExecution
  integer(OOOGglob_kint) :: status = 0 ! error status
                                                                call OOOGglob_subSetProcedures &
                                                                 ("OOOPimscGAElement_check_atomic_intTestArray_CA")
  !
  OOOPimscGAElement_check_atomic_intTestArray_CA = .false.
  !
                                                                ! check if intArrayIndex argument is to large:
                                                                ! ToDo: check if it is negative
                                                                intA_MaxVal(1) = ubound(Object_CA % &
                                                                              mA_atomic_intTestArray,1)
                                                                if (intArrayIndex .gt. intA_MaxVal(1)) then
                                                                  call IIimsc_ErrorHandler (Object_CA, &
                                                                       "intArrayIndex is to large", &
                                                                    OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
                                                                !
  !
  if (present(logExecuteSyncMemory)) then
    logSyncMemoryExecution = logExecuteSyncMemory
  else ! default:
    logSyncMemoryExecution = .true.
  end if
  !
  ! access an array element in local PGAS memory atomically:
  call atomic_ref(intArrayElementSyncStat, Object_CA % mA_atomic_intTestArray(intArrayIndex))
  ! unpack the intArrayElementSyncStat value:
  intPackedEnumValue = intArrayElementSyncStat
  intEnum_StepWidth = OOOPimscEnum_ArrayElementSyncStat % Enum_StepWidth
  call OOOPimsc_UnpackEnumValue (intPackedEnumValue, intEnum_StepWidth, &
                             intUnpackedEnumValue, intUnpackedAdditionalValue)
  intArrayElementSyncStat = intUnpackedEnumValue
  !
  if (intCheckArrayElementSyncStat == intArrayElementSyncStat) then
    if (logSyncMemoryExecution) call OOOPimsc_subSyncMemory (Object_CA) ! execute sync memory
    OOOPimscGAElement_check_atomic_intTestArray_CA = .true.
  end if
  !
  if (present(intAdditionalAtomicValue)) then
    intAdditionalAtomicValue = intUnpackedAdditionalValue
  end if
  !
                                                                call OOOGglob_subResetProcedures
end function OOOPimscGAElement_check_atomic_intTestArray_CA
```

- The OOOPimscSpinWaitArrayRang1Sync_atomic_intTestArray_CA  customized synchronization procedure for synchronization of each distinct array element through the  mA_atomic_intTestArray member/remote communication channel; the synchronization of each array element is non-blocking (i.e. no array element has to wait for another one to synchronize) and non-repeating (once synchronized, no further checking is required):

```fortran
subroutine OOOPimscSpinWaitArrayRang1Sync_atomic_intTestArray_CA (Object_CA, intCheckArrayElementSyncStat, &
                  intRemoteImageNumber, intArrayUpperBound, intA_ArrayElementSyncStatAndItsAdditionalAtomicValue, &
                  logExecuteSyncMemory)
  ! This routine is for atomic array synchronization (among the executing image and a single remote image)
  ! using a spin-wait loop synchronizaton. The routine synchronizes the array elements each resp.
  ! Thus, the procedure implements a customized synchronization routine using atomic subroutines and
  ! the sync memory statement. Ordered execution segments between the involved images
  ! are not required.
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intCheckArrayElementSyncStat
  integer(OOOGglob_kint), intent (in) :: intRemoteImageNumber
  integer(OOOGglob_kint) :: intArrayIndex
  integer(OOOGglob_kint), intent (in) :: intArrayUpperBound
  integer(OOOGglob_kint), dimension(1) :: intA_MaxVal
  integer(OOOGglob_kint), optional, dimension (1:intArrayUpperBound, 1:2), intent (out) :: &
                                                    intA_ArrayElementSyncStatAndItsAdditionalAtomicValue


  integer(OOOGglob_kint) :: intCount
  logical(OOOGglob_klog), dimension (1:intArrayUpperBound) :: logA_CheckArrayElementStates
  integer(OOOGglob_kint) :: intAtomicValue = 0
  logical(OOOGglob_klog), optional, intent (in) :: logExecuteSyncMemory
  logical(OOOGglob_klog) :: logSyncMemoryExecution
  integer(OOOGglob_kint) :: status = 0 ! error status
  !
                                                                call OOOGglob_subSetProcedures &
                                            ("OOOPimscSpinWaitArrayRang1Sync_atomic_intTestArray_CA")
                                                                !
                                                                ! check if the intArrayUpperBound argument is to large:
                                                                ! ToDo: check if it is negative
                                                                intA_MaxVal(1) = ubound(Object_CA % &
                                                                              mA_atomic_intTestArray,1)
                                                                if (intArrayUpperBound .gt. intA_MaxVal(1)) then
                                                                  call IIimsc_ErrorHandler (Object_CA, &
                                                                       "intArrayUpperBound is to large", &
                                                                    OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
                                                                !
  !
  !**********************************************************************
  !****
  if (present(logExecuteSyncMemory)) then
    logSyncMemoryExecution = logExecuteSyncMemory
  else ! default:
    logSyncMemoryExecution = .true.
  end if
  !****
  ! initialize the array elements with .false.:
  logA_CheckArrayElementStates = .false.
  !
  !**********************************************************************
  ! wait until all the array elements are in state intCheckArrayElementSyncStat
  ! spin-wait loop synchronization:
  do
    do intCount = 1, intArrayUpperBound
      intArrayIndex = intCount
      if (intRemoteImageNumber .ne. this_image()) then ! (synchronization is only required between distinct images)
        if (.not. logA_CheckArrayElementStates(intCount)) then ! check is only required if the array element is not already
                                                               ! in state intCheckArrayElementSyncStat:
          !
          if (OOOPimscGAElement_check_atomic_intTestArray_CA (OOOPimscImageStatus_CA_1, &
                           intCheckArrayElementSyncStat, intArrayIndex, &
                           intAdditionalAtomicValue = intAtomicValue, logExecuteSyncMemory = .false.)) then
            logA_CheckArrayElementStates(intCount) = .true. ! the array element is in state intCheckArrayElementSyncStat
            !
            if (present(intA_ArrayElementSyncStatAndItsAdditionalAtomicValue)) then
            ! save the array element synchronization status together with its sent AdditionalAtomicValue:
              intA_ArrayElementSyncStatAndItsAdditionalAtomicValue(intCount,1) = intCheckArrayElementSyncStat
              intA_ArrayElementSyncStatAndItsAdditionalAtomicValue(intCount,2) = intAtomicValue
            end if
          end if
        end if
      else ! (intRemoteImageNumber .eq. this_image())
      ! (be aware: this is to already prepare this routine for array synchronizations
      !  with several remote images involved)
        ! raise an error:
                                                                call IIimsc_ErrorHandler (Object_CA, &
                                                            "the executing image can't synchronize with itself yet", &
                                                                  OOOGglob_error, status)
                                                                !
        logA_CheckArrayElementStates(intCount) = .true. ! otherwise the outer do loop would turn into an endless loop
      end if
    end do
    !
    if (all(logA_CheckArrayElementStates)) then ! all array elements are in state intCheckArrayElementSyncStat
      if (logSyncMemoryExecution) call OOOPimsc_subSyncMemory (Object_CA) ! execute sync memory
      exit ! exit the do loop if all array elements are in state
                                         ! intCheckArrayElementSyncStat
    end if
  end do
  !
  !**********************************************************************
  !
                                                                call OOOGglob_subResetProcedures
  !
end subroutine OOOPimscSpinWaitArrayRang1Sync_atomic_intTestArray_CA
```

- The logic code procedure OOOPimsc_SynchronizeAndDistributeTheTestArray_CA, executed on coarray image 1 to distribute the TestArray data (values 1,2,3,4,5) to the remote images 2,3,4:

```fortran
subroutine OOOPimsc_SynchronizeAndDistributeTheTestArray_CA (Object_CA, intNumberOfImages,intA_RemoteImageNumbers, &
                                                             intA_TestArrayForRemoteTransfer)
  ! This routine is to synchronize and distribute the TestArray (using atomic subroutines)
  ! to the involved remote images (2,3,4).
  ! To do so, this routine gets executed on a separate coarray image
  ! (on image 1 with this example)
  !
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intNumberOfImages ! these are the number of involved remote images
  integer(OOOGglob_kint), dimension (intNumberOfImages), intent (in) :: intA_RemoteImageNumbers
  integer(OOOGglob_kint), dimension (1:5), intent (in) :: intA_TestArrayForRemoteTransfer
  integer(OOOGglob_kint) :: intCount
  integer(OOOGglob_kint) :: intCount2
  integer(OOOGglob_kint) :: intImageNumber
  integer(OOOGglob_kint) :: intImageActivityFlag
  integer(OOOGglob_kint) :: intPackedEnumValue
  integer(OOOGglob_kint) :: intArrayElementSyncStat
  integer(OOOGglob_kint), dimension (1:5) :: intA_TestArray ! will contain the packed enum values
                                                            ! for remote transfer
  integer(OOOGglob_kint) :: intEnumStepWidth
  !
                                                                call OOOGglob_subSetProcedures &
                                                            ("OOOPimsc_SynchronizeAndDistributeTheTestArray_CA")
  !
  !************************************************
  ! (1) wait until all the involved remote image(s) do signal that they are in state WaitForTestArrayTransfer:
  ! (counterpart routine is step 2 in OOOPimsc_InitiateAndWaitForTestArrayTransfer_CA)
  intImageActivityFlag = OOOPimscEnum_ImageActivityFlag % WaitForTestArrayTransfer
  ! spin-wait loop synchronization:
  call OOOPimscSpinWaitBulkSync_atomic_intImageActivityFlag99_CA (Object_CA, intImageActivityFlag, &
                         intNumberOfImages, intA_RemoteImageNumbers)
  !*************************************************
  ! (2) pack and fill the TestArray locally with values from intA_TestArrayForRemoteTransfer:
  ! - Firstly, set this to allow for later synchronization of the array elements on the remote images:
  intArrayElementSyncStat = OOOPimscEnum_ArrayElementSyncStat % ArrayElementSynchronized
  ! - pack the Enum value with input values from intA_TestArrayForRemoteTransfer and fill the
  !   intA_TestArray elements with these values:
  intEnumStepWidth = OOOPimscEnum_ArrayElementSyncStat % Enum_StepWidth ! only for error checking
  do intCount = 1, 5
    call OOOPimsc_PackEnumValue (Object_CA, intArrayElementSyncStat, &
                        intA_TestArrayForRemoteTransfer(intCount), intPackedEnumValue, intEnumStepWidth)
    intA_TestArray(intCount) = intPackedEnumValue
  end do
  !
  !**********************************************************************
  ! (3) distribute the TestArray to the TestArray coarray components of the involved remote image(s):
  ! (counterpart synchronization routine is step 3 in OOOPimsc_InitiateAndWaitForTestArrayTransfer_CA)
  call OOOPimsc_subSyncMemory (Object_CA) ! execute sync memory
  do intCount = 1, intNumberOfImages ! distribute the array to each involved remote image
     intImageNumber = intA_RemoteImageNumbers(intCount)
    do intCount2 = 1, ubound(intA_TestArray, 1) ! distribute the array elements each individually:
      call OOOPimscSAElement_atomic_intTestArray_CA (Object_CA, intA_TestArray(intCount2), &
                            intImageNumber, intArrayIndex = intCount2, logExecuteSyncMemory = .false.)
    end do
  end do
  !
  !************************************************
  ! (4) wait until all the involved remote image(s) do signal that they are in state TestArrayRemoteTransferDone:
  ! (counterpart routine is step 4 in OOOPimsc_InitiateAndWaitForTestArrayTransfer_CA)
  intImageActivityFlag = OOOPimscEnum_ImageActivityFlag % TestArrayRemoteTransferDone
  ! - spin-wait loop synchronization:
  call OOOPimscSpinWaitBulkSync_atomic_intImageActivityFlag99_CA (Object_CA, intImageActivityFlag, &
                         intNumberOfImages, intA_RemoteImageNumbers)
  !
  !**********************************************************************
  ! (5) finish execution on the executing image (not really required for this example):
  call OOOPimscSAElement_atomic_intImageActivityFlag99_CA (OOOPimscImageStatus_CA_1, OOOPimscEnum_ImageActivityFlag % &
                                    ExecutionFinished, this_image(), logExecuteSyncMemory = .false.)
  !

                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimsc_SynchronizeAndDistributeTheTestArray_CA
```

- And finally, the logic code procedure OOOPimsc_InitiateAndWaitForTestArrayTransfer_CA, executed on coarray images 2,3,4 to synchronize and receive the TestArray data:

```fortran
subroutine OOOPimsc_InitiateAndWaitForTestArrayTransfer_CA (Object_CA, intSetFromImageNumber, intA_TestArrayForRemoteTransfer)
  ! This routine is to synchronize and receive the TestArray (using atomic subroutines indirectly)
  ! on the involved images (2,3,4):
  ! (the involved images (not image 1) will execute this rotuine)
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intSetFromImageNumber ! this is the remote image number (image 1)
                                                               ! which initiated the synchronization
  integer(OOOGglob_kint), dimension (1:5), intent (out) :: intA_TestArrayForRemoteTransfer
  integer(OOOGglob_kint) :: intImageActivityFlag
  integer(OOOGglob_kint) :: intArrayElementSyncStat
  integer(OOOGglob_kint) :: intArrayUpperBound
  integer(OOOGglob_kint), dimension (1:5, 1:2) :: intA_ArrayElementSyncStatAndItsAdditionalAtomicValue
  integer(OOOGglob_kint) :: intPackedEnumValue
  integer(OOOGglob_kint) :: intRemoteImageNumber
  integer(OOOGglob_kint) :: intCount
  integer(OOOGglob_kint) :: intEnumStepWidth
  !
                                                                call OOOGglob_subSetProcedures &
                                                            ("OOOPimsc_InitiateAndWaitForTestArrayTransfer_CA")
  !
  !**********************************************************************
  ! (1) reset the TestArray component locally on this image (to allow for
  ! synchronization of the array elements later on):
  intArrayElementSyncStat = OOOPimscEnum_ArrayElementSyncStat % ArrayElementNotSynchronizedYet
  ! - pack the Enum value together with the value 0 for the TestArray elements:
  intEnumStepWidth = OOOPimscEnum_ArrayElementSyncStat % Enum_StepWidth ! only for error checking
  call OOOPimsc_PackEnumValue (Object_CA, intArrayElementSyncStat, &
                                         0, intPackedEnumValue, intEnumStepWidth)
  ! - fill the local TestArray component atomicly:
  do intCount = 1, ubound(intA_TestArrayForRemoteTransfer, 1)
    call OOOPimscSAElement_atomic_intTestArray_CA (Object_CA, intPackedEnumValue, &
                          this_image(), intArrayIndex = intCount, logExecuteSyncMemory = .false.)
                        ! here, we do not execute sync memory for local atomic_define yet
  end do
  !
  ! *********************************************************************
  ! (2) set this image to state 'WaitForTestArrayTransfer' and signal this to the remote image 1:
  ! (conterpart synchronization routine is step 1 in OOOPimsc_SynchronizeAndDistributeTheTestArray_CA)
  intRemoteImageNumber = intSetFromImageNumber
  intImageActivityFlag = OOOPimscEnum_ImageActivityFlag % WaitForTestArrayTransfer
  !
  ! - pack the ImageActivityFlag together with this_image():
  intEnumStepWidth = OOOPimscEnum_ImageActivityFlag % Enum_StepWidth
  call OOOPimsc_PackEnumValue (Object_CA, intImageActivityFlag, this_image(), intPackedEnumValue, &
                                        intEnumStepWidth)
  !
  ! - signal to the remote image (image 1) that this image is now in state 'WaitForTestArrayTransfer':
  call OOOPimscSAElement_atomic_intImageActivityFlag99_CA (Object_CA, intPackedEnumValue, &
                         intRemoteImageNumber, intArrayIndex = this_image(), logExecuteSyncMemory = .true.)
  !
  !*************************************************************************
  ! (3) wait until all the array elements of the TestArray coarray component are in state ArrayElementSynchronized
  ! (counterpart routine is step 3 in OOOPimsc_SynchronizeAndDistributeTheTestArray_CA)
  intArrayElementSyncStat = OOOPimscEnum_ArrayElementSyncStat % ArrayElementSynchronized
  intArrayUpperBound = ubound(intA_ArrayElementSyncStatAndItsAdditionalAtomicValue, 1)
  ! - spin-wait loop synchronization for all the distinct array elements:
  intRemoteImageNumber = intSetFromImageNumber
  call OOOPimscSpinWaitArrayRang1Sync_atomic_intTestArray_CA (Object_CA, intArrayElementSyncStat, &
                  intRemoteImageNumber, intArrayUpperBound, &
                  intA_ArrayElementSyncStatAndItsAdditionalAtomicValue, &
                  logExecuteSyncMemory = .true.)
  !
  intA_TestArrayForRemoteTransfer(:) = intA_ArrayElementSyncStatAndItsAdditionalAtomicValue(:,2) ! procedure output argument
  !
  !*************************************************************************
  ! (4) signal to the remote image (image 1) that this image is now in state 'TestArrayRemoteTransferDone'
  ! (conterpart synchronization routine is step 4 in OOOPimsc_SynchronizeAndDistributeTheTestArray_CA)
  intImageActivityFlag = OOOPimscEnum_ImageActivityFlag % TestArrayRemoteTransferDone
  call OOOPimscSAElement_atomic_intImageActivityFlag99_CA (Object_CA, intImageActivityFlag, &
                         intRemoteImageNumber, intArrayIndex = this_image(), logExecuteSyncMemory = .true.)
  !*************************************************************************
  ! (5) finish execution on the executing image (not really required for this example):
  call OOOPimscSAElement_atomic_intImageActivityFlag99_CA (OOOPimscImageStatus_CA_1, OOOPimscEnum_ImageActivityFlag % &
                                    ExecutionFinished, this_image(), logExecuteSyncMemory = .false.)
  !*************************************************************************
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimsc_InitiateAndWaitForTestArrayTransfer_CA
```
