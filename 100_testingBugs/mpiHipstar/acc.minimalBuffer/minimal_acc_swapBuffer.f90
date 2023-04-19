!+++++++++++++++++++++++++++++++++++++++++++++++
! MODULE: INTERFACES
!+++++++++++++++++++++++++++++++++++++++++++++++
      module interfaces
       implicit none
!=========  TYPE: INTERFACE as the "swap interface" in HiPSTAR
       type interface
          double precision,pointer,contiguous :: swap_1(:), swap_2(:)
          integer :: bufferLength
       contains
          procedure :: allocateBuffers
          procedure :: free
       end type
     contains
! =============== suroutine: allocateBuffers
      subroutine allocateBuffers(this,leni)
         implicit none
         class(interface) :: this
         integer,intent(in) :: leni
         double precision,pointer,contiguous :: buffer_1(:),buffer_2(:)

         this%bufferLength = leni

         allocate(this%swap_1(this%bufferLength))
         allocate(this%swap_2(this%bufferLength))

         !--- 1: Plain pointing + "data create" here + Initialising the array first in the host
         !    Works for: all compilers
         !    even if values are not moved to the gpu (using create)
         buffer_1 => this%swap_1
         buffer_1=0.0
         !$acc enter data create(buffer_1)

         !--- 2: Plain pointing + "data create" here
         !    Works for: all but cray compiler
         !    NOT working for: cray compiler
         buffer_2 => this%swap_2
         !$acc enter data create(buffer_2)
      end subroutine allocateBuffers
! ==============================================

! =============== subroutine: free
      subroutine free(this)
         implicit none
         class(interface) :: this
         double precision,pointer,contiguous :: buffer_1(:),buffer_2(:)

         buffer_1 => this%swap_1(:)
         buffer_2 => this%swap_2(:)
         !$acc exit data delete(buffer_1(:))         
         !$acc exit data delete(buffer_2(:))
         deallocate(this%swap_1)
         deallocate(this%swap_2)
      end subroutine free
! ==============================================

      end module interfaces
!+++++++++++++++++++++++++++++++++++++++++++++++
! END MODULE
!+++++++++++++++++++++++++++++++++++++++++++++++

!+++++++++++++++++++++++++++++++++++++++++++++++
! PROGRAM: TESTING
!+++++++++++++++++++++++++++++++++++++++++++++++
       program testing
#if defined (_OPENACC)
       use openacc
#endif
       use interfaces

       implicit none
       integer, parameter ::  GRIDX=3, GRIDY=6
       type(interface) :: swaper
       double precision, pointer,contiguous :: bufferM_1(:),bufferM_2(:)
       integer :: local_nx,local_ny


! -------- Checking arguments are correct
       local_ny=GRIDY
       local_nx=GRIDX

! --------- Allocating swaper-arrays using swaper-functions
       call swaper%allocateBuffers(local_nx)

! --------- Using host-"M"-pointers to point towards swaper-arrays
       bufferM_1=>swaper%swap_1
       bufferM_2=>swaper%swap_2

!===================== 
! Accessing arrays allocated in the GPU by the swaper-functions
!(Comment tests that are failng if you want the code to move forward)
       write(*,*) 'Testing access to the arrays on the GPU' 
! --------- Testing aproach 1
       print *,'initial bufferM_1=',bufferM_1
       !$acc serial present(bufferM_1)
           bufferM_1(:)=1.0
       !$acc end serial
       !$acc update host(bufferM_1)
       print *,'final bufferM_1=',bufferM_1

! --------- Testing aproach 2
       print *,'initial bufferM_2=',bufferM_2
       !$acc serial present(bufferM_2)
           bufferM_2(:)=2.0
       !$acc end serial
       !$acc update host(bufferM_2)
       print *,'final bufferM_2=',bufferM_2

!===================== 
! Free-ing of memory may fail in exactly the same "faulty" pointers
!(Check the free-ing of memroy in the free) subroutine of the
!"interface" type)
       write(*,*) 'Free-ing memory'
       call swaper%free()

!===================== 
       write(*,*) 'Done'
      end program testing
!+++++++++++++++++++++++++++++++++++++++++++++++
! END PROGRAM
!+++++++++++++++++++++++++++++++++++++++++++++++
