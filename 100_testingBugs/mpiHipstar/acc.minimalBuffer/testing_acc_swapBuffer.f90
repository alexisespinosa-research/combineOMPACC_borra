!+++++++++++++++++++++++++++++++++++++++++++++++
! MODULE: INTERFACES
!+++++++++++++++++++++++++++++++++++++++++++++++
      module interfaces
       implicit none
!=========  TYPE: INTERFACE as the "swap interface" in HiPSTAR
       type interface
          double precision,pointer,contiguous :: swap_1(:), swap_2(:)
          double precision,pointer,contiguous :: swap_3(:), swap_4(:)
          double precision,pointer,contiguous :: swap_5(:), swap_6(:)
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
         double precision,pointer,contiguous :: buffer_3(:),buffer_4(:)
         double precision,pointer,contiguous :: buffer_5(:),buffer_6(:)

         this%bufferLength = leni

         allocate(this%swap_1(this%bufferLength))
         allocate(this%swap_2(this%bufferLength))
         allocate(this%swap_3(this%bufferLength))
         allocate(this%swap_4(this%bufferLength))
         allocate(this%swap_5(this%bufferLength))

         !--- 1: Plain pointing + "data create" here + Initialising the array first in the host
         !    Works for: all compilers
         !    even if values are not moved to the gpu (using create)
         buffer_1 => this%swap_1
         buffer_1=0.0
         !$acc enter data create(buffer_1)

         !--- 2: Indicating array index limits when pointing + "data create" here
         !    Works for: all compilers
         buffer_2 => this%swap_2(:)
         !$acc enter data create(buffer_2)
         !---:These other pointings also work:
         !---:     buffer_2(1:) => this%swap_2(1:)
         !---:These other data create also work:
         !---:     $acc enter data create(buffer_2(:))
         !---:     $acc enter data create(buffer_2(1:))

         !--- 3: Plain pointing + "data copyin" here instead of "data create"
         !    Works for: all compilers
         buffer_3 => this%swap_3
         !$acc enter data copyin(buffer_3)

         !--- 4: Plain pointing + "data create" here 
         !       + additional "data create" in the calling function (check the call)
         !    Works for: all compilers
         buffer_4 => this%swap_4
         !$acc enter data create(buffer_4)

         !--- 5: Plain pointing + "data create" here
         !    Works for: nvhpc
         !    NOT working for: cray compiler
         buffer_5 => this%swap_5
         !$acc enter data create(buffer_5)
      end subroutine allocateBuffers
! ==============================================

! =============== subroutine: free
      subroutine free(this)
         implicit none
         class(interface) :: this
         double precision,pointer,contiguous :: buffer_1(:),buffer_2(:)
         double precision,pointer,contiguous :: buffer_3(:),buffer_4(:)
         double precision,pointer,contiguous :: buffer_5(:),buffer_6(:)

         buffer_1 => this%swap_1(:)
         buffer_2 => this%swap_2(:)
         buffer_3 => this%swap_3(:)
         buffer_4 => this%swap_4(:)
         buffer_5 => this%swap_5(:)
         !$acc exit data delete(buffer_1(:))         
         !$acc exit data delete(buffer_2(:))
         !$acc exit data delete(buffer_3(:))         
         !$acc exit data delete(buffer_4(:))
         !$acc exit data delete(buffer_5(:))
         deallocate(this%swap_1)
         deallocate(this%swap_2)
         deallocate(this%swap_3)
         deallocate(this%swap_4)
         deallocate(this%swap_5)
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
       !use iso_c_binding, only :: c_ptr, c_loc, c_f_pointer
       use iso_c_binding
       use interfaces

       implicit none
       integer, parameter ::  GRIDX=3, GRIDY=6
       integer :: nx,ny
       type(interface) :: swaper
       double precision, pointer,contiguous :: bufferM_1(:),bufferM_2(:)
       double precision, pointer,contiguous :: bufferM_4(:),bufferM_3(:)
       double precision, pointer,contiguous :: bufferM_5(:),bufferM_6(:)
       integer :: local_nx,local_ny


! -------- Checking arguments are correct
       nx = GRIDX
       ny = GRIDY
       local_ny=ny
       local_nx=nx

! --------- Allocating swaper-arrays using swaper-functions
       call swaper%allocateBuffers(local_nx)

! --------- Using host-"M"-pointers to point towards swaper-arrays
       bufferM_1=>swaper%swap_1
       bufferM_2=>swaper%swap_2
       bufferM_3=>swaper%swap_3
       bufferM_4=>swaper%swap_4
       bufferM_5=>swaper%swap_5

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

! --------- Testing aproach 3
       print *,'initial bufferM_3=',bufferM_3
       !$acc serial present(bufferM_3)
           bufferM_3(:)=3.0
       !$acc end serial
       !$acc update host(bufferM_3)
       print *,'final bufferM_3=',bufferM_3

! --------- Testing aproach 4
       print *,'initial bufferM_4=',bufferM_4
       !$acc enter data create(bufferM_4)
       !$acc serial present(bufferM_4)
           bufferM_4(:)=4.0
       !$acc end serial
       !$acc update host(bufferM_4)
       print *,'final bufferM_4=',bufferM_4

! --------- Testing aproach 5
       print *,'initial bufferM_5=',bufferM_5
       !$acc serial present(bufferM_5)
           bufferM_5(:)=5.0
       !$acc end serial
       !$acc update host(bufferM_5)
       print *,'final bufferM_5=',bufferM_5

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


