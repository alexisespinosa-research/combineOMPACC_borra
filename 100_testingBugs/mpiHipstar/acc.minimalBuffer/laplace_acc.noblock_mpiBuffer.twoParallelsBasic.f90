!+++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++
      module interfaces
       implicit none
!========= Swap interface as in HiPSTAR
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
! =============== SUBROUTINE allocateBuffers
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

         !--- 1: Initialising the array in the host works
         !    even if values are not moved to the gpu (using create)
         buffer_1 => this%swap_1
         buffer_1=0.0
         !$acc enter data create(buffer_1)

         !--- 2: Initialising array index limits works
         buffer_2 => this%swap_2(:)
         !This one also works:buffer_2(1:) => this%swap_2(1:)
         !$acc enter data create(buffer_2)
         !-$-:This one also works:$acc enter data create(buffer_2(:))
         !-$-:This one also works:$acc enter data create(buffer_2(1:))

         !--- 3: "copyin" instead of "create" works
         buffer_3 => this%swap_3
         !$acc enter data copyin(buffer_3)

         !--- 4: Plain pointing only works if there is a "create"
         !       in the calling function
         buffer_4 => this%swap_4
         !$acc enter data create(buffer_4)

         !--- 5: Plain pointing does not work
         buffer_5 => this%swap_5
         !$acc enter data create(buffer_5)
      end subroutine allocateBuffers
! ==============================================

! =============== SUBROUTINE free
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
!+++++++++++++++++++++++++++++++++++++++++++++++

!+++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++
       program laplace
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

! --------- Allocating arrays inside swaper and creating host-pointers
       call swaper%allocateBuffers(local_nx)
       bufferM_1=>swaper%swap_1
       bufferM_2=>swaper%swap_2
       bufferM_3=>swaper%swap_3
       bufferM_4=>swaper%swap_4
       bufferM_5=>swaper%swap_5

!===================== 
! Accessing arrays allocated in the GPU
!(Comment failed tests if you want the code to move forward)
       write(*,*) 'Testing access to the arrays on the GPU' 
! --------- Testing aproach 1
       !$acc update host(bufferM_1)
       print *,'bufferM_1=',bufferM_1

! --------- Testing aproach 2
       !$acc update host(bufferM_2)
       print *,'bufferM_2=',bufferM_2

! --------- Testing aproach 3
       !$acc update host(bufferM_3)
       print *,'bufferM_3=',bufferM_3

! --------- Testing aproach 4
       !$acc enter data create(bufferM_4)
       !$acc update host(bufferM_4)
       print *,'bufferM_4=',bufferM_4

! --------- Testing aproach 5
       !$acc update host(bufferM_5)
       print *,'bufferM_5=',bufferM_5

!===================== 
! Free-ing of memory may fail in exactly the same "faulty" pointers
!(Check the free-ing of memroy in the free) subroutine of the
!"interface" type)
       write(*,*) 'Free-ing memory'
       call swaper%free()

!===================== 
       write(*,*) 'Done'
      end program laplace
!+++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++


