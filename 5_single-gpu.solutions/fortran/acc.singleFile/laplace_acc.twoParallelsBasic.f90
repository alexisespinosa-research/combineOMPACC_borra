       program laplace

       implicit none
!       integer, parameter ::  GRIDX=2048, GRIDY=2048
!       integer, parameter ::  GRIDX=16384, GRIDY=16384
       integer ::  GRIDX, GRIDY
       double precision, parameter :: MAX_TEMP_ERROR=0.02
       integer, parameter :: CX=10,CY=10
!       double precision ::  T(GRIDX+2,GRIDY+2)
!       double precision ::  T_new(GRIDX+2,GRIDY+2)
       double precision, allocatable, target ::  T(:,:)
       double precision, allocatable, target ::  T_new(:,:)
       integer i,j
       integer max_iterations
       integer :: iteration=1 
       double precision :: dt=100
       character(len=32) :: arg
       integer :: start_time,stop_time,clock_rate
       real :: elapsed_time

       ! -------- Checking if input arguments are correct
       if (command_argument_count().ne.3) then
         call getarg(0, arg)
         print *, 'Usage: ',trim(arg),' <number_of_iterations> <grid_size_in_X> <grid_size_in_Y>'
         stop
       else
         call getarg(1,arg)
         read(arg,*)  max_iterations
         call getarg(2,arg)
         read(arg,*)  GRIDX
         call getarg(3,arg)
         read(arg,*)  GRIDY
       end if

       call system_clock(count_rate=clock_rate)
       call system_clock(count=start_time)

       ! --------- Allocating and Initialising distributed array
       allocate(T(0:GRIDX+1,0:GRIDY+1))
       allocate(T_new(0:GRIDX+1,0:GRIDY+1))
       !call init_linear128(T)
       call init_fixedIndexVal(T)

       !simulation iterations
       !$acc data copy(T) create(T_new)
       !{
       do while ((dt.gt.MAX_TEMP_ERROR).and. &
                (iteration.le.max_iterations))

          !reset dt
          dt=0.0 

          !main computational kernel, average over neighbours in the grid
          !$acc parallel loop collapse(2)
          do j=1,GRIDY
             do i=1,GRIDX
                T_new(i,j)=0.25*(T(i+1,j)+T(i-1,j)+T(i,j+1)+T(i,j-1))
             end do
          end do 
          !$acc end parallel loop

          !compute the largest change and copy T_new to T 
          !$acc parallel loop collapse(2) reduction(max:dt)
          do j=1,GRIDY
             do i=1,GRIDX
                dt = max(abs(T_new(i,j)-T(i,j)),dt)
                T(i,j)=T_new(i,j)
             end do
          end do
          !$acc end parallel loop

          !periodically print largest change
          if (mod(iteration,100).eq.0) then
             print "(a,i4,1(a,f15.10),2(a,i2),(a,f25.10))",&
             'Iteration ',iteration,', dt ',dt,&
             ',T(GXB-',CX,',GYB-',CY,')=',T(GRIDX+1-CX,GRIDY+1-CY)
          end if  
           
          iteration=iteration+1        
       end do
       !}
       !$acc end data
       print "(a,i4,1(a,f15.10),2(a,i2),(a,f25.10))",&
       'Iteration ',iteration,', dt ',dt,&
       ',T(GXB-',CX,',GYB-',CY,')=',T(GRIDX+1-CX,GRIDY+1-CY)

       call system_clock(count=stop_time)
       elapsed_time=real(stop_time-start_time)/real(clock_rate)
       print "(a,f10.6,a)",'Total time was ',elapsed_time,' seconds.'

! ===============================================
       contains
! =============== SUBROUTINE init_linear128
       subroutine init_linear128(T)
       implicit none
       double precision, intent( out ) :: T(0:,0:)
       integer :: hnx, hny, i, j
 
! ------ size of the received array
       hnx=size(T,1)-2
       hny=size(T,2)-2

! ------ interior of the array
       do j=0,hny+1
          do i=0,hnx+1
             T(i,j)=0.0
          end do
       end do

! ----- set lower boundary to 0
       do i=0,hnx+1
          T(i,0)=0.0
       end do

! ----- set upper boundary to the lineary varying temperature
       do i=0,hnx+1
          T(i,hny+1)=(128.0/dble(hnx))*dble(1+i-1)
       end do

! ----- set left boundary to 0
      do j=0,hny+1
         T(0,j)=0.0
      end do

! ----- set right boundary to the lineary varying temperature
       do j=0,hny+1
          T(hnx+1,j)=(128.0/dble(hny))*dble(1+j-1)
       end do
       end subroutine init_linear128
! ==============================================
! =============== SUBROUTINE init_fixedIndexVal
       subroutine init_fixedIndexVal(T)
       implicit none
       double precision, intent( out ) :: T(0:,0:)
       integer :: hnx, hny, i, j
 
! ------ size of the received array
       hnx=size(T,1)-2
       hny=size(T,2)-2

! ------ interior of the array
       do j=0,hny+1
          do i=0,hnx+1
             !T(i,j)=0.0
             T(i,j)=dble(i+j)
          end do
       end do

! ----- set lower boundary to 0
       do i=0,hnx+1
          T(i,0)=0.0
       end do

! ----- set upper boundary to same as index
       do i=0,hnx+1
          T(i,hny+1)=dble(i)
       end do

! ----- set left boundary to 0
      do j=0,hny+1
         T(0,j)=0.0
      end do

! ----- set right boundary to same as index
       do j=0,hny+1
          T(hnx+1,j)=dble(j)
       end do
       end subroutine init_fixedIndexVal
! ==============================================
      end program laplace
! ======================================================
