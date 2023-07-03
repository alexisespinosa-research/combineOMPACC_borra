       program laplace

       implicit none
!       integer, parameter ::  GRIDX=2048, GRIDY=2048
!       integer, parameter ::  GRIDX=16384, GRIDY=16384
       integer ::  GRIDX, GRIDY
       double precision, parameter :: MAX_TEMP_ERROR=0.02
!       double precision ::  T(GRIDX+2,GRIDY+2)
!       double precision ::  T_new(GRIDX+2,GRIDY+2)
       double precision, allocatable, target ::  T(:,:)
       double precision, allocatable, target ::  T_new(:,:)
       integer i,j
       integer max_iterations
       integer :: iteration=1 
       double precision :: dt=100
       character(len=32) :: arg
       integer start_time,stop_time,clock_rate
       real elapsed_time

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
       allocate(T(GRIDX+2,0:GRIDY+2))
       allocate(T_new(GRIDX+2,0:GRIDY+2))
       call init(GRIDX,GRIDY,T)

       !simulation iterations
       !$acc data copy(T) create(T_new)
       !{
       do while ((dt.gt.MAX_TEMP_ERROR).and. &
                (iteration.le.max_iterations))

          !reset dt
          dt=0.0 

          !$acc data copy(dt)
          !$acc parallel reduction(max:dt)
          !{
          !main computational kernel, average over neighbours in the grid
          !$acc loop collapse(2)
          do j=2,GRIDY+1
             do i=2,GRIDX+1
                T_new(i,j)=0.25*(T(i+1,j)+T(i-1,j)+T(i,j+1)+T(i,j-1))
             end do
          end do 

          !compute the largest change and copy T_new to T 
          !$acc loop collapse(2) reduction(max:dt)
          do j=2,GRIDY+1
             do i=2,GRIDX+1
                dt = max(abs(T_new(i,j)-T(i,j)),dt)
                T(i,j)=T_new(i,j)
             end do
          end do
          !}
          !$acc end parallel 
          !$acc end data

          !periodically print largest change
          if (mod(iteration,100).eq.0) then
             print "(a,i4.0,a,f8.6)",'Iteration ',iteration,', dt ',dt
          end if  
           
          iteration=iteration+1        
       end do
       !}
       !$acc end data

       call system_clock(count=stop_time)
       elapsed_time=real(stop_time-start_time)/real(clock_rate)
       print "(a,f10.6,a)",'Total time was ',elapsed_time,' seconds.'

       end program laplace

       subroutine init(GRIDX, GRIDY, T)
       implicit none
       integer, intent( in ) :: GRIDX, GRIDY 
       double precision, intent( out ) :: T(GRIDX+2,GRIDY+2)
       integer i,j
  
       do j=1,GRIDY+2
          do i=1,GRIDX+2
             T(i,j)=0.0
          end do
       end do

!      these booundary conditions never change throughout run
 
!      set left side to 0 and right to a linear increase
       do i=1,GRIDX+2
          T(i,1)=0.0
          T(i,GRIDY+2)=(128.0/GRIDX)*(i-1)
       end do

!      set top to 0 and bottom to linear increase
       do j=1,GRIDY+2
          T(1,j)=0.0
          T(GRIDY+2,j)=(128.0/GRIDY)*(j-1)   
       end do

       end subroutine init
