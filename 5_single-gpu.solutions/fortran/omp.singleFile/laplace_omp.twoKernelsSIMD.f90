       program laplace

       implicit none
!       integer, parameter ::  GRIDX=2048, GRIDY=2048
!       integer, parameter ::  GRIDX=16384, GRIDY=16384
!       integer, parameter ::  GRIDX=16384, GRIDY=8192
       integer, parameter ::  GRIDX=8192, GRIDY=4096
       double precision, parameter :: MAX_TEMP_ERROR=0.02
       integer, parameter :: CORNEROFFSET=10
       double precision, allocatable :: T(:,:)
       double precision, allocatable :: T_new(:,:)
       integer i,j
       integer max_iterations
       integer :: iteration=1 
       double precision :: dt=100
       character(len=32) :: arg
       integer start_time,stop_time,clock_rate
       real elapsed_time

       if (command_argument_count().ne.1) then
         call getarg(0, arg)
         print *, 'Usage ',trim(arg),' number_of_iterations'
       else 
         call getarg(1,arg)
         read(arg,*)  max_iterations
       end if 

       call system_clock(count_rate=clock_rate)
       call system_clock(count=start_time)

       !---- Allocating the arrays
       allocate(T(GRIDX+2,GRIDY+2))
       allocate(T_new(GRIDX+2,GRIDY+2))

       !---- Initialising arrays
       !call init_linear128(GRIDX,GRIDY,T)
       call init_fixedIndexVal(GRIDX,GRIDY,T)

       !---- Printing array
       !print *,T
!       do i=GRIDX+2-10+1,GRIDX+2
!          print "(2(a,i6.0),a,f10.5)",'iFort=',i,',jFort=',GRIDY+2,&
!              ',T(iFort,jFort)=',T(i,GRIDY+2)
!       end do
!       do j=GRIDY+2-10+1,GRIDY+2
!          print "(2(a,i6.0),a,f10.5)",'iFort=',GRIDX+2,',jFort=',j,&
!              ',T(iFort,jFort)=',T(GRIDX+2,j)
!       end do

       !simulation iterations
       !$omp target enter data map(to:T) map(alloc:T_new)
       do while ((dt.gt.MAX_TEMP_ERROR).and. &
                (iteration.le.max_iterations))

          !reset dt
          dt=0.0 

          !main computational kernel, average over neighbours in the grid
          !$omp target teams
          !$omp distribute parallel do collapse(2)
          do j=2,GRIDY+1
             do i=2,GRIDX+1
                T_new(i,j)=0.25*(T(i+1,j)+T(i-1,j)+T(i,j+1)+T(i,j-1))
             end do
          end do 
          !$omp end distribute parallel do
          !$omp end target teams

          !compute the largest change and copy T_new to T 
          !$omp target teams map(dt) reduction(max:dt)
          !$omp distribute parallel do collapse(2) reduction(max:dt)
          do j=2,GRIDY+1
             do i=2,GRIDX+1
                dt = max(abs(T_new(i,j)-T(i,j)),dt)
                T(i,j)=T_new(i,j)
             end do
          end do
          !$omp end distribute parallel do
          !$omp end target teams

          !periodically print largest change
          if (mod(iteration,100).eq.0) then
          !if (mod(iteration,1).eq.0) then
           print "(a,i4.0,2(a,f15.10))",'Iteration ',iteration,&
                   ', dt ',dt,', T[GXB-CO][GYB-CO]=',&
                   T(GRIDX+2-CORNEROFFSET,GRIDY+2-CORNEROFFSET)
           !print *,T
          end if
           
          iteration=iteration+1        
       end do
       !$omp target exit data map(from:T) map(delete:T_new)
       print "(a,i4.0,2(a,f15.10))",'Final values, iteration ',&
            iteration,', dt ',dt,', T[GXB-CO][GYB-CO]=',&
            T(GRIDX+2-CORNEROFFSET,GRIDY+2-CORNEROFFSET)

       call system_clock(count=stop_time)
       elapsed_time=real(stop_time-start_time)/real(clock_rate)
       print "(a,f10.6,a)",'Total time was ',elapsed_time,' seconds.'

       end program laplace
!=============================================

!=============================================
       subroutine init_linear128(GRIDX, GRIDY, T)
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
          T(i,GRIDY+2)=(128.0/dble(GRIDX))*dble(i-1)
       end do

!      set top to 0 and bottom to linear increase
       do j=1,GRIDY+2
          T(1,j)=0.0
          T(GRIDX+2,j)=(128.0/dble(GRIDY))*dble(j-1)
       end do
      end subroutine init_linear128
!=============================================
!=============================================
      subroutine init_fixedIndexVal(GRIDX, GRIDY, T)
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

!      set left side to 0 and right to same as index
       do i=1,GRIDX+2
          T(i,1)=0.0
          T(i,GRIDY+2)=dble(i-1)
       end do

!      set top to 0 and bottom to same as index
       do j=1,GRIDY+2
          T(1,j)=0.0
          T(GRIDX+2,j)=dble(j-1)
       end do
      end subroutine init_fixedIndexVal
!=============================================
