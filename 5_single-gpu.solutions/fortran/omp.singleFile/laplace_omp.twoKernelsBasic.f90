       program laplace
       !aeg:use openacc
       use omp_lib

       implicit none
       integer :: nx,ny
       double precision, parameter :: MAX_TEMP_ERROR=0.02
       integer, parameter :: CX=10,CY=10
       double precision, allocatable :: T(:,:)
       double precision, allocatable :: T_new(:,:)
       integer i,j
       integer max_iterations
       integer :: iteration=1 
       double precision :: dt=100.0D0
       character(len=32) :: arg
       integer :: start_time,stop_time,clock_rate
       double precision :: elapsed_time
       integer :: checkInput

! -------- Checking if input arguments are correct
       checkInput=0
       if (command_argument_count().ne.3) then
         call getarg(0, arg)
         print *, 'Usage: ',trim(arg),' <number_of_iterations> <grid_size_in_X> <grid_size_in_Y>'
         checkInput=-1
       end if
       if (checkInput /= 0) then 
          stop
       end if

       call getarg(1,arg)
       read(arg,*)  max_iterations
       call getarg(2,arg)
       read(arg,*)  nx
       call getarg(3,arg)
       read(arg,*)  ny
       print *, 'Solving for NumberIterations=',max_iterations,', grid_size_in_X=',nx,&
                'grid_size_in_Y=',ny

! --------- Allocating and Initialising distributed array
       allocate(T(0:nx+1,0:ny+1),T_new(0:nx+1,0:ny+1))
       !print *, 'Passed allocation'
       !call init_linear128(T,nx,ny)
       call init_fixedIndexVal(T,nx,ny)
       !call init_iIndex(T,nx,ny)
       !call init_jIndex(T,nx,ny)
       !call init_0(T,nx,ny)

       !---- Printing array
       !print *,T
!       do i=nx+2-10+1,nx+2
!          print "(2(a,i6.0),a,f10.5)",'iFort=',i,',jFort=',ny+2,&
!              ',T(iFort,jFort)=',T(i,ny+2)
!       end do
!       do j=ny+2-10+1,ny+2
!          print "(2(a,i6.0),a,f10.5)",'iFort=',nx+2,',jFort=',j,&
!              ',T(iFort,jFort)=',T(nx+2,j)
!       end do

! --------- Simulation Iterations
       call system_clock(count_rate=clock_rate)
       call system_clock(count=start_time)
       !$omp target enter data map(to:T) map(alloc:T_new)
       !$aeg-acc enter data copyin(T) create(T_new)
       do while ((dt.gt.MAX_TEMP_ERROR).and. &
                (iteration.le.max_iterations))

          !reset dt
          dt=0.0 

          !main computational kernel, average over neighbours in the grid
          !$omp target
          !$omp teams distribute parallel do simd collapse(2)
          !$aeg-acc parallel
          !$aeg-acc loop gang worker vector collapse(2)
          do j=1,ny
             do i=1,nx
                T_new(i,j)=0.25*(T(i+1,j)+T(i-1,j)+T(i,j+1)+T(i,j-1))
             end do
          end do 
          !$omp end teams distribute parallel do simd
          !$omp end target
          !$aeg-acc end loop
          !$aeg-acc end paralel

          !compute the largest change and copy T_new to T 
          !$omp target  map(dt)
          !$omp teams distribute parallel do simd collapse(2) reduction(max:dt)
          !$aeg-acc parallel
          !$aeg-acc loop gang worker vector collapse(2) reduction(max:dt)
          do j=1,ny
             do i=1,nx
                dt = max(abs(T_new(i,j)-T(i,j)),dt)
                T(i,j)=T_new(i,j)
             end do
          end do
          !$omp end teams distribute parallel do simd
          !$omp end target
          !$aeg-acc end loop
          !$aeg-acc end paralel

          !periodically print largest change
          if (mod(iteration,100).eq.0) then
          !if (mod(iteration,1).eq.0) then
             print "(a,i4,1(a,f15.10),2(a,i2),(a,f25.10))",&
             'Iteration ',iteration,', dt ',dt,&
             ',T(GXB-',CX,',GYB-',CY,')=',T(nx+1-CX,ny+1-CY)
             !print *, T
          end if
           
          iteration=iteration+1        
       end do
       !$omp target exit data map(from:T) map(delete:T_new)
       !$aeg-acc exit data copyout(T) delete(T_new)
       print "(a,i4,1(a,f15.10),2(a,i2),(a,f25.10))",&
       'Iteration ',iteration,', dt ',dt,&
       ',T(GXB-',CX,',GYB-',CY,')=',T(nx+1-CX,ny+1-CY)
       !print *, T

! --------- Final time measures
       call system_clock(count=stop_time)
       elapsed_time=real(stop_time-start_time)/real(clock_rate)
       print "(a,f10.6,a)",'Total time was (measured with system_clock)',elapsed_time,' seconds.'

! --------- Finalizing code
       deallocate(T, T_new)

! ===============================================
       contains
! =============== SUBROUTINE init_linear128
       subroutine init_linear128(T, nx, ny)
       implicit none
       integer, intent( in ) :: nx, ny
       double precision, intent( out ) :: T(0:,0:)
       integer :: hnx, hny, i, j

! ------ size of the received array
       hnx=size(T,1)-2
       hny=size(T,2)-2

! ------ interior of the array (goes beyond, but that will be fixed by boundary conditions)
       do j=0,hny+1
          do i=0,hnx+1
            T(i,j)=0.0D0
          end do
       end do

! ----- set upper boundary to 0
       do i=0,hnx+1
          T(i,0)=0.0D0
       end do

! ----- set lower boundary to the lineary varying temperature
       do i=0,hnx+1
          T(i,hny+1)=(128.0D0/dble(nx))*dble(i)
       end do

! ----- set left boundary to 0
       do j=0,hny+1
          T(0,j)=0.0D0
       end do

! ----- set right boundary to the lineary varying temperature
       do j=0,hny+1
          T(hnx+1,j)=(128.0D0/dble(ny))*dble(j)
       end do
      end subroutine init_linear128
! ==============================================
! =============== SUBROUTINE init_fixedIndexVal
       subroutine init_fixedIndexVal(T,nx,ny)
       implicit none
       double precision, intent( out ) :: T(0:,0:)
       integer, intent( in ) :: nx,ny
       integer :: hnx, hny, i, j

! ------ size of the received array
       hnx=size(T,1)-2
       hny=size(T,2)-2

! ------ interior of the array (goes beyond, but that will be fixed by boundary conditions)
       do j=0,hny+1
          do i=0,hnx+1
          T(i,j)=dble(i+j)
          end do
       end do

! ----- set upper boundary to 0
       do i=0,hnx+1
          T(i,0)=0.0D0
       end do

! ----- set lower boundary to same as index
       do i=0,hnx+1
          T(i,hny+1)=dble(i)
       end do

! ----- set left boundary to 0
       do j=0,hny+1
          T(0,j)=0.0D0
       end do

! ----- set right boundary to same as index
       do j=0,hny+1
          T(hnx+1,j)=dble(j)
       end do
       end subroutine init_fixedIndexVal
! ==============================================
! =============== SUBROUTINE init_iIndex
       subroutine init_iIndex(T,nx,ny)
       implicit none
       double precision, intent( out ) :: T(0:,0:)
       integer, intent( in ) :: nx,ny
       integer :: hnx, hny, i, j

! ------ size of the received array
       hnx=size(T,1)-2
       hny=size(T,2)-2

! ------ interior of the array (goes beyond, but that will be fixed by boundary conditions)
       do j=0,hny+1
          do i=0,hnx+1
          T(i,j)=dble(i)
          end do
       end do

! ----- set upper boundary to 0
       do i=0,hnx+1
          T(i,0)=0.0D0
       end do

! ----- set lower boundary to same as index
       do i=0,hnx+1
          T(i,hny+1)=dble(i)
       end do

! ----- set left boundary to 0
       do j=0,hny+1
          T(0,j)=0.0D0
       end do

! ----- set right boundary to same as index
       do j=0,hny+1
          T(hnx+1,j)=dble(j)
       end do
       end subroutine init_iIndex
! ==============================================
! =============== SUBROUTINE init_jIndex
       subroutine init_jIndex(T,nx,ny)
       implicit none
       double precision, intent( out ) :: T(0:,0:)
       integer, intent( in ) :: nx,ny
       integer :: hnx, hny, i, j

! ------ size of the received array
       hnx=size(T,1)-2
       hny=size(T,2)-2

! ------ interior of the array (goes beyond, but that will be fixed by boundary conditions)
       do j=0,hny+1
          do i=0,hnx+1
          T(i,j)=dble(j)
          end do
       end do

! ----- set upper boundary to 0
       do i=0,hnx+1
          T(i,0)=0.0D0
       end do

! ----- set lower boundary to same as index
       do i=0,hnx+1
          T(i,hny+1)=dble(i)
       end do

! ----- set left boundary to 0
       do j=0,hny+1
          T(0,j)=0.0D0
       end do

! ----- set right boundary to same as index
       do j=0,hny+1
          T(hnx+1,j)=dble(j)
       end do
       end subroutine init_jIndex
! ==============================================
! =============== SUBROUTINE init_0
       subroutine init_0(T,nx,ny)
       implicit none
       double precision, intent( out ) :: T(0:,0:)
       integer, intent( in ) :: nx,ny
       integer :: hnx, hny, i, j

! ------ size of the received array
       hnx=size(T,1)-2
       hny=size(T,2)-2

! ------ interior of the array (goes beyond, but that will be fixed by boundary conditions)
       do j=0,hny+1
          do i=0,hnx+1
          T(i,j)=0.0
          end do
       end do

! ----- set upper boundary to 0
       do i=0,hnx+1
          T(i,0)=0.0D0
       end do

! ----- set lower boundary to same as index
       do i=0,hnx+1
          T(i,hny+1)=dble(i)
       end do

! ----- set left boundary to 0
       do j=0,hny+1
          T(0,j)=0.0D0
       end do

! ----- set right boundary to same as index
       do j=0,hny+1
          T(hnx+1,j)=dble(j)
       end do
       end subroutine init_0
! ==============================================
      end program laplace
! ======================================================
