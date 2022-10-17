       program laplace
       use mpi
       implicit none
!       integer, parameter ::  GRIDX=1024, GRIDY=1024
!       integer, parameter ::  GRIDX=2048, GRIDY=2048
       integer, parameter ::  GRIDX=16384, GRIDY=16384
       integer :: nx,ny
       double precision, parameter :: MAX_TEMP_ERROR=0.02
       double precision, allocatable ::  T(:,:)
       double precision, allocatable ::  T_new(:,:)
       integer i,j
       integer max_iterations
       integer :: iteration=1 
       double precision :: dt=0.0,dt_world=100
       character(len=32) :: arg
       integer start_time,stop_time,clock_rate
       real elapsed_time
       integer :: ierr, csize, myrank, requests(4)
       integer status(MPI_STATUS_SIZE)
       integer :: local_nx,local_ny,bx,by,bxtot,bytot
       integer :: ixstart,jystart,leftx,lefty

! -------- MPI startup
       call mpi_init(ierr)
       call mpi_comm_size(MPI_COMM_WORLD, csize, ierr)
       call mpi_comm_rank(MPI_COMM_WORLD, myrank, ierr)
       start_time=MPI_Wtime()

! -------- Choosing device
       call omp_set_default_device(myrank)

! -------- Checking arguments are correct
       if (myrank == 0) then
          if (command_argument_count().ne.1) then
            call getarg(0, arg)
            print *, 'Usage ',trim(arg),' number_of_iterations'
            stop
          end if
       end if
       call getarg(1,arg)
       read(arg,*)  max_iterations
       nx = GRIDX
       ny = GRIDY

! --------- Distributing the MPI load 
!      Y direction
       bytot=csize
       by=myrank+1
       local_ny=ny/bytot
       if (local_ny*bytot .lt. ny) then
          if (by-1 .lt. ny-local_ny*bytot) then 
             local_ny=local_ny+1
          end if
       end if
       lefty=modulo(ny,bytot)
       jystart=(by-1)*local_ny+1
       if (by .gt. lefty) then
          jystart=jystart+lefty
       else
          jystart=jystart+(by-1)
       end if
       print *, 'myrank=',myrank,', of total csize=',csize
       print *, 'myrank=',myrank,', by=',by,' of total bytot=',bytot
       print *, 'myrank=',myrank,',local_ny=',local_ny, &
                ' of total ny=',ny,' with jystart=',jystart


!      X direction
       bxtot=1
       bx=1
       local_nx=nx
       ixstart=1


! --------- Allocating and Initialising distributed array
       allocate(T(0:local_nx+1,0:local_ny+1))
       allocate(T_new(0:local_nx+1,0:local_ny+1))
       call init(T,bx,by,bxtot,bytot,ixstart,jystart,nx,ny)

       !simulation iterations
       requests=MPI_REQUEST_NULL
       !$omp target data map(tofrom:T) map(alloc:T_new)
       do while ((dt_world.gt.MAX_TEMP_ERROR).and. &
                (iteration.le.max_iterations))

          !reset dt's
          dt=0.0 
          dt_world=0.0

          !main computational kernel, average over neighbours in the grid
          !$omp target teams
          !$omp distribute parallel do collapse(2)
          do j=1,local_ny
             do i=1,local_nx
                T_new(i,j)=0.25*(T(i+1,j)+T(i-1,j)+T(i,j+1)+T(i,j-1))
             end do
          end do 
          !$omp end distribute parallel do
          !$omp end target teams

          !compute the largest change and copy T_new to T 
          !$omp target teams map(dt) reduction(max:dt)
          !$omp distribute parallel do collapse(2) reduction(max:dt)
          do j=1,local_ny
             do i=1,local_nx
                dt = max(abs(T_new(i,j)-T(i,j)),dt)
                T(i,j)=T_new(i,j)
             end do
          end do
          !$omp end distribute parallel do
          !$omp end target teams

          !---- Retrieve the edge data from the GPU:
          !$aeg-omp target update from(T[1:local_nx][1:1])
          !$aeg-omp target update from(T[1:local_nx][local_ny:1])
          !$omp target update from(T)

          !---- send the edge data column into the left halo region
          !   - and receive data from the left into own halo region
          if (myrank.gt.0) then
             !print *,'Start to deal with left'
             call mpi_isend(T(1,1),local_nx, MPI_DOUBLE,&
                           myrank-1,0,MPI_COMM_WORLD,requests(1),ierr)
             call mpi_irecv(T(1,0),local_nx, MPI_DOUBLE,&
                           myrank-1,0,MPI_COMM_WORLD,requests(2),ierr)
             !print *,'End to deal with left'
          end if

          !---- send the edge data column into the right halo region
          !   - and receive data from the right into own halo region
          if (myrank.lt.csize-1) then
             !print *,'Start to deal with right'
             call mpi_isend(T(1,local_ny),local_nx, MPI_DOUBLE,& 
                           myrank+1,0,MPI_COMM_WORLD,requests(3),ierr)
             call mpi_irecv(T(1,local_ny+1),local_nx, MPI_DOUBLE,&
                           myrank+1,0,MPI_COMM_WORLD,requests(4),ierr)
             !print *,'End to deal with right'
          end if

          !---- reduce the dt value among all MPI ranks
          call mpi_allreduce(dt, dt_world, 1, MPI_DOUBLE,&
                             MPI_SUM, MPI_COMM_WORLD, ierr)

          !---- Waiting for the MPI messages to finalise
          !print *,'Start waiting all'
          call mpi_waitall(4, requests, MPI_STATUSES_IGNORE,ierr)
          !print *,'End waiting all'

          !---- Resend edge data to the GPU:
          !$aeg-omp target update to(T[1:local_nx][0:1])
          !$aeg-omp target update to(T[1:local_nx][local_ny+1:1])
          !$omp target update to(T)

          !periodically print largest change
          if (mod(iteration,100).eq.0) then
          !if (mod(iteration,1).eq.0) then
             print "(a,i4.0,a,f15.7)",'Iteration ',iteration,', dt ',dt
          end if  

          iteration=iteration+1        
       end do
       !$omp end target data

       stop_time=MPI_Wtime()
       elapsed_time=stop_time-start_time
       print "(a,f10.6,a)",'Total time was ',elapsed_time,' seconds.'

       deallocate(T, T_new)
       call mpi_finalize(ierr)

! ===============================================
       contains
! =============== SUBROUTINE INIT
       subroutine init(T,bx,by,bxtot,bytot,ixstart,jystart,nxtot,nytot)
       implicit none
       double precision, intent( out ) :: T(0:,0:)
       integer, intent(in) :: bx,by,bxtot,bytot
       integer, intent( in ) :: ixstart,jystart,nxtot,nytot 
       integer :: hnx, hny, i, j

! ------ size of the received array
       hnx=size(T,1)-2
       hny=size(T,2)-2
        
! ------ interior of the array
       do j=1,hny
          do i=1,hnx
             T(i,j)=0.0
          end do
       end do

! ----- if the piece is part of the top boundary bx=1
! ----- set left boundary to 0
       if (bx == 1) then
          do j=0,hny+1
             T(0,j)=0.0
          end do
       end if

! ----- if the piece is part of the bottom boundary bx=bxtot
! ----- set right boundary to the lineary varying temperature
       if (bx == bxtot) then
          do j=0,hny+1
             T(hnx+1,j)=(128.0/nytot)*(jystart+j-1)
          end do
       end if

! ----- if the piece is part of the left boundary by=1
! ----- set lower boundary to 0
       if (by == 1) then
          do i=0,hnx+1
             T(i,0)=0.0
          end do
       end if

! ----- if the piece is part of the right boundary by=bytot
! ----- set upper boundary to the lineary varying temperature
       if (by == bytot) then
          do i=0,hnx+1
             T(i,hny+1)=(128.0/nxtot)*(ixstart+i-1)   
          end do
       end if


!      set top to 0 and bottom to linear increase

       end subroutine init
     end program laplace
! ======================================================

