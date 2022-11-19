!+++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++
      module globals
!========= Global variables
       implicit none
       integer,public :: ierr, csize, myrank, requests(4)
       contains
      end module globals
!+++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++

!+++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++
      module interfaces
       use globals
       implicit none
!========= Swap interface as in HiPSTAR
       type interface
          double precision,pointer,contiguous :: swap_out(:), swap_in(:)
          integer :: bufferLength
       contains
          procedure :: allocateBuffers
          procedure :: pack
!          procedure :: send
!          procedure :: recv
          procedure :: unpack
          procedure :: free
       end type
     contains
! =============== SUBROUTINE allocateBuffers
      subroutine allocateBuffers(this,leni)
         implicit none
         class(interface) :: this
         integer,intent(in) :: leni
         double precision,pointer,contiguous :: buffer(:),buffer_in(:),buffer_out(:)

         this%bufferLength = leni

         allocate(this%swap_in(0:this%bufferLength+1))
         !aeg-buffer => this%swap_in(:)
         buffer_in(0:) => this%swap_in(0:)
         buffer_in=0.0
         !$acc enter data copyin(buffer_in(0:this%bufferLength+1))

         allocate(this%swap_out(0:this%bufferLength+1))
         !aeg-buffer=>this%swap_out(:)
         buffer_out(0:)=>this%swap_out(0:)
         buffer_out=0.0
         !$acc enter data copyin(buffer_out(0:this%bufferLength+1))

      end subroutine allocateBuffers
! ==============================================

! =============== SUBROUTINE free
      subroutine free(this)
         implicit none
         class(interface) :: this
         double precision,pointer,contiguous :: buffer(:),buffer_in(:),buffer_out(:)
         buffer_in =>this%swap_in(:)
         buffer_out=>this%swap_out(:)
         !$acc exit data delete(buffer_in(:))            
         !$acc exit data delete(buffer_out(:))  
         deallocate(this%swap_out)
         deallocate(this%swap_in)
      end subroutine free
! ==============================================

! =============== SUBROUTINE pack
      subroutine pack(this,arr,leni,ixini,ixfin,jyini,jyfin)
         use iso_c_binding
         implicit none
         class(interface) :: this
         integer, intent(in) :: leni,ixini,ixfin,jyini,jyfin
         double precision,intent(in) :: arr(0:,0:)
         double precision,pointer,contiguous :: buffer(:)
         integer :: i,j,k
         buffer(0:) => this%swap_out(0:)
         !print *, 'myrank=',myrank,', Passed in pack buffer pointing'

         !$acc parallel loop present(buffer,arr) collapse(2)
         do j=jyini,jyfin
            do i=ixini,ixfin
               k=i
               buffer(k)=arr(i,j)
               !buffer(k)=k
            end do
         end do
         !$acc end parallel loop
      end subroutine pack
! ==============================================
! =============== SUBROUTINE unpack
      subroutine unpack(this,arr,leni,ixini,ixfin,jyini,jyfin)
         use iso_c_binding
         implicit none
         class(interface) :: this
         integer, intent(in) :: leni,ixini,ixfin,jyini,jyfin
         double precision,intent(out) :: arr(0:,0:)
         double precision,pointer,contiguous :: buffer(:)
         integer :: i,j,k
         buffer(0:) => this%swap_in(0:)
         !print *, 'myrank=',myrank,', Passed in unpack buffer pointing'
         !$acc parallel loop present(buffer,arr) collapse(2)
         do j=jyini,jyfin
            do i=ixini,ixfin
               k=i
               arr(i,j)=buffer(k)
            end do
         end do
         !$acc end parallel loop
      end subroutine unpack
! ==============================================
      end module interfaces
!+++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++

!+++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++
       program laplace
       use globals
       use mpi
#if defined (_OPENACC)
       use openacc
#endif
       !use iso_c_binding, only :: c_ptr, c_loc, c_f_pointer
       use iso_c_binding
       use interfaces

       implicit none
!       integer, parameter ::  GRIDX=3, GRIDY=6
!       integer, parameter ::  GRIDX=12, GRIDY=12
!       integer, parameter ::  GRIDX=100, GRIDY=100
!       integer, parameter ::  GRIDX=1024, GRIDY=1024
!       integer, parameter ::  GRIDX=2048, GRIDY=2048
!       integer, parameter ::  GRIDX=8192, GRIDY=8192
       integer, parameter ::  GRIDX=8192, GRIDY=4096
!       integer, parameter ::  GRIDX=16384, GRIDY=16384
!       integer, parameter ::  GRIDX=16384, GRIDY=8192
!       integer, parameter ::  GRIDX=8192, GRIDY=16384
       integer :: nx,ny
       double precision, parameter :: MAX_TEMP_ERROR=0.02
       integer, parameter :: CX=10,CY=10
       double precision, allocatable, target ::  T(:,:)
       double precision, allocatable, target ::  T_new(:,:)
       double precision, pointer ::  Tp(:,:)
       double precision, pointer ::  Tp_new(:,:)
       type(interface) :: swaperLeft,swaperRight
       double precision, pointer,contiguous :: bufferInLeft(:),bufferInRight(:)
       double precision, pointer,contiguous :: bufferOutLeft(:),bufferOutRight(:)
       integer i,j
       integer max_iterations
       integer :: iteration=1 
       double precision :: dt=0.0,dt_world=100
       character(len=32) :: arg
       double precision :: start_time,stop_time
       real elapsed_time
       integer status(MPI_STATUS_SIZE)
       integer :: local_nx,local_ny,bx,by,bxtot,bytot
       integer :: ixstart,jystart,leftx,lefty
#if defined (_OPENACC)
       integer(acc_device_kind) :: hereDeviceType
#endif

! -------- MPI startup
       call mpi_init(ierr)
       call mpi_comm_size(MPI_COMM_WORLD, csize, ierr)
       call mpi_comm_rank(MPI_COMM_WORLD, myrank, ierr)

! -------- Choosing device
       !aeg-call omp_set_default_device(myrank)
#if defined (_OPENACC)
       call acc_set_device_num(myrank,acc_get_device_type()) !acc_device_amd or acc_device_radeon
#endif

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
!      -- Y direction
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


!      -- X direction
       bxtot=1
       bx=1
       local_nx=nx
       ixstart=1


! --------- Allocating and Initialising distributed array
       allocate(T(0:local_nx+1,0:local_ny+1))
       allocate(T_new(0:local_nx+1,0:local_ny+1))
       Tp=>T
       Tp_new=>T_new
       print *, 'myrank=',myrank,', Passed pointer pointing'
       !call init_linear128(Tp,bx,by,bxtot,bytot,ixstart,jystart,nx,ny)
       call init_fixedIndexVal(Tp,bx,by,bxtot,bytot,ixstart,jystart,nx,ny)
       !call init_jIndex(Tp,bx,by,bxtot,bytot,ixstart,jystart,nx,ny)
       !call init_jIndexPow(Tp,bx,by,bxtot,bytot,ixstart,jystart,nx,ny)
       !print *,'Tini=',Tp

! --------- Preparing the swap interface
       call swaperLeft%allocateBuffers(local_nx)
       call swaperRight%allocateBuffers(local_nx)
       print *, 'myrank=',myrank,', Passed allocateBuffers'
       bufferInLeft(0:)=>swaperLeft%swap_in(0:)
       bufferOutLeft(0:)=>swaperLeft%swap_out(0:)
       bufferInRight(0:)=>swaperRight%swap_in(0:)
       bufferOutRight(0:)=>swaperRight%swap_out(0:)
       print *, 'myrank=',myrank,', Passed buffer pointing'
       !$acc update self(bufferOutLeft,bufferInLeft,bufferOutRight,bufferInRight)
       !aeg-print *,'bufferOutLeft=',bufferOutLeft
       !aeg-print *,'bufferInLeft=',bufferInLeft
       !aeg-print *,'bufferOutRight=',bufferOutRight
       !aeg-print *,'bufferInRight=',bufferInRight

! --------- Simulation Iterations
       start_time=MPI_Wtime()
       requests=MPI_REQUEST_NULL
       !$aeg-omp target enter data map(to:Tp) map(alloc:Tp_new)
       !$acc enter data copyin(Tp) create(Tp_new)
       do while ((dt_world.gt.MAX_TEMP_ERROR).and. &
                (iteration.le.max_iterations))

          !reset dt's
          dt=0.0 
          dt_world=0.0

          !main computational kernel, average over neighbours in the grid
          !$aeg-omp target teams
          !$aeg-omp distribute parallel do collapse(2)
          !$acc parallel loop collapse(2)
          do j=1,local_ny
             do i=1,local_nx
                !T_new(i,j)=0.25*(T(i+1,j)+T(i-1,j)+T(i,j+1)+T(i,j-1))
                Tp_new(i,j)=0.25*(Tp(i+1,j)+Tp(i-1,j)+Tp(i,j+1)+Tp(i,j-1))
             end do
          end do 
          !$acc end parallel loop
          !$aeg-omp end distribute parallel do
          !$aeg-omp end target teams

          !compute the largest change and copy T_new to T 
          !$aeg-omp target teams map(dt) reduction(max:dt)
          !$aeg-omp distribute parallel do collapse(2) reduction(max:dt)
          !$acc parallel loop collapse(2) reduction(max:dt)
          do j=1,local_ny
             do i=1,local_nx
                !dt = max(abs(T_new(i,j)-T(i,j)),dt)
                !T(i,j)=T_new(i,j)
                dt = max(abs(Tp_new(i,j)-Tp(i,j)),dt)
                Tp(i,j)=Tp_new(i,j)
             end do
          end do
          !$acc end parallel loop
          !$aeg-omp end distribute parallel do
          !$aeg-omp end target teams

          !---- Retrieve own-edge data from the GPU to the host:
          !$aeg-omp target update from(Tp(1:local_nx,1:1))
          !$aeg-omp target update from(Tp(1:local_nx,local_ny:local_ny))
          !$aeg-acc update self(Tp(1:local_nx,1:1))
          !$aeg-acc update self(Tp(1:local_nx,local_ny:local_ny))

          !---- Packing own-edge data inside the GPU:
          if (myrank.gt.0) then
             !$aeg-acc update self(bufferOutLeft)
             !aeg-print *,'iteration=',iteration,'pre bufferOutLeft=',bufferOutLeft
             call swaperLeft%pack(Tp,local_nx,1,local_nx,1,1)
             !$aeg-acc update self(bufferOutLeft)
             !aeg-print *,'iteration=',iteration,'post bufferOutLeft=',bufferOutLeft
          end if
          if (myrank.lt.csize-1) then
             !$aeg-acc update self(bufferOutRight)
             !aeg-print *,'iteration=',iteration,'pre bufferOutRight=',bufferOutRight
             call swaperRight%pack(Tp,local_nx,1,local_nx,local_ny,local_ny)
             !$aeg-acc update self(bufferOutRight)
             !aeg-print *,'iteration=',iteration,'post bufferOutRight=',bufferOutRight
          end if
          !print *, 'myrank=',myrank,', Passed packing'

          !---- send own-leftEdge into the neighLeft-rightHalo region
          !   - and receive from neighLeft-rightEdge into own-leftHalo region
          if (myrank.gt.0) then
             !$aeg-omp target data use_device_ptr(Tp)
             !$aeg-acc host_data use_device(Tp)
             !$acc data present (bufferOutLeft,bufferInLeft)
             !$acc host_data use_device(bufferOutLeft,bufferInLeft)
             !print *,'Start to deal with left'
             !aeg-call mpi_isend(Tp(1,1),local_nx, MPI_DOUBLE,&
             call mpi_isend(bufferOutLeft,local_nx+2, MPI_DOUBLE,&
                           myrank-1,0,MPI_COMM_WORLD,requests(1),ierr)
             !aeg-call mpi_irecv(Tp(1,0),local_nx, MPI_DOUBLE,&
             call mpi_irecv(bufferInLeft,local_nx+2, MPI_DOUBLE,&
                           myrank-1,0,MPI_COMM_WORLD,requests(2),ierr)
             !print *,'End to deal with left'
             !$acc end host_data
             !$acc end data
             !$aeg-omp end target data
          end if

          !---- send own-right-edge into the neigh-right-left-halo region
          !   - and receive data neigh-right-left-edge into own-right-halo region
          if (myrank.lt.csize-1) then
             !$aeg-omp target data use_device_ptr(Tp)
             !$aeg-acc host_data use_device(Tp)
             !$acc data present (bufferOutRight,bufferInRight)
             !$acc host_data use_device(bufferOutRight,bufferInRight)
             !print *,'Start to deal with right'
             !aeg-call mpi_isend(Tp(1,local_ny),local_nx, MPI_DOUBLE,& 
             call mpi_isend(bufferOutRight,local_nx+2, MPI_DOUBLE,& 
                           myrank+1,0,MPI_COMM_WORLD,requests(3),ierr)
             !aeg-call mpi_irecv(Tp(1,local_ny+1),local_nx, MPI_DOUBLE,&
             call mpi_irecv(bufferInRight,local_nx+2, MPI_DOUBLE,&
                           myrank+1,0,MPI_COMM_WORLD,requests(4),ierr)
             !print *,'End to deal with right'
             !$acc end host_data
             !$acc end data
             !$aeg-omp end target data
          end if

          !---- Waiting for the MPI messages to finalise
          !print *,'Start waiting all'
          call mpi_waitall(4, requests, MPI_STATUSES_IGNORE,ierr)
          !print *,'End waiting all'

          !---- Send recently-updated own-halo data to the GPU:
          !$aeg-omp target update to(Tp(1:local_nx,0:0))
          !$aeg-omp target update to(Tp(1:local_nx,local_ny+1:local_ny+1))
          !$aeg-acc update device(Tp(1:local_nx,0:0))
          !$aeg-acc update device(Tp(1:local_nx,local_ny+1:local_ny+1))

          !---- UnPacking own-edge data inside the GPU:
          if (myrank.gt.0) then
             !$aeg-acc update self(bufferInLeft)
             !aeg-print *,'iteration=',iteration,'pre bufferInLeft=',bufferInLeft
             call swaperLeft%unpack(T,local_nx,1,local_nx,0,0)
             !$aeg-acc update self(bufferInLeft)
             !aeg-print *,'iteration=',iteration,'post bufferInLeft=',bufferInLeft
          end if
          if (myrank.lt.csize-1) then
             !$aeg-acc update self(bufferInRight)
             !aeg-print *,'iteration=',iteration,'pre bufferInRight=',bufferInRight
             call swaperRight%unpack(T,local_nx,1,local_nx,local_ny+1,local_ny+1)
             !$aeg-acc update self(bufferInRight)
             !aeg-print *,'iteration=',iteration,'post bufferInRight=',bufferInRight
          end if
          !$aeg-acc update self(Tp)
          !aeg-print *,'iteration=',iteration,'Tp=',Tp

          !---- reduce the dt value among all MPI ranks
          call mpi_allreduce(dt, dt_world, 1, MPI_DOUBLE,&
                             MPI_MAX, MPI_COMM_WORLD, ierr)

          !periodically print largest change
          if (mod(iteration,100).eq.0) then
          !if (mod(iteration,1).eq.0) then
             print "(a,i4,2(a,f15.10),2(a,i2),(a,f15.10))",&
             'Iteration ',iteration,', dt ',dt,', dt_world=',dt_world,&
             ',T(GXB-',CX,',GYB-',CY,')=',Tp(local_nx+1-CX,local_ny+1-CY)
             !print *, Tp
          end if  

          iteration=iteration+1        
       end do
       !$aeg-omp target exit data map(from:Tp) map(delete:Tp_new)
       !$acc exit data copyout(Tp) delete(Tp_new)
       print "(a,i4,2(a,f15.10),2(a,i2),(a,f15.10))",&
       'Iteration ',iteration,', dt ',dt,', dt_world=',dt_world,&
       ',T(GXB-',CX,',GYB-',CY,')=',Tp(local_nx+1-CX,local_ny+1-CY)
       !print *,'Tfin=', Tp

       stop_time=MPI_Wtime()
       elapsed_time=stop_time-start_time
       print "(a,f10.6,a)",'Total time was ',elapsed_time,' seconds.'

       deallocate(T, T_new)
       call swaperLeft%free()
       call swaperRight%free()
       call mpi_finalize(ierr)

! ===============================================
       contains
! =============== SUBROUTINE init_linear128
       subroutine init_linear128(T,bx,by,bxtot,bytot,ixstart,jystart,nxtot,nytot)
       implicit none
       double precision, intent( out ) :: T(0:,0:)
       integer, intent(in) :: bx,by,bxtot,bytot
       integer, intent( in ) :: ixstart,jystart,nxtot,nytot 
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
             T(i,hny+1)=(128.0/dble(nxtot))*dble(ixstart+i-1)   
          end do
       end if
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
             T(hnx+1,j)=(128.0/dble(nytot))*dble(jystart+j-1)
          end do
       end if
       end subroutine init_linear128
! ==============================================

! =============== SUBROUTINE init_fixedIndexVal
       subroutine init_fixedIndexVal(T,bx,by,bxtot,bytot,ixstart,jystart,nxtot,nytot)
       implicit none
       double precision, intent( out ) :: T(0:,0:)
       integer, intent(in) :: bx,by,bxtot,bytot
       integer, intent( in ) :: ixstart,jystart,nxtot,nytot 
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

! ----- if the piece is part of the left boundary by=1
! ----- set lower boundary to 0
       if (by == 1) then
          do i=0,hnx+1
             T(i,0)=0.0
          end do
       end if

! ----- if the piece is part of the right boundary by=bytot
! ----- set upper boundary to same as index
       if (by == bytot) then
          do i=0,hnx+1
             T(i,hny+1)=dble(ixstart+i-1)   
          end do
       end if
! ----- if the piece is part of the top boundary bx=1
! ----- set left boundary to 0
       if (bx == 1) then
          do j=0,hny+1
             T(0,j)=0.0
          end do
       end if

! ----- if the piece is part of the bottom boundary bx=bxtot
! ----- set right boundary to same as index
       if (bx == bxtot) then
          do j=0,hny+1
             T(hnx+1,j)=dble(jystart+j-1)
          end do
       end if
       end subroutine init_fixedIndexVal
! ==============================================
! =============== SUBROUTINE init_iIndex
       subroutine init_iIndex(T,bx,by,bxtot,bytot,ixstart,jystart,nxtot,nytot)
       implicit none
       double precision, intent( out ) :: T(0:,0:)
       integer, intent(in) :: bx,by,bxtot,bytot
       integer, intent( in ) :: ixstart,jystart,nxtot,nytot 
       integer :: hnx, hny, i, j

! ------ size of the received array
       hnx=size(T,1)-2
       hny=size(T,2)-2
        
! ------ interior of the array
       do j=0,hny+1
          do i=0,hnx+1
          T(i,j)=dble(ixstart+i-1)
          end do
       end do
       end subroutine init_iIndex
! ==============================================
! =============== SUBROUTINE init_jIndex
       subroutine init_jIndex(T,bx,by,bxtot,bytot,ixstart,jystart,nxtot,nytot)
       implicit none
       double precision, intent( out ) :: T(0:,0:)
       integer, intent(in) :: bx,by,bxtot,bytot
       integer, intent( in ) :: ixstart,jystart,nxtot,nytot 
       integer :: hnx, hny, i, j

! ------ size of the received array
       hnx=size(T,1)-2
       hny=size(T,2)-2
        
! ------ interior of the array
       do j=0,hny+1
          do i=0,hnx+1
          T(i,j)=dble(jystart+j-1)
          end do
       end do
       end subroutine init_jIndex
! ==============================================
! =============== SUBROUTINE init_iIndexPow
       subroutine init_iIndexPow(T,bx,by,bxtot,bytot,ixstart,jystart,nxtot,nytot)
       implicit none
       double precision, intent( out ) :: T(0:,0:)
       integer, intent(in) :: bx,by,bxtot,bytot
       integer, intent( in ) :: ixstart,jystart,nxtot,nytot 
       integer :: hnx, hny, i, j

! ------ size of the received array
       hnx=size(T,1)-2
       hny=size(T,2)-2
        
! ------ interior of the array
       do j=0,hny+1
          do i=0,hnx+1
          T(i,j)=(dble(ixstart+i-1))**2
          end do
       end do
       end subroutine init_iIndexPow
! ==============================================
! =============== SUBROUTINE init_jIndexPow
       subroutine init_jIndexPow(T,bx,by,bxtot,bytot,ixstart,jystart,nxtot,nytot)
       implicit none
       double precision, intent( out ) :: T(0:,0:)
       integer, intent(in) :: bx,by,bxtot,bytot
       integer, intent( in ) :: ixstart,jystart,nxtot,nytot 
       integer :: hnx, hny, i, j

! ------ size of the received array
       hnx=size(T,1)-2
       hny=size(T,2)-2
        
! ------ interior of the array
       do j=0,hny+1
          do i=0,hnx+1
          T(i,j)=(dble(jystart+j-1))**2
          end do
       end do
       end subroutine init_jIndexPow
! ==============================================
! =============== SUBROUTINE init_0
       subroutine init_0(T,bx,by,bxtot,bytot,ixstart,jystart,nxtot,nytot)
       implicit none
       double precision, intent( out ) :: T(0:,0:)
       integer, intent(in) :: bx,by,bxtot,bytot
       integer, intent( in ) :: ixstart,jystart,nxtot,nytot 
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
       end subroutine init_0
! ==============================================
      end program laplace
!+++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++


