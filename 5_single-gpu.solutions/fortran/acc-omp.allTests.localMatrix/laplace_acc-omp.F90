       program laplace
       use globals
       use functions_cpu
#      ifndef _ALL_INTERNAL_
#         ifndef _JUSTOMP_
             use functions_acc
#         endif
#         ifndef _JUSTACC_
             use functions_omp
#         endif
#      endif
#      if defined (_NVTX_TRUE_) && defined (_NVHPC_)
          use nvtx
#      endif
       implicit none
       double precision, parameter :: MAX_TEMP_ERROR=0.02
       integer, parameter :: CORNEROFFSET=10
       double precision, allocatable :: T(:,:)
       double precision, allocatable :: T_new(:,:)
       integer i,j
       integer max_iterations
       integer :: iteration=1 
       double precision :: dt=5
       character(len=32) :: arg
       integer :: start_iterations,stop_iterations,stop_one,clock_rate
       integer :: start_host2device,stop_host2device
       integer :: start_device2host,stop_device2host
       double precision :: elapsed_time

       if (command_argument_count().ne.1) then
         call getarg(0, arg)
         print *, 'Usage ',trim(arg),' number_of_iterations'
       else 
         call getarg(1,arg)
         read(arg,*)  max_iterations
       end if 


!---- Allocating the arrays
       allocate(T(GRIDX+2,GRIDY+2))
       allocate(T_new(GRIDX+2,GRIDY+2))

!---------- Initialising arrays in the host
       !call init_linear128(T)
       call init_fixedIndexVal(T)
       !call init_jIndex(T)
       !call init_jIndexPow(T)
       !print *,'Tini=',T

!---------- Preloading arrays into the GPU. Default is to use OpenACC,
!           both as function and internal
!           Using "enter data" both in functions and internal pragmas
       call system_clock(count_rate=clock_rate)
       call system_clock(count=start_host2device)
#      if defined (_NVTX_TRUE_) && defined (_NVHPC_)
          call nvtxStartRange("Data into GPU")
#      endif
#      ifndef _NOPRELOAD_
#         if defined (_PRELOAD_UNSTRUCTURED_) && defined (_ALL_INTERNAL_)
#            if defined(_JUSTOMP_) || defined(_PRELOADOMP_)
!$omp           target enter data map(to:T) map(alloc:T_new)
!$aeg-omp           target enter data map(to:T(:GRIDX+2,:GRIDY+2)) map(alloc:T_new(:GRIDX+2,:GRIDY+2))
#            else
!$acc           enter data copyin(T) create(T_new)
!$aeg-acc           enter data copyin(T(:GRIDX+2,:GRIDY+2)) create(T_new(:GRIDX+2,:GRIDY+2))
#            endif
#         elif defined (_PRELOAD_STRUCTURED_) && defined (_ALL_INTERNAL_)
#            if defined(_JUSTOMP_) || defined(_PRELOADOMP_)
!$omp           target data map(to:T) map(alloc:T_new)
!$aeg-omp           target data map(to:T(:GRIDX+2,:GRIDY+2)) map(alloc:T_new(:GRIDX+2,:GRIDY+2))
#            else
!$acc           data copyin(T) create(T_new)
!$aeg-acc           data copyin(T(:GRIDX+2,:GRIDY+2)) create(T_new(:GRIDX+2,:GRIDY+2))
#            endif
#         else
#            if defined(_JUSTOMP_) || defined(_PRELOADOMP_)
                call loadGPU_omp(T,T_new) 
#            else
                call loadGPU_acc(T,T_new)
#            endif
#         endif
#      endif
#      if defined (_NVTX_TRUE_) && defined (_NVHPC_)
          call nvtxEndRange
#      endif
       call system_clock(count=stop_host2device)

!--------- Simulation while loop
       call system_clock(count=start_iterations)
       do while ((dt.gt.MAX_TEMP_ERROR).and. &
                 (iteration.le.max_iterations))

!         --- reset dt
          dt=0.0 

!         --- Average loop: default is OpenACC function
#         if defined (_NVTX_TRUE_) && defined (_NVHPC_)
             call nvtxStartRange("Average loop")
#         endif
#         if defined (_AVERAGE_INTERNAL_) || defined (_ALL_INTERNAL_)
#            ifndef _JUSTOMP_ 
!$acc           parallel loop copy(T) copyout(T_new) collapse(2)
!$aeg-acc           parallel loop copy(T(:GRIDX+2,:GRIDY+2)) copyout(T_new(:GRIDX+2,:GRIDY+2)) collapse(2)
#            else 
!$omp           target teams map(tofrom:T) map(from:T_new)
!$aeg-omp           target teams map(tofrom:T(:GRIDX+2,:GRIDY+2)) map(from:T_new(:GRIDX+2,:GRIDY+2))
!$omp           distribute parallel do collapse(2)
#            endif
             do j=2,GRIDY+1
                do i=2,GRIDX+1
                   T_new(i,j)=0.25*(T(i+1,j)+T(i-1,j)+T(i,j+1)+T(i,j-1))
                end do
             end do 
#            ifndef _JUSTOMP_ 
!$acc           end parallel loop
#            else
!$omp           end distribute parallel do
!$omp           end target teams
#            endif
#         else
#            ifndef _JUSTOMP_
                call getAverage_acc(T,T_new)
#            else
                call getAverage_omp(T,T_new)
#            endif
#         endif
#         if defined (_NVTX_TRUE_) && defined (_NVHPC_)
             call nvtxEndRange
#         endif

!         --- Update loop (and calculation of max diff): default is with OpenMP
#         if defined (_NVTX_TRUE_) && defined (_NVHPC_)
             call nvtxStartRange("Update loop")
#         endif
#         if defined (_UPDATE_INTERNAL_) || defined (_ALL_INTERNAL_)
#            ifndef _JUSTACC_ 
!$omp           target teams map(tofrom:T) map(to:T_new) map(tofrom:dt) reduction(max:dt)
!$aeg-omp           target teams map(tofrom:T(:GRIDX+2,:GRIDY+2)) map(to:T_new(:GRIDX+2,:GRIDY+2)) map(tofrom:dt) reduction(max:dt)
!$omp           distribute parallel do collapse(2) reduction(max:dt)
#            else 
!$acc           parallel loop copy(T) copyin(T_new) reduction(max:dt) collapse(2)
!$aeg-acc           parallel loop copy(T(:GRIDX+2,:GRIDY+2)) copyin(T_new(:GRIDX+2,:GRIDY+2)) reduction(max:dt) collapse(2)
#            endif
             do j=2,GRIDY+1
                do i=2,GRIDX+1
                   dt = max(abs(T_new(i,j)-T(i,j)),dt)
                   T(i,j)=T_new(i,j)
                end do
             end do
#            ifndef _JUSTACC_ 
!$omp           end distribute parallel do
!$omp           end target teams
#            else
!$acc           end parallel loop
#            endif
#         else
#            ifndef _JUSTACC_
                dt= updateT_omp(T,T_new,dt)
#            else
                dt= updateT_acc(T,T_new,dt)
#            endif
#         endif
#         if defined (_NVTX_TRUE_) && defined (_NVHPC_)
             call nvtxEndRange
#         endif

!         --- periodically print largest change
          if (mod(iteration,100).eq.0) then
             print "(a,i4.0,a,f18.10,a,f18.10)",'Iteration ',iteration,&
                   ', dt ',dt,', T[GXB-CO][GYB-CO]=',&
                   T(GRIDX+2-CORNEROFFSET,GRIDY+2-CORNEROFFSET)
          end if  
          if (iteration.eq.1) then 
             call system_clock(count=stop_one)
          end if
          iteration=iteration+1        
       end do
       call system_clock(count=stop_iterations)
       iteration=iteration-1
!----- Final copies of arrays from the GPU. Default is to use OpenMP,
!      both as function and internal
!      Using "enter data" both in functions and internal pragmas
!      OpenACC is used as default only for the STRUCTURED case where
!      "end data" closing pragmas need to conincide with the opening
!      ones before the while loop
       call system_clock(count=start_device2host)
#      if defined (_NVTX_TRUE_) && defined (_NVHPC_)
          call nvtxStartRange("Data into Host")
#      endif
#      ifndef _NOPRELOAD_
#         if defined (_PRELOAD_UNSTRUCTURED_) && defined (_ALL_INTERNAL_)
#            if defined(_JUSTACC_) || defined(_PRELOADACC_)
!$aeg-acc           exit data copyout(T(:GRIDX+2,:GRIDY+2))
!$acc           exit data copyout(T)
!$aeg-acc           exit data delete(T,T_new) !Cray complains about T
!$acc           exit data delete(T_new)
#            else
!$aeg-omp           target exit data map(from:T(:GRIDX+2,:GRIDY+2))
!$omp           target exit data map(from:T)
!$omp           target exit data map(delete:T,T_new)
#            endif
#         elif defined (_PRELOAD_STRUCTURED_) && defined (_ALL_INTERNAL_)
#            if defined(_JUSTOMP_) || defined(_PRELOADOMP_)
!$omp           end target data
#            else
!$acc           end data
#            endif
#         else
#            if defined(_JUSTACC_) || defined(_PRELOADACC_)
                call copy2HOST_acc(T,T_new)
#            else
                call copy2HOST_omp(T,T_new) 
#            endif
#         endif
#      endif
#      if defined (_NVTX_TRUE_) && defined (_NVHPC_)
          call nvtxEndRange
#      endif
       call system_clock(count=stop_device2host)

!---- Do we have T in the host ready to be saved?
      !print *,'Tfin=',T
      print "(a,i4.0,a,f18.10,a,f18.10)",'Final values, iteration ',&
            iteration,', dt ',dt,', T[GXB-CO][GYB-CO]=',&
            T(GRIDX+2-CORNEROFFSET,GRIDY+2-CORNEROFFSET)
           
 
       elapsed_time=dble(stop_host2device-start_host2device)/dble(clock_rate)
       print "(a,f12.6,a)",'Total time for initial host2device transfer was ',&
              elapsed_time,' seconds.'
       elapsed_time=dble(stop_one-start_iterations)/dble(clock_rate)
       print "(a,f12.6,a)",'Total time for first interation was ',&
              elapsed_time,' seconds.'
       elapsed_time=dble(stop_iterations-stop_one)/dble(clock_rate)
       print "(a,i9,a,i9,a,i9,a,f12.6,a)",'Total time for mesh GRID(X,Y)=(',&
              GRIDX,',',GRIDY,') rest ',iteration-1,' iterations was ',elapsed_time,' seconds.'
       print "(a,i9,a,i9,a,i9,a,f12.6,a)",'Average time for mesh GRID(X,Y)=(',&
               GRIDX,',',GRIDY,') rest ',iteration-1,' iterations was ',elapsed_time/real(iteration-1),' seconds.'
       elapsed_time=dble(stop_device2host-start_device2host)/dble(clock_rate)
       print "(a,f12.6,a)",'Total time for final device2host transfer was ',&
              elapsed_time,' seconds.'

       end program laplace
