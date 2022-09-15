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
       implicit none
       double precision, parameter :: MAX_TEMP_ERROR=0.02
       integer, parameter :: PLACEDATA=3
!       double precision, allocatable :: T(:,:)
!       double precision, allocatable :: T_new(:,:)
       double precision :: T(GRIDX+2,GRIDY+2)
       double precision :: T_new(GRIDX+2,GRIDY+2)
       integer i,j
       integer max_iterations
       integer :: iteration=1 
       double precision :: dt=5
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
!       allocate(T(GRIDX+2,GRIDY+2))
!       allocate(T_new(GRIDX+2,GRIDY+2))

!---------- Initialising arrays in the host
       call init(T)

!---------- Preloading arrays into the GPU. Default is to use OpenACC,
!           both as function and internal
!           Using "enter data" both in functions and internal pragmas
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

!--------- Simulation while loop
       do while ((dt.gt.MAX_TEMP_ERROR).and. &
                 (iteration.le.max_iterations))

!         --- reset dt
          dt=0.0 

!         --- Average loop: default is OpenACC function
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

!         --- Update loop (and calculation of max diff): default is with OpenMP
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

!         --- periodically print largest change
          if (mod(iteration,100).eq.0) then
             print "(a,i4.0,a,f15.10,a,f15.10)",'Iteration ',iteration,&
                   ', dt ',dt,', T[Fac*GX][Fac*GY]=',&
                   T(GRIDX-PLACEDATA,GRIDY-PLACEDATA)
          end if  
           
          iteration=iteration+1        
       end do
!----- Final copies of arrays from the GPU. Default is to use OpenMP,
!      both as function and internal
!      Using "enter data" both in functions and internal pragmas
!      OpenACC is used as default only for the STRUCTURED case where
!      "end data" closing pragmas need to conincide with the opening
!      ones before the while loop
#      ifndef _NOPRELOAD_
#         if defined (_PRELOAD_UNSTRUCTURED_) && defined (_ALL_INTERNAL_)
#            if defined(_JUSTACC_) || defined(_PRELOADACC_)
!$aeg-acc           exit data copyout(T(:GRIDX+2,:GRIDY+2))
!$acc           exit data copyout(T)
!$acc           exit data delete(T,T_new)
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

!---- Do we have T in the host ready to be saved?
      print "(a,i4.0,a,f15.10,a,f15.10)",'Final values, iteration ',&
            iteration,', dt ',dt,', T[Fac*GX][Fac*GY]=',&
            T(GRIDX-PLACEDATA,GRIDY-PLACEDATA)
           
 
       call system_clock(count=stop_time)
       elapsed_time=real(stop_time-start_time)/real(clock_rate)
       print "(a,f10.6,a)",'Total time was ',elapsed_time,' seconds.'

       end program laplace
