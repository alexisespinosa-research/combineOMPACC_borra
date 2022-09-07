       program laplace
       use globals
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
       double precision :: T(GRIDX+2,GRIDY+2)
       double precision :: T_new(GRIDX+2,GRIDY+2)
       integer i,j
       integer max_iterations
       integer :: iteration=1 
       double precision :: dt=100
       character(len=32) :: arg
       integer start_time,stop_time,clock_rate
       real elapsed_time
#      ifndef _ALL_INTERNAL_
#         ifndef _JUSTOMP_
             call allocateMe_acc
#         endif
#         ifndef _JUSTACC_
             call allocateMe_omp
!             allocate (T_new3(GRIDX+2,GRIDY+2))
#         endif
#      endif
       if (command_argument_count().ne.1) then
         call getarg(0, arg)
         print *, 'Usage ',trim(arg),' number_of_iterations'
       else 
         call getarg(1,arg)
         read(arg,*)  max_iterations
       end if 

       call system_clock(count_rate=clock_rate)
       call system_clock(count=start_time)

       call init(T)

#      ifndef _NOPRELOAD_
#         if defined(_JUSTOMP_) || defined(_PRELOADOMP_)
!AEG@!$omp        target data map(tofrom:T(:GRIDX+2,:GRIDY+2)) map(alloc:T_new(:GRIDX+2,:GRIDY+2))
#         else
!--!$acc        data copy(T) create(T_new)
!AEG@!$acc        data copy(T(:GRIDX+2,:GRIDY+2)) create(T_new(:GRIDX+2,:GRIDY+2))
#         endif
#      endif
!      simulation iterations
       do while ((dt.gt.MAX_TEMP_ERROR).and. &
                (iteration.le.max_iterations))

!         reset dt
          dt=0.0 

#         if defined (_AVERAGE_INTERNAL_) || defined (_ALL_INTERNAL_)
#            ifndef _JUSTOMP_ 
!$acc           parallel loop copyin(T(:GRIDX+2,:GRIDY+2)) copyout(T_new(:GRIDX+2,:GRIDY+2)) collapse(2)
#            else 
!$omp           target teams map(tofrom:T(:GRIDX+2,:GRIDY+2)) map(from:T_new(:GRIDX+2,:GRIDY+2))
!$omp           distribute parallel do collapse(2)
#            endif
!            main computational kernel, average over neighbours in the grid
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

#         if defined (_UPDATE_INTERNAL_) || defined (_ALL_INTERNAL_)
#            ifndef _JUSTACC_ 
!$omp           target teams map(tofrom:T(:GRIDX+2,:GRIDY+2)) map(to:T_new(:GRIDX+2,:GRIDY+2)) map(tofrom:dt) reduction(max:dt)
!$omp           distribute parallel do collapse(2) reduction(max:dt)
#            else 
!$acc           parallel loop copy(T(:GRIDX+2,:GRIDY+2)) copyin(T_new(:GRIDX+2,:GRIDY+2)) reduction(max:dt) collapse(2)
#            endif
!            compute the largest change and copy T_new to T 
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

          !periodically print largest change
          if (mod(iteration,100).eq.0) then
             print "(a,i4.0,a,f8.6)",'Iteration ',iteration,', dt ',dt
          end if  
           
          iteration=iteration+1        
       end do
#      ifndef _NOPRELOAD_
#         if defined(_JUSTOMP_) || defined(_PRELOADOMP_)
!AEG@!$omp        end target data
#         else
!AEG@!$acc        end data
#         endif
#      endif

       call system_clock(count=stop_time)
       elapsed_time=real(stop_time-start_time)/real(clock_rate)
       print "(a,f10.6,a)",'Total time was ',elapsed_time,' seconds.'

       end program laplace

       subroutine init(T)
       use globals
       implicit none
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
