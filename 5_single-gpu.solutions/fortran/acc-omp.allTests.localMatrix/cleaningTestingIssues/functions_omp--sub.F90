      submodule (functions_omp) sub 
      use globals
      implicit none
      contains
! ----------------------------------
         module procedure allocateArrays_omp
          allocate (T(GRIDX+2,GRIDY+2))
          allocate (T_new(GRIDX+2,GRIDY+2))
         end procedure allocateArrays_omp
! ----------------------------------

! ----------------------------------
         module procedure deallocateArrays_omp
          deallocate (T)
          deallocate (T_new)
         end procedure deallocateArrays_omp
! ----------------------------------

! ----------------------------------
         module procedure getAverage_omp
          integer :: i,j
!$omp     target teams map(to:T(:GRIDX+2,:GRIDY+2)) map(from:T_new(:GRIDX+2,:GRIDY+2))
!$omp     distribute parallel do collapse(2)
!         main computational kernel, average over neighbours in the grid
          do j=2,GRIDY+1
             do i=2,GRIDX+1
                T_new(i,j)=0.25*(T(i+1,j)+T(i-1,j)+T(i,j+1)+T(i,j-1))
             end do
          end do 
!$omp     end distribute parallel do
!$omp     end target teams

         end procedure getAverage_omp
! ----------------------------------

! ----------------------------------
         module procedure updateT_omp
          integer :: i,j
!$omp     target teams map(tofrom:T(:GRIDX+2,:GRIDY+2)) map(to:T_new(:GRIDX+2,:GRIDY+2)) map(dt) reduction(max:dt)
!$omp     distribute parallel do reduction(max:dt) collapse(2)
!         compute the largest change and copy T_new to T 
          do j=2,GRIDY+1
             do i=2,GRIDX+1
                dt = max(abs(T_new(i,j)-T(i,j)),dt)
                T(i,j)=T_new(i,j)
             end do
          end do
!$omp     end distribute parallel do
!$omp     end target teams
         end procedure updateT_omp
! ----------------------------------

      end submodule sub
