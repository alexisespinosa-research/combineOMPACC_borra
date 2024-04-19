      submodule (functions_omp) sub 
      use globals
      use omp_lib
      implicit none
      contains
! ----------------------------------
         module procedure getAverage_omp
          integer :: i,j
!$omp     target map(to:U) map(from:U_new)
!$aeg-omp     target map(to:U(:GRIDX+2,:GRIDY+2)) map(from:U_new(:GRIDX+2,:GRIDY+2))
!$omp     teams distribute parallel do simd collapse(2)
!$aeg-omp     teams distribute parallel do simd collapse(2) num_teams(1048576) num_threads(256)
!         main computational kernel, average over neighbours in the grid
          do j=2,GRIDY+1
             do i=2,GRIDX+1
                U_new(i,j)=0.25*(U(i+1,j)+U(i-1,j)+U(i,j+1)+U(i,j-1))
             end do
          end do 
!$omp     end teams distribute parallel do simd
!$omp     end target

         end procedure getAverage_omp
! ----------------------------------

! ----------------------------------
         module procedure updateT_omp
            integer :: i,j
            dt=dt_old
!$omp       target map(tofrom:U) map(to:U_new) map(tofrom:dt)
!$aeg-omp      target map(tofrom:U(:GRIDX+2,:GRIDY+2)) map(to:U_new(:GRIDX+2,:GRIDY+2)) map(tofrom:dt)
!$omp       teams distribute parallel do simd reduction(max:dt) collapse(2)
!$aeg-omp       teams distribute parallel do simd reduction(max:dt) collapse(2) num_teams(880) num_threads(256)
!           compute the largest change and copy U_new to U 
            do j=2,GRIDY+1
               do i=2,GRIDX+1
                  dt = max(abs(U_new(i,j)-U(i,j)),dt)
                  U(i,j)=U_new(i,j)
               end do
            end do
!$omp       end teams distribute parallel do simd
!$omp       end target
         end procedure updateT_omp
! ----------------------------------

! ----------------------------------
         module procedure loadGPU_omp
!$omp       target enter data map(to:U) map(alloc:U_new)
         end procedure loadGPU_omp
! ----------------------------------

! ----------------------------------
         module procedure copy2HOST_omp
!$omp       target exit data map(from:U)
!$omp       target exit data map(delete:U,U_new)
         end procedure copy2HOST_omp
! ----------------------------------
      end submodule sub
