      module aegFunctions_omp
      use globals
      implicit none

      public :: aegGetAverage_omp

      contains

        subroutine aegGetAverage_omp(U, U_new)
          use globals
          use omp_lib
          implicit none
          double precision, intent(in) :: U(:,:)
          double precision, intent(out) :: U_new(:,:)

          integer :: i,j

!$omp     target map(to:U) map(from:U_new)
!$aeg-!omp     target map(to:U(:GRIDX+2,:GRIDY+2)) map(from:U_new(:GRIDX+2,:GRIDY+2))
!$omp      teams distribute parallel do simd collapse(2)
!$aeg-omp     teams distribute parallel do simd collapse(2) num_teams(1048576) num_threads(256)
!          main computational kernel, average over neighbours in the grid
          do j=2,GRIDY+1
             do i=2,GRIDX+1
                U_new(i,j)=0.25*(U(i+1,j)+U(i-1,j)+U(i,j+1)+U(i,j-1))
             end do
          end do
!$omp     end teams distribute parallel do simd
!$omp     end target

        end subroutine aegGetAverage_omp
      end module
