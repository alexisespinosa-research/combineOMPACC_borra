      module functions_omp
      use globals
      implicit none

      interface
! ----------------------------------
        module subroutine allocateArrays_omp
          implicit none
        end subroutine allocateArrays_omp
! ----------------------------------

! ----------------------------------
        module subroutine deallocateArrays_omp
          implicit none
        end subroutine deallocateArrays_omp
! ----------------------------------

! ----------------------------------
        module subroutine getAverage_omp(T,T_new)
          implicit none
          double precision, intent(in) :: T(GRIDX+2,GRIDY+2)
          double precision, intent(out) :: T_new(GRIDX+2,GRIDY+2)
        end subroutine getAverage_omp
! ----------------------------------

! ----------------------------------
        module function updateT_omp(T,T_new,dt_old) result(dt)
          implicit none
          double precision, intent(out) :: T(GRIDX+2,GRIDY+2)
          double precision, intent(in) :: T_new(GRIDX+2,GRIDY+2)
          double precision, intent(in) :: dt_old
          double precision :: dt 
         end function updateT_omp
! ----------------------------------
      end interface
      end module functions_omp
