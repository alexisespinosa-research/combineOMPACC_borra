      module functions_omp
      use globals
      implicit none

      interface
! ----------------------------------
!AEG@: For testing:
        module subroutine allocateMe_omp
          implicit none
        end subroutine allocateMe_omp
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
