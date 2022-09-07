      module functions_acc
      use globals
      implicit none

      interface
! ----------------------------------
        module subroutine allocateArrays_acc
         implicit none
        end subroutine allocateArrays_acc
! ----------------------------------

! ----------------------------------
        module subroutine deallocateArrays_acc
         implicit none
        end subroutine deallocateArrays_acc
! ----------------------------------

! ----------------------------------
        module subroutine getAverage_acc(T,T_new)
          implicit none
          double precision, intent(in) :: T(GRIDX+2,GRIDY+2)
          double precision, intent(out) :: T_new(GRIDX+2,GRIDY+2)
        end subroutine getAverage_acc
! ----------------------------------

! ----------------------------------
        module function updateT_acc(T,T_new,dt_old) result(dt)
          implicit none
          double precision, intent(out) :: T(GRIDX+2,GRIDY+2)
          double precision, intent(in) :: T_new(GRIDX+2,GRIDY+2)
          double precision, intent(in) :: dt_old
          double precision :: dt 
         end function updateT_acc
! ----------------------------------
      end interface
      end module functions_acc
