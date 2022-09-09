      module functions_acc
      use globals
      implicit none

      interface
! ----------------------------------
        module subroutine getAverage_acc(U,U_new)
          implicit none
          double precision, intent(in) :: U(:,:)
          double precision, intent(out) :: U_new(:,:)
        end subroutine getAverage_acc
! ----------------------------------

! ----------------------------------
        module function updateT_acc(U,U_new,dt_old) result(dt)
          implicit none
          double precision, intent(out) :: U(:,:)
          double precision, intent(in) :: U_new(:,:)
          double precision, intent(in) :: dt_old
          double precision :: dt 
         end function updateT_acc
! ----------------------------------

! ----------------------------------
        module subroutine loadGPU_acc(U,U_new)
          implicit none
          double precision, intent(inout) :: U(:,:)
          double precision, intent(inout) :: U_new(:,:)
        end subroutine loadGPU_acc
! ----------------------------------

! ----------------------------------
        module subroutine copy2HOST_acc(U)
          implicit none
          double precision, intent(inout) :: U(:,:)
        end subroutine copy2HOST_acc
! ----------------------------------

      end interface
      end module functions_acc
