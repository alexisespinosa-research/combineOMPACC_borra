      module functions_omp
      use globals
      implicit none

      interface
! ----------------------------------
        module subroutine getAverage_omp(U,U_new)
          implicit none
          double precision, intent(in) :: U(:,:)
          double precision, intent(out) :: U_new(:,:)
        end subroutine getAverage_omp
! ----------------------------------

! ----------------------------------
        module function updateT_omp(U,U_new,dt_old) result(dt)
          implicit none
          double precision, intent(out) :: U(:,:)
          double precision, intent(in) :: U_new(:,:)
          double precision, intent(in) :: dt_old
          double precision :: dt 
         end function updateT_omp
! ----------------------------------

! ----------------------------------
        module subroutine loadGPU_omp(U,U_new)
          implicit none
          double precision, intent(inout) :: U(:,:)
          double precision, intent(inout) :: U_new(:,:)
        end subroutine loadGPU_omp
! ----------------------------------

! ----------------------------------
        module subroutine copy2HOST_omp(U)
          implicit none
          double precision, intent(inout) :: U(:,:)
        end subroutine copy2HOST_omp
! ----------------------------------

      end interface
      end module functions_omp
