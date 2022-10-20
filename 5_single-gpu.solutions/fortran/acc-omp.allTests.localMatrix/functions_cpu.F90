      module functions_cpu
      use globals
      implicit none

      interface
! ----------------------------------
         module subroutine init_linear128(U)
         double precision, intent(inout) :: U(:,:)
         end subroutine init_linear128
! ----------------------------------
! ----------------------------------
         module subroutine init_fixedIndexVal(U)
         double precision, intent(inout) :: U(:,:)
         end subroutine init_fixedIndexVal
! ----------------------------------
      end interface
      end module functions_cpu
