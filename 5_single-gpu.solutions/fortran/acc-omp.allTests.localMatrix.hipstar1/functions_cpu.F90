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
! ----------------------------------
         module subroutine init_iIndex(U)
         double precision, intent(inout) :: U(:,:)
         end subroutine init_iIndex
! ----------------------------------
! ----------------------------------
         module subroutine init_jIndex(U)
         double precision, intent(inout) :: U(:,:)
         end subroutine init_jIndex
! ----------------------------------
! ----------------------------------
         module subroutine init_iIndexPow(U)
         double precision, intent(inout) :: U(:,:)
         end subroutine init_iIndexPow
! ----------------------------------
! ----------------------------------
         module subroutine init_jIndexPow(U)
         double precision, intent(inout) :: U(:,:)
         end subroutine init_jIndexPow
! ----------------------------------
      end interface
      end module functions_cpu
