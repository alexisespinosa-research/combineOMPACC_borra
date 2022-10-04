      module functions_cpu
      use globals
      implicit none

      interface
! ----------------------------------
         module subroutine init(U)
         double precision, intent(inout) :: U(:,:)
         end subroutine init
! ----------------------------------
      end interface
      end module functions_cpu
