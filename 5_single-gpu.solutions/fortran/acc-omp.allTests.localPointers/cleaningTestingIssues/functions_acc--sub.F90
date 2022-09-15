      submodule (functions_acc) sub 
      use globals
      implicit none
      contains
! ----------------------------------
         module procedure allocateArrays_acc
          allocate (T(GRIDX+2,GRIDY+2))
          allocate (T_new(GRIDX+2,GRIDY+2))
         end procedure allocateArrays_acc
! ----------------------------------

! ----------------------------------
         module procedure deallocateArrays_acc
          deallocate (T)
          deallocate (T_new)
         end procedure deallocateArrays_acc
! ----------------------------------

! ----------------------------------
         module procedure getAverage_acc
          integer :: i,j
!$acc     parallel loop copyin(T(:GRIDX+2,:GRIDY+2)) &
!$acc&     copyout(T_new(:GRIDX+2,:GRIDY+2)) &
!$acc&     collapse(2)
!         main computational kernel, average over neighbours in the grid
          do j=2,GRIDY+1
             do i=2,GRIDX+1
                T_new(i,j)=0.25*(T(i+1,j)+T(i-1,j)+T(i,j+1)+T(i,j-1))
             end do
          end do 
!$acc     end parallel loop
         end procedure getAverage_acc
! ----------------------------------

! ----------------------------------
         module procedure updateT_acc
          integer :: i,j
!$acc     parallel loop copy(T(:GRIDX+2,:GRIDY+2)) &
!$acc&     copyin(T_new(:GRIDX+2,:GRIDY+2)) &
!$acc&     reduction(max:dt) collapse(2)
!         compute the largest change and copy T_new to T 
          do j=2,GRIDY+1
             do i=2,GRIDX+1
                dt = max(abs(T_new(i,j)-T(i,j)),dt)
                T(i,j)=T_new(i,j)
             end do
          end do
!$acc     end parallel loop
         end procedure updateT_acc
! ----------------------------------

      end submodule sub
