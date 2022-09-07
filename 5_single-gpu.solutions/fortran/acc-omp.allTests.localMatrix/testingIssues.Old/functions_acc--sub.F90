      submodule (functions_acc) sub 
      use globals
      implicit none
!AEG@: T_new2 declaration below is for testing use of allocatable,save:
      double precision,allocatable,save :: T_new2(:,:)
      contains
! ----------------------------------
!AEG@: For testing:
         module procedure allocateMe_acc
          allocate (T_new2(GRIDX+2,GRIDY+2))
         end procedure allocateMe_acc
! ----------------------------------

! ----------------------------------
         module procedure getAverage_acc
          integer :: i,j
!$acc     parallel loop copyin(T(:GRIDX+2,:GRIDY+2)) &
!AEG@!$acc&     copyout(T_new(:GRIDX+2,:GRIDY+2)) &
!$acc&     copyout(T_new2(:GRIDX+2,:GRIDY+2)) &
!$acc&     collapse(2)
!         main computational kernel, average over neighbours in the grid
          do j=2,GRIDY+1
             do i=2,GRIDX+1
!AEG@                T_new(i,j)=0.25*(T(i+1,j)+T(i-1,j)+T(i,j+1)+T(i,j-1))
                T_new2(i,j)=0.25*(T(i+1,j)+T(i-1,j)+T(i,j+1)+T(i,j-1))
             end do
          end do 
!$acc     end parallel loop
         end procedure getAverage_acc
! ----------------------------------

! ----------------------------------
         module procedure updateT_acc
          integer :: i,j
!$acc     parallel loop copy(T(:GRIDX+2,:GRIDY+2)) &
!AEG@!$acc&     copyin(T_new(:GRIDX+2,:GRIDY+2)) &
!$acc&     copyin(T_new2(:GRIDX+2,:GRIDY+2)) &
!$acc&     reduction(max:dt) collapse(2)
!         compute the largest change and copy T_new to T 
          do j=2,GRIDY+1
             do i=2,GRIDX+1
!AEG@                dt = max(abs(T_new(i,j)-T(i,j)),dt)
!AEG@                T(i,j)=T_new(i,j)
                dt = max(abs(T_new2(i,j)-T(i,j)),dt)
                T(i,j)=T_new2(i,j)
             end do
          end do
!$acc     end parallel loop
         end procedure updateT_acc
! ----------------------------------

      end submodule sub
