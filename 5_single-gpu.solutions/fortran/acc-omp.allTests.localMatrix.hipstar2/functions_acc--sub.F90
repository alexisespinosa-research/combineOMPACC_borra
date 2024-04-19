      submodule (functions_acc) sub 
      use globals
      implicit none
      contains

! ----------------------------------
         module procedure getAverage_acc
          integer :: i,j
!$acc     parallel loop copyin(U) copyout(U_new) collapse(2)
!         main computational kernel, average over neighbours in the grid
          do j=2,GRIDY+1
             do i=2,GRIDX+1
                U_new(i,j)=0.25*(U(i+1,j)+U(i-1,j)+U(i,j+1)+U(i,j-1))
             end do
          end do 
!$acc     end parallel loop
         end procedure getAverage_acc
! ----------------------------------

! ----------------------------------
         module procedure updateT_acc
          integer :: i,j
          dt=dt_old
!$acc     parallel loop copy(U) copyin(U_new) reduction(max:dt) collapse(2)
!         compute the largest change and copy T_new to T 
          do j=2,GRIDY+1
             do i=2,GRIDX+1
                dt = max(abs(U_new(i,j)-U(i,j)),dt)
                U(i,j)=U_new(i,j)
             end do
          end do
!$acc     end parallel loop
         end procedure updateT_acc
! ----------------------------------

! ----------------------------------
         module procedure loadGPU_acc
!$acc       enter data copyin(U) create(U_new)
         end procedure loadGPU_acc
! ----------------------------------

! ----------------------------------
         module procedure copy2HOST_acc
!$acc       exit data copyout(U)
!$aeg-acc       exit data delete(U,U_new) !Cray complains about U
!$acc       exit data delete(U_new)
         end procedure copy2HOST_acc
! ----------------------------------
      end submodule sub
