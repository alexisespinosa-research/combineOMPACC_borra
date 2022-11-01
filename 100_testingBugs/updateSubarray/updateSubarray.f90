program updateSubarray

   implicit none
   
   integer, parameter :: GRIDX=3,GRIDY=6
   double precision,target :: T1(GRIDX,GRIDY), T2(GRIDX,GRIDY)
   double precision,pointer :: pt1_1(:,:), pt1_n(:,:), pt2_1(:,:), pt2_n(:,:)
   integer :: i,j

   !-------- Initialise T1 and T2 differently
   do j=1,GRIDY
      do i=1, GRIDX
         T1(i,j)=dble(i)+dble(j)
         T2(i,j)=dble(i)*dble(j)
      end do
   end do

   !-------- Printing intial valaues
   write (*,*) "Printing T1 initial:"
   call printArray(T1)
   write (*,*) "Printing T2 initial:"
   call printArray(T2)

   !-------- Copying arrays to the GPU
   write (*,*) "Copying arrays to the GPU"
   !$omp target enter data map(to:T1,T2)

   !-------- Changing values in the host
   write (*,*) "Changing values in the HOST but not in the GPU"
   do j=1,GRIDY
      do i=1, GRIDX
         T1(i,j)=10.0
         T2(i,j)=5.0
      end do
   end do

   !-------- Printing  current values
   write (*,*) "Printing current host T1:"
   call printArray(T1)
   write (*,*) "Printing current host T2:"
   call printArray(T2)

   !-------- Pointing the pointers to the first and last column
   pt1_1 => T1(1:GRIDX,1:1)
   pt2_1 => T2(1:GRIDX,1:1)
   pt1_n => T1(1:GRIDX,GRIDY:GRIDY)
   pt2_n => T2(1:GRIDX,GRIDY:GRIDY)

   !-------- Copying arrays to the HOST
   write (*,*) "Copying arrays to the HOST"
   !$omp target update from(T1,T2)

   !-------- Printing  current values
   write (*,*) "Printing current host T1:"
   call printArray(T1)
   write (*,*) "Printing current host T2:"
   call printArray(T2)

   !-------- Changing values in the host
   write (*,*) "Changing values in the HOST but not in the GPU"
   do j=1,GRIDY
      do i=1, GRIDX
         T1(i,j)=10.0
         T2(i,j)=5.0
      end do
   end do

   !-------- Printing  current values
   write (*,*) "Printing current host T1:"
   call printArray(T1)
   write (*,*) "Printing current host T2:"
   call printArray(T2)

   !-------- Copying first and last column to the HOST
   write (*,*) "Copying first and last column to the HOST"
   !$omp target update from(T1(1:GRIDX,1:1))
   !$omp target update from(T2(1:GRIDX,1:1))
   !$aeg-omp target update from(T1(1:GRIDX,GRIDY:1))
   !$aeg-omp target update from(T2(1:GRIDX,GRIDY:1))
   !$omp target update from(T1(1:GRIDX,GRIDY:GRIDY))
   !$omp target update from(T2(1:GRIDX,GRIDY:GRIDY))

   !-------- Printing  current values
   write (*,*) "Printing current host T1:"
   call printArray(T1)
   write (*,*) "Printing current host T2:"
   call printArray(T2)

   !-------- Changing values in the host
   write (*,*) "Changing values in the HOST but not in the GPU"
   do j=1,GRIDY
      do i=1, GRIDX
         T1(i,j)=10.0
         T2(i,j)=5.0
      end do
   end do

   !-------- Printing  current values
   write (*,*) "Printing current host T1:"
   call printArray(T1)
   write (*,*) "Printing current host T2:"
   call printArray(T2)

   !-------- Copying half and half to the HOST
   write (*,*) "Copying half and half to the HOST"
   !$omp target update from(T1(1:GRIDX,1:GRIDY/2))
   !$omp target update from(T2(1:GRIDX,1:GRIDY/2))
   !$aeg-omp target update from(T1(1:GRIDX,GRIDY/2+1:GRIDY/2))
   !$aeg-omp target update from(T2(1:GRIDX,GRIDY/2+1:GRIDY/2))
   !$omp target update from(T1(1:GRIDX,GRIDY/2+1:GRIDY))
   !$omp target update from(T2(1:GRIDX,GRIDY/2+1:GRIDY))

   !-------- Printing  current values
   write (*,*) "Printing current host T1:"
   call printArray(T1)
   write (*,*) "Printing current host T2:"
   call printArray(T2)

contains
subroutine printArray(A)
   double precision,intent(in) :: A(:,:)
   integer :: i,j
   integer :: sizeX, sizeY
   sizeX = size(A,1)
   sizeY = size(A,2)
   do i=1, sizeX
      do j=1, sizeY
          write (*, fmt="(f15.10)",advance="no") A(i,j) 
      end do
      write (*,*) ""
   end do
end subroutine printArray
end program updateSubarray
