program MainProgram
    implicit none

    ! Declare variables
    real(8) :: myVariable=10.0d0

    ! Call the subroutine
    call sub1(myVariable)
    print *, myVariable
    call sub2(myVariable)
    print *, myVariable
    call sub3(myVariable)
    print *, myVariable


end program MainProgram
