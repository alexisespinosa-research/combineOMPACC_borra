program TestPreProcessor
implicit none
#if TEST
write(*,*)"Compiled with Test Flag"
#else
write(*,*)"Not Compiled with Test Flag"
#endif
end program TestPreProcessor
