       module globals
       implicit none

       integer, parameter :: GRIDX=2048, GRIDY=2048
       double precision,allocatable,save :: T(:,:)
       double precision,allocatable,save :: T_new(:,:)

       end module globals
