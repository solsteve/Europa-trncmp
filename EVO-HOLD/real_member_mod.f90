module real_member_mod
  use trncmp_env
  implicit none

  type :: RealMember
     real(dp), allocatable :: param(:)
 
