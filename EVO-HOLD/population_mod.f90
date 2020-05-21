module population_mod
  use trncmp_env
  implicit none

  type RealPopulation
     real(dp) :: param(:,:,:)  !! parameters(nvar,npop,2)
     real(dp) :: metric(:)     !! metrics(npop)
     integer  :: parent(:)     !! parend_index(npop)

   contains

  end type RealPopulation
  
