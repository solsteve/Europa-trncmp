module plot_tools_mod
  use trncmp_env


contains


  subroutine LineFit( x, y, name, title, xlabel, ylabel )
    real(dp), intent(in) :: x(:)
    real(dp), intent(in) :: y(:)
    character(*), optional, intent(in) :: name
    character(*), optional, intent(in) :: title
    character(*), optional, intent(in) :: xlabel
    character(*), optional, intent(in) :: ylabel
  end subroutine LineFit

end module plot_tools_mod
