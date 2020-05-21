!/ ====================================================================== BEGIN FILE =====
!/ **                               P S G R A P H _ M O D                               **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  This file is part of the TRNCMP Research Library, `Europa' (Fortran 2018)        **
!/ **                                                                                   **
!/ **  Copyright (c) 2012,2015,2019, Stephen W. Soliday                                 **
!/ **                                stephen.soliday@trncmp.org                         **
!/ **                                http://research.trncmp.org                         **
!/ **                                                                                   **
!/ **  -------------------------------------------------------------------------------  **
!/ **                                                                                   **
!/ **  Europa is free software: you can redistribute it and/or modify it under the      **
!/ **  terms of the GNU General Public License as published by the Free Software        **
!/ **  Foundation, either version 3 of the License, or (at your option)                 **
!/ **  any later version.                                                               **
!/ **                                                                                   **
!/ **  Europa is distributed in the hope that it will be useful, but WITHOUT ANY        **
!/ **  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR    **
!/ **  A PARTICULAR PURPOSE. See the GNU General Public License for more details.       **
!/ **                                                                                   **
!/ **  You should have received a copy of the GNU General Public License along with     **
!/ **  Europa. If not, see <http://www.gnu.org/licenses/>.                              **
!/ **                                                                                   **
!/ =======================================================================================
module psgraph_mod
  !/ -------------------------------------------------------------------------------------
  !! Provides multi paged multi windowed PostScript vector graphics.
  !!
  !! author:  Stephen W. Soliday
  !! date:    2019-12-23
  !! license: GPL
  !! -------------------------------------------------------------------------------------
  use deque_object_class
  use tlogger
  use trncmp_env
  implicit none


  integer, parameter :: psfont_normal = 1
  integer, parameter :: psfont_bold   = 2
  integer, parameter :: psfont_italic = 3

  integer :: PSWindow_count = 0

  integer, parameter :: ERR_DIAMOND = 1
  integer, parameter :: ERR_PLUS    = 2
  integer, parameter :: ERR_VBEAM   = 3
  integer, parameter :: ERR_HBEAM   = 4


  !/ =====================================================================================
  type :: PSColor
     !/ ----------------------------------------------------------------------------------
     real :: r = 0.0
     real :: g = 0.0
     real :: b = 0.0
   contains
     procedure, private :: fix_pscolor
     procedure, private :: copy_pscolor
     procedure, private :: set_pscolor_R8
     procedure, private :: set_pscolor_R4

     generic :: fix  => fix_pscolor
     generic :: copy => copy_pscolor
     generic :: set  => set_pscolor_R8, set_pscolor_R4

  end type PSColor

  type( PSColor ), parameter :: color_white   = PSColor( 1.0, 1.0, 1.0 )
  type( PSColor ), parameter :: color_black   = PSColor( 0.0, 0.0, 0.0 )
  type( PSColor ), parameter :: color_red     = PSColor( 1.0, 0.0, 0.0 )
  type( PSColor ), parameter :: color_green   = PSColor( 0.0, 1.0, 0.0 )
  type( PSColor ), parameter :: color_blue    = PSColor( 0.0, 0.0, 1.0 )
  type( PSColor ), parameter :: color_cyan    = PSColor( 0.0, 1.0, 1.0 )
  type( PSColor ), parameter :: color_magenta = PSColor( 1.0, 0.0, 1.0 )
  type( PSColor ), parameter :: color_yellow  = PSColor( 1.0, 1.0, 0.0 )

  type( PSColor ), parameter :: color_gray1   = PSColor( 0.1, 0.1, 0.1 )
  type( PSColor ), parameter :: color_gray2   = PSColor( 0.2, 0.2, 0.2 )
  type( PSColor ), parameter :: color_gray3   = PSColor( 0.3, 0.3, 0.3 )
  type( PSColor ), parameter :: color_gray4   = PSColor( 0.4, 0.4, 0.4 )
  type( PSColor ), parameter :: color_gray5   = PSColor( 0.5, 0.5, 0.5 )
  type( PSColor ), parameter :: color_gray6   = PSColor( 0.6, 0.6, 0.6 )
  type( PSColor ), parameter :: color_gray7   = PSColor( 0.7, 0.7, 0.7 )
  type( PSColor ), parameter :: color_gray8   = PSColor( 0.8, 0.8, 0.8 )
  type( PSColor ), parameter :: color_gray9   = PSColor( 0.9, 0.9, 0.9 )


  !/ =====================================================================================
  type, abstract :: PSWindow
     !/ ----------------------------------------------------------------------------------
     !! This class defines a graphics window that will be physically placed on a page.
     !/ ----------------------------------------------------------------------------------
     real(dp) :: device_width  = 0.0d0  !! physical window width    ( inches )
     real(dp) :: device_height = 0.0d0  !! physical window height   ( inches )
     real(dp) :: device_x      = 0.0d0  !! physical window position ( inches )
     real(dp) :: device_y      = 0.0d0  !! physical window position ( inches )

   contains

     procedure(pswrite_abst_pswin), pass(dts), deferred :: pswrite

     procedure :: setDevice => set_device_pswin
     procedure :: write_header_pswin
     procedure :: write_trailer_pswin

  end type PSWindow


  !/ =====================================================================================
  abstract interface
     !/ ----------------------------------------------------------------------------------
     subroutine pswrite_abst_pswin( dts, unit )
       import :: PSWindow
       class(PSWindow), intent(inout) :: dts
       integer,         intent(in)    :: unit
     end subroutine pswrite_abst_pswin
  end interface


  !/ =====================================================================================
  type, extends( PSWindow ) :: PSDraw
     !/ ----------------------------------------------------------------------------------
     integer                 :: id            = 0
     character(len=MAX_PATH) :: temp_filename = ''
     integer                 :: file_unit     = 0

     real(dp) :: world_x1 = 0.0d0  !! windows lower left  device x coordinate
     real(dp) :: world_y1 = 0.0d0  !! windows lower left  device y coordinate
     real(dp) :: world_x2 = 1.0d0  !! windows upper right device x coordinate
     real(dp) :: world_y2 = 1.0d0  !! windows upper right device y coordinate

     real(dp) :: slope_x = 1.0d0
     real(dp) :: slope_y = 1.0d0
     real(dp) :: inter_x = 0.0d0
     real(dp) :: inter_y = 0.0d0

     real(dp) :: default_point_half_width  = 2.0d-3
     real(dp) :: default_point_half_height = 2.0d-3

     type( PSColor ) :: last
     type( PSColor ) :: saved

   contains

     procedure, pass(dts) :: pswrite => ps_write_psdraw

     procedure :: init_psdraw
     procedure :: drawPoint     => draw_point_psdraw
     procedure :: drawLine      => draw_line_psdraw
     procedure :: drawRay       => draw_ray_psdraw
     procedure :: drawEllipse   => draw_ellipse_psdraw
     procedure :: drawCircle    => draw_circle_psdraw
     procedure :: drawPolygon   => draw_polygon_psdraw
     procedure :: drawRectangle => draw_rect_psdraw
     procedure :: drawBorder    => draw_border_psdraw

     procedure :: drawCircleInch => draw_circle_inch_psdraw
     procedure :: fillCircleInch => fill_circle_inch_psdraw

     procedure :: write         => write_psdraw
     procedure :: write_inch    => write_inch_psdraw
     procedure :: saveColor     => save_color_psdraw
     procedure :: restoreColor  => restore_color_psdraw
     procedure :: setFont       => set_font_psdraw
     procedure :: scaleX        => scaleX_psdraw
     procedure :: scaleY        => scaleY_psdraw

     procedure :: set_rgb_color_psdraw
     procedure :: set_rgb_component_psdraw_R4
     procedure :: set_rgb_component_psdraw_R8

     generic :: setRGB => set_rgb_color_psdraw, set_rgb_component_psdraw_R4, &
          &               set_rgb_component_psdraw_R8

  end type PSDraw




  !/ =====================================================================================
  type :: PSPage
     !/ ----------------------------------------------------------------------------------
     type(deque)       :: psd_list
     character(len=32) :: name
     integer           :: page_number

   contains

     procedure, private :: write_header_pspage
     procedure, private :: write_trailer_pspage

     procedure :: init    => init_pspage
     procedure :: delete  => delete_pspage
     procedure :: pswrite => ps_write_pspage
     procedure :: setName => set_name_pspage
     procedure :: add     => add_pspage

  end type PSPage




  !/ =====================================================================================
  type :: PSGraph
     !/ ----------------------------------------------------------------------------------
     class( PSPage ), pointer :: page(:)     => null()
     integer                  :: num_page    =  0
     character(len=MAX_PATH)  :: psfile_name =  ''
   contains

     procedure :: delete  => delete_psgraph
     procedure :: setName => set_name_psgraph
     procedure :: add     => add_psgraph
     procedure :: pswrite => ps_write_psgraph

     procedure, private :: write_header_psgraph
     procedure, private :: write_trailer_psgraph

  end type PSGraph


  !/ -------------------------------------------------------------------------------------
  interface PSDraw
     !/ ----------------------------------------------------------------------------------
     procedure :: init_wh_xy_psdraw
     procedure :: init_wh_psdraw
  end interface PSDraw

  !/ -------------------------------------------------------------------------------------
  interface PSGraph
     !/ ----------------------------------------------------------------------------------
     procedure :: init_psgraph
  end interface PSGraph




  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine fix_pscolor( dts )
    !/ -----------------------------------------------------------------------------------
    !! Clip the color values so that the result is 0 <= r,g,b <= 255.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class( PSColor ), intent(inout) :: dts  !! reference to this PSColor.
    !/ -----------------------------------------------------------------------------------
    if ( dts%r.lt.0.00 ) dts%r =   0.0
    if ( dts%g.lt.0.00 ) dts%g =   0.0
    if ( dts%b.lt.0.00 ) dts%b =   0.0
    if ( dts%r.gt.2.55 ) dts%r = 255.0
    if ( dts%g.gt.2.55 ) dts%g = 255.0
    if ( dts%b.gt.2.55 ) dts%b = 255.0
  end subroutine fix_pscolor


  !/ =====================================================================================
  subroutine set_pscolor_R8( dts, tr, tg, tb )
    !/ -----------------------------------------------------------------------------------
    !! Set the color using RGB values.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class( PSColor ), intent(inout) :: dts !! reference to this PSColor.
    real(dp),         intent(in)    :: tr  !! red   value.
    real(dp),         intent(in)    :: tg  !! green value.
    real(dp),         intent(in)    :: tb  !! blue  value.
    !/ -----------------------------------------------------------------------------------
    dts%r = real(tr)
    dts%g = real(tg)
    dts%b = real(tb)
    call dts%fix
  end subroutine set_pscolor_R8


  !/ =====================================================================================
  subroutine set_pscolor_R4( dts, tr, tg, tb )
    !/ -----------------------------------------------------------------------------------
    !! Set the color using RGB values.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class( PSColor ), intent(inout) :: dts !! reference to this PSColor.
    real,             intent(in)    :: tr  !! red   value.
    real,             intent(in)    :: tg  !! green value.
    real,             intent(in)    :: tb  !! blue  value.
    !/ -----------------------------------------------------------------------------------
    dts%r = tr
    dts%g = tg
    dts%b = tb
    call dts%fix
  end subroutine set_pscolor_R4


  !/ =====================================================================================
  subroutine copy_pscolor( dts, src )
    !/ -----------------------------------------------------------------------------------
    !! Set the color using a source color.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class( PSColor ), intent(inout) :: dts  !! reference to this PSColor.
    class( PSColor ), intent(in)    :: src  !! reference to a source PSColor.
    !/ -----------------------------------------------------------------------------------
    dts%r = src%r
    dts%g = src%g
    dts%b = src%b
    call dts%fix
  end subroutine copy_pscolor








  !/ =====================================================================================
  subroutine set_device_pswin( dts, dev_x, dev_y )
    !/ -----------------------------------------------------------------------------------
    !! Set Position.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class( PSWindow ), intent(inout) :: dts    !! reference to this PSWindow.
    real(dp),          intent(in)    :: dev_x  !! window position in device coordinates (inches).
    real(dp),          intent(in)    :: dev_y  !! window position in device coordinates (inches).
    !/ -----------------------------------------------------------------------------------
    dts%device_x = dev_x
    dts%device_y = dev_y
  end subroutine set_device_pswin


  !/ =====================================================================================
  subroutine write_header_pswin( dts, unit )
    !/ -----------------------------------------------------------------------------------
    !! Write the postscript header for this window on the output stream.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class( PSWindow ), intent(inout) :: dts   !! reference to this PSWindow.
    integer,           intent(in)    :: unit  !! pointer to an output file.
    !/ -----------------------------------------------------------------------------------
    write(unit,'(A)') '%%----------------------------------'
    write(unit,'(A)') 'gsave'
    write(unit,*) dts%device_x, ' ', dts%device_y, ' translate'
  end subroutine write_header_pswin


  !/ =====================================================================================
  subroutine write_trailer_pswin( dts, unit )
    !/ -----------------------------------------------------------------------------------
    !! Write the postscript trailer for this window on the output stream.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class( PSWindow ), intent(inout) :: dts   !! reference to this PSWindow.
    integer,           intent(in)    :: unit  !! output file unit.
    !/ -----------------------------------------------------------------------------------
    write(unit,'(A)') 'grestore'
  end subroutine write_trailer_pswin








  !/ =====================================================================================
  function init_wh_xy_psdraw( dw, dh, x1, y1, x2, y2 ) result( psd )
    !/ -----------------------------------------------------------------------------------
    !! Constructor.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in)   :: dw  !! window width  in device coordinates (inches).
    real(dp), intent(in)   :: dh  !! window height in device coordinates (inches).
    real(dp), intent(in)   :: x1  !! windows lower left  device x coordinate.
    real(dp), intent(in)   :: y1  !! windows lower left  device y coordinate.
    real(dp), intent(in)   :: x2  !! windows upper right device x coordinate.
    real(dp), intent(in)   :: y2  !! windows upper right device x coordinate.
    class(PSDraw), pointer :: psd !! pointer to a PSDraw
    !/ -----------------------------------------------------------------------------------
    allocate(psd)
    call psd%init_psdraw( dw, dh, x1, y1, x2, y2 )
  end function init_wh_xy_psdraw


  !/ =====================================================================================
  function init_wh_psdraw( dw, dh ) result( psd )
    !/ -----------------------------------------------------------------------------------
    !! Constructor.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in)   :: dw  !! window width  in device coordinates (inches).
    real(dp), intent(in)   :: dh  !! window height in device coordinates (inches).
    class(PSDraw), pointer :: psd !! pointer to a PSDraw.
    !/ -----------------------------------------------------------------------------------
    allocate(psd)
    call psd%init_psdraw( dw, dh, D_ZERO, D_ZERO, dw, dh )
  end function init_wh_psdraw


  !/ =====================================================================================
  subroutine init_psdraw( dts, dw, dh, x1, y1, x2, y2 )
    !/ -----------------------------------------------------------------------------------
    !! Initialize the physical position of this object on a device.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSDraw), intent(inout) :: dts !! reference to this PSDraw.
    real(dp),      intent(in)    :: dw  !! window width  in device coordinates (inches).
    real(dp),      intent(in)    :: dh  !! window height in device coordinates (inches).
    real(dp),      intent(in)    :: x1  !! windows lower left  device x coordinate.
    real(dp),      intent(in)    :: y1  !! windows lower left  device y coordinate.
    real(dp),      intent(in)    :: x2  !! windows upper right device x coordinate.
    real(dp),      intent(in)    :: y2  !! windows upper right device x coordinate.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: dx, dy
    integer  :: ios
    !/ -----------------------------------------------------------------------------------
    PSWindow_count = PSWindow_count + 1
    dts%id         = PSWindow_count

    write( dts%temp_filename, 100 ) getpid(), dts%id
100 format( '/tmp/pswork_',I0,'_',I0,'.tmp' )

    open(NEWUNIT=dts%file_unit,FILE=dts%temp_filename,ACTION='WRITE',IOSTAT=ios)
    if ( 0.ne.ios ) then
       call log_error( 'Cannot write to file', STR=dts%temp_filename )
       call log_info( 'IOSTAT=', I4=ios )
       return
    end if

    call log_debug( 'Temporary file name', STR=dts%temp_filename )

    dts%device_width  = dw
    dts%device_height = dh

    dts%world_x1 = x1
    dts%world_x2 = x2
    dts%world_y1 = y1
    dts%world_y2 = y2

    dx = dts%world_x2 - dts%world_x1;
    dy = dts%world_y2 - dts%world_y1;

    dts%slope_x = dts%device_width  / dx
    dts%slope_y = dts%device_height / dy

    dts%inter_x = -dts%slope_x * dts%world_x1
    dts%inter_y = -dts%slope_y * dts%world_y1

    dts%default_point_half_width  = dts%default_point_half_width  *   &
         &                           ( dts%world_x2 - dts%world_x1 )
    dts%default_point_half_height = dts%default_point_half_height *   &
         &                           ( dts%world_y2 - dts%world_y1 )
  end subroutine init_psdraw


  !/ =====================================================================================
  subroutine draw_point_psdraw( dts, xc, yc, wdt, hgt, bar )
    !/ -----------------------------------------------------------------------------------
    !! Draw a error bar + centered at (x,y) with height and width defined as +/- (wdt,hgt)
    !/ -----------------------------------------------------------------------------------
    class(PSDraw), intent(inout) :: dts   !! reference to this PSDraw.
    real(dp),           intent(in) :: xc  !! center x world coordinate for the point.
    real(dp),           intent(in) :: yc  !! center y world coordinate for the point.
    real(dp), optional, intent(in) :: wdt !! +/- width (half the drawn width).
    real(dp), optional, intent(in) :: hgt !! +/- half  (half the drawn height).
    integer,  optional, intent(in) :: bar !! error bar type (default: ERR_DIAMOND)
    !/ -----------------------------------------------------------------------------------
    real(dp) :: x1, x2, y1, y2
    integer  :: typ
    !/ -----------------------------------------------------------------------------------
    if ( present(wdt) ) then
       x1 = xc - wdt
       x2 = xc+ wdt
    else
       x1 = xc - dts%default_point_half_width
       x2 = xc + dts%default_point_half_width
    end if

    if ( present(hgt) ) then
       y1 = yc - hgt
       y2 = yc + hgt
    else
       y1 = yc - dts%default_point_half_width
       y2 = yc + dts%default_point_half_width
    end if

    typ = ERR_DIAMOND
    if ( present(bar) ) then
       typ = bar
    end if

    if     ( typ.eq.ERR_PLUS  ) then
       call dts%drawLine( x1, yc, x2, yc )
       call dts%drawLine( xc, y1, xc, y2 )
    elseif ( typ.eq.ERR_VBEAM ) then
       call dts%drawLine( x1, y1, x2, y1 )
       call dts%drawLine( x1, y2, x2, y2 )
       call dts%drawLine( xc, y1, xc, y2 )
    elseif ( typ.eq.ERR_HBEAM ) then
       call dts%drawLine( x1, yc, x2, yc )
       call dts%drawLine( x1, y1, x1, y2 )
       call dts%drawLine( x2, y1, x2, y2 )
    else  ! default to ERR_DIAMOND
       call dts%drawLine( xc, y1, x2, yc )
       call dts%drawLine( x2, yc, xc, y2 )
       call dts%drawLine( xc, y2, x1, yc )
       call dts%drawLine( x1, yc, xc, y1 )
    end if
  end subroutine draw_point_psdraw


  !/ =====================================================================================
  subroutine draw_line_psdraw( dts, x1, y1, x2, y2 )
    !/ -----------------------------------------------------------------------------------
    !! Draw a line between two world coordinates.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSDraw), intent(inout) :: dts !! reference to this PSDraw.
    real(dp), intent(in) :: x1 !! starting x world coordinate for the line.
    real(dp), intent(in) :: y1 !! starting y world coordinate for the line.
    real(dp), intent(in) :: x2 !! ending   x world coordinate for the line.
    real(dp), intent(in) :: y2 !! ending   y world coordinate for the line.
    !/ -----------------------------------------------------------------------------------
    write( dts%file_unit, 100 ) dts%scaleX(x1), dts%scaleY(y1), &
         &                      dts%scaleX(x2), dts%scaleY(y2)
100 format( F0.5,' ',F0.5,' ',F0.5,' ',F0.5,'  DL' )
  end subroutine draw_line_psdraw


  !/ =====================================================================================
  subroutine draw_ray_psdraw( dts, xc, yc, m, theta )
    !/ -----------------------------------------------------------------------------------
    !! Draw a ray length m from (x,y) and rotated theta radians.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSDraw), intent(inout) :: dts   !! reference to this PSDraw.
    real(dp),      intent(in)    :: xc    !! x coordinate of the origin of the ray.
    real(dp),      intent(in)    :: yc    !! y coordinate of the origin of the ray.
    real(dp),      intent(in)    :: m     !! magnitude.
    real(dp),      intent(in)    :: theta !! angle in radians of the semi-marjor axis 
    !!                                       counter clockwise from the positive x axis.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: x, y
    x = xc + m*cos(theta)
    y = yc + m*sin(theta)
    call dts%drawLine( xc, yc, x, y )
  end subroutine draw_ray_psdraw


  !/ =====================================================================================
  subroutine draw_ellipse_psdraw( dts, xc, yc, a, b, theta, parts )
    !/ -----------------------------------------------------------------------------------
    !! Draw an ellipse centered at (x,y) and rotated theta radians.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSDraw),     intent(inout) :: dts   !! reference to this PSDraw.
    real(dp),          intent(in)    :: xc    !! x coordinate of the center of the ellipse.
    real(dp),          intent(in)    :: yc    !! y coordinate of the center of the ellipse.
    real(dp),          intent(in)    :: a     !! value of the semi-major axis.
    real(dp),          intent(in)    :: b     !! value of the semi-minor axis.
    real(dp),          intent(in)    :: theta !! angle in radians of the semi-marjor axis 
    !!                                           counter clockwise from the positive x axis.
    integer, optional, intent(in)    :: parts !! number of segments.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: T, dT, C, S, x0, y0, X, Y, x1, y1
    integer  :: i, n
    !/ -----------------------------------------------------------------------------------
    n=64
    if ( present( parts ) ) then
       n=parts
    end if
    T  = D_ZERO
    dT = D_2PI / real(n,dp)
    C  = cos(theta)
    S  = sin(theta)
    x0 = xc + C*a
    y0 = yc + S*a
    do i=1,n
       T  = T + dT
       X  = a*cos(T)
       Y  = b*sin(T)
       x1 = xc + C*X - S*Y
       y1 = yc + S*X + C*Y
       call dts%drawLine( x0, y0, x1, y1 )
       x0 = x1
       y0 = y1
    end do
  end subroutine draw_ellipse_psdraw


  !/ =====================================================================================
  subroutine draw_circle_psdraw( dts, xc, yc, r, parts )
    !/ -----------------------------------------------------------------------------------
    !! Draw a circle centered at (x,y).
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSDraw),     intent(inout) :: dts    !! reference to this PSDraw.
    real(dp),          intent(in)    :: xc     !! x coordinate of the center of the ellipse.
    real(dp),          intent(in)    :: yc     !! y coordinate of the center of the ellipse.
    real(dp),          intent(in)    :: r      !! value of the radius.
    integer, optional, intent(in)    :: parts  !! number of segments.
    !/ -----------------------------------------------------------------------------------
    call dts%drawEllipse( xc, yc, r, r, D_ZERO, parts )
  end subroutine draw_circle_psdraw


  !/ =====================================================================================
  subroutine draw_circle_inch_psdraw( dts, xc, yc, r )
    !/ -----------------------------------------------------------------------------------
    !! Draw a circle centered at (x,y).
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSDraw),     intent(inout) :: dts    !! reference to this PSDraw.
    real(dp),          intent(in)    :: xc     !! x coordinate of the center in inches.
    real(dp),          intent(in)    :: yc     !! y coordinate of the center in inches.
    real(dp),          intent(in)    :: r      !! value of the radius in inches.
    !/ -----------------------------------------------------------------------------------
    write(dts%file_unit,*) xc, yc, r, 'DCIR'
  end subroutine draw_circle_inch_psdraw


  !/ =====================================================================================
  subroutine fill_circle_inch_psdraw( dts, xc, yc, r )
    !/ -----------------------------------------------------------------------------------
    !! Fill a circle centered at (x,y).
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSDraw),     intent(inout) :: dts    !! reference to this PSDraw.
    real(dp),          intent(in)    :: xc     !! x coordinate of the center in inches.
    real(dp),          intent(in)    :: yc     !! y coordinate of the center in inches.
    real(dp),          intent(in)    :: r      !! value of the radius in inches.
    !/ -----------------------------------------------------------------------------------
     write(dts%file_unit,*) xc, yc, r, 'FCIR'   
  end subroutine fill_circle_inch_psdraw


  !/ =====================================================================================
  subroutine draw_polygon_psdraw( dts, x, y, n, fill )
    !/ -----------------------------------------------------------------------------------
    !!  Draw a polygon defined my a set of vertices and optionally fill.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSDraw),     intent(inout) :: dts  !! reference to this PSDraw.
    real(dp),          intent(in)    :: x(:) !! x array of x world coordinates.
    real(dp),          intent(in)    :: y(:) !! y array of y world coordinates.
    integer,           intent(in)    :: n    !! number of coordinates to use ( n <= x.length )
    logical, optional, intent(in)    :: fill !! fill the polygon if true.
    !/ -----------------------------------------------------------------------------------
    integer :: i
    logical :: f
    f=.false.
    if ( present(fill) ) then
       f = fill
    end if
    write(dts%file_unit,*) 'newpath'
    write(dts%file_unit,*) dts%scaleX(x(1)),' ',dts%scaleY(y(1)),' moveto'
    do i=2,n
       write(dts%file_unit,*) dts%scaleX(x(i)),' ',dts%scaleY(y(i)),' lineto'
    end do
    write(dts%file_unit,*) 'closepath'
    if (f) then
       write(dts%file_unit,*) 'fill'
    else
       write(dts%file_unit,*) 'stroke'
    end if
  end subroutine draw_polygon_psdraw


  !/ =====================================================================================
  subroutine draw_rect_psdraw( dts, x1, y1, x2, y2, fill )
    !/ -----------------------------------------------------------------------------------
    !! Draw a rectangle in his window and optionally fill.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSDraw),     intent(inout) :: dts  !! reference to this PSDraw.
    real(dp),          intent(in)    :: x1   !! lower left  in device x coordinate.
    real(dp),          intent(in)    :: y1   !! lower left  in device y coordinate.
    real(dp),          intent(in)    :: x2   !! upper right in device x coordinate.
    real(dp),          intent(in)    :: y2   !! upper right in device y coordinate.
    logical, optional, intent(in)    :: fill !! fill the polygon if true.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: x(4)
    real(dp) :: y(4)
    x(1) = x1
    x(2) = x2
    x(3) = x2
    x(4) = x1
    y(1) = y1
    y(2) = y1
    y(3) = y2
    y(4) = y2
    call dts%drawPolygon( x, y, 4, fill )
  end subroutine draw_rect_psdraw


  !/ =====================================================================================
  subroutine draw_border_psdraw( dts )
    !/ -----------------------------------------------------------------------------------
    !! Draw a rectangle around the border of this window.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSDraw), intent(inout) :: dts  !! reference to this PSDraw.
    !/ -----------------------------------------------------------------------------------
    call dts%drawLine( dts%world_x1, dts%world_y1,  dts%world_x1, dts%world_y2 )
    call dts%drawLine( dts%world_x1, dts%world_y2,  dts%world_x2, dts%world_y2 )
    call dts%drawLine( dts%world_x2, dts%world_y2,  dts%world_x2, dts%world_y1 )
    call dts%drawLine( dts%world_x2, dts%world_y1,  dts%world_x1, dts%world_y1 )
  end subroutine draw_border_psdraw


  !/ =====================================================================================
  subroutine write_psdraw( dts, text, x1, y1, x2, y2, theta )
    !/ -----------------------------------------------------------------------------------
    !! Scale a string of text within a box of width and height. Place the text at the x and y
    !! coordinates and rotate it counter clockwise theta degrees.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSDraw),      intent(inout) :: dts   !! reference to this PSDraw.
    character(len=*),   intent(in)    :: text  !! string containing the text to write.
    real(dp),           intent(in)    :: x1    !! starting x world coordinates.
    real(dp),           intent(in)    :: y1    !! starting y world coordinates.
    real(dp),           intent(in)    :: x2    !! ending   x world coordinates.
    real(dp),           intent(in)    :: y2    !! ending   y world coordinates.
    real(dp), optional, intent(in)    :: theta !! rotation about the starting coordinates
    !!                                            in degrees counter clockwise.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: x, y, width, height, th
    th = 0.0d0
    if ( present( theta ) ) then
       th = theta
    end if
    x      = dts%scaleX(x1)
    y      = dts%scaleY(y1)
    width  = dts%scaleX(x2) - x
    height = dts%scaleY(y2) - y
    write( dts%file_unit, 100 ) x, y, width, height, th, trim(text)
100 format( 5(G0.5,' '),'(',A,') BS' )
  end subroutine write_psdraw


  !/ =====================================================================================
  subroutine write_inch_psdraw( dts, text, x, y, width, height, theta )
    !/ -----------------------------------------------------------------------------------
    !! Scale a string of text within a box of width and height. 
    !! Place the text at the x and y coordinates and rotate it 
    !! counter clockwise theta degrees.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSDraw),      intent(inout) :: dts    !! reference to this PSDraw.
    character(len=*),   intent(in)    :: text   !! string containing the text to write.
    real(dp),           intent(in)    :: x      !! starting x device coordinates.
    real(dp),           intent(in)    :: y      !! starting y device coordinates.
    real(dp),           intent(in)    :: width  !! width in device coordinates.
    real(dp),           intent(in)    :: height !! height in device coordinates.
    real(dp), optional, intent(in)    :: theta  !! rotation about the starting coordinates
    !!                                             in degrees counter clockwise.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: th
    th = 0.0d0
    if ( present( theta ) ) then
       th = theta
    end if
    write( dts%file_unit, 100 ) x, y, width, height, th, trim(text)
100 format( 5(G0.5,' '),'(',A,') BS' )
  end subroutine write_inch_psdraw


  !/ =====================================================================================
  subroutine save_color_psdraw( dts )
    !/ -----------------------------------------------------------------------------------
    !! Save the current drawing color.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSDraw), intent(inout) :: dts !! reference to this PSDraw.
    !/ -----------------------------------------------------------------------------------
    call dts%saved%copy( dts%last )
  end subroutine save_color_psdraw


  !/ =====================================================================================
  subroutine restore_color_psdraw( dts )
    !/ -----------------------------------------------------------------------------------
    !! Restore the last saved drawing color.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSDraw), intent(inout) :: dts !! reference to this PSDraw.
    !/ -----------------------------------------------------------------------------------
    call dts%setRGB( dts%saved%r, dts%saved%g, dts%saved%b )
  end subroutine restore_color_psdraw


  !/ =====================================================================================
  subroutine set_font_psdraw( dts, ft )
    !/ -----------------------------------------------------------------------------------
    !! Set Font
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSDraw), intent(inout) :: dts !! reference to this PSDraw.
    integer,       intent(in)    :: ft  !! font as defined by PSGraph constants.
    !/ -----------------------------------------------------------------------------------
    if      ( ft.eq.psfont_normal ) then
       write(dts%file_unit,*) 'TIMES'
    else if ( ft.eq.psfont_bold )   then
       write(dts%file_unit,*) 'TIMESB'
    else if ( ft.eq.psfont_italic ) then
       write(dts%file_unit,*) 'TIMESI'
    else
       call log_warn( 'setFont: invalid font number, defual to times normal. ft', I4=ft )
       call log_warn( 'setFont: examine the psfont_ parameters in psgraph_mod' )
       write(dts%file_unit,*) 'TIMES'
    end if
  end subroutine set_font_psdraw


  !/ =====================================================================================
  function scaleX_psdraw( dts, x ) result( sx )
    !/ -----------------------------------------------------------------------------------
    !! Scale X.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSDraw), intent(inout) :: dts !! reference to this PSDraw.
    real(dp),      intent(in)    :: x   !! world coordinate.
    real(dp)                     :: sx  !! device coordinate.
    !/ -----------------------------------------------------------------------------------
    sx = (dts%slope_x * x) + dts%inter_x
  end function scaleX_psdraw


  !/ =====================================================================================
  function scaleY_psdraw( dts, y ) result( sy )
    !/ -----------------------------------------------------------------------------------
    !! Scale Y.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSDraw), intent(inout) :: dts !! reference to this PSDraw.
    real(dp),      intent(in)    :: y   !! world coordinate.
    real(dp)                     :: sy  !! device coordinate.
    !/ -----------------------------------------------------------------------------------
    sy = (dts%slope_y * y) + dts%inter_y
  end function scaleY_psdraw


  !/ =====================================================================================
  subroutine set_rgb_color_psdraw( dts, clr )
    !/ -----------------------------------------------------------------------------------
    !! Set the current drawing color to the RGB value in clr.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSDraw), intent(inout) :: dts !! reference to this PSDraw.
    type(PSColor), intent(in)    :: clr !! reference to a source PSColor.
    !/ -----------------------------------------------------------------------------------
    call dts%setRGB( clr%r, clr%g, clr%b )
  end subroutine set_rgb_color_psdraw


  !/ =====================================================================================
  subroutine set_rgb_component_psdraw_R8( dts, r, g, b )
    !/ -----------------------------------------------------------------------------------
    !! Set the current RGB value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSDraw), intent(inout) :: dts !! reference to this PSDraw.
    real(dp),      intent(in)    :: r   !! red   color component.
    real(dp),      intent(in)    :: g   !! green color component.
    real(dp),      intent(in)    :: b   !! blue  color component.
    !/ -----------------------------------------------------------------------------------
    call dts%setRGB( real(r), real(g), real(b) )
  end subroutine set_rgb_component_psdraw_R8


  !/ =====================================================================================
  subroutine set_rgb_component_psdraw_R4( dts, r, g, b )
    !/ -----------------------------------------------------------------------------------
    !! Set the current RGB value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSDraw), intent(inout) :: dts !! reference to this PSDraw.
    real,          intent(in)    :: r   !! red   color component.
    real,          intent(in)    :: g   !! green color component.
    real,          intent(in)    :: b   !! blue  color component.
    !/ -----------------------------------------------------------------------------------
    call dts%last%set( r, g, b )
    write( dts%file_unit, 100 ) dts%last%r, dts%last%g, dts%last%b
100 format( F0.3,' ',F0.3,' ',F0.3,' setrgbcolor' )
  end subroutine set_rgb_component_psdraw_R4


  !/ =====================================================================================
  subroutine ps_write_psdraw( dts, unit )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSDraw), intent(inout) :: dts  !! reference to this PSDraw.
    integer,       intent(in)    :: unit !! file unit to transfer data.
    !/ -----------------------------------------------------------------------------------
    character(len=512) :: line_buffer
    integer            :: ios
    !/ -----------------------------------------------------------------------------------

    if ( dts%file_unit.lt.0 ) then
       close(dts%file_unit)

       open(NEWUNIT=dts%file_unit,FILE=dts%temp_filename,ACTION='READ',IOSTAT=ios)
       if ( 0.ne.ios ) then
          call log_error( 'Cannot reopen temporary file', STR=dts%temp_filename )
          call log_info( 'IOSTAT=', I4=ios )
          return
       end if

       call dts%write_header_pswin(unit)

       do
          read( dts%file_unit,'(A)',end=100 ) line_buffer
          write( unit, '(A)' ) trim(line_buffer)
       end do
100    continue

       call dts%write_trailer_pswin(unit)

       close(dts%file_unit)
       open(NEWUNIT=dts%file_unit,FILE=dts%temp_filename,ACTION='WRITE', &
            &                      POSITION="APPEND",IOSTAT=ios)
       if ( 0.ne.ios ) then
          call log_error( 'Cannot reopen temporary file', STR=dts%temp_filename )
       end if
    end if
  end subroutine ps_write_psdraw








  !/ =====================================================================================
  subroutine init_pspage( dts, id )
    !/ -----------------------------------------------------------------------------------
    !! Constructor.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSPage), intent(inout) :: dts !! reference to this PSPage.
    integer,       intent(in)    :: id  !! page identification.
    !/ -----------------------------------------------------------------------------------
    !dts%psd_list    => deque()
    dts%page_number =  id
  end subroutine init_pspage


  !/ =====================================================================================
  subroutine delete_pspage( dts )
    !/ -----------------------------------------------------------------------------------
    !! Destructor.
    !/ -----------------------------------------------------------------------------------
    class(PSPage), intent(inout) :: dts  !! reference to this PSPage.
    !/ -----------------------------------------------------------------------------------
    !! WHILE(LIST) POP
  end subroutine delete_pspage


  !/ =====================================================================================
  subroutine set_name_pspage( dts, nm )
    !/ -----------------------------------------------------------------------------------
    !! 
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSPage),      intent(inout) :: dts !! reference to this PSPage.
    character( len=* ), intent(in)    :: nm  !! page name.
    !/ -----------------------------------------------------------------------------------
  end subroutine set_name_pspage


  !/ =====================================================================================
  subroutine add_pspage( dts, w, x, y );
    !/ -----------------------------------------------------------------------------------
    !! Add a window to this page.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSPage),   intent(inout) :: dts !! reference to this PSPage.
    class(PSWindow), target        :: w   !! pointer to a window.
    real(dp),        intent(in)    :: x   !! horizontal position on page in inches.
    real(dp),        intent(in)    :: y   !! vertical   position on page in inches.
    !/ -----------------------------------------------------------------------------------
    class(*), pointer :: P
    P=> w
    call w%setDevice( x, y )
    call dts%psd_list%push(P)
  end subroutine add_pspage


  !/ =====================================================================================
  subroutine ps_write_pspage( dts, unit )
    !/ -----------------------------------------------------------------------------------
    !! Write the page.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSPage), intent(inout) :: dts  !! reference to this PSPage.
    integer,       intent(in)    :: unit !! output file unit.
    !/ -----------------------------------------------------------------------------------
    class(*), pointer :: obj
    !/ -----------------------------------------------------------------------------------

    call dts%write_header_pspage( unit )

    call dts%psd_list%head

    do
       obj => dts%psd_list%next()
       if ( .not.associated(obj) ) exit
       select type(obj)
       class is(PSWindow)
          call obj%pswrite( unit )
       end select
    end do

    call dts%write_trailer_pspage( unit )

  end subroutine ps_write_pspage


  !/ =====================================================================================
  subroutine write_header_pspage( dts, unit )
    !/ -----------------------------------------------------------------------------------
    !! Write the page header.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSPage), intent(inout) :: dts  !! reference to this PSPage.
    integer,       intent(in)    :: unit !! output file unit.
    !/ -----------------------------------------------------------------------------------
    write(unit,'(A)') '%%=============================================================='
    !    if ( ''.eq.dts%name ) then
    write(unit,'(A,I0,1X,I0)') '%%Page: ',dts%page_number,dts%page_number
    !    else
    !       write(unit,'(A)') '%%Page: ',dts%name,dts%page_number
    !    end if
    write(unit,'(A)') '%%--------------------------------------------------------------'
    write(unit,'(A)') '%%BeginPageSetup'
    write(unit,'(A)') 'startpage'
    write(unit,'(A)') 'TIMES'
    write(unit,'(A)') '%%EndPageSetup'
  end subroutine write_header_pspage


  !/ =====================================================================================
  subroutine write_trailer_pspage( dts, unit )
    !/ -----------------------------------------------------------------------------------
    !! Write the page trailer.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSPage), intent(inout) :: dts  !! reference to this PSPage.
    integer,       intent(in)    :: unit !! output file unit.
    !/ -----------------------------------------------------------------------------------
    write(unit,'(A)') '%%----------------------------------'
    write(unit,'(A)') 'stoppage'
    write(unit,'(A)') 'showpage'
  end subroutine write_trailer_pspage








  !/ =====================================================================================
  function init_psgraph( np ) result( psg )
    !/ -----------------------------------------------------------------------------------
    !! Constructor.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer,        intent(in) :: np  !! page number.
    class(PSGraph), pointer    :: psg !! pointer to a PSGraph.
    !/ -----------------------------------------------------------------------------------
    integer :: i
    !/ -----------------------------------------------------------------------------------
    allocate(psg)
    psg%num_page = np
    allocate(psg%page(np))
    do i=1,psg%num_page
       call psg%page(i)%init( i )
    end do
  end function init_psgraph


  !/ =====================================================================================
  subroutine delete_psgraph( dts )
    !/ -----------------------------------------------------------------------------------
    !! Destructor.
    !/ -----------------------------------------------------------------------------------
    class(PSGraph), intent(inout) :: dts !! reference to this PSGraph.
    !/ -----------------------------------------------------------------------------------
    integer :: i
    !/ -----------------------------------------------------------------------------------
    do i=1,dts%num_page
       call dts%page(i)%delete
    end do
    deallocate( dts%page )
  end subroutine delete_psgraph


  !/ =====================================================================================
  subroutine set_name_psgraph( dts, pn, nm )
    !/ -----------------------------------------------------------------------------------
    !! Set the name of an individual page.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSGraph),     intent(inout) :: dts !! reference to this PSGraph.
    integer,            intent(in)    :: pn  !! page name.
    character( len=* ), intent(in)    :: nm  !! page number.
    !/ -----------------------------------------------------------------------------------
  end subroutine set_name_psgraph


  !/ =====================================================================================
  subroutine add_psgraph( dts, w, pn, x, y )
    !/ -----------------------------------------------------------------------------------
    !! Add a window to this PSGraph.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSGraph),          intent(inout) :: dts !! reference to this PSGraph.
    class(PSWindow), target, intent(inout) :: w   !! pointer to a PSWindow.
    integer,                 intent(in)    :: pn  !! page number.
    real(dp),                intent(in)    :: x   !! x position in device coordinates.
    real(dp),                intent(in)    :: y   !! y position in device coordinates.
    !/ -----------------------------------------------------------------------------------
    class(*), pointer :: P
    P=> w
    call dts%page(pn)%add( w, x, y )
  end subroutine add_psgraph


  !/ =====================================================================================
  subroutine ps_write_psgraph( dts, fspc )
    !/ -----------------------------------------------------------------------------------
    !! Write the postscript for this graph on the output stream.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSGraph),   intent(inout) :: dts  !! reference to this PSGraph.
    character(len=*), intent(in)    :: fspc !! full path to output file.
    !/ -----------------------------------------------------------------------------------
    integer :: i, un, ios
    !/ -----------------------------------------------------------------------------------
    open(NEWUNIT=un,FILE=fspc,IOSTAT=ios)
    if ( 0.ne.ios ) then
       call log_error( 'Unable to write to file', STR=fspc )
       call log_info( 'IOSTAT=', I4=ios )
       return
    end if
    call dts%write_header_psgraph( fspc, un )
    do i=1,dts%num_page
       call dts%page(i)%pswrite(un)
    end do
    call dts%write_trailer_psgraph( un )
    close(un)
  end subroutine ps_write_psgraph


  !/ =====================================================================================
  subroutine write_header_psgraph( dts, fspc, unit )
    !/ -----------------------------------------------------------------------------------
    !! Write the postscript header for this graph on the output stream.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSGraph),   intent(inout) :: dts   !! reference to this PSGraph.
    character(len=*), intent(in)    :: fspc  !! full path to output file, or
    integer,          intent(in)    :: unit  !! output file unit.
    !/ -----------------------------------------------------------------------------------
    write(unit,'(A)') '%!PS-Adobe-3.0'
    write(unit,'(A)') '%%============================================= BEGIN FILE ====='
    write(unit,'(A,A)') '%%Title: ', trim(fspc)
    write(unit,'(A)') '%%Creator: PSGraph (F2008) PROXIMA17'
    write(unit,'(A,A)') '%%CreationDate: ', ctime(time())
    write(unit,'(A)') '%%Orientation: Landscape'
    write(unit,'(A,I0)') '%%Pages: ', dts%num_page
    write(unit,'(A)') '%%BoundingBox: 0 0 612 792'
    write(unit,'(A)') '%%DocumentPaperSizes: letter'
    write(unit,'(A)') '%Magnification: 1.0000'
    write(unit,'(A)') '%%EndComments'
    write(unit,'(A)') '%%--------------------------------------------------------------'
    write(unit,'(A)') '%%BeginSetup'
    write(unit,'(A)') '[{'
    write(unit,'(A)') '%%BeginFeature: *PageRegion Letter'
    write(unit,'(A)') '<</PageSize [612 792]>> setpagedevice'
    write(unit,'(A)') '%%EndFeature'
    write(unit,'(A)') '} stopped cleartomark'
    write(unit,'(A)') '%%EndSetup'
    write(unit,'(A)') '%%--------------------------------------------------------------'
    write(unit,'(A)') '%%BeginProlog'
    write(unit,'(A)') '%%----------------------'
    write(unit,'(A)') ''
    write(unit,'(A)') 'userdict /forms_ops 10 dict dup begin put'
    write(unit,'(A)') ''
    write(unit,'(A)') '/StartEPSF { % prepare for EPSF inclusion'
    write(unit,'(A)') 'userdict begin'
    write(unit,'(A)') '/PreEPS_state save def'
    write(unit,'(A)') '/dict_stack countdictstack def'
    write(unit,'(A)') '/ops_count count 1 sub def'
    write(unit,'(A)') '/showpage {} def'
    write(unit,'(A)') '} bind def'
    write(unit,'(A)') ''
    write(unit,'(A)') '/EPSFCleanUp { % clean up after EPSF inclusion'
    write(unit,'(A)') 'count ops_count sub {pop} repeat'
    write(unit,'(A)') 'countdictstack dict_stack sub {end} repeat'
    write(unit,'(A)') 'PreEPS_state restore'
    write(unit,'(A)') 'end % userdict'
    write(unit,'(A)') '} bind def'
    write(unit,'(A)') ''
    write(unit,'(A)') '%%----------------------'
    write(unit,'(A)') ''
    write(unit,'(A)') '/$PSGDict 200 dict def'
    write(unit,'(A)') '/$PSGBegin {$PSGDict begin /$PSGSaveState save def} def'
    write(unit,'(A)') '/$PSGEnd   {$PSGSaveState restore end} def'
    write(unit,'(A)') '%%----------------------'
    write(unit,'(A)') '/startpage {'
    write(unit,'(A)') 'save'
    write(unit,'(A)') 'newpath 0 792 moveto 0 0 lineto 612 0'
    write(unit,'(A)') 'lineto 612 792 lineto closepath clip newpath'
    write(unit,'(A)') '$PSGBegin'
    write(unit,'(A)') '10 setmiterlimit 0 setlinejoin 0 setlinecap 0 setlinewidth'
    write(unit,'(A)') '612 0 translate 90 rotate 72 72 scale'
    write(unit,'(A)') '} bind def'
    write(unit,'(A)') '%%----------------------'
    write(unit,'(A)') '/stoppage { $PSGEnd restore } bind def'
    write(unit,'(A)') '/TIMESB { /Times-Bold findfont setfont } bind def'
    write(unit,'(A)') '/TIMESI { /Times-Italic findfont setfont } bind def'
    write(unit,'(A)') '/TIMES  { /Times findfont setfont } bind def'
    write(unit,'(A)') '/DL { newpath moveto lineto stroke } bind def'
    write(unit,'(A)') '/DR { newpath rectstroke } bind def'
    write(unit,'(A)') '/DCIR { newpath 0.0 360.0 arc stroke } bind def'
    write(unit,'(A)') '/FCIR { newpath 0.0 360.0 arc fill } bind def'
    write(unit,'(A)') '/BS { % x, y, width, height, string'
    write(unit,'(A)') 'gsave'
    write(unit,'(A)') '  /str exch def /rot exch def /hgt exch def /wdt exch def'
    write(unit,'(A)') '  /yco exch def /xco exch def'
    write(unit,'(A)') '  xco yco translate 0 0 moveto rot rotate'
    write(unit,'(A)') '  wdt str stringwidth pop div hgt scale str show'
    write(unit,'(A)') 'grestore'
    write(unit,'(A)') '} bind def'
    write(unit,'(A)') '%%EndProlog'
  end subroutine write_header_psgraph


  !/ =====================================================================================
  subroutine write_trailer_psgraph( dts, unit )
    !/ -----------------------------------------------------------------------------------
    !! Write the postscript trailer for this graph on the output stream.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSGraph), intent(inout) :: dts  !! reference to this PSGraph.
    integer,        intent(in)    :: unit !! output file unit.
    !/ -----------------------------------------------------------------------------------
    write(unit,'(A)') '%%=============================================== END FILE ====='
    write(unit,'(A)') '%%Trailer'
    write(unit,'(A)') '%EOF'
  end subroutine write_trailer_psgraph


end module psgraph_mod


!/ =======================================================================================
!/ **                               P S G R A P H _ M O D                               **
!/ =========================================================================== END FILE ==
