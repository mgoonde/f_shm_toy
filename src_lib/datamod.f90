module datamod

  implicit none

  real :: datamod_r

  integer, parameter :: &
       DATA_UNKNOWN = 0, &
       DATA_INT0D   = 1, &
       DATA_INT1D   = 2, &
       DATA_INT2D   = 3, &
       DATA_REAL0D  = 4, &
       DATA_REAL1D  = 5, &
       DATA_REAL2D  = 6


  !! this is the data type we want to pass from library to fprog
  type :: data
     integer :: a
     real :: r
     real, allocatable :: r2d(:,:)
  end type data



  interface get_dtyp
     module procedure :: get_dtype_f, get_dtype_c
  end interface get_dtyp


contains


  function get_dtype_f( name ) result(dtyp)
    implicit none
    character(*), intent(in) :: name
    integer :: dtyp
    select case( name )
    case( "a" )
       dtyp = DATA_INT0D
    case( "r" )
       dtyp = DATA_REAL0D
    case( "r2d" )
       dtyp = DATA_REAL2D
    case default
       write(*,*) "data name unknown:",name
       dtyp = DATA_UNKNOWN
    end select
  end function get_dtype_f
  function get_dtype_c( cname ) result(dtyp)
    use iso_c_binding
    use tools
    implicit none
    type( c_ptr ), value, intent(in) :: cname
    integer :: dtyp
    character(:), allocatable :: fname
    fname = c2f_string( cname )
    dtyp = get_dtype_f( fname )
  end function get_dtype_c


  subroutine get_dsize( me, name, dsize )
    use iso_c_binding
    implicit none
    type( c_ptr ), value, intent(in) :: me
    character(*), intent(in) :: name
    integer( c_size_t ), intent(out) :: dsize(2)

    integer :: dtyp
    type( data ), pointer :: fdata

    call c_f_pointer( me, fdata )

    dtyp = get_dtyp( name )

    select case( dtyp )
    case( DATA_REAL0D, DATA_INT0D )
       dsize(1) = 1; dsize(2) = 1
    case( DATA_INT1D, DATA_REAL1D )
       select case( name )
       end select

    case( DATA_INT2D, DATA_REAL2D )
       select case( name )
       case( "r2d" )
          if( .not. allocated(fdata% r2d) ) then
             dsize(:) = 0
          else
             dsize(1) = size( fdata% r2d, 1)
             dsize(2) = size( fdata% r2d, 2)
          end if
       end select

    case( DATA_UNKNOWN )
       dsize(1) = 0; dsize(2) = 0
    end select

  end subroutine get_dsize


end module datamod
