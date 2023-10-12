module my_f

  use iso_c_binding

  implicit none

  !! encoded datatypes, must correspond to datamod.f90
  integer, parameter :: &
       DATA_UNKNOWN = 0, &
       DATA_INT0D   = 1, &
       DATA_INT1D   = 2, &
       DATA_INT2D   = 3, &
       DATA_REAL0D  = 4, &
       DATA_REAL1D  = 5, &
       DATA_REAL2D  = 6


  !! intermediate type to hold data
  type :: val
     integer( c_size_t ) :: dsize(2)
     integer :: dtyp
     integer :: i0d
     integer, allocatable :: i1d(:), i2d(:,:)
     real :: r0d
     real, allocatable :: r1d(:), r2d(:,:)
  end type val


  !! overload assignment from intermediate type to actual variable
  interface assignment(=)
     module procedure &
          assign_int0d_val, &
          assign_real0d_val, &
          assign_real2d_val
  end interface assignment(=)



  interface
     subroutine  memcpy(dest, src, n) bind(C,name='memcpy')
       import c_intptr_t, c_size_t
       INTEGER(c_intptr_t), value :: dest
       INTEGER(c_intptr_t), value :: src
       integer(c_size_t),   value :: n
     end subroutine memcpy
  end interface
  interface
     function fetch_intptr()result(intptr)bind(C,name="fetch_intptr")
       import c_intptr_t
       integer( c_intptr_t ) intptr
     end function fetch_intptr
     function lib_shm_unlink()result(ierr)bind(C, name="lib_shm_unlink")
       import c_int
       integer( c_int ) :: ierr
     end function lib_shm_unlink
  end interface

contains


  ! function get_intptr( fname )result(intptr)
  !   use tools
  !   implicit none
  !   character(*), intent(in) :: fname
  !   integer( c_intptr_t ) :: intptr

  !   character( kind=c_char, len = 1), allocatable :: cname(:)
  !   integer( c_int ) :: shm_f

  !   !! the name of shm needs to be c-string
  !   cname = f_c_string(fname)

  !   !! create/open the shared mem in /dev/shm
  !   shm_f = shm_open( cname, O_RDONLY, S_IRUSR )
  !   if( shm_f .eq. -1 ) then
  !      !! shm file does not exist?
  !      write(*,*) "error in shm_open"
  !      intptr = -1_c_intptr_t
  !      return
  !   end if

  !   !! get the ptr to mem address which maps data
  !   intptr = mmap( 0_c_intptr_t, shm_dsize, PROT_READ, MAP_SHARED, shm_f, 0_c_intptr_t )

  !   !! can close shm after the mmap
  !   close( shm_f )

  ! end function get_intptr



  function read_var( intptr, name ) result( a_val )
    integer( c_intptr_t ), intent(inout) :: intptr
    character(*), intent(in) :: name
    type( val ) :: a_val

    integer( c_size_t ) :: fsize
    integer :: dtyp

    ! get dtyp
    fsize = c_sizeof( a_val% dtyp )
    call memcpy( loc(a_val% dtyp), intptr, fsize )
    intptr = intptr + fsize

    ! write(*,*) "dtyp", a_val% dtyp

    ! get dsize
    fsize = c_sizeof( a_val% dsize(1) )
    call memcpy( loc(a_val% dsize(1)), intptr, fsize )
    intptr = intptr + fsize

    fsize = c_sizeof( a_val% dsize(2) )
    call memcpy( loc(a_val% dsize(2)), intptr, fsize )
    intptr = intptr + fsize

    ! write(*,*) "dsize:",a_val% dsize


    ! get val
    select case( a_val% dtyp )
    case( DATA_INT0D )
       fsize = c_sizeof(a_val% i0d)
       call memcpy( loc(a_val% i0d), intptr, fsize )
       intptr = intptr + fsize

    case( DATA_INT1D )

    case( DATA_INT2D )

    case( DATA_REAL0D )
       fsize = c_sizeof( a_val% r0d )
       call memcpy( loc(a_val% r0d), intptr, fsize )
       intptr = intptr + fsize

    case( DATA_REAL1D )

    case( DATA_REAL2D )
       allocate( a_val% r2d(1:a_val% dsize(1), 1:a_val% dsize(2) ) )
       fsize = c_sizeof( a_val% r2d(1,1) )*a_val% dsize(1)*a_val% dsize(2)
       call memcpy( loc(a_val% r2d), intptr, fsize )
       intptr = intptr + fsize

    case( DATA_UNKNOWN )

    end select

  end function read_var


  subroutine assign_int0d_val( lhs, rhs )
    integer, intent(out) :: lhs
    class(val), intent(in) :: rhs
    if( rhs% dtyp == DATA_INT0D ) then
       lhs = int( rhs% i0d )
    else
       write(*,*) "error in assignment i0d"
    end if
  end subroutine assign_int0d_val
  subroutine assign_real0d_val( lhs, rhs )
    real, intent(out) :: lhs
    class( val ), intent(in) :: rhs
    if( rhs% dtyp == DATA_REAL0D ) then
       lhs = real( rhs% r0d )
    else
       write(*,*) "error in assignmet r0d"
    end if
  end subroutine assign_real0d_val
  subroutine assign_real2d_val( lhs, rhs )
    real, allocatable, intent(out) :: lhs(:,:)
    class( val ), intent(in) :: rhs
    if( rhs% dtyp == DATA_REAL2D ) then
       allocate( lhs, source = rhs% r2d )
    else
       write(*,*) "error in assignmet r2d"
    end if
  end subroutine assign_real2d_val


  ! subroutine assign_( lhs, rhs )
  !   real, intent(out) :: lhs
  !   class(val), intent(in) :: rhs
  ! end subroutine assign_
  

end module my_f


! fsize = c_sizeof(dtyp)
! call memcpy( loc(dtyp), pp, fsize )
! pp = pp + fsize
