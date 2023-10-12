module api
  use datamod

  !! hard-code the name of shm as parameter, like this we don't need to carry it around
  character(*), parameter :: shm_fname = "/this_shm"
contains


  function lib_create()result(me)bind(C,name='lib_create')
    use iso_c_binding
    use datamod
    implicit none
    type( c_ptr ) :: me
    type(data), pointer :: fdata

    allocate( fdata )
    me = c_loc(fdata)

  end function lib_create


  subroutine lib_close(me)bind(C,name="lib_close")
    use iso_c_binding
    implicit none
    type( c_ptr ), value :: me
    type(data), pointer :: fdata

    call c_f_pointer( me, fdata )

    deallocate( fdata )
  end subroutine lib_close



  subroutine lib_print(me)bind(C,name="lib_print")

    use datamod
    use iso_c_binding
    implicit none
    type( c_ptr ), value :: me
    type(data), pointer :: fdata

    integer :: i

    ! write(*,*) c_associated( me )
    call c_f_pointer( me, fdata )
    write(*,*) "printing from api"

    write(*,*) "a", fdata% a
    write(*,*) "r", fdata% r
    ! write(*,*) datamod_r

    if( allocated( fdata% r2d)) then
       write(*,*) "r2d"
       do i = 1, size( fdata% r2d, 2)
          write(*,*) fdata% r2d(:,i)
       end do
    end if

  end subroutine lib_print


  function lib_get_dtyp( me, cname )bind(C, name="lib_get_dtyp")
    use iso_c_binding
    implicit none
    type( c_ptr ), value, intent(in) :: me
    type( c_ptr ), value, intent(in) :: cname
    integer( c_int ) :: lib_get_dtyp

    lib_get_dtyp = get_dtyp( cname )
  end function lib_get_dtyp


  subroutine lib_set( me, cname, cval )bind(C,name="lib_set")
    use iso_c_binding
    use datamod
    use tools
    implicit none
    type( c_ptr ), value, intent(in) :: me
    type( c_ptr ), value, intent(in) :: cname
    type( c_ptr ), value, intent(in) :: cval

    type(data), pointer :: fdata
    character(:), allocatable :: fname
    real( c_double ), pointer :: r
    integer( c_int ), pointer :: a

    call c_f_pointer( me, fdata )

    fname = c2f_string( cname )

    select case( fname )
    case( "a" )
       call c_f_pointer( cval, a )
       fdata% a = int( a )
    case( "r" )
       call c_f_pointer( cval, r )
       !! set r into datamod
       fdata% r = real( r )
       datamod_r = real( r )
    case default
       write(*,*) "variable does not exist ",fname
    end select

  end subroutine lib_set


  subroutine lib_setarray( me, cname, csize, cval )bind(C,name="lib_setarray" )
    use tools
    use iso_c_binding

    type( c_ptr ), value, intent(in) :: me
    type( c_ptr ), value, intent(in) :: cname
    type( c_ptr ),        intent(in) :: csize
    type( c_ptr ),        intent(in) :: cval

    type( data ), pointer :: fdata
    character(:), allocatable :: fname
    integer( c_int ), pointer :: fsize(:)
    integer( c_int ), pointer :: int0d, int1d(:), in2d(:,:)
    real( c_double ), pointer :: real0d, real1d(:), real2d(:,:)
    integer :: dtyp, d1, d2


    call c_f_pointer( me, fdata )

    fname = c2f_string( cname )
    dtyp = get_dtyp( fname )
    call c_f_pointer( csize, fsize, [2] )

    select case( dtyp )
    case( DATA_REAL2D )
       call c_f_pointer( cval, real2d, shape=[fsize(1), fsize(2)] )
    case default
       write(*,*) "set array not implemented fro dtyp ",dtyp
       return
    end select

    select case( fname )
    case( "r2d" )
       allocate( fdata% r2d, source=real(real2d) )
    case default
       write(*,*) "setarray not implemented for name ",fname
       return
    end select


  end subroutine lib_setarray


  subroutine lib_dump2shm( me, ccname )bind(C, name="lib_dump2shm")
    use tools
    use iso_c_binding
    implicit none
    type( c_ptr ), value, intent(in) :: me
    type( c_ptr ), value, intent(in) :: ccname !! name of the shm

    type( data ), pointer :: fdata
    character(len=1,kind=c_char), dimension(:), pointer :: ptr_cname
    character( kind=c_char, len = 1), allocatable :: cname(:)
    integer( c_size_t ) :: n
    integer( c_int ) :: ierr, shm_f, dim1, dim2
    integer( c_size_t ) :: fsize
    integer( c_intptr_t ) :: intptr
    integer :: dtyp

    !! get obj
    call c_f_pointer( me, fdata )

    ! n = c_strlen( ccname )
    ! call c_f_pointer( ccname, ptr_cname, [n])

    cname = f_c_string( shm_fname )

    !! create shm with oflag=CREAT+RDWR, and mode=RUSR+WUSR
    shm_f = shm_open( cname, O_CREAT+O_RDWR, S_IRUSR+S_IWUSR )
    if( shm_f .eq. -1 ) then
       write(*,*) "error in shm_open", cname
       return
    end if


    !! initial size
    ierr = ftruncate( shm_f, shm_dsize )
    if( ierr .ne. 0 ) then
       write(*,*) "error in ftruncate", ierr
       return
    end if

    !! map memory: intptr is ptr to start of destination memory
    intptr = mmap( 0_c_intptr_t, shm_dsize, PROT_WRITE, MAP_SHARED, shm_f, 0_c_intptr_t )
    !! mem is mapped, can close the shm
    close( shm_f)

    !! write data in order
    call cpy_var( me, intptr, "a" )
    call cpy_var( me, intptr, "r" )
    call cpy_var( me, intptr, "r2d")

  end subroutine lib_dump2shm

  subroutine cpy_var( me, intptr, name )
    use tools
    use iso_c_binding
    implicit none
    type( c_ptr ), value, intent(in) :: me
    integer( c_intptr_t ), intent(inout) :: intptr
    character(*), intent(in) :: name

    type( data ), pointer :: fdata
    integer( c_size_t ) :: fsize
    integer :: dtyp
    integer( c_int ), pointer :: csize
    integer( c_size_t ) :: dsize(2)

    call c_f_pointer(me, fdata)
    dtyp = get_dtyp( name )
    call get_dsize( me, name, dsize )

    ! 1. write dtyp
    fsize = c_sizeof(dtyp)
    call memcpy( intptr, loc(dtyp), fsize )
    intptr = intptr + fsize


    ! write(*,*) "dsize:",dsize(1), dsize(2)
    ! 2. write dsize
    fsize = c_sizeof( dsize(1) )
    call memcpy( intptr, loc(dsize(1)), fsize)
    intptr = intptr + fsize

    fsize = c_sizeof( dsize(2) )
    call memcpy( intptr, loc(dsize(2)), fsize)
    intptr = intptr + fsize


    ! 3. copy val
    select case( name )
    case( "a" )
       fsize = c_sizeof( 1 )
       call memcpy( intptr, loc( fdata% a), fsize )
       intptr = intptr + fsize
    case( "r" )
       fsize = c_sizeof( 1.0 )
       call memcpy( intptr, loc( fdata% r), fsize )
       intptr = intptr + fsize
    case( "r2d" )
       if( .not. allocated(fdata% r2d) ) return
       !! c_sizeof doesnt like allocatables, so do manually
       fsize = c_sizeof( 1.0 )*dsize(1)*dsize(2)
       call memcpy( intptr, loc( fdata% r2d), fsize)
       intptr = intptr + fsize
    case default
       write(*,*) "unknown varname:", name
       return
    end select

  end subroutine cpy_var


  subroutine lib_killshm( me, ccname )bind(C, name="lib_killshm")
    use tools
    use iso_c_binding
    implicit none
    type( c_ptr ), value, intent(in) :: me
    type( c_ptr ), value, intent(in) :: ccname !! name of the shm

    character(len=1,kind=c_char), dimension(:), pointer :: ptr_cname
    character( kind=c_char, len = 1), allocatable :: cname(:)
    integer( c_size_t ) :: n
    integer( c_int ) :: ierr

    ! n = c_strlen( ccname )
    ! call c_f_pointer( ccname, ptr_cname, [n])
    cname = f_c_string( shm_fname )

    !! unlink shm
    ierr = shm_unlink( cname )
    if( ierr .ne. 0 ) then
       write(*,*) "error in unlink_shm",cname
       return
    end if

  end subroutine lib_killshm




  !! return the intptr of existing shm
  function fetch_intptr()result(intptr)bind(C, name="fetch_intptr")
    use tools
    implicit none
    ! character(*), intent(in) :: fname
    integer( c_intptr_t ) :: intptr

    character( kind=c_char, len = 1), allocatable :: cname(:)
    integer( c_int ) :: shm_f

    !! the name of shm needs to be c-string
    cname = f_c_string(shm_fname)

    !! create/open the shared mem in /dev/shm
    shm_f = shm_open( cname, O_RDONLY, S_IRUSR )
    if( shm_f .eq. -1 ) then
       !! shm file does not exist?
       write(*,*) "error in shm_open"
       intptr = -1_c_intptr_t
       return
    end if

    !! get the ptr to mem address which maps data
    intptr = mmap( 0_c_intptr_t, shm_dsize, PROT_READ, MAP_SHARED, shm_f, 0_c_intptr_t )

    !! can close shm after the mmap
    close( shm_f )

  end function fetch_intptr


end module api
