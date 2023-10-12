program produce
  use iso_c_binding
  use tools
  implicit none

  integer( c_int ) :: dim1, dim2
  real( c_double ), allocatable :: r(:,:)
  integer :: i, flen
  integer( c_int ) :: shm_f, ierr
  character(len=20) :: fname
  character(kind=c_char,len=1), allocatable :: cname(:)
  integer( c_size_t ), parameter :: dsize = 4096_c_size_t
  integer( c_size_t ) :: fsize
  integer( c_intptr_t ) :: pp

  fname = "/this_shm"
  cname = f_c_string(fname)

  read(*,*) dim1, dim2
  ! dim1 = int(dim1,c_int)
  ! dim2 = int(dim2,c_int)
  allocate( r(1:dim1, 1:dim2), source=0.0_c_double)

  do i = 1, dim2
     read(*,*) r(:, i)
     ! r(:,i) = real(r(:,i), c_double)
  end do

  !! create shm
  shm_f = shm_open( cname, o_creat, s_irwxu )
  write(*,*) "shm_f",shm_f
  if( shm_f .eq. -1 ) then
     write(*,*) "error in shm_open"
     return
  end if

  !! increase size
  ierr = ftruncate( shm_f, dsize )
  if( ierr /= 0 ) then
     write(*,*) "error in ftruncate"
     return
  end if


  !! map memory :: pp is ptr to destination
  pp = mmap( 0_c_intptr_t, dsize, p_rwr, mshare, shm_f, 0_c_intptr_t)
  write(*,*) "pp",pp

  !! write dim1
  fsize = c_sizeof(dim1)
  call memcpy( pp, loc(dim1), fsize )
  !! increment pp for next write
  pp = pp + fsize

  !! write dim2
  fsize = c_sizeof(dim2)
  call memcpy( pp, loc(dim2), fsize )
  pp = pp + fsize

  !! write r
  fsize = c_sizeof(1.0_c_double)*dim1*dim2
  call memcpy( pp, loc(r), fsize)

end program produce
