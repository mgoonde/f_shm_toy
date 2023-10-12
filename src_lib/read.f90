program consume
  use tools
  use iso_c_binding
  implicit none

  integer( c_int ) :: dim1, dim2
  real( c_double ), allocatable :: r(:,:)
  integer( c_size_t ), parameter :: dsize = 4096 !! in bytes
  integer( c_int ) :: shm_f

  integer( c_int ) :: c_oflag
  integer( c_int16_t) :: c_mode

  character(len=20) :: fname
  character(kind=c_char,len=1), allocatable :: cname(:)
  integer( c_size_t) :: name_len
  integer, parameter :: fperm = 384
  integer :: flen, i
  integer( c_int ) :: ierr
  integer( c_intptr_t ) :: pp
  integer(c_int), parameter :: preadwrite = 1  !! 1 for read, 3 for write
  integer( c_size_t ) :: fsize


  fname = "/this_shm"
  cname = f_c_string(fname)

  !! create/open the shared mem in /dev/shm
  shm_f = shm_open( cname, o_read, s_irwxu )
  write(*,*) "shm_f",shm_f
  if( shm_f .eq. -1 ) then
     write(*,*) "error in shm_open"
     return
  end if

  !! get the file unit which maps data
  pp = mmap( 0_c_intptr_t, dsize, p_r, mshare, shm_f, 0_c_intptr_t)
  write(*,*) "pp",pp

  !! get dim1
  fsize = c_sizeof(1_c_int)
  call memcpy( loc(dim1), pp, fsize )
  pp = pp + fsize

  !! get dim2
  fsize = c_sizeof(1_c_int)
  call memcpy( loc(dim2), pp, fsize )
  pp = pp + fsize

  write(*,*) dim1, dim2
  allocate( r(1:dim1, 1:dim2), source = 0.0_c_double)

  !! get r
  fsize = c_sizeof(1.0_c_double)*dim1*dim2
  call memcpy( loc(r), pp, fsize )

  do i = 1, dim2
     write(*,*) r(:,i)
  end do

  ierr = shm_unlink( cname )
end program consume
