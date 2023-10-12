program readshm
  !! assume this is an ENGINE that does not support being used as library,
  !! but we can run it as SUBPROCESS from python.
  !! The goal is how to send data from python to this program, through first
  !! assigning some data into the library, then dumping it into shm from python,
  !! and then reading the shm here.
  use my_f
  use iso_c_binding
  implicit none



  character(len=20) :: fname
  integer :: i
  integer( c_intptr_t ) :: pp
  integer( c_int ) :: ierr

  real :: r0d
  integer :: i0d
  real, allocatable :: r2d(:,:)

  !! need the name of shm -- same as in prog.py
  ! fname = "/this_shm"

  !! get the ptr to shm
  ! pp = get_intptr( fname )
  pp = fetch_intptr()
  if( pp .eq. -1) then
     write(*,*) "error fetching shm ptr"
     return
  end if


  !! read the same order as vars were wtirren in dump2shm
  i0d = read_var( pp, "a" )
  r0d = read_var( pp, "r" )
  r2d = read_var( pp, "r2d" )

  write(*,*) "F read i0d",i0d
  write(*,*) "F read r0d:",r0d
  write(*,*) "F read r2d:"
  do i = 1, size( r2d, 2 )
     write(*,*) r2d(:,i)
  end do


  deallocate( r2d )
end program readshm


