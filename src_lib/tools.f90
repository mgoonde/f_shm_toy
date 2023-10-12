module tools


  use iso_c_binding
  implicit none

  public

  integer( c_int ), parameter :: p_rwr = 3_c_int
  integer( c_int ), parameter :: p_r = 1_c_int
  integer( c_int ), parameter :: o_creat = 66_c_int
  integer( c_int ), parameter :: o_read = 0_c_int
  integer( c_int16_t ), parameter :: s_irwxu = 384_c_int16_t
  integer( c_int ), parameter :: mshare = 1_c_int
  interface
     function shm_open(cname, o_flag, mode) bind(c,name='shm_open')
       import c_int, c_char, c_int16_t
       character(kind=c_char) :: cname(*)
       integer( c_int ), value :: o_flag
       integer( c_int16_t ), value :: mode
       integer( c_int ) :: shm_open
     end function shm_open

     function ftruncate(fd, dsize) bind(c,name='ftruncate')
       import c_int, c_size_t
       integer( c_int ), value :: fd
       integer( c_size_t ), value :: dsize
       integer( c_int ) :: ftruncate
     end function ftruncate

     function mmap(addr,len,prot, flags,fildes,off) bind(c,name='mmap')
       import c_intptr_t, c_size_t, c_int
       INTEGER(c_intptr_t), value :: addr
       integer(c_size_t), value :: len
       integer(c_int), value :: prot
       integer(c_int), value :: flags
       integer(c_int), value :: fildes
       integer(c_size_t), value :: off
       integer( c_intptr_t) :: mmap
     end function mmap

     subroutine  memcpy(dest, src, n) bind(C,name='memcpy')
       import c_intptr_t, c_size_t
       INTEGER(c_intptr_t), value:: dest
       INTEGER(c_intptr_t), value:: src
       integer(c_size_t), value :: n
     end subroutine memcpy

     function shm_unlink(name) bind(c,name='shm_unlink')
       import c_int, c_char
       character( kind=c_char ) :: name(*)
       integer( c_int ) :: shm_unlink
     end function shm_unlink

  end interface


contains



  function f_c_string(string)
    use, intrinsic :: iso_c_binding, only: c_char,c_null_char
    character(len=*), intent(in) :: string

    character(kind=c_char,len=1), allocatable :: f_c_string(:)

    integer :: i, flen

    flen = len_trim(string)
    allocate( f_c_string(1:flen+1))
    do i = 1, flen
       f_c_string(i) = string(i:i)
    end do
    f_c_string(flen + 1) = c_null_char

  end function f_c_string


end module tools
