module tools


  use iso_c_binding
  implicit none

  public

  integer( c_size_t ), parameter :: shm_dsize = 4096_c_size_t  !! initial size of shm in bytes.. could be better

  ! from: /usr/include/asm-generic/mman-common.h
  ! #define PROT_READ       0x1             /* page can be read */
  ! #define PROT_WRITE      0x2             /* page can be written */
  ! #define PROT_EXEC       0x4             /* page can be executed */
  integer( c_int ), parameter :: PROT_READ  = 1_c_int
  integer( c_int ), parameter :: PROT_WRITE = 2_c_int
  integer( c_int ), parameter :: PROT_EXEC  = 4_c_int

  ! from: /usr/include/asm-generic/fcntl.h
  ! #define O_RDONLY        00000000
  ! #define O_WRONLY        00000001
  ! #define O_RDWR          00000002
  ! #define O_CREAT         00000100        /* not fcntl */
  integer( c_int ), parameter :: O_RDONLY = 0_c_int
  integer( c_int ), parameter :: O_WRONLY = 1_c_int
  integer( c_int ), parameter :: O_RDWR   = 2_c_int
  integer( c_int ), parameter :: O_CREAT  = 64_c_int

  ! from: /usr/include/linux/stat.h
  ! #define S_IRWXU 00700
  ! #define S_IRUSR 00400
  ! #define S_IWUSR 00200
  ! #define S_IXUSR 00100
  integer( c_int16_t ), parameter :: S_IRWXU = 448_c_int16_t
  integer( c_int16_t ), parameter :: S_IRUSR = 256_c_int16_t
  integer( c_int16_t ), parameter :: S_IWUSR = 128_c_int16_t
  integer( c_int16_t ), parameter :: S_IXUSR = 64_c_int16_t

  ! from /usr/include/linux/mman.h
  ! #define MAP_SHARED      0x01            /* Share changes */
  ! #define MAP_PRIVATE     0x02            /* Changes are private */
  integer( c_int ), parameter :: MAP_SHARED  = 1_c_int
  integer( c_int ), parameter :: MAP_PRIVATE = 2_c_int

  interface

     ! shm_open, shm_unlink - create/open or unlink POSIX shared memory objects
     !
     ! shm_open()  creates and opens a new, or opens an existing, POSIX shared memory object.  A POSIX
     ! shared memory object is in effect a handle which can be used by unrelated processes to  mmap(2)
     ! the  same  region of shared memory.  The shm_unlink() function performs the converse operation,
     ! removing an object previously created by shm_open().
     !
     ! #include <sys/mman.h>
     ! #include <sys/stat.h>        /* For mode constants */
     ! #include <fcntl.h>           /* For O_* constants */
     ! int shm_open(const char *name, int oflag, mode_t mode);
     function shm_open(cname, oflag, mode) bind(c,name='shm_open')
       import c_int, c_char, c_int16_t
       character(kind=c_char) :: cname(*)
       integer( c_int ),     value :: oflag
       integer( c_int16_t ), value :: mode
       integer( c_int ) :: shm_open
     end function shm_open

     ! truncate, ftruncate - truncate a file to a specified length
     !
     ! The  truncate() and ftruncate() functions cause the regular file named by path or referenced by
     ! fd to be truncated to a size of precisely length bytes.
     !
     ! #include <unistd.h>
     ! #include <sys/types.h>
     ! int ftruncate(int fd, off_t length);
     function ftruncate(fd, length) bind(c,name='ftruncate')
       import c_int, c_size_t
       integer( c_int ),    value :: fd
       integer( c_size_t ), value :: length
       integer( c_int ) :: ftruncate
     end function ftruncate

     ! mmap, munmap - map or unmap files or devices into memory
     !
     ! mmap() creates a new mapping in the virtual address space of the calling process.  The starting
     ! address for the new mapping is specified in addr.  The length argument specifies the length  of
     ! the mapping (which must be greater than 0).
     !
     ! #include <sys/mman.h>
     ! void *mmap(void addr[.length], size_t length, int prot, int flags, int fd, off_t offset);
     function mmap(addr, length, prot, flags, fildes, offset) bind(c,name='mmap')
       import c_intptr_t, c_size_t, c_int
       INTEGER( c_intptr_t ), value :: addr
       integer( c_size_t ),   value :: length
       integer( c_int ),      value :: prot
       integer( c_int ),      value :: flags
       integer( c_int ),      value :: fildes
       integer( c_size_t ),   value :: offset
       integer( c_intptr_t ) :: mmap
     end function mmap

     ! The  memcpy() function copies n bytes from memory area src to memory area dest.
     ! The memory areas must not overlap.  Use memmove(3) if the memory areas do overlap.
     !
     ! #include <string.h>
     ! void *memcpy(void *dest, const void *src, size_t n);
     subroutine  memcpy(dest, src, n) bind(C,name='memcpy')
       import c_intptr_t, c_size_t
       INTEGER(c_intptr_t), value :: dest
       INTEGER(c_intptr_t), value :: src
       integer(c_size_t),   value :: n
     end subroutine memcpy


     ! int shm_unlink(const char *name);
     function shm_unlink(name) bind(c,name='shm_unlink')
       import c_int, c_char
       character( kind=c_char ) :: name(*)
       integer( c_int ) :: shm_unlink
     end function shm_unlink


     !! length of c string
     FUNCTION c_strlen(str) BIND(C, name='strlen')
       IMPORT :: c_ptr, c_size_t
       IMPLICIT NONE
       TYPE(c_ptr), INTENT(IN), VALUE :: str
       INTEGER(c_size_t) :: c_strlen
     END FUNCTION c_strlen
     !! malloc
     FUNCTION c_malloc(size) BIND(C, name='malloc')
       use iso_c_binding, only: c_ptr, c_size_t
       IMPLICIT NONE
       INTEGER(c_size_t), VALUE :: size
       TYPE(c_ptr) :: c_malloc
     END FUNCTION c_malloc

  end interface


contains



  !! convert F string to C_char ctring
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


  FUNCTION f2c_string(f_string) RESULT(ptr)
    use iso_c_binding
    CHARACTER(LEN=*), INTENT(IN)           :: f_string
    CHARACTER(LEN=1, KIND=c_char), POINTER :: c_string(:)
    TYPE(c_ptr) :: ptr
    INTEGER(c_size_t) :: i, n

    n = LEN_TRIM(f_string)
    ptr = c_malloc(n+1)
    CALL C_F_POINTER(ptr, c_string, [n+1])
    DO i=1, n
        c_string(i) = f_string(i:i)
    END DO
    c_string(n+1) = c_null_char
  END FUNCTION f2c_string

  ! copy null-terminated C string to fortran string
  FUNCTION c2f_string(ptr) RESULT(f_string)
    use iso_c_binding
    TYPE(c_ptr), INTENT(IN) :: ptr
    CHARACTER(LEN=:), ALLOCATABLE :: f_string
    CHARACTER(LEN=1, KIND=c_char), DIMENSION(:), POINTER :: c_string
    INTEGER :: n

    IF (.NOT. C_ASSOCIATED(ptr)) THEN
      f_string = ' '
    ELSE
      n = INT(c_strlen(ptr), KIND=KIND(n))
      CALL C_F_POINTER(ptr, c_string, [n+1])
      f_string = array2string(c_string, n)
    END IF
  END FUNCTION c2f_string

  ! Copy a known-length or null-terminated array of C characters into a string
  FUNCTION array2string(array, length) RESULT(string)
    use iso_c_binding
    CHARACTER(LEN=1, KIND=c_char), DIMENSION(:) :: array
! NOTE: giving "length" the VALUE attribute reveals a bug in gfortran 12.2.1
! https://gcc.gnu.org/bugzilla/show_bug.cgi?id=107441
    INTEGER, INTENT(IN), OPTIONAL :: length
    CHARACTER(LEN=:), ALLOCATABLE :: string
    INTEGER :: n, i

    IF (PRESENT(length)) THEN
      n = length
    ELSE
      n = 1
      DO WHILE (n < SIZE(array) .AND. array(n+1) /= c_null_char)
        n = n + 1
      END DO
    END IF
    ALLOCATE(CHARACTER(LEN=n) :: string)
    DO i = 1, n
      string(i:i) = array(i)
    END DO
  END FUNCTION array2string




end module tools
