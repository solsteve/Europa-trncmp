!/ ====================================================================== BEGIN FILE =====
!/ **                           P O S I X _ E R R O R _ M O D                           **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  Copyright (c) 2019, Stephen W. Soliday                                           **
!/ **                      stephen.soliday@trncmp.org                                   **
!/ **                      http://research.trncmp.org                                   **
!/ **                                                                                   **
!/ **  -------------------------------------------------------------------------------  **
!/ **                                                                                   **
!/ **  This program is free software: you can redistribute it and/or modify it under    **
!/ **  the terms of the GNU General Public License as published by the Free Software    **
!/ **  Foundation, either version 3 of the License, or (at your option)                 **
!/ **  any later version.                                                               **
!/ **                                                                                   **
!/ **  This program is distributed in the hope that it will be useful, but WITHOUT      **
!/ **  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS    **
!/ **  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.   **
!/ **                                                                                   **
!/ **  You should have received a copy of the GNU General Public License along with     **
!/ **  this program. If not, see <http://www.gnu.org/licenses/>.                        **
!/ **                                                                                   **
!/ =======================================================================================
module posix_error_mod
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2019-01-20
  !! license: GPL
  !!
  !!##POSIX Compatibility
  !!
  !! Provides an POSIX error numbers and messages.
  !
  !/ -------------------------------------------------------------------------------------

  integer, public, parameter :: P_EPERM           =   1  !! Operation not permitted
  integer, public, parameter :: P_ENOENT          =   2  !! No such file or directory
  integer, public, parameter :: P_ESRCH           =   3  !! No such process
  integer, public, parameter :: P_EINTR           =   4  !! Interrupted system call
  integer, public, parameter :: P_EIO             =   5  !! I/O error
  integer, public, parameter :: P_ENXIO           =   6  !! No such device or address
  integer, public, parameter :: P_E2BIG           =   7  !! Argument list too long
  integer, public, parameter :: P_ENOEXEC         =   8  !! Exec format error
  integer, public, parameter :: P_EBADF           =   9  !! Bad file number
  integer, public, parameter :: P_ECHILD          =  10  !! No child processes
  integer, public, parameter :: P_EAGAIN          =  11  !! Try again
  integer, public, parameter :: P_ENOMEM          =  12  !! Out of memory
  integer, public, parameter :: P_EACCES          =  13  !! Permission denied
  integer, public, parameter :: P_EFAULT          =  14  !! Bad address
  integer, public, parameter :: P_ENOTBLK         =  15  !! Block device required
  integer, public, parameter :: P_EBUSY           =  16  !! Device or resource busy
  integer, public, parameter :: P_EEXIST          =  17  !! File exists
  integer, public, parameter :: P_EXDEV           =  18  !! Cross-device link
  integer, public, parameter :: P_ENODEV          =  19  !! No such device
  integer, public, parameter :: P_ENOTDIR         =  20  !! Not a directory
  integer, public, parameter :: P_EISDIR          =  21  !! Is a directory
  integer, public, parameter :: P_EINVAL          =  22  !! Invalid argument
  integer, public, parameter :: P_ENFILE          =  23  !! File table overflow
  integer, public, parameter :: P_EMFILE          =  24  !! Too many open files
  integer, public, parameter :: P_ENOTTY          =  25  !! Not a typewriter
  integer, public, parameter :: P_ETXTBSY         =  26  !! Text file busy
  integer, public, parameter :: P_EFBIG           =  27  !! File too large
  integer, public, parameter :: P_ENOSPC          =  28  !! No space left on device
  integer, public, parameter :: P_ESPIPE          =  29  !! Illegal seek
  integer, public, parameter :: P_EROFS           =  30  !! Read-only file system
  integer, public, parameter :: P_EMLINK          =  31  !! Too many links
  integer, public, parameter :: P_EPIPE           =  32  !! Broken pipe
  integer, public, parameter :: P_EDOM            =  33  !! Math argument out of domain of func
  integer, public, parameter :: P_UNK_34          =  34  !! Unassigned 34
  integer, public, parameter :: P_EDEADLK         =  35  !! Resource deadlock would occur
  integer, public, parameter :: P_ENAMETOOLONG    =  36  !! File name too long
  integer, public, parameter :: P_ENOLCK          =  37  !! No record locks available
  integer, public, parameter :: P_ENOSYS          =  38  !! Invalid system call number
  integer, public, parameter :: P_ENOTEMPTY       =  39  !! Directory not empty
  integer, public, parameter :: P_ELOOP           =  40  !! Too many symbolic links encountered
  integer, public, parameter :: P_UNK_41          =  41  !! Unassigned 41
  integer, public, parameter :: P_ENOMSG          =  42  !! No message of desired type
  integer, public, parameter :: P_EIDRM           =  43  !! Identifier removed
  integer, public, parameter :: P_ECHRNG          =  44  !! Channel number out of range
  integer, public, parameter :: P_EL2NSYNC        =  45  !! Level 2 not synchronized
  integer, public, parameter :: P_EL3HLT          =  46  !! Level 3 halted
  integer, public, parameter :: P_EL3RST          =  47  !! Level 3 reset
  integer, public, parameter :: P_ELNRNG          =  48  !! Link number out of range
  integer, public, parameter :: P_EUNATCH         =  49  !! Protocol driver not attached
  integer, public, parameter :: P_ENOCSI          =  50  !! No CSI structure available
  integer, public, parameter :: P_EL2HLT          =  51  !! Level 2 halted
  integer, public, parameter :: P_EBADE           =  52  !! Invalid exchange
  integer, public, parameter :: P_EBADR           =  53  !! Invalid request descriptor
  integer, public, parameter :: P_EXFULL          =  54  !! Exchange full
  integer, public, parameter :: P_ENOANO          =  55  !! No anode
  integer, public, parameter :: P_EBADRQC         =  56  !! Invalid request code
  integer, public, parameter :: P_EBADSLT         =  57  !! Invalid slot
  integer, public, parameter :: P_UNK_58          =  58  !! Unassigned 58
  integer, public, parameter :: P_EBFONT          =  59  !! Bad font file format
  integer, public, parameter :: P_ENOSTR          =  60  !! Device not a stream
  integer, public, parameter :: P_ENODATA         =  61  !! No data available
  integer, public, parameter :: P_ETIME           =  62  !! Timer expired
  integer, public, parameter :: P_ENOSR           =  63  !! Out of streams resources
  integer, public, parameter :: P_ENONET          =  64  !! Machine is not on the network
  integer, public, parameter :: P_ENOPKG          =  65  !! Package not installed
  integer, public, parameter :: P_EREMOTE         =  66  !! Object is remote
  integer, public, parameter :: P_ENOLINK         =  67  !! Link has been severed
  integer, public, parameter :: P_EADV            =  68  !! Advertise error
  integer, public, parameter :: P_ESRMNT          =  69  !! Srmount error
  integer, public, parameter :: P_ECOMM           =  70  !! Communication error on send
  integer, public, parameter :: P_EPROTO          =  71  !! Protocol error
  integer, public, parameter :: P_EMULTIHOP       =  72  !! Multihop attempted
  integer, public, parameter :: P_EDOTDOT         =  73  !! RFS specific error
  integer, public, parameter :: P_EBADMSG         =  74  !! Not a data message
  integer, public, parameter :: P_EOVERFLOW       =  75  !! Value too large for defined data type
  integer, public, parameter :: P_ENOTUNIQ        =  76  !! Name not unique on network
  integer, public, parameter :: P_EBADFD          =  77  !! File descriptor in bad state
  integer, public, parameter :: P_EREMCHG         =  78  !! Remote address changed
  integer, public, parameter :: P_ELIBACC         =  79  !! Can not access a needed shared library
  integer, public, parameter :: P_ELIBBAD         =  80  !! Accessing a corrupted shared library
  integer, public, parameter :: P_ELIBSCN         =  81  !! .lib section in a.out corrupted
  integer, public, parameter :: P_ELIBMAX         =  82  !! Attempting to link in too many shared libraries
  integer, public, parameter :: P_ELIBEXEC        =  83  !! Cannot exec a shared library directly
  integer, public, parameter :: P_EILSEQ          =  84  !! Illegal byte sequence
  integer, public, parameter :: P_ERESTART        =  85  !! Interrupted system call should be restarted
  integer, public, parameter :: P_ESTRPIPE        =  86  !! Streams pipe error
  integer, public, parameter :: P_EUSERS          =  87  !! Too many users
  integer, public, parameter :: P_ENOTSOCK        =  88  !! Socket operation on non-socket
  integer, public, parameter :: P_EDESTADDRREQ    =  89  !! Destination address required
  integer, public, parameter :: P_EMSGSIZE        =  90  !! Message too long
  integer, public, parameter :: P_EPROTOTYPE      =  91  !! Protocol wrong type for socket
  integer, public, parameter :: P_ENOPROTOOPT     =  92  !! Protocol not available
  integer, public, parameter :: P_EPROTONOSUPPORT =  93  !! Protocol not supported
  integer, public, parameter :: P_ESOCKTNOSUPPORT =  94  !! Socket type not supported
  integer, public, parameter :: P_EOPNOTSUPP      =  95  !! Operation not supported on transport endpoint
  integer, public, parameter :: P_EPFNOSUPPORT    =  96  !! Protocol family not supported
  integer, public, parameter :: P_EAFNOSUPPORT    =  97  !! Address family not supported by protocol
  integer, public, parameter :: P_EADDRINUSE      =  98  !! Address already in use
  integer, public, parameter :: P_EADDRNOTAVAIL   =  99  !! Cannot assign requested address
  integer, public, parameter :: P_ENETDOWN        = 100  !! Network is down
  integer, public, parameter :: P_ENETUNREACH     = 101  !! Network is unreachable
  integer, public, parameter :: P_ENETRESET       = 102  !! Network dropped connection because of reset
  integer, public, parameter :: P_ECONNABORTED    = 103  !! Software caused connection abort
  integer, public, parameter :: P_ECONNRESET      = 104  !! Connection reset by peer
  integer, public, parameter :: P_ENOBUFS         = 105  !! No buffer space available
  integer, public, parameter :: P_EISCONN         = 106  !! Transport endpoint is already connected
  integer, public, parameter :: P_ENOTCONN        = 107  !! Transport endpoint is not connected
  integer, public, parameter :: P_ESHUTDOWN       = 108  !! Cannot send after transport endpoint shutdown
  integer, public, parameter :: P_ETOOMANYREFS    = 109  !! Too many references: cannot splice
  integer, public, parameter :: P_ETIMEDOUT       = 110  !! Connection timed out
  integer, public, parameter :: P_ECONNREFUSED    = 111  !! Connection refused
  integer, public, parameter :: P_EHOSTDOWN       = 112  !! Host is down
  integer, public, parameter :: P_EHOSTUNREACH    = 113  !! No route to host
  integer, public, parameter :: P_EALREADY        = 114  !! Operation already in progress
  integer, public, parameter :: P_EINPROGRESS     = 115  !! Operation now in progress
  integer, public, parameter :: P_ESTALE          = 116  !! Stale file handle
  integer, public, parameter :: P_EUCLEAN         = 117  !! Structure needs cleaning
  integer, public, parameter :: P_ENOTNAM         = 118  !! Not a XENIX named type file
  integer, public, parameter :: P_ENAVAIL         = 119  !! No XENIX semaphores available
  integer, public, parameter :: P_EISNAM          = 120  !! Is a named type file
  integer, public, parameter :: P_EREMOTEIO       = 121  !! Remote I/O error
  integer, public, parameter :: P_EDQUOT          = 122  !! Quota exceeded
  integer, public, parameter :: P_ENOMEDIUM       = 123  !! No medium found
  integer, public, parameter :: P_EMEDIUMTYPE     = 124  !! Wrong medium type
  integer, public, parameter :: P_ECANCELED       = 125  !! Operation Canceled
  integer, public, parameter :: P_ENOKEY          = 126  !! Required key not available
  integer, public, parameter :: P_EKEYEXPIRED     = 127  !! Key has expired
  integer, public, parameter :: P_EKEYREVOKED     = 128  !! Key has been revoked
  integer, public, parameter :: P_EKEYREJECTED    = 129  !! Key was rejected by service
  integer, public, parameter :: P_EOWNERDEAD      = 130  !! Owner died
  integer, public, parameter :: P_ENOTRECOVERABLE = 131  !! State not recoverable
  integer, public, parameter :: P_ERFKILL         = 132  !! Operation not possible due to RF-kill
  integer, public, parameter :: P_EHWPOISON       = 133  !! Memory page has hardware error
  integer, public, parameter :: P_EWOULDBLOCK     = P_EAGAIN !! Operation would block


  character(len=47), private, parameter :: P_MESSAGE(133) = (/ 'Operation not permitted                        ', &
       &                                                     'No such file or directory                      ', &
       &                                                     'No such process                                ', &
       &                                                     'Interrupted system call                        ', &
       &                                                     'I/O error                                      ', &
       &                                                     'No such device or address                      ', &
       &                                                     'Argument list too long                         ', &
       &                                                     'Exec format error                              ', &
       &                                                     'Bad file number                                ', &
       &                                                     'No child processes                             ', &
       &                                                     'Try again                                      ', &
       &                                                     'Out of memory                                  ', &
       &                                                     'Permission denied                              ', &
       &                                                     'Bad address                                    ', &
       &                                                     'Block device required                          ', &
       &                                                     'Device or resource busy                        ', &
       &                                                     'File exists                                    ', &
       &                                                     'Cross-device link                              ', &
       &                                                     'No such device                                 ', &
       &                                                     'Not a directory                                ', &
       &                                                     'Is a directory                                 ', &
       &                                                     'Invalid argument                               ', &
       &                                                     'File table overflow                            ', &
       &                                                     'Too many open files                            ', &
       &                                                     'Not a typewriter                               ', &
       &                                                     'Text file busy                                 ', &
       &                                                     'File too large                                 ', &
       &                                                     'No space left on device                        ', &
       &                                                     'Illegal seek                                   ', &
       &                                                     'Read-only file system                          ', &
       &                                                     'Too many links                                 ', &
       &                                                     'Broken pipe                                    ', &
       &                                                     'Math argument out of domain of func            ', &
       &                                                     'Unassigned 34                                  ', &
       &                                                     'Resource deadlock would occur                  ', &
       &                                                     'File name too long                             ', &
       &                                                     'No record locks available                      ', &
       &                                                     'Invalid system call number                     ', &
       &                                                     'Directory not empty                            ', &
       &                                                     'Too many symbolic links encountered            ', &
       &                                                     'Unassigned 41                                  ', &
       &                                                     'No message of desired type                     ', &
       &                                                     'Identifier removed                             ', &
       &                                                     'Channel number out of range                    ', &
       &                                                     'Level 2 not synchronized                       ', &
       &                                                     'Level 3 halted                                 ', &
       &                                                     'Level 3 reset                                  ', &
       &                                                     'Link number out of range                       ', &
       &                                                     'Protocol driver not attached                   ', &
       &                                                     'No CSI structure available                     ', &
       &                                                     'Level 2 halted                                 ', &
       &                                                     'Invalid exchange                               ', &
       &                                                     'Invalid request descriptor                     ', &
       &                                                     'Exchange full                                  ', &
       &                                                     'No anode                                       ', &
       &                                                     'Invalid request code                           ', &
       &                                                     'Invalid slot                                   ', &
       &                                                     'Unassigned 58                                  ', &
       &                                                     'Bad font file format                           ', &
       &                                                     'Device not a stream                            ', &
       &                                                     'No data available                              ', &
       &                                                     'Timer expired                                  ', &
       &                                                     'Out of streams resources                       ', &
       &                                                     'Machine is not on the network                  ', &
       &                                                     'Package not installed                          ', &
       &                                                     'Object is remote                               ', &
       &                                                     'Link has been severed                          ', &
       &                                                     'Advertise error                                ', &
       &                                                     'Srmount error                                  ', &
       &                                                     'Communication error on send                    ', &
       &                                                     'Protocol error                                 ', &
       &                                                     'Multihop attempted                             ', &
       &                                                     'RFS specific error                             ', &
       &                                                     'Not a data message                             ', &
       &                                                     'Value too large for defined data type          ', &
       &                                                     'Name not unique on network                     ', &
       &                                                     'File descriptor in bad state                   ', &
       &                                                     'Remote address changed                         ', &
       &                                                     'Can not access a needed shared library         ', &
       &                                                     'Accessing a corrupted shared library           ', &
       &                                                     '.lib section in a.out corrupted                ', &
       &                                                     'Attempting to link in too many shared libraries', &
       &                                                     'Cannot exec a shared library directly          ', &
       &                                                     'Illegal byte sequence                          ', &
       &                                                     'Interrupted system call should be restarted    ', &
       &                                                     'Streams pipe error                             ', &
       &                                                     'Too many users                                 ', &
       &                                                     'Socket operation on non-socket                 ', &
       &                                                     'Destination address required                   ', &
       &                                                     'Message too long                               ', &
       &                                                     'Protocol wrong type for socket                 ', &
       &                                                     'Protocol not available                         ', &
       &                                                     'Protocol not supported                         ', &
       &                                                     'Socket type not supported                      ', &
       &                                                     'Operation not supported on transport endpoint  ', &
       &                                                     'Protocol family not supported                  ', &
       &                                                     'Address family not supported by protocol       ', &
       &                                                     'Address already in use                         ', &
       &                                                     'Cannot assign requested address                ', &
       &                                                     'Network is down                                ', &
       &                                                     'Network is unreachable                         ', &
       &                                                     'Network dropped connection because of reset    ', &
       &                                                     'Software caused connection abort               ', &
       &                                                     'Connection reset by peer                       ', &
       &                                                     'No buffer space available                      ', &
       &                                                     'Transport endpoint is already connected        ', &
       &                                                     'Transport endpoint is not connected            ', &
       &                                                     'Cannot send after transport endpoint shutdown  ', &
       &                                                     'Too many references: cannot splice             ', &
       &                                                     'Connection timed out                           ', &
       &                                                     'Connection refused                             ', &
       &                                                     'Host is down                                   ', &
       &                                                     'No route to host                               ', &
       &                                                     'Operation already in progress                  ', &
       &                                                     'Operation now in progress                      ', &
       &                                                     'Stale file handle                              ', &
       &                                                     'Structure needs cleaning                       ', &
       &                                                     'Not a XENIX named type file                    ', &
       &                                                     'No XENIX semaphores available                  ', &
       &                                                     'Is a named type file                           ', &
       &                                                     'Remote I/O error                               ', &
       &                                                     'Quota exceeded                                 ', &
       &                                                     'No medium found                                ', &
       &                                                     'Wrong medium type                              ', &
       &                                                     'Operation Canceled                             ', &
       &                                                     'Required key not available                     ', &
       &                                                     'Key has expired                                ', &
       &                                                     'Key has been revoked                           ', &
       &                                                     'Key was rejected by service                    ', &
       &                                                     'Owner died                                     ', &
       &                                                     'State not recoverable                          ', &
       &                                                     'Operation not possible due to RF-kill          ', &
       &                                                     'Memory page has hardware error                 ' /)


  interface POSIX_ERROR_STRING
     module procedure :: posix_string_from_error_number
  end interface POSIX_ERROR_STRING


  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================


  

  !/ =====================================================================================
  function posix_string_from_error_number( err_num ) result( str )
    !! POSIX error strings
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(:), allocatable :: str
    integer, intent(in)       :: err_num
    !/ -----------------------------------------------------------------------------------
    if ( err_num.lt.0 ) then
       str = 'error number to small'
    else
       if ( err_num.eq.0 ) then
          str = 'success'
       else
          if ( err_num.gt.size(P_MESSAGE) ) then
             str = 'error number to large'
          else
             str = P_MESSAGE(err_num)
          end if
       end if
    end if
  end function posix_string_from_error_number

end module posix_error_mod


!/ =======================================================================================
!/ **                           P O S I X _ E R R O R _ M O D                           **
!/ =========================================================================== END FILE ==
