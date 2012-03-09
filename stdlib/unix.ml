(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: unix.ml,v 1.4 1999/10/13 14:36:12 pessaux Exp $ *)

type error =
    E2BIG
  | EACCES
  | EAGAIN
  | EBADF
  | EBUSY
  | ECHILD
  | EDEADLK
  | EDOM
  | EEXIST
  | EFAULT
  | EFBIG
  | EINTR
  | EINVAL
  | EIO
  | EISDIR
  | EMFILE
  | EMLINK
  | ENAMETOOLONG
  | ENFILE
  | ENODEV
  | ENOENT
  | ENOEXEC
  | ENOLCK
  | ENOMEM
  | ENOSPC
  | ENOSYS
  | ENOTDIR
  | ENOTEMPTY
  | ENOTTY
  | ENXIO
  | EPERM
  | EPIPE
  | ERANGE
  | EROFS
  | ESPIPE
  | ESRCH
  | EXDEV
  | EWOULDBLOCK
  | EINPROGRESS
  | EALREADY
  | ENOTSOCK
  | EDESTADDRREQ
  | EMSGSIZE
  | EPROTOTYPE
  | ENOPROTOOPT
  | EPROTONOSUPPORT
  | ESOCKTNOSUPPORT
  | EOPNOTSUPP
  | EPFNOSUPPORT
  | EAFNOSUPPORT
  | EADDRINUSE
  | EADDRNOTAVAIL
  | ENETDOWN
  | ENETUNREACH
  | ENETRESET
  | ECONNABORTED
  | ECONNRESET
  | ENOBUFS
  | EISCONN
  | ENOTCONN
  | ESHUTDOWN
  | ETOOMANYREFS
  | ETIMEDOUT
  | ECONNREFUSED
  | EHOSTDOWN
  | EHOSTUNREACH
  | ELOOP
  | EUNKNOWNERR of int

exception Unix_error of error * string * string

let _ = Callback.register_exception "Unix.Unix_error"
                                    (Unix_error(E2BIG, "", ""))

external error_message : error[`a] <[`b]-> string[_] = "unix_error_message"

let handle_unix_error f arg =
  try
    f arg
  with Unix_error(err, fun_name, arg) ->
    prerr_string Sys.argv.(0);
    prerr_string ": \"";
    prerr_string fun_name;
    prerr_string "\" failed";
    if String.length arg > 0 then begin
      prerr_string " on \"";
      prerr_string arg;
      prerr_string "\""
    end;
    prerr_string ": ";
    prerr_endline (error_message err);
    exit 2

external environment : unit[() :Pre; `a] <[`b]-> string[_] array[_]
    = "unix_environment"
external getenv: string[`a] <[__Not_found22:Pre;`b]->
  string[_] = "sys_getenv"
external putenv: string[`a] <[`b]-> string[`c] <[`d]->
  unit[() :Pre; `e] = "unix_putenv"

type process_status =
    WEXITED of int
  | WSIGNALED of int
  | WSTOPPED of int

type wait_flag =
    WNOHANG
  | WUNTRACED

external execv : string[_] <[`a]-> string[_] array[_]
    <[Unix__Unix_error94(error[E2BIG:Pre;EACCES:Pre;`b] *
			  string[_] *
			  string[_]);`c]->
    unit[() :Pre; `d] = "unix_execv"
external execve : string[_] <[`a]-> string[_] array[_] <[`b]->
  string[_] array[_]
    <[Unix__Unix_error94(error[E2BIG:Pre;EACCES:Pre;`c] *
			 string[_] *
			 string[_]);`d]->
    unit[() :Pre; `e] = "unix_execve"
external execvp : string[_] <[`a]-> string[_] array[_]
    <[Unix__Unix_error94(error[E2BIG:Pre;EACCES:Pre;`b] *
			 string[_] *
			 string[_]);`c]->
  unit[() :Pre; `d] = "unix_execvp"
external execvpe : string[_] <[`a]-> string[_] array[_] <[`b]->
  string[_] array[_]
    <[Unix__Unix_error94(error[E2BIG:Pre;EACCES:Pre;`c] *
			 string[_] *
			 string[_]);`d]->
  unit[() :Pre; `e] = "unix_execvpe"
external fork : unit[() :Pre; `a] <[`b] -> int[_] = "unix_fork"
external wait : unit[() :Pre; `a] <[`b] ->
  int[_] * process_status[WEXITED(int[_]);
			  WSIGNALED(int[_]);
			  WSTOPPED(int[_]); `c] = "unix_wait"

external waitpid : wait_flag[`a] list[`b] <[`c]->
    int[`d] <[`e]->
    int[_] *
    process_status
    [WEXITED (int[_]); WSIGNALED (int[_]); WSTOPPED (int[_]); `f]
    = "unix_waitpid"

external getpid : unit[() :Pre; `a] <[`b]-> int[_] = "unix_getpid"
external getppid : unit[() :Pre; `a] <[`b]-> int[_] = "unix_getppid"
external nice : int[`a] <[`b]-> int[_] = "unix_nice"

type file_descr = int

let stdin = 0
let stdout = 1
let stderr = 2

type open_flag =
    O_RDONLY
  | O_WRONLY
  | O_RDWR
  | O_NONBLOCK
  | O_APPEND
  | O_CREAT
  | O_TRUNC
  | O_EXCL

type file_perm = int

external openfile : string[`a] <[`b]->
    open_flag[`c] list[`d] <[`e]-> int[`f] <[`g]-> int[_]
           = "unix_open"

external close : int[`a] <[`b]-> unit[() :Pre; `c] = "unix_close"
external unsafe_read : int[`a] <[`b]-> string[`c] <[`d]->
  int[`e]  <[`f]-> int[`g] <[`h]-> int[_] = "unix_read"
external unsafe_write : int[`a] <[`b]-> string[`c] <[`d]->
  int[`e] <[`f]-> int[`g] <[`h]-> int[_] = "unix_write"

let read fd buf ofs len =
  if len < 0 or ofs + len > String.length buf
  then invalid_arg "Unix.read"
  else unsafe_read fd buf ofs len
let write fd buf ofs len =
  if len < 0 or ofs + len > String.length buf
  then invalid_arg "Unix.write"
  else unsafe_write fd buf ofs len

external in_channel_of_descr : int[`a] <[`b]-> in_channel[_]
                             = "caml_open_descriptor"
external out_channel_of_descr : int[`a] <[`b] -> out_channel[_]
                              = "caml_open_descriptor"
external descr_of_in_channel : in_channel[`a] <[`b]->
  int[_] = "channel_descriptor"
external descr_of_out_channel : out_channel[`a] <[`b]-> int[_]
                              = "channel_descriptor"

type seek_command =
    SEEK_SET
  | SEEK_CUR
  | SEEK_END

external lseek : int[`a] <[`b]-> int[`c] <[`d]->
  seek_command[SEEK_SET:Pre;SEEK_CUR:Pre;SEEK_END:Pre;`e] <[`f]->
    int[_] = "unix_lseek"
external truncate : string[`a] <[`b]-> int[`c] <[`d]->
  unit[() :Pre; `e]= "unix_truncate"
external ftruncate : int[`a] <[`b]-> int[`c] <[`d] ->
  unit[() :Pre; `e] = "unix_ftruncate"

type file_kind =
    S_REG
  | S_DIR
  | S_CHR
  | S_BLK
  | S_LNK
  | S_FIFO
  | S_SOCK

type stats =
  { st_dev : int;
    st_ino : int;
    st_kind : file_kind;
    st_perm : file_perm;
    st_nlink : int;
    st_uid : int;
    st_gid : int;
    st_rdev : int;
    st_size : int;
    st_atime : float;
    st_mtime : float;
    st_ctime : float }


external stat : string[`a] <[`b]->
    stats
    [{ st_atime : float[_];  st_ctime : float[_];  st_dev : int[_]; 
       st_gid : int[_];  st_ino : int[_]; 
       st_kind : file_kind[S_REG:Pre; `c];  st_mtime : float[_]; 
       st_nlink : int[_];  st_perm : int[_];  st_rdev : int[_]; 
       st_size : int[_];  st_uid : int[_] }; `d] = "unix_stat"

external lstat : string[`a] <[`b]->
    stats
    [{ st_atime : float[_];  st_ctime : float[_];  st_dev : int[_]; 
       st_gid : int[_];  st_ino : int[_]; 
       st_kind : file_kind[S_REG:Pre; `c];  st_mtime : float[_]; 
       st_nlink : int[_];  st_perm : int[_];  st_rdev : int[_]; 
       st_size : int[_];  st_uid : int[_] }; `d] = "unix_lstat"

(* Indeed, file_descr = int *)
external fstat : int[`a] <[`b]->
    stats
    [{ st_atime : float[_];  st_ctime : float[_];  st_dev : int[_]; 
       st_gid : int[_];  st_ino : int[_]; 
       st_kind : file_kind[S_REG:Pre; `c];  st_mtime : float[_]; 
       st_nlink : int[_];  st_perm : int[_];  st_rdev : int[_]; 
       st_size : int[_];  st_uid : int[_] }; `d] = "unix_fstat"


external unlink : string[`a] <[`b]-> unit[() :Pre; `c] = "unix_unlink"
external rename : string[`a] <[`b]-> string[`c] <[`d]->
  unit[() :Pre; `e] = "unix_rename"
external link : string[`a] <[`b]-> string[`c] <[`d]->
  unit[() :Pre; `e] = "unix_link"

type access_permission =
    R_OK
  | W_OK
  | X_OK
  | F_OK

external chmod : string[`a] <[`b]-> int[`c] <[`d]->
  unit[() :Pre; `e] = "unix_chmod"
external fchmod : int[`a] <[`b]-> int[`c] <[`d]->
  unit[() :Pre; `e] = "unix_fchmod"
external chown : string[`a] <[`b]-> int[`c] <[`d]-> int[`e] <[`f]->
  unit[() :Pre; `g] = "unix_chown"
external fchown : int[`a] <[`b]-> int[`c] <[`d]-> int[`e] <[`f]->
  unit[() :Pre; `g] = "unix_fchown"
external umask : int[`a] <[`b]-> int[_] = "unix_umask"
external access :
    string[`a] <[`b]-> access_permission[`c] list[`d] <[`e]->
      unit[() :Pre; `f] = "unix_access"


external dup : int[`a] <[`b]-> int[_] = "unix_dup"
external dup2 : int[`a] <[`b]-> int[`c] <[`d]->
  unit[() :Pre; `e] = "unix_dup2"
external set_nonblock : int[`a] <[`b]->
  unit[() :Pre; `c] = "unix_set_nonblock"
external clear_nonblock : int[`a] <[`b]->
  unit[() :Pre; `c] = "unix_clear_nonblock"
external set_close_on_exec : int[`a] <[`b]->
  unit[() :Pre; `c] = "unix_set_close_on_exec"
external clear_close_on_exec : int [`a] <[`b]->
  unit[() :Pre; `c] = "unix_clear_close_on_exec"

external mkdir : string[`a] <[`b]-> int[`c] <[`d]->
  unit[() :Pre; `e] = "unix_mkdir"
external rmdir : string[`a] <[`b]-> unit[() :Pre; `c] = "unix_rmdir"
external chdir : string[`a] <[`b]-> unit[() :Pre; `c] = "unix_chdir"
external getcwd : unit[() :Pre; `a] <[`b]-> string[_] = "unix_getcwd"
external chroot : string[`a] <[`b]-> unit[() :Pre; `b] = "unix_chroot"

type dir_handle

external opendir : string[`a] <[`b]-> dir_handle[_] = "unix_opendir"
external readdir : dir_handle[`a] <[__Not_found22:Pre;`b]->
  string[_] = "unix_readdir"
external rewinddir : dir_handle[`a] <[`b]-> unit[() :Pre; `c]
    = "unix_rewinddir"
external closedir : dir_handle[`a] <[`b]-> unit[() :Pre; `c]
    = "unix_closedir"

external pipe : unit[() :Pre; `a] <[`b]->
  int[_] * int[_] = "unix_pipe"
external symlink : string[`a] <[`b]-> string[`c] <[`d]->
  unit[() :Pre; `e] = "unix_symlink"
external readlink : string[`a] <[`b]-> string[_] = "unix_readlink"
external mkfifo : string[`a] <[`b]-> int[`c] <[`d]-> unit[() :Pre; `e]
    = "unix_mkfifo"
external select :
   'tyabbr0 <[`a]->
   'tyabbr1 <[`b]->
   'tyabbr2 <[`c]->
   float[`d] <[`f]-> ('tyabbr3 * 'tyabbr4 * 'tyabbr5)
     with 'tyabbr0 = int[`g] list[[]:Pre; :: (int[`g] * 'tyabbr0); `h]
     and 'tyabbr1 = int[`i] list[[]:Pre; :: (int[`i] * 'tyabbr1); `j]
     and 'tyabbr2 = int[`k] list[[]:Pre; :: (int[`k] * 'tyabbr2); `l]
     and 'tyabbr3 = int[_] list[[]:Pre; :: (int[_] * 'tyabbr3); `m]
     and 'tyabbr4 = int[_] list[[]:Pre; :: (int[_] * 'tyabbr4); `n]
     and 'tyabbr5 = int[_] list[[]:Pre; :: (int[_] * 'tyabbr5); `o]
 = "unix_select"


type lock_command =
    F_ULOCK
  | F_LOCK
  | F_TLOCK
  | F_TEST
  | F_RLOCK
  | F_TRLOCK

external lockf :
      int[`a] <[`b]->
	lock_command[F_ULOCK:Pre;F_LOCK:Pre;F_TLOCK:Pre;F_TEST:Pre;F_RLOCK:Pre;
		     F_TRLOCK:Pre;`c] <[`d]-> int[`e] <[`f]->
	unit[() :Pre; `g] = "unix_lockf"
external kill : int[`a] <[`b]-> int[`c] <[`d]-> unit[() :Pre; `e]
    = "unix_kill"
type sigprocmask_command = SIG_SETMASK | SIG_BLOCK | SIG_UNBLOCK


external sigprocmask :
   sigprocmask_command[`a] <[`b]-> int[`c] list[`d] <[`e]-> 'tyabbr0
     with 'tyabbr0 = int[_] list[[]:Pre; :: (int[_] * 'tyabbr0); `f]
        = "unix_sigprocmask"

external sigpending : unit[():Pre; `a] <[`b]-> 'tyabbr0
     with 'tyabbr0 = int[_] list[[]:Pre; :: (int[_] * 'tyabbr0); `c]
    = "unix_sigpending"

external sigsuspend : int[`a] list[`b] <[`c]-> unit[():Pre; `d]
    = "unix_sigsuspend"

let pause() =
  let sigs = sigprocmask SIG_BLOCK [] in sigsuspend sigs

type process_times =
  { tms_utime : float;
    tms_stime : float;
    tms_cutime : float;
    tms_cstime : float }

type tm =
  { tm_sec : int;
    tm_min : int;
    tm_hour : int;
    tm_mday : int;
    tm_mon : int;
    tm_year : int;
    tm_wday : int;
    tm_yday : int;
    tm_isdst : bool }

external time : unit[() :Pre; `a] <[`b]-> float[_] = "unix_time"
external gettimeofday : unit[() :Pre; `a] <[`b]-> float[_]
    = "unix_gettimeofday"

external gmtime : float[`a] <[`b]->
    tm
    [{ tm_hour : int[_];  tm_isdst : bool[false:Pre; true:Pre; `c]; 
       tm_mday : int[_];  tm_min : int[_];  tm_mon : int[_]; 
       tm_sec : int[_];  tm_wday : int[_];  tm_yday : int[_]; 
       tm_year : int[_] }; `d] = "unix_gmtime"

external localtime : float[`a] <[`b]->
    tm
    [{ tm_hour : int[_];  tm_isdst : bool[false:Pre; true:Pre; `c]; 
       tm_mday : int[_];  tm_min : int[_];  tm_mon : int[_]; 
       tm_sec : int[_];  tm_wday : int[_];  tm_yday : int[_]; 
       tm_year : int[_] }; `d]  = "unix_localtime"

external mktime : tm[`a] <[`b]->
    float[_] *
    tm
    [{ tm_hour : int[_];  tm_isdst : bool[false:Pre; true:Pre; `c]; 
       tm_mday : int[_];  tm_min : int[_];  tm_mon : int[_]; 
       tm_sec : int[_];  tm_wday : int[_];  tm_yday : int[_]; 
       tm_year : int[_] }; `d]  = "unix_mktime"

external alarm : int[`a] <[`b]-> int[_] = "unix_alarm"
external sleep : int[`a] <[`b]-> unit[() :Pre; `b] = "unix_sleep"

external times : unit[`a] <[`b]->
  process_times
    [{ tms_utime : float[_] ;
       tms_stime : float[_] ;
       tms_cutime : float[_] ;
       tms_cstime : float[_] }; `c] = "unix_times"

external utimes : string[`a] <[`b]-> float[`c] <[`d]->
  float[`e] <[`f]-> unit[() :Pre; `g] = "unix_utimes"

type interval_timer =
    ITIMER_REAL
  | ITIMER_VIRTUAL
  | ITIMER_PROF

type interval_timer_status =
  { it_interval: float;                 (* Period *)
    it_value: float }                   (* Current value of the timer *)


external getitimer: interval_timer[`a] <[`b]->
  interval_timer_status
    [{ it_interval: float[_] ;
       it_value: float[_] }; `c] = "unix_getitimer"

external setitimer: interval_timer[`a] <[`b]->
  interval_timer_status[`c] <[`d]->
    interval_timer_status
      [{ it_interval: float[_] ;
	 it_value: float[_] }; `e] 
  = "unix_setitimer"


external getuid : unit[() :Pre;`a] <[`b]-> int[_] = "unix_getuid"
external geteuid : unit[() :Pre;`a] <[`b]-> int[_] = "unix_geteuid"
external setuid : int[`a] <[`b]-> unit[() :Pre; `c] = "unix_setuid"
external getgid : unit[() :Pre;`a] <[`b]-> int[_] = "unix_getgid"
external getegid : unit[() :Pre;`a] <[`b]-> int[_] = "unix_getegid"
external setgid : int[`a] <[`b]-> unit[() :Pre; `c] = "unix_setgid"
external getgroups : unit[() :Pre;`a] <[`b]-> int[_] array[_]
    = "unix_getgroups"

type passwd_entry =
  { pw_name : string;
    pw_passwd : string;
    pw_uid : int;
    pw_gid : int;
    pw_gecos : string;
    pw_dir : string;
    pw_shell : string }

type group_entry =
  { gr_name : string;
    gr_passwd : string;
    gr_gid : int;
    gr_mem : string array }


external getlogin : unit[() :Pre;`a] <[`b]-> string[_] = "unix_getlogin"

external getpwnam : string[`a] <[`b]->
  passwd_entry
    [ { pw_name : string[_];
	pw_passwd : string[_];
	pw_uid : int[_];
	pw_gid : int[_];
	pw_gecos : string[_];
	pw_dir : string[_];
	pw_shell : string[_] }; `c] = "unix_getpwnam"

external getgrnam : string[`a] <[`b]->
  group_entry
    [{ gr_name : string[_];
       gr_passwd : string[_];
       gr_gid : int[_];
       gr_mem : string[_] array[_] }; `c] = "unix_getgrnam"

external getpwuid : int[`a] <[`b]->
  passwd_entry
    [ { pw_name : string[_];
	pw_passwd : string[_];
	pw_uid : int[_];
	pw_gid : int[_];
	pw_gecos : string[_];
	pw_dir : string[_];
	pw_shell : string[_] }; `c] = "unix_getpwuid"

external getgrgid : int[`a] <[`b]->
  group_entry
    [{ gr_name : string[_];
       gr_passwd : string[_];
       gr_gid : int[_];
       gr_mem : string[_] array[_] }; `c] = "unix_getgrgid"


type inet_addr

external inet_addr_of_string : string[`a]
      <[__Failure13 (string["inet_addr_of_string":Pre;`b]);`c]-> inet_addr[_]
          = "unix_inet_addr_of_string"
external string_of_inet_addr : inet_addr[`a]
    <[__Failure13 (string["string_of_inet_addr":Pre;`b]);`c]-> string[_]
        = "unix_string_of_inet_addr"

let inet_addr_any = inet_addr_of_string "0.0.0.0"

type socket_domain =
    PF_UNIX
  | PF_INET

type socket_type =
    SOCK_STREAM
  | SOCK_DGRAM
  | SOCK_RAW
  | SOCK_SEQPACKET

type sockaddr =
    ADDR_UNIX of string
  | ADDR_INET of inet_addr * int

type shutdown_command =
    SHUTDOWN_RECEIVE
  | SHUTDOWN_SEND
  | SHUTDOWN_ALL

type msg_flag =
    MSG_OOB
  | MSG_DONTROUTE
  | MSG_PEEK

type socket_option =
    SO_DEBUG
  | SO_BROADCAST
  | SO_REUSEADDR
  | SO_KEEPALIVE
  | SO_DONTROUTE
  | SO_OOBINLINE

external socket : socket_domain[`a] <[`b]-> socket_type[`c] <[`d]->
	    int[`e] <[`f]-> int[_] = "unix_socket"
external socketpair : socket_domain[`a] <[`b]-> socket_type[`c] <[`d]->
	int[`e] <[`f]-> int[_] * int[_]  = "unix_socketpair"
external accept : int[`a] <[`b]->
  int[_] * sockaddr[ADDR_UNIX(string[_]);
                           ADDR_INET(inet_addr[_] * int[_]);`c] = "unix_accept"

external bind : int[`a] <[`b]-> sockaddr[`c] <[`d]->
  unit[() :Pre;`e] = "unix_bind"
external connect : int[`a] <[`b]-> sockaddr[`c] <[`d]->
  unit[() :Pre;`e] = "unix_connect"
external listen : int[`a] <[`b]-> int[`c] <[`d]->
  unit[() :Pre;`e] = "unix_listen"
external shutdown : int[`a] <[`b]-> shutdown_command[`c] <[`d]->
  unit[() :Pre;`e] = "unix_shutdown" 
external getsockname : int[`a] <[`b]->
  sockaddr[ADDR_UNIX(string[_]);
           ADDR_INET(inet_addr[_] * int[_]);`c] = "unix_getsockname"
external getpeername : int[`a] <[`b]->
  sockaddr[ADDR_UNIX(string[_]);
           ADDR_INET(inet_addr[_] * int[_]);`c] = "unix_getpeername"

(* Indeed, file_descr = int *)
external unsafe_recv :
  int[`a] <[`b]-> string[`c] <[`d]-> int[`e] <[`f]->
    int[`g] <[`h]-> msg_flag[`i] list[`j] <[`k]-> int[_] = "unix_recv"


external unsafe_recvfrom :
   int[`a] <[`b]-> string[`c] <[`d]-> int[`e] <[`f]->
     int[`g] <[`h]-> msg_flag[`i] list[`j] <[`k]->
       int[_] * sockaddr
	 [ADDR_UNIX(string[_]); ADDR_INET(inet_addr[_] * int[_]);`l]
     = "unix_recvfrom"

external unsafe_send :
	 int[`a] <[`b]-> string[`c] <[`d]-> int[`e] <[`f]->
	   int[`g] <[`h]-> msg_flag[`i] list[`j] <[`k]-> int[_]
     = "unix_send"

external unsafe_sendto :
	 int[`a] <[`b]-> string[`c] <[`d]-> int[`e] <[`f]->
	   int[`g] <[`h]-> msg_flag[`i] list[`j] <[`k]-> sockaddr[`l]
	       <[`m]-> int[_]
     = "unix_sendto" "unix_sendto_native"


let recv fd buf ofs len flags =
  if len < 0 or ofs + len > String.length buf
  then invalid_arg "Unix.recv"
  else unsafe_recv fd buf ofs len flags
let recvfrom fd buf ofs len flags =
  if len < 0 or ofs + len > String.length buf
  then invalid_arg "Unix.recvfrom"
  else unsafe_recvfrom fd buf ofs len flags
let send fd buf ofs len flags =
  if len < 0 or ofs + len > String.length buf
  then invalid_arg "Unix.send"
  else unsafe_send fd buf ofs len flags
let sendto fd buf ofs len flags addr =
  if len < 0 or ofs + len > String.length buf
  then invalid_arg "Unix.sendto"
  else unsafe_sendto fd buf ofs len flags addr


external getsockopt : int[`a] <[`b]->
  socket_option[`c] <[`d]-> bool[false:Pre;true:Pre;`e] = "unix_getsockopt"
external setsockopt : int[`a] <[`b]->
  socket_option[`c] <[`d]-> bool[`e] <[`f]-> unit[() :Pre;`g]
      = "unix_setsockopt"
type host_entry =
  { h_name : string;
    h_aliases : string array;
    h_addrtype : socket_domain;
    h_addr_list : inet_addr array }

type protocol_entry =
  { p_name : string;
    p_aliases : string array;
    p_proto : int }

type service_entry =
  { s_name : string;
    s_aliases : string array;
    s_port : int;
    s_proto : string }

external gethostname : unit[() :Pre;`a] <[__Not_found22:Pre;`b]->
  string[_] = "unix_gethostname"
external gethostbyname : string[`a] <[`b]->
  host_entry
    [{ h_addr_list : inet_addr[_] array[_];
       h_addrtype : socket_domain[PF_INET:Pre; PF_UNIX:Pre; `c]; 
       h_aliases : string[_] array[_];  h_name : string[_] }; `d]
    = "unix_gethostbyname"
external gethostbyaddr : inet_addr[`a] <[`b]->
  host_entry
    [{ h_addr_list : inet_addr[`c] array[_];
       h_addrtype : socket_domain[PF_INET:Pre; PF_UNIX:Pre; `d]; 
       h_aliases : string[_] array[_];  h_name : string[_] }; `e]
    = "unix_gethostbyaddr"
external getprotobyname : string[`a] <[`b]->
  protocol_entry
    [{ p_name : string[_] ;
       p_aliases : string[_] array[_];
       p_proto : int[_] }; `c]
    = "unix_getprotobyname"
external getprotobynumber : int [`a] <[`b]->
  protocol_entry
    [{ p_name : string[_] ;
       p_aliases : string[_] array[_];
       p_proto : int[_] }; `c]
    = "unix_getprotobynumber"
external getservbyname : string[`a] <[`b]-> string[`c] <[`d]->
  service_entry
    [{ s_name : string[_];
       s_aliases : string[_] array[_];
       s_port : int[_];
       s_proto : string[_] }; `e]
    = "unix_getservbyname"
external getservbyport : int[`a] <[`b]-> string[`c] <[`d]->
  service_entry
    [{ s_name : string[_];
       s_aliases : string[_] array[_];
       s_port : int[_];
       s_proto : string[_] }; `e]
    = "unix_getservbyport"


type terminal_io = {
    mutable c_ignbrk: bool;
    mutable c_brkint: bool;
    mutable c_ignpar: bool;
    mutable c_parmrk: bool;
    mutable c_inpck: bool;
    mutable c_istrip: bool;
    mutable c_inlcr: bool;
    mutable c_igncr: bool;
    mutable c_icrnl: bool;
    mutable c_ixon: bool;
    mutable c_ixoff: bool;
    mutable c_opost: bool;
    mutable c_obaud: int;
    mutable c_ibaud: int;
    mutable c_csize: int;
    mutable c_cstopb: int;
    mutable c_cread: bool;
    mutable c_parenb: bool;
    mutable c_parodd: bool;
    mutable c_hupcl: bool;
    mutable c_clocal: bool;
    mutable c_isig: bool;
    mutable c_icanon: bool;
    mutable c_noflsh: bool;
    mutable c_echo: bool;
    mutable c_echoe: bool;
    mutable c_echok: bool;
    mutable c_echonl: bool;
    mutable c_vintr: char;
    mutable c_vquit: char;
    mutable c_verase: char;
    mutable c_vkill: char;
    mutable c_veof: char;
    mutable c_veol: char;
    mutable c_vmin: int;
    mutable c_vtime: int;
    mutable c_vstart: char;
    mutable c_vstop: char
  }


(* Indeed, file_descr = int *)
external tcgetattr: int[`zz] <[`zzz]->
   terminal_io
   [{ c_brkint : bool[false:Pre; true:Pre; `a]; 
   c_clocal : bool[false:Pre; true:Pre; `b]; 
   c_cread : bool[false:Pre; true:Pre; `c];  c_csize : int[_]; 
   c_cstopb : int[_];  c_echo : bool[false:Pre; true:Pre; `d]; 
   c_echoe : bool[false:Pre; true:Pre; `e]; 
   c_echok : bool[false:Pre; true:Pre; `f]; 
   c_echonl : bool[false:Pre; true:Pre; `g]; 
   c_hupcl : bool[false:Pre; true:Pre; `h];  c_ibaud : int[_]; 
   c_icanon : bool[false:Pre; true:Pre; `i]; 
   c_icrnl : bool[false:Pre; true:Pre; `j]; 
   c_ignbrk : bool[false:Pre; true:Pre; `k]; 
   c_igncr : bool[false:Pre; true:Pre; `l]; 
   c_ignpar : bool[false:Pre; true:Pre; `m]; 
   c_inlcr : bool[false:Pre; true:Pre; `n]; 
   c_inpck : bool[false:Pre; true:Pre; `o]; 
   c_isig : bool[false:Pre; true:Pre; `p]; 
   c_istrip : bool[false:Pre; true:Pre; `q]; 
   c_ixoff : bool[false:Pre; true:Pre; `r]; 
   c_ixon : bool[false:Pre; true:Pre; `s]; 
   c_noflsh : bool[false:Pre; true:Pre; `t];  c_obaud : int[_]; 
   c_opost : bool[false:Pre; true:Pre; `u]; 
   c_parenb : bool[false:Pre; true:Pre; `v]; 
   c_parmrk : bool[false:Pre; true:Pre; `w]; 
   c_parodd : bool[false:Pre; true:Pre; `x];  c_veof : char[_]; 
   c_veol : char[_];  c_verase : char[_];  c_vintr : char[_]; 
   c_vkill : char[_];  c_vmin : int[_];  c_vquit : char[_]; 
   c_vstart : char[_];  c_vstop : char[_];  c_vtime : int[_] }; `y]
 = "unix_tcgetattr"

type setattr_when = TCSANOW | TCSADRAIN | TCSAFLUSH

external tcsetattr: int[`a] <[`b]-> setattr_when[`c] <[`d]->
  terminal_io[`e] <[`f]-> unit[() :Pre;`g] = "unix_tcsetattr"
external tcsendbreak: int[`a] <[`b]-> int[`c] <[`d]->
  unit[() :Pre;`e] = "unix_tcsendbreak"
external tcdrain: int[`a] <[`b]-> unit[() :Pre;`c] = "unix_tcdrain"

type flush_queue = TCIFLUSH | TCOFLUSH | TCIOFLUSH

external tcflush: int[`a] <[`b]-> flush_queue[`c] <[`d]->
  unit[() :Pre;`e] = "unix_tcflush"

type flow_action = TCOOFF | TCOON | TCIOFF | TCION

external tcflow: int[`a] <[`b]-> flow_action[`c] <[`d]->
  unit[() :Pre;`e] = "unix_tcflow"

external setsid : unit[() :Pre;`a] <[`b]-> int[_] = "unix_setsid"

(* High-level process management (system, popen) *)

let system cmd =
  match fork() with
     0 -> execv "/bin/sh" [| "/bin/sh"; "-c"; cmd |];
          exit 127
  | id -> snd(waitpid [] id)

let perform_redirections new_stdin new_stdout new_stderr =
  if new_stdin <> stdin then begin
    dup2 new_stdin stdin; close new_stdin
  end;
  if new_stdout <> stdout then begin
    dup2 new_stdout stdout; close new_stdout
  end;
  if new_stderr <> stderr then begin
    dup2 new_stderr stderr; close new_stderr
  end

let create_process cmd args new_stdin new_stdout new_stderr =
  match fork() with
    0 ->
      perform_redirections new_stdin new_stdout new_stderr;
      execvp cmd args;
      exit 127
  | id -> id

let create_process_env cmd args env new_stdin new_stdout new_stderr =
  match fork() with
    0 ->
      perform_redirections new_stdin new_stdout new_stderr;
      execvpe cmd args env;
      exit 127
  | id -> id

type popen_process =
    Process of in_channel * out_channel
  | Process_in of in_channel
  | Process_out of out_channel

let popen_processes = (Hashtbl.create 7 : (popen_process, int) Hashtbl.t)

let open_proc cmd proc input output toclose =
  match fork() with
     0 -> if input <> stdin then begin dup2 input stdin; close input end;
          if output <> stdout then begin dup2 output stdout; close output end;
          List.iter close toclose;
          execv "/bin/sh" [| "/bin/sh"; "-c"; cmd |];
          exit 127
  | id -> Hashtbl.add popen_processes proc id

let open_process_in cmd =
  let (in_read, in_write) = pipe() in
  let inchan = in_channel_of_descr in_read in
  open_proc cmd (Process_in inchan) stdin in_write [in_read];
  close in_write;
  inchan

let open_process_out cmd =
  let (out_read, out_write) = pipe() in
  let outchan = out_channel_of_descr out_write in
  open_proc cmd (Process_out outchan) out_read stdout [out_write];
  close out_read;
  outchan

let open_process cmd =
  let (in_read, in_write) = pipe() in
  let (out_read, out_write) = pipe() in
  let inchan = in_channel_of_descr in_read in
  let outchan = out_channel_of_descr out_write in
  open_proc cmd (Process(inchan, outchan)) out_read in_write
                                           [in_read; out_write];
  close out_read;
  close in_write;
  (inchan, outchan)

let find_proc_id fun_name proc =
  try
    let pid = Hashtbl.find popen_processes proc in
    Hashtbl.remove popen_processes proc;
    pid
  with Not_found ->
    raise(Unix_error(EBADF, fun_name, ""))

let close_process_in inchan =
  let pid = find_proc_id "close_process_in" (Process_in inchan) in
  close_in inchan;
  snd(waitpid [] pid)

let close_process_out outchan =
  let pid = find_proc_id "close_process_out" (Process_out outchan) in
  close_out outchan;
  snd(waitpid [] pid)

let close_process (inchan, outchan) =
  let pid = find_proc_id "close_process" (Process(inchan, outchan)) in
  close_in inchan; close_out outchan;
  snd(waitpid [] pid)

(* High-level network functions *)

let open_connection sockaddr =
  let domain =
    match sockaddr with ADDR_UNIX _ -> PF_UNIX | ADDR_INET(_,_) -> PF_INET in
  let sock =
    socket domain SOCK_STREAM 0 in
  try
    connect sock sockaddr;
    (in_channel_of_descr sock, out_channel_of_descr sock)
  with exn ->
    close sock; raise exn

let shutdown_connection inchan =
  shutdown (descr_of_in_channel inchan) SHUTDOWN_SEND

let establish_server server_fun sockaddr =
  let domain =
    match sockaddr with ADDR_UNIX _ -> PF_UNIX | ADDR_INET(_,_) -> PF_INET in
  let sock =
    socket domain SOCK_STREAM 0 in
  bind sock sockaddr;
  listen sock 3;
  while true do
    let (s, caller) = accept sock in
    (* The "double fork" trick, the process which calls server_fun will not
       leave a zombie process *)
    match fork() with
       0 -> if fork() <> 0 then exit 0; (* The son exits, the grandson works *)
            let inchan = in_channel_of_descr s in
            let outchan = out_channel_of_descr s in
            server_fun inchan outchan;
            close_in inchan;
            close_out outchan
    | id -> close s; ignore(waitpid [] id) (* Reclaim the son *)
  done

