AC_DEFUN([AX_F08_NEWUNIT], [

AC_REQUIRE([AC_PROG_FC])
AC_LANG_PUSH([Fortran])

AC_MSG_CHECKING([for NEWUNIT support])
AC_COMPILE_IFELSE([
       program conftest
       integer :: i
       open(newunit=i,file='test')
       end program conftest],
[ax_f08_newunit=yes], [ax_f08_newunit=no])

AC_LANG_POP([Fortran])

if test "x$ax_f08_newunit" = "xyes"; then
    AC_MSG_RESULT([yes])
    AC_DEFINE([HAVE_NEWUNIT], [1], [NEWUNIT support])
else
    AC_MSG_RESULT([no])
fi

])
