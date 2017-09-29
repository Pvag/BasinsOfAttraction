module utils

  implicit none

  contains

  subroutine progressionIndicator(progress)
    integer, intent(in) :: progress
    ! NOTE char(13) is the carriage return: it makes
    !      the program overwrite the same line !
    write(*,101,advance='no') "Computation Progress: ", progress, "%" // char(13)
    101 format(x,a,i3,a)
    if (progress .eq. 100) print *,
  end subroutine progressionIndicator

end module utils
