module testsModule
  use utils

  implicit none

  contains

  subroutine t_progressionIndicator()
    integer :: i = 1

    do i = 1, 100
      call progressionIndicator(i)

      if (mod(i,10) .eq. 0) then
        call execute_command_line("sleep .1")
      end if

    end do
  end subroutine t_progressionIndicator

end module testsModule
