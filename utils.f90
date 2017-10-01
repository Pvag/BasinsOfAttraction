module utils

  use dataFiles
  use mathData

  implicit none

  integer :: ioStatus
  integer, parameter :: NO_ERROR_STATUS = 0
  character(len=*), parameter :: sc = '=========='
  character(len=*), parameter :: ls = '----------'

  contains

  subroutine progressionIndicator(progress)
    integer, intent(in) :: progress
    ! NOTE char(13) is the carriage return: it makes
    !      the program overwrite the same line !
    write(*,101,advance='no') "Computation Progress: ", progress, "%" // char(13)
    101 format(x,a,i3,a)
    if (progress .eq. 100) print *,
  end subroutine progressionIndicator

  subroutine printPoint(point, infoFileUnit)
    integer, intent(in) :: infoFileUnit
    double complex, intent(in) :: point

    write(infoFileUnit, 102) "(", real(point), ", ", aimag(point), ")"
    102 format(a,f10.4,a,f10.4,a)
  end subroutine printPoint

  subroutine checkIoStatus(ioStatus)
    integer, intent(in) :: ioStatus
    if (ioStatus .ne. 0) then
      print *, "Error with i/o: ", ioStatus
    end if
  end subroutine checkIoStatus

  subroutine minusSep(fileUnit, andNumberOfBlankLines)
    integer, intent(in), optional :: fileUnit, andNumberOfBlankLines

    if (present(fileUnit)) then
      write(fileUnit, *) ls
      if (present(andNumberOfBlankLines)) then
        call insertBlankLines(fileUnit, andNumberOfBlankLines)
      end if
    else
      write(*,*) ls
    end if
  end subroutine minusSep

  subroutine insertBlankLines(fileUnit, nOfBlankLines)
    integer, intent(in), optional :: fileUnit, nOfBlankLines
    integer :: i

    if (present(fileUnit)) then
      if (present(nOfBlankLines)) then
        do i = 1, nOfBlankLines
          write(fileUnit, *)
        end do
      else
        write(fileUnit, *)
      end if
    else  
      write(*, *)
    end if
  end subroutine insertBlankLines

  subroutine equalSep(fileUnit, andBlankLines)
    integer, intent(in), optional :: fileUnit, andBlankLines
    if (present(fileUnit)) then
      write(fileUnit, *) sc
      if (present(andBlankLines)) then
        call insertBlankLines(fileUnit, andBlankLines)
      end if
    else
      write(*,*) sc
    end if
  end subroutine equalSep

  subroutine fuffa()
    integer :: theFuffa
    read(*,*) theFuffa
  end subroutine fuffa

  subroutine greetUser()
    call insertBlankLines()
    write(*, *) "* This program finds informations about roots "
    write(*, *) "  and basins of attraction of given equations "
    write(*, *) "  and stores them to different files."
    write(*, *) "  -"
    write(*, *) "  Ok my friend, let's crunch some numbers ! *"
    call insertBlankLines()
  end subroutine greetUser

  logical function answerIsYes(question)
    ! Just end your question with a question mark, no space needed.
    character(len=*) :: question
    character(len=1) :: yn
    answerIsYes = .false.
    write(*, 100, advance="no") question
    read(*, *) yn
    if (yn.eq.'y' .or. yn.eq.'Y') answerIsYes = .true.
    100 format(x,a,x,"(y/n) : ")
  end function answerIsYes

end module utils
