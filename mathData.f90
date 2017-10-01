module mathData
  ! Holds type definition and procedures about the data
  ! type used throughout the program to hold informations.

  implicit none

  character(len=*), parameter :: RENDER_NR = "RENDER_NR"
  character(len=*), parameter :: RENDER_ROOTS = "RENDER_ROOTS"

  type grid
    double complex :: topRight
    double complex :: bottomLeft
    double precision :: delta, tol
    logical :: gridCompleted
    double complex, dimension(15) :: roots ! TODO NO hard-coded !
    integer :: nRoots, pointsInGrid, run = 1
    real :: tInspectGridStart, tInspectGridEnd
    character(len=20) :: renderType
    procedure(ff), pointer, nopass :: f => null()
    procedure(dff), pointer, nopass :: df => null()
  end type grid

  abstract interface
    double complex function ff(z)
      double complex, intent(in) :: z
    end function ff
  end interface

  abstract interface
    double complex function dff(z)
      double complex, intent(in) :: z
    end function dff
  end interface

  type(grid) :: gp

  contains

  subroutine setFunction(f, df)
    interface fi
      double complex function f(z)
        double complex, intent(in) :: z
      end function f
    end interface fi

    interface di
      double complex function df(z)
        double complex, intent(in) :: z
      end function df
    end interface di

    gp%f  => f
    gp%df => df
  end subroutine setFunction
end module mathData
