module mathUtil
  implicit none

  ! type definitions and variable declarations

  type grid
    double complex :: topRight
    double complex :: bottomLeft
    double precision :: delta, tol
    logical :: gridCompleted
    double complex, dimension(15) :: roots ! TODO NO hard-coded !
    integer :: nRoots
    real :: tInspectGridStart, tInspectGridEnd
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

  type files
    character (len=300) :: basinsFile, rootsFile, outputImageFile
  end type files

  type(grid) :: gp
  type(files) :: analysisFiles
  double precision :: rightX, rightY, leftX, leftY, deltaR
  character (len=1) :: gnuplotDrawYN, openOutputImageYN

  character (len=*), parameter :: sc = "==========================="
  character (len=*), parameter :: ls = "---------------------------"

  ! implementation of functions and procedure

  contains

  subroutine initGridAndData(basinsFile, rootsFile, outputImageFile, f, df)
    ! TODO check if these interfaces are still needed
    ! The interface is needed in order to be able
    ! to pass functions as arguments.
    ! These don't need to be changed when you change
    ! the complex function (and derivative) in main.f90
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

    character (len=*) :: basinsFile
    character (len=*) :: rootsFile, outputImageFile

    ! TODO Check the next 2 lines !
    gp%f => f
    gp%df => df

    call equalSep()
    write(*,*) "Initialization of the grid"
    gp%gridCompleted = .false.
    gp%roots = 0.d0
    call askLimitsDeltaTol()
    ! TODO insert a while loop, to iterate until the user is satisfied with input
    gp%topRight   = complex(rightX, rightY)
    gp%bottomLeft = complex(leftX, leftY)
    call echoLimits()
    analysisFiles%basinsFile = basinsFile
    analysisFiles%rootsFile = rootsFile
    analysisFiles%outputImageFile = outputImageFile
  end subroutine initGridAndData

  ! TODO use the grid type
  subroutine askLimitsDeltaTol()
    call minusSep()
    write(*,*) "Insert the value of top right x:"
    read(*,*) rightX
    write(*,*) "Insert the value of top right y:"
    read(*,*) rightY
    write(*,*) "Insert the value of bottom left x:"
    read(*,*) leftX
    write(*,*) "Insert the value of bottom left y:"
    read(*,*) leftY
    write(*,*) "Insert the value for the delta between points of the grid (e.g. 1.d-2):"
    read(*,*) gp%delta
    write(*,*) "Insert the value for the tolerance for convergence (N.R., e.g. 1.d-10):"
    read(*,*) gp%tol
  end subroutine askLimitsDeltaTol

  subroutine echoLimits()
    call equalSep()
    write(*,*) "You have set these parameters:"
    call minusSep()
    write(*,*) "Top right corner (x, y): ", gp%topRight
    write(*,*) "Bottom left corner (x, y): ", gp%bottomLeft
    write(*,*) "Delta between points of the grid (both x and y directions):", gp%delta
    write(*,*) "Convergence (N.R.) considered reached at:", gp%tol
  end subroutine echoLimits

  subroutine minusSep()
    write(*,*) ls
  end subroutine minusSep
  
  subroutine equalSep()
    write(*,*) sc
  end subroutine equalSep

  double complex function nextPoint(p, gp)
    ! Return next point, starting from top left,
    ! proceeding right, row by row, until the
    ! point at bottom right is reached.
    double complex, intent(in) :: p ! the actual point
    double precision :: oldRe, oldIm, newRe, newIm
    type(grid) :: gp
    
    oldRe = real(p)
    oldIm = aimag(p)
    newRe = oldRe + gp%delta 
    ! right edge not exceeded
    if (newRe <= real(gp%topRight)) then
      nextPoint = complex(newRe, oldIm)
    else ! right edge exceeded
      newIm = oldIm - gp%delta ! decrease y
      ! bottom edge not exceeded
      if (newIm >= aimag(gp%bottomLeft)) then
        nextPoint = complex(real(gp%bottomLeft), newIm)
      else ! both edges exceeded
        gp%gridCompleted = .true.
      end if
    end if
  end function nextPoint

  subroutine exploreGrid()
    double complex :: z0, root
    integer :: iter = 0, rn, io, i
    logical :: valid

    open(unit=1, file=analysisFiles%basinsFile, iostat=io, action="write")
    call equalSep()
    write(*,*) "Grid inspection started."
    call cpu_time(gp%tInspectGridStart)
    ! first point (top left corner)
    z0 = complex( real(gp%bottomLeft), aimag(gp%topRight)  )
    do while (.not.gp%gridCompleted)
      call findRoot(z0, iter, root, gp%tol, valid)
      rn = rootNumber(root, gp, valid) ! rn : root number
      ! Write basins to basins.out
      write(1,"(2F25.19,2(2X,I3))") real(z0), aimag(z0), rn, iter
      z0 = nextPoint(z0, gp)
    end do
    call cpu_time(gp%tInspectGridEnd)
    close(1)

  end subroutine exploreGrid

  integer function rootNumber(root, gp, valid)
    ! Returns the root number
    double complex, intent(in) :: root
    type(grid), intent(inout) :: gp
    logical, intent(in) :: valid
    integer :: i
    logical :: found

    i = 0
    found = .false.
    if (.not.valid) then
      rootNumber = 0 ! root is not valid
    else
      ! Scan every root that has been found already 
      do while (i <= gp%nRoots .and. .not.found)
        i = i + 1
        found = match(root, gp%roots(i))
      end do
      ! If root was already in the root "database"
      if (found) then
        rootNumber = i
      else ! Add root as new root to the root "database"
        gp%nRoots = gp%nRoots + 1
        gp%roots(gp%nRoots) = root
        rootNumber = gp%nRoots
      end if
    end if
  end function rootNumber

  logical function match(z1, z2)
    double complex, intent(in) :: z1, z2
    double precision, parameter :: matchTol = 1.d-12 ! TODO experiment with different values

    ! TODO create a function to make this more clear
    if ( (dabs(real(z1)-real(z2)) < matchTol) .and. (dabs(aimag(z1)-aimag(z2)) < matchTol) ) then
      match = .true.
    else
      match = .false.
    end if
  end function match

  subroutine cleanRoots()
    ! Rounds to 0 everything below tol
    double complex :: temp
    double precision, parameter :: tol = 1.d-20
    double precision :: re, im
    integer :: i

    do i = 1, gp%nRoots
      temp = gp%roots(i)
      re = real(temp)
      im = aimag(temp)
      if (dabs(re) < tol) then
        re = 0.d0
      end if
      if (dabs(im) < tol) then
        im = 0.d0
      end if
      gp%roots(i) = complex(re, im)
    end do
  end subroutine cleanRoots

  subroutine printRoots()
    integer :: i

    call equalSep()
    write(*,*) "Roots found: index, value"
    call minusSep()
    do i = 1, gp%nRoots
      write(*,"(X,I2,2X,2F20.16)") i, gp%roots(i)
    end do
  end subroutine printRoots

  subroutine findRoot(z0, iter, root, tol, valid)
    double complex, intent(in)  :: z0   ! starting point
    double complex, intent(out) :: root ! attractor
    integer, intent(out) :: iter ! number of iterations needed to reach convergence
    logical, intent(out) :: valid ! false if the root was not found
    double precision, intent(in) :: tol
    double complex :: zOld, z, dz, denum
    logical :: converged ! TODO check if I can avoid this using an interface in the module
    integer, parameter :: maxIter = 300 ! TODO ask this to the user?
    double precision, parameter :: zero = 0.d0

    valid = .true.
    iter = 0
    zOld = z0
    dz = complex(1.d10, -1.d10) ! TODO find a better way to define this
    ! TODO rewrite this in a cleaner form
    do while( ( dabs(real(dz)) > tol .or. dabs(aimag(dz)) > tol )  .and.  valid )
      denum = gp%df(zOld)
      ! Avoid division by 0 and exceeding maximum number of iterations
      if ( dabs(real(denum)) > zero .or. dabs(aimag(denum)) > zero &
          .and. iter < maxIter) then
        z = zOld - gp%f(zOld)/denum
        dz = z - zOld
        zOld = z
        iter = iter + 1
      else
        valid = .false. ! iteration exceeded or division by 0
        ! Notify the user with the reason for not finding the root of this initial point
        call minusSep()
        if (iter == 0) then
          write(*,*) "! Warning: division by zero in N.R.! Root not found"
        else
          write(*,*) "! Warning: Convergence not reached after ", iter, " iterations"
        end if
        write(*,*) "!          for initial point: ", z0
      end if ! not valid initial point (division by zero or maxIter reached)
    end do ! N.R.
    root = z
  end subroutine findRoot

  ! TODO fix undefined symbol error
  logical function converged(dz, tol)
    double complex, intent(in) :: dz
    double precision, intent(in) :: tol
    
    if ( abs(real(dz)) > tol .and. abs(aimag(dz)) > tol ) then
      converged = .false.
    else
      converged = .true.
    end if
  end function converged

  subroutine outputRenderInspection()
  ! Ask the user if she wants to render the data
  ! and then if she wants to display it.
  ! If positive answers are given, the relative actions
  ! are performed.

    print *, "Do you want gnuplot to render the output file? (y/n)"
    read(*,*) gnuplotDrawYN
    if (gnuplotDrawYN .eq. 'y' .or. gnuplotDrawYN .eq. 'Y') then
      call gnuplotDraw()
    end if

    print *, "Do you want to open the rendere image? (y/n)"
    read(*,*) openOutputImageYN
    if (openOutputImageYN .eq. 'y' .or. openOutputImageYN .eq. 'Y') then
      call openOutputImage()
    end if

  end subroutine outputRenderInspection

  ! TODO implement
  subroutine gnuplotDraw()
    ! analysisFiles%basinsFile
  end subroutine gnuplotDraw

  ! TODO implement
  subroutine openOutputImage()
    ! analysisFiles%outputImageFile
  end subroutine openOutputImage

  subroutine printFinalInformations()
    write(*,*) "Grid inspection completed."
    write(*,*) "CPU time used to inspect the grid: ", gp%tInspectGridEnd - gp%tInspectGridStart, " s"
    write(*,*) "Basins informations written to file: ", analysisFiles%basinsFile
    write(*,*) "Roots informations written to file: ", analysisFiles%rootsFile
  end subroutine printFinalInformations

  subroutine writeRoots()
    integer :: i, io
    open(unit=1, file=analysisFiles%rootsFile, iostat=io, action="write")
    do i = 1, gp%nRoots
      write(1,"(X,I3,2(2X,F25.19))") i, real(gp%roots(i)), aimag(gp%roots(i))
    end do
    close(1)
  end subroutine writeRoots

  subroutine run()
    call exploreGrid()
    call cleanRoots()
    call writeRoots()
    call minusSep()
    call printFinalInformations()
    call printRoots()
    call outputRenderInspection()
  end subroutine run

end module mathUtil
