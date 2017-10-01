! TODO create a directory and put inside it the gnuplot script file
!      the basins.out file, the roots.out file and render.jpg .
!      The directory name should express the inspected area, like
!      'xM1yM2xm3ym4b500x600', meaning:
!      xMax = 1, yMax = 2, xMin = -3, yMin = -4, basins of
!         attractions, 500x600 pixels x pixels
!      I don't know how to code details of the function yet - maybe
!      I can use an external file.
! TODO Ask the user if she wants to render the basins of attractions,
!      or the iterations of N.R. .
! TODO Find an easier way to define / require a function.
! TODO Parallelize (OpenMP, MPI, CUDA)
! TODO Use graphic libraries
!         Make the drawing dynamic ! Inspect zones on the go !
! TODO Take the code that generates the gnuplot script file out
!      of the gnuplotDraw function ! Generate the script file,
!      anyways !
! TODO Write the informations about the data
!      inspected, the method used, and anything valuable
!      that the user may appreciate/need to a text file.

module mathUtil
  use utils
  use mathData
  use dataFiles

  implicit none
  
  logical :: alreadyRendered

  ! implementation of functions and procedure

  contains

!  subroutine initGridAndData(f, df)
  subroutine initGridAndData()
    ! TODO check if these interfaces are still needed
    ! The interface is needed in order to be able
    ! to pass functions as arguments.
    ! These don't need to be changed when you change
    ! the complex function (and derivative) in main.f90
!    interface fi
!      double complex function f(z)
!        double complex, intent(in) :: z
!      end function f
!    end interface fi
!
!    interface di 
!      double complex function df(z)
!        double complex, intent(in) :: z
!      end function df
!    end interface di
!
!    gp%f => f
!    gp%df => df

    alreadyRendered = .false.
    
    call greetUser()

    write(*,*) "Initialization of the grid"
    call equalSep()
    gp%gridCompleted = .false.
    gp%roots = 0.d0
    ! TODO insert a while loop, to iterate until the user is satisfied with input
    call askAndEchoLimitsDeltaTol()

    gp%pointsInGrid = computePointsInGrid()
  end subroutine initGridAndData

  subroutine askAndEchoLimitsDeltaTol()
    logical :: proceed = .false.

    do while (.not.proceed)
      call askLimitsDeltaTol()
      call echoLimitsDeltaTol()
      proceed = answerIsYes("Do you want to proceed with the analysis ?")
      call insertBlankLines()
    end do
  end subroutine askAndEchoLimitsDeltaTol

  subroutine askLimitsDeltaTol()
    double precision :: rightX, rightY, leftX, leftY, deltaR

    write(*,101,advance='no') "Insert the value of top right x:"
    read(*,*) rightX
    write(*,101,advance='no') "Insert the value of top right y:"
    read(*,*) rightY
    write(*,101,advance='no') "Insert the value of bottom left x:"
    read(*,*) leftX
    write(*,101,advance='no') "Insert the value of bottom left y:"
    read(*,*) leftY
    write(*,101,advance='no') "Insert the value for the delta between points of the grid (e.g. 1.d-2):"
    gp%topRight   = complex(rightX, rightY)
    gp%bottomLeft = complex(leftX, leftY)
    read(*,*) gp%delta
    write(*,101,advance='no') "Insert the value for the tolerance for convergence (N.R., e.g. 1.d-10):"
    101 format(x,a,x,i2)
    read(*,*) gp%tol
    call insertBlankLines()
  end subroutine askLimitsDeltaTol

  subroutine echoLimitsDeltaTol()
    write(*,*) "You have set these parameters for the analysis:"
    write(*,100) " - Top-right corner   (x, y): ", gp%topRight
    write(*,100) " - Bottom-left corner (x, y): ", gp%bottomLeft
    write(*,101) " - Distance between points of the grid (both x and y directions):", gp%delta
    write(*,101) " - Convergence (N.R.) considered reached at:", gp%tol
    call insertBlankLines()
    100 format(x,a,'(',f10.6,',',f10.6,')')
    101 format(x,a,es11.4)
  end subroutine echoLimitsDeltaTol

  double complex function nextPoint(p)
    ! Return next point, starting from top left,
    ! proceeding right, row by row, until the
    ! point at bottom right is reached.
    double complex, intent(in) :: p ! the actual point
    double precision :: oldRe, oldIm, newRe, newIm
    
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
    integer :: iter = 0, rn, io, pointIdx
    logical :: valid

    open(unit=1, file=basinsFile, iostat=io, action="write")
    write(*,*) "Grid inspection started."
    call equalSep()
    call cpu_time(gp%tInspectGridStart) ! TODO rename time vars

    pointIdx = 1

    ! first point (top left corner)
    z0 = complex( real(gp%bottomLeft), aimag(gp%topRight)  )
    do while (.not.gp%gridCompleted)
      call findRoot(z0, iter, root, valid)
      rn = rootNumber(root, gp, valid) ! rn : root number
      ! Write basins to basins.out
      write(1,"(2F25.19,2(2X,I3))") real(z0), aimag(z0), rn, iter
      z0 = nextPoint(z0)
      call progressionIndicator( int( pointIdx * 100 / gp%pointsInGrid ) )
      pointIdx = pointIdx + 1
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
    double precision, parameter :: cleanTol = 1.d-20
    double precision :: re, im
    integer :: i

    do i = 1, gp%nRoots
      temp = gp%roots(i)
      re = real(temp)
      im = aimag(temp)
      if (dabs(re) < cleanTol) then
        re = 0.d0
      end if
      if (dabs(im) < cleanTol) then
        im = 0.d0
      end if
      gp%roots(i) = complex(re, im)
    end do
  end subroutine cleanRoots

  subroutine printRoots()
    ! Prints the computed roots on screen.
    integer :: i

    write(*,*) "Roots found: Index, (Re, Im)"
    call equalSep()
    do i = 1, gp%nRoots
      write(*,"(X,I2,2X,'(',F12.6,', ',F12.6,')')") i, gp%roots(i)
    end do
  end subroutine printRoots

  subroutine findRoot(z0, iter, root, valid)
    double complex, intent(in)  :: z0   ! starting point
    double complex, intent(out) :: root ! attractor
    integer, intent(out) :: iter ! number of iterations needed to reach convergence
    logical, intent(out) :: valid ! false if the root was not found
    double complex :: zOld, z, dz, denum
    integer, parameter :: maxIter = 300 ! TODO ask this to the user?
    double precision, parameter :: zero = 0.d0

    valid = .true.
    iter = 0
    zOld = z0
    dz = complex(1.d10, -1.d10) ! TODO find a better way to define this
    ! TODO rewrite this in a cleaner form
    do while( ( dabs(real(dz)) > gp%tol .or. dabs(aimag(dz)) > gp%tol )  .and.  valid )
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

  logical function converged(dz)
    double complex, intent(in) :: dz
    
    if ( abs(real(dz)) > gp%tol .and. abs(aimag(dz)) > gp%tol ) then
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

    if (answerIsYes("Do you want gnuplot to render the output file?")) then
      call askRenderTypeAndPlot()
      alreadyRendered = .true.
    end if

  end subroutine outputRenderInspection

  subroutine askRenderTypeAndPlot()
    call askRenderType()
    call gnuplotDraw()
    if (answerIsYes("Do you want to open the rendered image?")) then
      call openOutputImage()
    end if
  end subroutine askRenderTypeAndPlot

  subroutine gnuplotDraw()
    integer :: hPixels, vPixels, io
    character(len=150) :: launchGnuplotBashLine
    double precision :: xMin, xMax, yMin, yMax

    xMin = real(gp%bottomLeft)
    xMax = real(gp%topRight)
    yMin = aimag(gp%bottomLeft)
    yMax = aimag(gp%topRight)
    ! write the gnuplot script file with proper window size
    hPixels = int( ( xMax - xMin ) / gp%delta )
    vPixels = int( ( yMax - yMin ) / gp%delta )

    ! Generate the gnuplot script file
    open(unit=1, file=trim(gnuplotScriptFile), iostat=io, action="write")
    write(1,*) "set term png transparent size ", hPixels, ",", vPixels 
    write(1,*) "set output '", trim(renderImageFile), "'"
    write(1,*) "set lmargin ", xMin 
    write(1,*) "set bmargin ", yMin
    write(1,*) "set tmargin ", yMax
    write(1,*) "set rmargin ", xMax
    write(1,*) "unset border"
    write(1,*) "unset xtics"
    write(1,*) "unset ytics"
    write(1,*) "rgb(r,g,b) = 50*int(r) + 5*int(g) + 100*int(b)"
    if (trim(gp%renderType) .eq. RENDER_NR) then
      write(1,*) "plot '", trim(basinsFile), "' using 1:2:(rgb($4,$4,$4)) with dots lc rgb variable"
    else if (trim(gp%renderType) .eq. RENDER_ROOTS) then
      write(1,*) "plot '", trim(basinsFile), "' using 1:2:(rgb($3,$3,$3)) with dots lc rgb variable"
    end if
    close(1)
    ! launch gnuplot gnuplotScriptFile
    write(launchGnuplotBashLine,*) "gnuplot ", trim(gnuplotScriptFile)

    call execute_command_line(trim(launchGnuplotBashLine))
  end subroutine gnuplotDraw

  subroutine openOutputImage()
    ! 
    character(len=300) :: openImageInOSXBashCommand
    write(openImageInOSXBashCommand,*) "open ", trim(renderImageFile)
    call execute_command_line(trim(openImageInOSXBashCommand))
  end subroutine openOutputImage

  subroutine printFinalInformations()
    write(*,*) "Grid inspection completed."
    write(*,*) "CPU time used to inspect the grid: ", gp%tInspectGridEnd - gp%tInspectGridStart, " s"
    write(*,*) "Basins informations written to file: ", trim(basinsFile)
    write(*,*) "Roots  informations written to file: ", trim(rootsFile)
    call insertBlankLines()
  end subroutine printFinalInformations

  subroutine writeRoots()
    ! Writes roots to file set in the rootsFile variable
    ! (specified in dataFiles.f90)
    integer :: i, io
    open(unit=1, file=rootsFile, iostat=io, action="write")
    do i = 1, gp%nRoots
      write(1,"(X,I3,2(2X,F25.19))") i, real(gp%roots(i)), aimag(gp%roots(i))
    end do
    close(1)
  end subroutine writeRoots

  integer function computePointsInGrid()
    integer :: pointsInGridH, pointsInGridV

    pointsInGridH = int( ( real(gp%topRight) - real(gp%bottomLeft) ) / &
                      gp%delta )
    pointsInGridV = int( ( aimag(gp%topRight) - aimag(gp%bottomLeft) ) / &
                      gp%delta )
    computePointsInGrid = pointsInGridH * pointsInGridV
  end function computePointsInGrid

  subroutine writeAnalysisInfoToFiles()
    integer, parameter :: infoFileUnit = 1000

    open(infoFileUnit, file=trim(reportFile), iostat=ioStatus, &
         action="write")
    call checkIoStatus(ioStatus)

    VALID_OPEN: if (ioStatus .eq. NO_ERROR_STATUS) then
      ! what and how to write
      write(infoFileUnit, *) "Basins of Attraction Analysis"
      call minusSep(infoFileUnit, 1)
      call writeWindowExtentsTo(infoFileUnit)
      call insertBlankLines(infoFileUnit)

      write(infoFileUnit, 100) "Distance between points (both " &
        // "x and y directions)", gp%delta
      write(infoFileUnit, 100) "Tolerance for the N.R. method: ", &
        gp%tol
      call insertBlankLines(infoFileUnit)

      call writeRenderType(infoFileUnit)
      100 format(x,a,es12.5)
    end if VALID_OPEN

    close(infoFileUnit, iostat=ioStatus)
    call checkIoStatus(ioStatus)

    call writeRoots()
  end subroutine writeAnalysisInfoToFiles

  subroutine writeRenderType(infoFileUnit)
    integer, intent(in) :: infoFileUnit
    write(infoFileUnit, 101, advance='no') "Render type: "
    if (trim(gp%renderType) .eq. RENDER_NR) then
      write(infoFileUnit, *) "N.R. basins."
    else if (trim(gp%renderType) .eq. RENDER_ROOTS) then
      write(infoFileUnit, *) "roots basins."
    end if
    101 format(x,a)
  end subroutine writeRenderType

  subroutine writeWindowExtentsTo(infoFileUnit)
    ! Writes info to file
    integer, intent(in) :: infoFileUnit

    write(infoFileUnit,*) "Rectangular grid inspected:"
    call minusSep(infoFileUnit)
    write(infoFileUnit, 101, advance="no") "Top-right point in grid: "
    call printPoint(gp%topRight, infoFileUnit)
    write(infoFileUnit, 102, advance="no") "Bottom-left point in grid: "
    call printPoint(gp%bottomLeft, infoFileUnit)
    101 format(3x,a)
    102 format(1x,a)
  end subroutine writeWindowExtentsTo

  subroutine run()
    logical :: programShouldTerminate = .false.
    do while (.not.programShouldTerminate)
      call initGridAndData()
      call exploreGrid() ! actual computations of roots
      call cleanRoots()
      call printFinalInformations()
      call printRoots()
      call outputRenderInspection()
      call writeAnalysisInfoToFiles()
      if (alreadyRendered) call moreRenders()
      if (.not.answerIsYes("Do you want to perform a new analysis ?")) then
        programShouldTerminate = .true.
        call quittingMessage()
      end if
      gp%run = gp%run + 1
    end do
  end subroutine run

  subroutine quittingMessage()
    write(*, *) "Program is quitting."
  end subroutine quittingMessage

  subroutine moreRenders()
    logical :: wantsMore = .false.
    do while (answerIsYes("Do you want to perform more renders "&
        //"of the same data set ?"))
      call askRenderTypeAndPlot()
    end do
  end subroutine moreRenders

  subroutine askRenderType()
    integer :: ans
    integer, parameter :: NR = 1, ROOTS = 2
    character(len=2) :: runStr
    write(*, *) "Which type of render would you like to perform ?"
    write(*, 100, advance="no") "(", NR, " - N.R. basins ; ", ROOTS, " - roots basins) : "
    read(*, *) ans
    if (ans .eq. 1) then
      gp%renderType = RENDER_NR
    else if (ans .eq. 2) then
      gp%renderType = RENDER_ROOTS
    end if
    write(runStr,'(i0.2)') gp%run
    renderImageFile = trim(gp%renderType)//"_"//trim(runStr)//renderImageExtension
    100 format(2x,a,i2,a,i2,a)
  end subroutine askRenderType

end module mathUtil
