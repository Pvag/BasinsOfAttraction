! Description
! ===========
! This program computes the roots of the complex cubic equation z**3 = 1
! (attractors) for a grid of points on the complex plane. The extents
! and accuracy of the grid are chosen by the user at run time.
!
! Each attractor is identified by a unique integer number, starting
! from 1. The special identifier 0 is used to indicate "no convergence".
!
! The coordinates of each initial point in the grid (z0), together with
! the number of iterations needed to reach its attractor (iter)
! and the attractor reached (bas) are stored in the basins.dat file,
! for further analysis and plotting.
!
! The output file will contain these informations, in each row:
! Re(z0) Im(z0) iter bas
 
! Programmers
! ===========
! Dr. Euaggelos E. Zotos
! Eng. Paolo Vagnini ( paolondon at gmail dot com )

! Compilation
! ===========
! gfortran mathUtil.f90 main.f90 -o basinsExplorer

! Execution
! =========
! ./basinsExplorer

! Creation date
! =============
! Sat. Dec. 12th 2015

! TODO
! ====
! Notify the user about the % of calculations completed.

! Revision history
! ================
! rev. n. |    date    | programmer | description
! -----------------------------------------
!       1 | 12/12/2015 | P. Vagnini | The basic program works:
! it outputs basins.out and roots.out . I have tested it in the
! window (+1, +1) (top right corner) - (-1, -1) bottom left
! corner. The 3 attractors are numerically correct.
! I didn't plot the points with gnuplot, I will do it soon.
! The program is quite general, thanks to the fact that it
! passes the mathematical function and its derivative to the
! part of the program that performs the computation, namely,
! exploreGrid. I didn't try it with other equations.

program main

  use mathUtil

  ! variables definition
  type(grid) :: gp
  character(len=*), parameter :: basinsFile = "./basins.out"
  character(len=*), parameter :: rootsFile = "./roots.out"
  double complex :: next

  write(*,*)
  call equalSep()
  write(*,*) "This program finds basins and roots and stores them to different files."
  write(*,*) "Ok, let's crunch some numbers!"
  ! init the grid parameters
  call initGrid(gp)
  ! for each point in grid
  ! compute the attractor
  ! and store info in outFile
  ! TODO Refactor this splitting in more functions
  call exploreGrid(gp, f, df, basinsFile, rootsFile)

  ! ATTENTION: VERY COOL PART :)
  ! Definition of the complex function and its derivative
  ! Change these, to test with a different function!
  contains
    double complex function f(z) ! the function
      double complex, intent(in) :: z
      f = z**10 - 1.d0
    end function f

    double complex function df(z) ! the derivative of the function
      double complex, intent(in) :: z
      df = 10.d0*z**9
    end function df

end program main
