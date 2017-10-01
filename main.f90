! Description
! ===========
! This program computes the roots of (quite simple) complex equations
! (attractors) for a grid of points on the complex plane. The extents
! and accuracy of the grid are chosen by the user at run time.
!
! Each attractor is identified by a unique integer number, starting
! from 1. The special identifier 0 is used to indicate "no convergence".
!
! The coordinates of each initial point in the grid (z0), together with
! the attractor reached (bas) 
! and the number of iterations needed to reach its attractor (iter)
! are stored in the basins.out file,
! for further analysis and plotting.
!
! The output file (basins.out) will contain these informations, in each row:
! Re(z0) Im(z0) bas iter

! Keys (symbols) in the code
! ==========================
! D : line to be deleted
 
! Programmers
! ===========
! Dr. Euaggelos E. Zotos
! Eng. Paolo Vagnini ( paolondon at gmail dot com )

! Compilation
! ===========
! OLD $> gfortran mathUtil.f90 main.f90 -o basinsExplorer
! $> make

! Execution
! =========
! ./basinsExplorer

! Creation date
! =============
! Sat. Dec. 12th 2015

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
!
!       2 | 25/09/2017 | P. Vagnini | Refactoring.
! Added a legend: comments ending with a D represent lines to
! be deleted.
!
!       --> use "git log", from now, on

program main

  use utils
  use mathUtil

  ! init the grid parameters (you may want to change paths in dataFiles.f90)
  call setFunction(f, df)

  call run()

  ! ATTENTION: VERY COOL PART :)
  ! Definition of the complex function!
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
