module dataFiles
  implicit none

  type files
    character (len=300) :: basinsFile, rootsFile, outputImageFile,&
                           gnuplotScriptFile
  end type files

  character(len=*), parameter :: basinsFile = "./basins.out"
  character(len=*), parameter :: rootsFile = "./roots.out"
  character(len=*), parameter :: outputImageFile = "./render.png"
  character(len=*), parameter :: gnuplotScriptFile = "./gnuplotScript"
  character(len=*), parameter :: reportFile = "analysisData.txt"

end module dataFiles
