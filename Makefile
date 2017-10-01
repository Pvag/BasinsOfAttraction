SOURCES=main.f90 mathUtil.f90
OBJECTS=main.o
MODULES=mathData.o dataFiles.o utils.o mathUtil.o
MAIN=main
GNUPLOT_SCRIPT_FILE=gnuplotScript
BASINS_FILE=basins.out
ROOTS_FILE=roots.out
TMAIN=tests

FF=gfortran
FLAGS=-O3 -Wall

$(MAIN): $(MODULES) $(OBJECTS)
	$(FF) $(FLAGS) $(MODULES) $(OBJECTS) -o $@

%.o: %.f90
	$(FF) $(FLAGS) -c $< -o $@

tests: utils.o testsModule.o tests.o
	$(FF) $(FLAGS) $^ -o $(TMAIN)

clean:
	rm *.o *.mod $(MAIN) $(GNUPLOTSCRIPTFILE) $(BASINS_FILE) $(ROOTS_FILE)
