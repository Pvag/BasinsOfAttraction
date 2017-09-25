SOURCES=main.f90 mathUtil.f90
OBJECTS=main.o
MODULES=mathUtil.o

FF=gfortran
FLAGS=-O3 -Wall

main: $(MODULES) $(OBJECTS)
	$(FF) $(FLAGS) $(MODULES) $(OBJECTS) -o $@

%.o: %.f90
	$(FF) $(FLAGS) -c $< -o $@

clean:
	rm *.o *.mod main
