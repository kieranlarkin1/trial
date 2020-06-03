# Make file compiler

FF = gfortran
silent = @

mods1 = param.mod
objects = param.o main.o 

# Main command
trial: $(objects)
	$(silent)$(FF) -o trial $(switch) $(objects)

# Modules
param.mod: param.o param.f90
	$(silent)$(FF) -c $(switch) param.f90
param.o: param.f90
	$(silent)$(FF) -c $(switch) param.f90

# Program:
main.o: $(mods1) main.f90
	$(silent)$(FF) -c $(switch) main.f90

# Cleaning everything
clean: 
	$(silent)rm *.mod trial
	$(silent)rm $(objects)

