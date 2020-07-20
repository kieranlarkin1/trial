# Make file compiler

#FF = gfortran
FF = ifort
#silent = @
silent =

mods1 = param.mod mod_subrou.mod
objects = param.o mod_subrou.o main.o

# Main command
trial: $(objects)
	$(silent)$(FF) -o trial $(switch) $(objects)

# Modules
param.mod: param.o param.f90
	$(silent)$(FF) -c $(switch) param.f90
param.o: param.f90
	$(silent)$(FF) -c $(switch) param.f90
mod_subrou.mod: mod_subrou.o mod_subrou.f90
		$(silent)$(FF) -c $(switch) mod_subrou.f90
mod_subrou.o: mod_subrou.f90
		$(silent)$(FF) -c $(switch) mod_subrou.f90

# Program:
main.o: $(mods1) main.f90
	$(silent)$(FF) -c $(switch) main.f90

# Cleaning everything
clean:
	$(silent)rm *.mod trial
	$(silent)rm $(objects)
