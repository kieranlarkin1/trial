# Make file compiler

#FF = gfortran
FF = ifort
#silent = @
silent =

mods = param.mod mod_subrou.mod kindset.mod ppsplinefit3edit.mod
objects = param.o mod_subrou.o kindset.o ppsplinefit3edit.o main.o

# Main command
trial: $(objects)
	$(silent)$(FF) -o trial $(switch) $(objects)

# Modules
kindset.o: kindset.f90
	$(silent)$(FF) -c kindset.f90
ppsplinefit3edit.o: ppsplinefit3edit.f90 kindset.o
	$(silent)$(FF) -c ppsplinefit3edit.f90
param.o: param.f90
	$(silent)$(FF) -c $(switch) param.f90
mod_subrou.o: mod_subrou.f90 param.o
		$(silent)$(FF) -c $(switch) mod_subrou.f90

# Program:
main.o: $(mods) main.f90
	$(silent)$(FF) -c $(switch) main.f90

# Cleaning everything
clean:
	$(silent)rm *.mod trial
	$(silent)rm $(objects)
