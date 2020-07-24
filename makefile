# Make file compiler

#FF = gfortran
FF = ifort
#silent = @
silent =

mods1 = param.mod mod_subrou.mod
objects = param.o mod_subrou.o kindset.o ppsplinefit3edit.o main.o

# Main command
trial: $(objects)
	$(silent)$(FF) -o trial $(switch) $(objects)

# Modules
kindset.o : kindset.f90
	$(silent)$(FF) kindset.f90 -$(nobuild)
ppsplinefit3edit.o : ppsplinefit3edit.f90 kindset.o $(object)
	$(silent)$(FF) ppsplinefit3edit.f90 -$(nobuild)
param.mod: param.o param.f90
	$(silent)$(FF) -c $(switch) param.f90
param.o: param.f90
	$(silent)$(FF) -c $(switch) param.f90
mod_subrou.mod: mod_subrou.o mod_subrou.f90
		$(silent)$(FF) -c $(switch) mod_subrou.f90
mod_subrou.o: mod_subrou.f90 param.mod
		$(silent)$(FF) -c $(switch) mod_subrou.f90

# Program:
main.o: $(mods1) main.f90
	$(silent)$(FF) -c $(switch) main.f90

# Cleaning everything
clean:
	$(silent)rm *.mod trial
	$(silent)rm $(objects)
