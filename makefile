# Make file compiler

#FF = gfortran
FF = ifort
#silent = @
silent =
optim = -O2 -wrap-margin-
#optim =

objects = param.o mod_subrou.o kindset.o ppsplinefit3edit.o mod_space.o

# Main command
trial: $(objects) main.o
	$(silent)$(FF) -o trial $(optim) $(objects) main.o

# Modules (No dependency, except kindset.o)
kindset.o: kindset.f90
	$(silent)$(FF) -c $(optim) kindset.f90
param.o: param.f90 kindset.o
	$(silent)$(FF) -c $(optim) param.f90
ppsplinefit3edit.o: ppsplinefit3edit.f90 kindset.o
	$(silent)$(FF) -c $(optim) ppsplinefit3edit.f90
mod_space.o: mod_space.f90 kindset.o
		$(silent)$(FF) -c $(optim) mod_space.f90

# Modules (with dependencies)
mod_subrou.o: mod_subrou.f90 kindset.o param.o
	$(silent)$(FF) -c $(optim) mod_subrou.f90

# Program:
main.o: $(objects) main.f90
	$(silent)$(FF) -c $(optim) main.f90

# Cleaning everything
clean:
	$(silent)rm *.o
	$(silent)rm *.mod
	$(silent)rm trial
