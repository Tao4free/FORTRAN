TARGET = getnearflood_dist_wt
FF = gfortran

OBJECTS = $(wildcard *.f90)

#all:
#	echo $(OBJECTS)

$(TARGET): $(OBJECTS)
	$(FF)  $(OBJECTS) -o $@

clean:
	-rm -f *.o core *.core
