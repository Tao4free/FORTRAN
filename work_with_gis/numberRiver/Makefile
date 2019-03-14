TARGET = $(patsubst %.f, %, $(wildcard *.f))
FF = gfortran

SRC = $(wildcard *.f)

$(TARGET): $(SRC)
	$(FF) $< -o $@
#	-mv a.exe $@
	
clean:
	-rm *.exe
