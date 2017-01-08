################################################################################
F77=gfortran
AR=ar
FLAGS+=-g -fno-range-check -fno-automatic -std=legacy -Iinclude -Llib
################################################################################
#all:       jobs nastran nasthelp nastplot chkfil ff
nastran:   obj bin libnas nasinfo bin/nastran.x
nasinfo:   NASINFO
libnas:    lib libnasmis libnasmds libnasbd lib/libnas.a
libnasmis: lib lib/libnasmis.a
libnasmds: lib lib/libnasmds.a
libnasbd:  lib lib/libnasbd.a
nasthelp:  obj bin bin/nasthelp.x
nastplot:  obj bin bin/nastplot.x
chkfil:    obj bin bin/chkfil.x
ff:        obj bin bin/ff.x
lib:
	mkdir -p lib
obj:
	mkdir -p obj
bin:
	mkdir -p bin
clean:
	rm -rf bin obj lib
################################################################################
MISOBJ+=$(patsubst mis/%.f,obj/%.o,$(wildcard mis/*.f))
MDSOBJ+=$(patsubst mds/%.f,obj/%.o,$(wildcard mds/*.f))
BDOBJ+=$(patsubst bd/%.f,obj/%.o,$(wildcard bd/*.f))
################################################################################
lib/libnasmis.a: $(MISOBJ)
	$(AR) cr $@ $^
lib/libnasmds.a: $(MDSOBJ)
	$(AR) cr $@ $^
lib/libnasbd.a: $(BDOBJ)
	$(AR) cr $@ $^
lib/libnas.a: lib/libnasmis.a lib/libnasmds.a lib/libnasbd.a 
	$(AR) crT $@ $^
bin/nastran.x: obj/nastrn.o
	$(F77) $(FLAGS) $^ -lnas -o $@    # Note that "-lnas" is after "$^"!
bin/nasthelp.x: obj/nasthelp.o
	$(F77) $(FLAGS) $^ -o $@
bin/nastplot.x: obj/nastplot.o
	$(F77) $(FLAGS) $^ -o $@
bin/ff.x: obj/ff.o 
	$(F77) $(FLAGS) $^ -lnas -o $@
bin/chkfil.x: obj/chkfil.o
	$(F77) $(FLAGS) $^ -o $@
NASINFO: rf/NASINFO
	cp $^ $@
################################################################################
obj/%.o : bd/%.f
	$(F77) $(FLAGS) -c $< -o $@
obj/%.o : mds/%.f
	$(F77) $(FLAGS) -c $< -o $@
obj/%.o : mis/%.f
	$(F77) $(FLAGS) -c $< -o $@
obj/%.o : src/%.f
	$(F77) $(FLAGS) -c $< -o $@
################################################################################
