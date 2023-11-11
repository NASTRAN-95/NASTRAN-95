################################################################################
# NASTRAN MAKEFILE
# D. Everhart
# 02 JAN 2017
################################################################################
# The MIT License (MIT)
# 
# Copyright (c) 2017 Daniel Everhart
# 
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the 
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject
# to the following conditions:
# 
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
################################################################################
#Windows
#SHELL=C:/opt/bin/sh.exe
#GBIN=C:/opt/gcc/bin
#F77=$(GBIN)/gfortran.exe
#AR=$(GBIN)/ar.exe
#NASTRAN=C:/opt/python27/win64/python.exe ./sbin/nastran.py
#LN=cp
# Unix/Linux
F77=gfortran
AR=ar
NASTRAN=./sbin/nastran.py
LN=ln -sf
#
FLAGS+=-g -fno-range-check -fno-automatic -fallow-invalid-boz -std=legacy -Iinclude -Llib -Jobj
################################################################################
all:       nastran nasthelp nastplot chkfil ff jobs
nastran:   obj bin libnas NASINFO bin/nastran.x
libnas:    lib libnasmis libnasmds libnasbd lib/libnas.a
libnasmis: lib lib/libnasmis.a
libnasmds: lib lib/libnasmds.a
libnasbd:  lib lib/libnasbd.a
nasthelp:  obj bin bin/nasthelp.x
nastplot:  obj bin bin/nastplot.x
chkfil:    obj bin bin/chkfil.x
ff:        obj bin bin/ff.x
OUTPUT:
	mkdir -p OUTPUT
lib:
	mkdir -p lib
obj:
	mkdir -p obj
bin:
	mkdir -p bin
clean:
	rm -rf bin obj lib
	rm -rf OUTPUT
	rm -f testnas.*
	rm -f dsnames.*
	rm -f NASINFO COS* RSCARDS
	rm -f fort.*
################################################################################
MISOBJ+=$(patsubst mis/%.f,obj/%.o,$(wildcard mis/*.f))
MDSOBJ+=$(patsubst mds/%.f,obj/%.o,$(wildcard mds/*.f))
BDOBJ+=$(patsubst bd/%.f,obj/%.o,$(wildcard bd/*.f))
DOCTXT+=$(patsubst um/%.TXT,bin/%.TXT,$(wildcard um/*.TXT))
################################################################################
lib/libnasmis.a: $(MISOBJ)
	$(AR) cr $@ $^
lib/libnasmds.a: $(MDSOBJ)
	$(AR) cr $@ $^
lib/libnasbd.a: $(BDOBJ)
	$(AR) cr $@ $^
lib/libnas.a: lib/libnasmis.a lib/libnasmds.a lib/libnasbd.a 
	$(AR) crT $@ $^
bin/nastran.x: obj/nastrn.o $(BDOBJ)
  # Note that "-lnas" is after "$^"!
	$(F77) $(FLAGS) -static $^ -lnas -o $@
bin/nasthelp.x: obj/nasthelp.o $(DOCTXT)
	$(F77) $(FLAGS) $< -o $@
bin/nastplot.x: obj/nastplot.o
	$(F77) $(FLAGS) $^ -o $@
bin/ff.x: obj/ff.o 
	$(F77) $(FLAGS) $^ -lnas -o $@
bin/chkfil.x: obj/chkfil.o
	$(F77) $(FLAGS) $^ -o $@
NASINFO: rf/NASINFO
	$(LN) $^ $@
################################################################################
bin/%.TXT : um/%.TXT
	$(LN) $< $@
obj/%.o : bd/%.f
	$(F77) $(FLAGS) -c $< -o $@
obj/%.o : mds/%.f
	$(F77) $(FLAGS) -c $< -o $@
obj/%.o : mis/%.f
	$(F77) $(FLAGS) -c $< -o $@
obj/%.o : src/%.f
	$(F77) $(FLAGS) -c $< -o $@
obj/%.o : src/%.f90
	$(F77) $(FLAGS) -c $< -o $@
################################################################################
JOBS+=$(patsubst inp/%.inp,OUTPUT/%.f06,$(wildcard inp/*.inp))
################################################################################
COS=COSDBCL COSDDAM COSDFVA COSHYD1 COSHYD2 COSMFVA
jobs: nastran OUTPUT $(COS) $(JOBS)
################################################################################
COSDBCL: alt/COSDBCL
	$(LN) $^ $@
COSDDAM: alt/COSDDAM
	$(LN) $^ $@
COSDFVA: alt/COSDFVA
	$(LN) $^ $@
COSHYD1: alt/COSHYD1
	$(LN) $^ $@
COSHYD2: alt/COSHYD2
	$(LN) $^ $@
COSMFVA: alt/COSMFVA
	$(LN) $^ $@
################################################################################
OUTPUT/%.f06 : inp/%.inp
	$(NASTRAN) -o OUTPUT $<
################################################################################
OUTPUT/d02022a.f06: inp/d02022a.inp OUTPUT/d02021a.f06
	$(NASTRAN) -o OUTPUT --SOF1 OUTPUT/d02021a.sof1 $<
OUTPUT/d02023a.f06: inp/d02023a.inp OUTPUT/d02022a.f06
	$(NASTRAN) -o OUTPUT --SOF1 OUTPUT/d02021a.sof1 $<
OUTPUT/d02024a.f06: inp/d02024a.inp OUTPUT/d02023a.f06
	$(NASTRAN) -o OUTPUT --SOF1 OUTPUT/d02021a.sof1 $<
OUTPUT/d02025a.f06: inp/d02025a.inp OUTPUT/d02024a.f06
	$(NASTRAN) -o OUTPUT --SOF1 OUTPUT/d02021a.sof1 $<
OUTPUT/d02026a.f06: inp/d02026a.inp OUTPUT/d02025a.f06
	$(NASTRAN) -o OUTPUT --SOF1 OUTPUT/d02021a.sof1 $<
OUTPUT/d02027a.f06: inp/d02027a.inp OUTPUT/d02026a.f06
	$(NASTRAN) -o OUTPUT --SOF1 OUTPUT/d02021a.sof1 $<
OUTPUT/d02032a.f06: inp/d02032a.inp OUTPUT/d02031a.f06
	$(NASTRAN) -o OUTPUT --SOF1 OUTPUT/d02031a.sof1 $<
OUTPUT/d02033a.f06: inp/d02033a.inp OUTPUT/d02032a.f06
	$(NASTRAN) -o OUTPUT --SOF1 OUTPUT/d02031a.sof1 $<
OUTPUT/d02034a.f06: inp/d02034a.inp OUTPUT/d02033a.f06
	$(NASTRAN) -o OUTPUT --SOF1 OUTPUT/d02031a.sof1 $<
OUTPUT/d02035a.f06: inp/d02035a.inp OUTPUT/d02034a.f06
	$(NASTRAN) -o OUTPUT --SOF1 OUTPUT/d02031a.sof1 $<
OUTPUT/d02036a.f06: inp/d02036a.inp OUTPUT/d02035a.f06
	$(NASTRAN) -o OUTPUT --SOF1 OUTPUT/d02031a.sof1 $<
OUTPUT/d01011b.f06: inp/d01011b.inp OUTPUT/d01011a.f06
	rm -f RSCARDS
	$(LN) OUTPUT/d01011a.dict RSCARDS
	$(NASTRAN) -o OUTPUT --OPTPNM OUTPUT/d01011a.nptp $<
OUTPUT/d01011c.f06: inp/d01011c.inp OUTPUT/d01011a.f06
	rm -f RSCARDS
	$(LN) OUTPUT/d01011a.dict RSCARDS
	$(NASTRAN) -o OUTPUT --OPTPNM OUTPUT/d01011a.nptp $<
OUTPUT/d01021b.f06: inp/d01021b.inp OUTPUT/d01021a.f06
	rm -f RSCARDS
	$(LN) OUTPUT/d01021a.dict RSCARDS
	$(NASTRAN) -o OUTPUT --OPTPNM OUTPUT/d01021a.nptp $<
OUTPUT/d11011b.f06: inp/d11011b.inp OUTPUT/d11011a.f06
	rm -f RSCARDS
	$(LN) OUTPUT/d11011a.dict RSCARDS
	$(NASTRAN) -o OUTPUT --OPTPNM OUTPUT/d11011a.nptp $<
OUTPUT/t00001a.f06: inp/t00001a.inp
	$(NASTRAN) -o OUTPUT --FTN15 inp/t00001a.inp1 --FTN16 inp/t00001a.inp2 $<
OUTPUT/t03111b.f06: inp/t03111b.inp OUTPUT/t03111a.f06
	rm -f RSCARDS
	$(LN) OUTPUT/t03111a.dict RSCARDS
	$(NASTRAN) -o OUTPUT --OPTPNM OUTPUT/t03111a.nptp $<
OUTPUT/t03121b.f06: inp/t03121b.inp OUTPUT/t03121a.f06
	rm -f RSCARDS
	$(LN) OUTPUT/t03121a.dict RSCARDS
	$(NASTRAN) -o OUTPUT --OPTPNM OUTPUT/t03121a.nptp $<
OUTPUT/t03121c.f06: inp/t03121c.inp OUTPUT/t03121a.f06
	rm -f RSCARDS
	$(LN) OUTPUT/t03121a.dict RSCARDS
	$(NASTRAN) -o OUTPUT --OPTPNM OUTPUT/t03121a.nptp $<
OUTPUT/t04021b.f06: inp/t04021b.inp OUTPUT/t04021a.f06
	rm -f RSCARDS
	$(LN) OUTPUT/t04021a.dict RSCARDS
	$(NASTRAN) -o OUTPUT --OPTPNM OUTPUT/t04021a.nptp $<
################################################################################
