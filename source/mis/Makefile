FC = gfortran

FFLAGS += -g -w
FFLAGS += -fno-range-check
FFLAGS += -fno-automatic
FFLAGS += -fcray-pointer
FFLAGS += -std=legacy
FFLAGS += -J../../include

# Archive

AR = ar
MOD = cr

# Objects

SRCF = $(wildcard *.f)
OBJS = $(patsubst %.f,%.o,$(SRCF))

# Default target

default: libnastran.a

all: libnastran.a

# Specific targets

libnastran.a:	$(OBJS)
	$(AR) $(MOD) -o $@ $(OBJS)

# Template rules

%.o:	%.f
	$(FC) $(FFLAGS) -c -o $@ $<

# Phony rules

.PHONY:	clean distclean

distclean:	clean
	rm -rf libnastran.a

clean:
	rm -rf *.o
