
SUBROUTINE btstrp
   IMPLICIT NONE
   REAL A , B(100) , Dum(3)
   INTEGER Highpw , Ibmcdc , Idate(3) , Idrum , Ihalf , Intp , Iprec , Jhalf , Kshift , Ldict , Linkno , Lnknos(15) , Lowpw , Lpch ,&
         & Lqro , Mach , Machx , Mask2 , Mask3 , Mtisa , Mxfl , Mzero , Nbpc , Nbpw , Ncpw , Nlpp , Nudflw , Nwpic , Outtap ,       &
         & Sperlk , Sysbuf , Two(32)
   CHARACTER*7 Machos
   CHARACTER*11 Mchnam
   COMMON /chmach/ Mchnam , Machos
   COMMON /lhpwx / Lowpw , Highpw , Nwpic , Nudflw , Mxfl , Kshift , Mtisa
   COMMON /machin/ Machx , Ihalf , Jhalf , Lqro
   COMMON /sem   / A , Mask2 , Mask3 , Lnknos
   COMMON /system/ B
   COMMON /two   / Two , Mzero
   COMMON /xxread/ Dum , Ibmcdc
   INTEGER abcd , ak , i , imnth , iyr1 , iyr2 , ka , locf , m1(110) , m2(110) , mask , mconst(220) , mvax , nmach , order , qp ,   &
         & recl
   INTEGER andf , complf , khrfn1 , locfx , lshift , rshift
   CHARACTER*7 compos(22)
   CHARACTER*11 comput(22)
   REAL xx , yy
   EXTERNAL andf , complf , lshift , rshift
!
!     BASED ON MACHINE NUMBER, BTSTRP WILL DEFINE ALL THE
!     MACHINE-DEPENDENT CONSTANTS NEEDED IN NASTRAN. THESE CONSTANTS
!     ARE SAVED IN LABEL COMMONS /SYSTEM/, /LHPWX/, /MACHIN/ & /CHMACH/
!
!     SEE ALSO  PRTPRM, SDCMPS, SDR2E, AND UPCASE WHEN NASTRAN SOURCE
!     CODE IS PORTED TO OTHER (NEW) MACHINE
!
!                ===
!
   !>>>>EQUIVALENCE (B(1),Sysbuf) , (B(22),Linkno) , (B(40),Nbpw) , (B(2),Outtap) , (B(41),Ncpw) , (B(42),Idate(1)) , (B(4),Intp) ,      &
!>>>>    & (B(34),Idrum) , (B(55),Iprec) , (B(9),Nlpp) , (B(39),Nbpc) , (B(91),Lpch) , (B(92),Ldict) , (B(95),Sperlk) , (Machx,Mach) ,   &
!>>>>    & (m1(1),mconst(1)) , (m2(1),mconst(111))
!
!  DEFINE SYSTEM RELEASE DATE
!
   DATA imnth , iyr1 , iyr2/4HAPR. , 3H 19 , 2H95/
!
   DATA xx , yy/1.2E-38 , 0.3E-38/
   DATA mvax , abcd , ka/1H1 , 4HABCD , 4HA   /
!
!     MACH   = MACHX = HOST MACHINE
!              ANY SUBROUTINE, THAT USES 'MACHX' INSTEAD OF 'MACH' IN
!              LABEL COMMON /MACHIN/, CONTAINES MACHINE CONSTANTS THAT
!              ARE USED LOCALLY.
!     NMACH  = NUMBER OF MACHINES
!     MCONST = ARRAY CONTAINING MACHINE DEPENDENT CONSTANTS
!
!
!     COSMIC/NASTRAN SUPPORTS ONLY MACHINES 2, 5, 6, 7, 8, 9, 10, 16,
!     21, & 22.   CONSTANTS FOR OTHER MACHINES MAY NOT BE EXACT
!
!     -MACHINE-    IBM/  UNIVAC   CDC   DEC/    DEC/   SUN   IBM/    HP
!           DUMMY   MVS    FTN   FTN5    VMS  ULTRIX SOLARIS  AIX    UX
!     MACH = -1-  ---2-  ---3-  ---4-  ---5-  ---6-  ---7-  ---8-  ---9-
!
!           SGI    MAC   CRAY  CONVEX   NEC  FUJITSU  SUN  AMDAHL  PRIME
!           IRIS        UNICOS                       SUNOS
!          --10-  --11-  --12-  --13-  --14-  --15-  --16-  --17-  --18-
!
!           PC            DEC/   DEC/
!         MS/DOS  DUMMY OPENVMS  OSF
!          --19-  --20-  --21-  --22-
!
!  MACHINE NAMES
!
   DATA comput/'DUMMY      ' , 'IBM        ' , 'UNIVAC     ' , 'CDC        ' , 'DEC-VAX    ' , 'DEC-MIPS   ' , 'SUN        ' ,      &
       &'IBM RS6000 ' , 'HP         ' , 'SGI        ' , 'MACINTOCH  ' , 'CRAY       ' , 'CONVEX     ' , 'NEC        ' ,             &
       &'FUJITSU    ' , 'SUN        ' , 'AMDAHL     ' , 'PRIME      ' , 'PC         ' , 'DUMMY      ' , 'DEC-ALPHA  ' ,             &
       &'DEC-ALPHA  '/
!
!  MACHINE OPERATING SYSTEM
!
   DATA compos/'       ' , 'MVS    ' , 'FTN    ' , 'FTN5   ' , 'VMS    ' , 'ULTRIX ' , 'SOLARIS' , 'AIX    ' , 'HP-UX  ' ,          &
      & 'IRIX   ' , '       ' , 'UNICOS ' , '       ' , '       ' , '       ' , 'SUNOS  ' , '       ' , '       ' , 'MS-DOS ' ,     &
       &'       ' , 'OPENVMS' , 'OSF    '/
!
!
!     SYSBUF  =   LENGTH OF NASTRAN I/O BUFFER
!
!
!     INTP(X100)  =  FORTRAN UNIT NO. FOR INPUT DATA
!     OUTTAP      =  FORTRAN UNIT NO. FOR PRINTED OUTPUT
!
!
!
!     NLPP(X100)  =  NUMBER OF LINES PRINTED PER PAGE
!     NWPIC       =  NUMBER OF WORDS PER INPUT CARD, USED ONLY IN XGPIBS
!
!
!     NBPC(X100)  =  NUMBER OF BITS PER CHARACTER
!     NBPW        =  NUMBER OF BITS PER WORD
!
!
!     IPREC(X100) =  PRECISION (1 = S.P., 2 = D.P.)
!     RECL(X10)   =  DIRECT FILE RECORD LENGTH (USED IN FORTRAN OPEN
!                    STATEMENT) BY WORDS (= 1), OR BYTE (= NCPW)
!     QP          =  REAL*16 PRECISION FLAG (1 = YES, 0 = NO)
!
!WKBR3    2 0 0, 2 4 0, 2 1 1, 1 1 0, 2 1 1, 2 1 0, 2 4 0, 2 4 1, 2 4 1,
   DATA nmach/22/ , m1/200 , 4100 , 871 , 1042 , 1028 , 1028 , 1028 , 1028 , 1028 , 1028 , 1028 , 2052 , 1028 , 2052 , 2052 , 1028 ,&
      & 1028 , 1028 , 1028 , 1028 , 1028 , 1028 , 506 , 506 , 506 , 506 , 506 , 506 , 506 , 506 , 506 , 506 , 506 , 506 , 506 ,     &
      & 506 , 506 , 506 , 506 , 506 , 506 , 506 , 506 , 506 , 5000 , 5518 , 5518 , 4208 , 5518 , 5518 , 5518 , 5518 , 5518 , 5518 , &
      & 5518 , 5509 , 5518 , 5500 , 5500 , 5500 , 5500 , 5500 , 5500 , 5500 , 5518 , 550 , 636 , 832 , 936 , 660 , 832 , 832 , 832 ,&
      & 832 , 832 , 832 , 832 , 864 , 832 , 864 , 864 , 832 , 832 , 832 , 832 , 832 , 832 , 832 , 200 , 240 , 210 , 110 , 210 ,     &
      & 210 , 240 , 240 , 240 , 210 , 200 , 180 , 240 , 100 , 100 , 200 , 200 , 200 , 200 , 200 , 210 , 200/
!
!
!
!     LPCH(X100)  =  FORTRAN UNIT NO. FOR PUNCHED OUTPUT
!     LDICT       =  FORTRAN UNIT NO. FOR RESTART DICTIONARY PUNCH
!
!
!     LOWPW, HIGHPW = MACHINE NUMERIC RANGE FOR S. P. REAL NUMBER,
!     USED ONLY BY RCARD, RCARD2, XRCARD AND YRCARD
!
!
!     NUDFLW(X100) =  FLOATING NUMBER UNDERFLOW CONTROL
!                     (USED ONLY BY FQRW AND FQRWV)
!     MXFL         =  MAXINUM FILES MAXFIL CAN REQUEST VIA THE NASTRAN
!                     CARD, USED ONLY IN NASCAR
!
!
!     KSHIFT  =  SHIFT COUNTS USED IN A DIVIDE TO CONVERT A GINO LOC
!                RETURNED FROM SAVPOS TO GINO BLOCK NUMBER, USED IN EMA
!
!
!     MANTISSA BITS, USED ONLY IN SDCMPS
!
   DATA m2/703 , 707 , 103 , 707 , 104 , 104 , 104 , 104 , 104 , 104 , 104 , 104 , 104 , 104 , 104 , 104 , 104 , 104 , 104 , 104 ,  &
      & 104 , 104 , 38 , 75 , 38 , 321 , 38 , 38 , 38 , 38 , 38 , 38 , 38 , 2465 , 38 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 38 , 0 , 1650 , &
      & 1650 , 1849 , 1475 , 875 , 1675 , 1675 , 1675 , 1675 , 1675 , 1675 , 1675 , 1675 , 1675 , 1675 , 1675 , 1675 , 1675 , 1675 ,&
      & 1675 , 975 , 1675 , 1 , 4096 , 4096 , 262144 , 4096 , 4096 , 4096 , 4096 , 4096 , 4096 , 4096 , 4096 , 4096 , 0 , 0 , 0 ,   &
      & 0 , 0 , 0 , 0 , 4096 , 0 , 000 , 2426 , 2760 , 4896 , 2355 , 2355 , 2352 , 2355 , 2355 , 2355 , 2355 , 4896 , 2352 , 4896 , &
      & 4896 , 2355 , 2355 , 2355 , 000 , 000 , 2355 , 000/
!
!     DEFINE SYSTEM (42), SYSTEM(43), SYSTEM(44)
!
   Idate(1) = imnth
   Idate(2) = iyr1
   Idate(3) = iyr2
!
!     MACHINE TYPE IS SET HERE
!     +++++++++++++++++++++++++++++++
   Mach = 7
   Mchnam = comput(Mach)
   Machos = compos(Mach)
   Sysbuf = mconst(Mach)
   Ibmcdc = 1
   IF ( Mach==2 .OR. Mach==4 ) Ibmcdc = 0
!
   i = Mach + nmach
   Intp = mconst(i)/100
   Outtap = mod(mconst(i),100)
!
   i = i + nmach
   Nlpp = mconst(i)/100
   Nwpic = mod(mconst(i),100)
!
   i = i + nmach
   Nbpc = mconst(i)/100
   Nbpw = mod(mconst(i),100)
!
   i = i + nmach
   Iprec = mconst(i)/100
   recl = mod(mconst(i),100)/10
   qp = mod(mconst(i),10)
!
!
   i = i + nmach
   Lpch = mconst(i)/100
   Ldict = mod(mconst(i),100)
!
!     MACHINE S.P. RANGE
!
   i = i + nmach
   Highpw = mconst(i)
   Lowpw = 1 - Highpw
   IF ( Mach==2 ) Lowpw = -78
   IF ( Mach==4 ) Lowpw = -292
!
!     FLOATING NUMBER UNDERFLOW CONTROL
!     MAXINUM FILES FOR MAXFIL CHECK
!
   i = i + nmach
   Nudflw = mconst(i)/100
   Mxfl = mod(mconst(i),100)
!
!     SHIFT COUNTER FOR EMA SUBROUTINE
!
   i = i + nmach
   Kshift = mconst(i)
!
!     MANTISSA BITS
!
   i = i + nmach
   Mtisa = mconst(i)/100
   IF ( Iprec==2 ) Mtisa = mod(mconst(i),100)
!
!     NUMBER OF BITS PER HALF WORD, USED MAINLY FOR INTEGER PACKING
!
!     IHALF = NBPW/2
!     JHALF = 2**IHALF - 1
   Ihalf = 16
   Jhalf = 65535
!
!     NUMBER OF CHARACTERS PER WORD
!
   Ncpw = Nbpw/Nbpc
!
!     ZERO FIELD KA, AK AND GENERATE A MASK FOR FIRST BYTE
!
   ak = khrfn1(0,1,ka,4)
   ka = khrfn1(0,1,ka,1)
   i = 2**Nbpc - 1
   mask = lshift(i,Nbpw-Nbpc)
!
!     CHECK BCD WORD (NOT CHARACTER WORD) STORING ORDER.
!     IF 'ABCD' IS STORED INTERNALLY IN A-B-C-D ORDER, SET ORDER TO 0,
!     OR  IF IT IS STORED IN REVERSED ORDER, D-C-B-A,  SET ORDER TO 1
!
   i = andf(abcd,mask)
   order = 0
   IF ( Nbpw<60 .AND. i/=ka .AND. i/=ak ) order = 1
!
!     CHECK SYSTEM LOC OR %LOC FUNCTION.
!     IF SYSTEM LOC FUNCTION IS WORD COUNT, SET LOCF TO 1
!     IF SYSTEM LOC FUNCTION IS BYTE COUNT, SET LOCF TO NCPW
!
   Lqro = 1000
   i = locfx(B(11)) - locfx(B(1))
   locf = i/10
!
!     MERGE LOCF, QP, RECL, AND ORDER INTO LQRO
!
   Lqro = locf*1000 + qp*100 + recl*10 + order
!
!
!     GENERATE MASKS
!                   7094      360         1108            6600
!     MASK2  = 777777007777,FFFFFFF0,777777607777,77777760777777777777
!     MASK3  = 377777777777,7FFFFFFF,377777777777,37777777777777777777
!     TWO(1) = 020000000000,80000000,020000000000,00000000020000000000
!
   Mask2 = complf(lshift(2**Nbpc-1,Nbpw-4*Nbpc))
   Mask3 = rshift(complf(0),1)
   Mzero = lshift(1,Nbpw-1)
   Two(1) = lshift(1,31)
!
!     TWO(1) = LSHIFT(1,31) = 2**31
!            = +2147483648   IN MACHINES WITH MORE THAN 32-BIT WORD
!            = -2147483648   IN 32-BIT MACHINES. A NEGATIVE NUMBER!
!            = -0.000E0      IN SOME  32-BIT MACHINES
!            = +0.000E0      IN OTHER 32-BIT MACHINES
!     NOTICE FOR THE 32-BIT MACHINES, IABS(-2147483648) IS FATAL!
!
!     DEFINE COMMONLY USED PHYSICAL CONSTANTS
!
   CALL cnstdd
   Linkno = Lnknos(1)
!
END SUBROUTINE btstrp