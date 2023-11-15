
SUBROUTINE givens
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Alpha(4) , Apc , Apu , Beta(4) , Core(1) , D1 , D2 , D3 , D4 , Degra , Dum(100) , Epsi , Fps , Hfreq , Lfreq , Mb(1) ,      &
      & Order , Pi , Radeg , Rmax , Rmin , Rminr , Slmdas , Tcons(15) , Twopi , Xlmdas
   DOUBLE PRECISION Dalpha(2) , Dbeta(2) , Dcore(1) , Dlmdas
   INTEGER Ibuck , Icase , Icore(1) , Iev(7) , Iip , Iiu , Ik(7) , Im(7) , Incrp , Incru , Iprob(2) , Itp1 , Itp2 , Itu , Itypa ,   &
         & Itypb , Jjp , Jju , Jprec , Ksys(51) , Lamda , Lcore , Llcore , Lntime , Mcba(7) , Mcbb(7) , Mcbcde(36) , Mcbx(7) , Mz , &
         & N , Ne , Nev , Nevm , Nfound , Nfr , Nit , Nogo , Nomat , Nout , Nsym , Nummod , Nv , Option , Scr1 , Scr2 , Scr3 ,      &
         & Scr4 , Scr5 , Scr6 , Scr7 , Sysbuf
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Iprob , Nummod , Icase , Xlmdas
   COMMON /condas/ Pi , Twopi , Radeg , Degra , Fps
   COMMON /givn  / Dum , N , Lfreq , Order , D1 , Hfreq , D2 , Nv , D3 , D4 , Nfr
   COMMON /mgivxx/ Dlmdas
   COMMON /ntime / Lntime , Tcons
   COMMON /packx / Itp1 , Itp2 , Iip , Jjp , Incrp
   COMMON /regean/ Im , Ik , Iev , Scr1 , Scr2 , Scr3 , Scr4 , Scr5 , Lcore , Rmax , Rmin , Mz , Nev , Epsi , Rminr , Ne , Nit ,    &
                 & Nevm , Scr6 , Scr7 , Nfound , Lamda , Ibuck , Nsym
   COMMON /reigkr/ Option
   COMMON /saddx / Nomat , Llcore , Mcba , Itypa , Alpha , Mcbb , Itypb , Beta , Mcbcde , Mcbx
   COMMON /system/ Sysbuf , Nout , Nogo , Ksys , Jprec
   COMMON /unpakx/ Itu , Iiu , Jju , Incru
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ Core
!
! Local variable declarations
!
   REAL ak , am , amb , an , an2 , anv , av , av2 , az , t1 , t2 , t23 , t3
   INTEGER eigr(4) , end , file , i , ibuf1 , ibuf2 , icr1 , icr2 , ifile1 , ifile2 , ip1 , ix(7) , k , kaa , l , m , maa , mgiv ,  &
         & name(4) , ncol , nnv , nz , phia , t
   INTEGER korsz
!
! End of declarations
!
!
!     DRIVER FOR GIVENS-HOUSEHOLDER METHOD
!
   EQUIVALENCE (Beta(1),Dbeta(1)) , (Alpha(1),Dalpha(1)) , (Slmdas,Dlmdas) , (Tcons(8),Mb(1)) , (Core(1),Dcore(1)) ,                &
    & (Icore(1),Core(1)) , (Tcons(4),Apc) , (Tcons(5),Apu)
   DATA name/4HGIVE , 4HNS   , 4HBEGI , 4HNS  / , mgiv/4HMGIV/ , end/4HENDS/ , icr1 , icr2/301 , 302/
!
!
   CALL conmsg(name,4,0)
   i = 0
   kaa = Icore(1)
   maa = Icore(i+2)
   phia = Icore(i+3)
   DO i = 1 , 4
      eigr(i) = Icore(i+3)
   ENDDO
   nnv = Nv
   nz = korsz(Core(1))
   ibuf1 = nz - 3 - Sysbuf
   ibuf2 = ibuf1 - Sysbuf
   ix(1) = kaa
   CALL rdtrl(ix)
   IF ( ix(1)<=0 ) THEN
      WRITE (Nout,99001) Sfm , ix , kaa , maa , phia
99001 FORMAT (A25,' FROM GIVENS.  FILE ERROR,  TRAIL =',5I5,2I8,/5X,'KAA,MAA,PHIA = ',3I5)
      CALL errtrc('GIVENS  ',60)
   ENDIF
   an = ix(2)
!
!     CHECK THE CORE SIZE REQUIREMENT FOR WILVEC/VALVEC BEFORE GOING
!     BLINDLY INTO EIGENVALUE COMPUTATION AND EVENTUALLY STOP DUE TO
!     INSUFFICIENT CORE IN THOSE ROUTINES.
!     PRESENTLY CDC IS USING D.P. IN GIVENS COMPUTATION.  IF CDC VERSION
!     IS MODIFIED TO USE S.P., 19 IN THE FOLLOWING FORMULA SHOULD CHANGE
!     TO 10. (COMMENT FROM G.CHAN/UNISYS)
!
   N = (9*Jprec+1)*ix(2) + 2*Sysbuf - nz
   IF ( N>0 ) THEN
      WRITE (Nout,99002) Uim , ix(2) , ix(2) , N
99002 FORMAT (A29,' 3008, INSUFFICIENT CORE FOR GIVENS METHOD.',/5X,'MATRIX SIZE IS',I5,3H BY,I5,'.  ADDITIONAL CORE OF',I7,        &
             &' WORDS IS NEEDED.',/5X,'OR SWITCH TO INVPWR OR FEER ','METHOD.')
      CALL mesage(-37,0,name)
   ELSE
      az = nz - (3*Jprec+1)*ix(2) - 2*Sysbuf
      az = az/Jprec
      am = sqrt(2.0*az)
      ak = an - am
      an2 = an**2
      amb = Mb(Jprec)
      av = Nv
      anv = an*av
      av2 = av**2
      t1 = amb*an*(3.0*(an2+anv)+av2)
      t23 = Apu*(10.0*an2+5.0*anv)
      t2 = Apc*(5.0*an2+3.0*anv+av2) + t23
      t3 = 0
      IF ( am<an ) t3 = t23 + .5*(Apc+Apu)*ak*(an2-ak*(an+.5+ak/3.)+an)
      t = (t1+t2+t3)*1.0E-6
      N = an
      m = am
      WRITE (Nout,99003) Uim , t , N , m
99003 FORMAT (A29,' 2016, GIVENS TIME ESTIMATE IS ',I8,' SECONDS.',/36X,'PROBLEM SIZE IS',I8,', SPILL WILL OCCUR FOR THIS ',        &
             &'CORE AT A PROBLEM SIZE OF',I8,2H .)
      IF ( t>2000 .OR. N>1000 ) WRITE (Nout,99004) Uim
99004 FORMAT (A29,', FEER METHOD WOULD BE MORE EFFICIENT FOR PROBLEM ','OF THIS SIZE',/)
      CALL tmtogo(i)
      IF ( i>=t ) GOTO 200
      ip1 = -50
      file = t
   ENDIF
 100  CALL mesage(ip1,file,name)
!
!     CHOLESKI DECOMPOSE  MAA
!
 200  IF ( Option/=mgiv ) THEN
      ifile1 = maa
      ifile2 = kaa
   ELSE
      Nomat = 2
      Mcba(1) = kaa
      Mcbb(1) = maa
      CALL rdtrl(Mcba)
      CALL rdtrl(Mcbb)
      Mcbx(1) = icr1
      Mcbx(2) = Mcba(2)
      Mcbx(3) = Mcba(3)
      Mcbx(4) = Mcba(4)
      Mcbx(5) = Jprec
      Mcbx(6) = 0
      Mcbx(7) = 0
      Dalpha(1) = 0.0D0
      Dalpha(2) = 0.0D0
      Dbeta(1) = 0.0D0
      Dbeta(2) = 0.0D0
      IF ( Jprec==2 ) THEN
         Dlmdas = Xlmdas
         Dalpha(1) = 1.0D0
         Dbeta(1) = Dlmdas
         Itypa = 2
         Itypb = 2
      ELSE
         Slmdas = Xlmdas
         Alpha(1) = 1.0
         Beta(1) = Slmdas
         Itypa = 1
         Itypb = 1
      ENDIF
      Llcore = nz
      CALL sadd(Core,Core)
      CALL wrttrl(Mcbx)
      ifile1 = icr1
      ifile2 = maa
   ENDIF
   CALL factor(ifile1,Scr3,-Scr4,Scr5,Scr6,Scr7)
!
!     C  IS ON SCR3
!
!     CHANGE SIGNS OF THE OFF-DIAGONAL TERMS OF C AS SDCOMP HAS THEM
!     REVERSED.
!
   ip1 = -5
   file = Scr3
   ix(1) = Scr3
   CALL rdtrl(ix)
   ix(5) = Jprec
   Itp1 = ix(5)
   Itp2 = Itp1
   Itu = Itp1
   Incrp = 1
   Incru = 1
   ncol = ix(2)
   ix(1) = Scr7
   ix(2) = 0
   ix(6) = 0
   ix(7) = 0
   CALL gopen(Scr3,Core(ibuf1+1),0)
   CALL gopen(Scr7,Core(ibuf2+1),1)
   DO l = 1 , ncol
      Iiu = 1
      Jju = ncol
      CALL unpack(*100,Scr3,Core)
      IF ( Itu==2 ) THEN
         DO k = 1 , ncol
            Dcore(k) = -Dcore(k)
         ENDDO
         Dcore(l) = -Dcore(l)
      ELSE
         DO k = 1 , ncol
            Core(k) = -Core(k)
         ENDDO
         Core(l) = -Core(l)
      ENDIF
      Iip = Iiu
      Jjp = Jju
      CALL pack(Core,Scr7,ix)
   ENDDO
   CALL close(Scr3,1)
   CALL close(Scr7,1)
   CALL wrttrl(ix)
!
!     C IS NOW ON SCR7
!
!     INVERT  C
!
   CALL invert(Scr7,Scr5,Scr6)
!
!     C INVERSE IS ON SCR5
!
!
!     GET C INVERSE TRANSPOSE ON SCR6
!
!     CALL TRANP1 (SCR5,SCR6,4,SCR4,SCR3,SCR7,ICR1,0,0,0,0)
!     GINO UNITS    308, 305,   304, 303, 204, 301
!                   ARE THESE UNITS AVAILABEL?    , 306, 307, 309
!                                                  SCR1,SCR2, EMPTY
!
!     TRANP1 SHOULD BE 60 PERCENT FASTER BY ADDING 3 MORE SCRATCH FILES
!
   CALL tranp1(Scr5,Scr6,7,Scr4,Scr3,Scr7,icr1,Scr1,Scr2,309,0)
!
!     COMPUTE  J
!
   CALL ssg2b(ifile2,Scr6,0,Scr5,0,Jprec,1,Scr4)
   CALL ssg2b(Scr6,Scr5,0,Scr4,1,Jprec,1,Scr3)
!
!     J IS ON SCR4
!
!     EXTRACT EIGENVALUES
!
   CALL valvec
!
!     TRANSFORM
!
   CALL ssg2b(Scr6,Scr5,0,Scr4,0,Jprec,1,Scr7)
!
!     MERGE MODES AND FREE BODY MODES
!
   CALL read6(icr2,Scr4,Nfr,phia)
   Icore(1) = nnv
   name(3) = end
   CALL conmsg(name,3,0)
END SUBROUTINE givens
