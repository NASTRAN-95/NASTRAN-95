!*==givens.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE givens
   USE c_blank
   USE c_condas
   USE c_givn
   USE c_mgivxx
   USE c_ntime
   USE c_packx
   USE c_regean
   USE c_reigkr
   USE c_saddx
   USE c_system
   USE c_unpakx
   USE c_xmssg
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL :: ak , am , amb , an , an2 , anv , apc , apu , av , av2 , az , slmdas , t1 , t2 , t23 , t3
   REAL(REAL64) , DIMENSION(2) :: dalpha , dbeta
   REAL(REAL64) , DIMENSION(1) :: dcore
   INTEGER , DIMENSION(4) :: eigr
   INTEGER , SAVE :: end , icr1 , icr2 , mgiv
   INTEGER :: file , i , ibuf1 , ibuf2 , ifile1 , ifile2 , ip1 , k , kaa , l , m , maa , ncol , nnv , nz , phia , t
   INTEGER , DIMENSION(1) :: icore
   INTEGER , DIMENSION(7) :: ix
   REAL , DIMENSION(1) :: mb
   INTEGER , DIMENSION(4) , SAVE :: name
   EXTERNAL close , conmsg , errtrc , factor , gopen , invert , korsz , mesage , pack , rdtrl , read6 , sadd , ssg2b , tmtogo ,     &
          & tranp1 , unpack , valvec , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     DRIVER FOR GIVENS-HOUSEHOLDER METHOD
!
   !>>>>EQUIVALENCE (Beta(1),Dbeta(1)) , (Alpha(1),Dalpha(1)) , (Slmdas,Dlmdas) , (Tcons(8),Mb(1)) , (Core(1),Dcore(1)) ,                &
!>>>>    & (Icore(1),Core(1)) , (Tcons(4),Apc) , (Tcons(5),Apu)
   DATA name/4HGIVE , 4HNS   , 4HBEGI , 4HNS  / , mgiv/4HMGIV/ , end/4HENDS/ , icr1 , icr2/301 , 302/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
         CALL conmsg(name,4,0)
         i = 0
         kaa = icore(1)
         maa = icore(i+2)
         phia = icore(i+3)
         DO i = 1 , 4
            eigr(i) = icore(i+3)
         ENDDO
         nnv = nv
         nz = korsz(core(1))
         ibuf1 = nz - 3 - sysbuf
         ibuf2 = ibuf1 - sysbuf
         ix(1) = kaa
         CALL rdtrl(ix)
         IF ( ix(1)<=0 ) THEN
            WRITE (nout,99001) sfm , ix , kaa , maa , phia
99001       FORMAT (A25,' FROM GIVENS.  FILE ERROR,  TRAIL =',5I5,2I8,/5X,'KAA,MAA,PHIA = ',3I5)
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
         n = (9*jprec+1)*ix(2) + 2*sysbuf - nz
         IF ( n>0 ) THEN
            WRITE (nout,99002) uim , ix(2) , ix(2) , n
99002       FORMAT (A29,' 3008, INSUFFICIENT CORE FOR GIVENS METHOD.',/5X,'MATRIX SIZE IS',I5,3H BY,I5,'.  ADDITIONAL CORE OF',I7,  &
                   &' WORDS IS NEEDED.',/5X,'OR SWITCH TO INVPWR OR FEER ','METHOD.')
            CALL mesage(-37,0,name)
         ELSE
            az = nz - (3*jprec+1)*ix(2) - 2*sysbuf
            az = az/jprec
            am = sqrt(2.0*az)
            ak = an - am
            an2 = an**2
            amb = mb(jprec)
            av = nv
            anv = an*av
            av2 = av**2
            t1 = amb*an*(3.0*(an2+anv)+av2)
            t23 = apu*(10.0*an2+5.0*anv)
            t2 = apc*(5.0*an2+3.0*anv+av2) + t23
            t3 = 0
            IF ( am<an ) t3 = t23 + .5*(apc+apu)*ak*(an2-ak*(an+.5+ak/3.)+an)
            t = (t1+t2+t3)*1.0E-6
            n = an
            m = am
            WRITE (nout,99003) uim , t , n , m
99003       FORMAT (A29,' 2016, GIVENS TIME ESTIMATE IS ',I8,' SECONDS.',/36X,'PROBLEM SIZE IS',I8,', SPILL WILL OCCUR FOR THIS ',  &
                   &'CORE AT A PROBLEM SIZE OF',I8,2H .)
            IF ( t>2000 .OR. n>1000 ) WRITE (nout,99004) uim
99004       FORMAT (A29,', FEER METHOD WOULD BE MORE EFFICIENT FOR PROBLEM ','OF THIS SIZE',/)
            CALL tmtogo(i)
            IF ( i>=t ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ip1 = -50
            file = t
         ENDIF
 20      CALL mesage(ip1,file,name)
         spag_nextblock_1 = 2
      CASE (2)
!
!     CHOLESKI DECOMPOSE  MAA
!
         IF ( option/=mgiv ) THEN
            ifile1 = maa
            ifile2 = kaa
         ELSE
            nomat = 2
            mcba(1) = kaa
            mcbb(1) = maa
            CALL rdtrl(mcba)
            CALL rdtrl(mcbb)
            mcbx(1) = icr1
            mcbx(2) = mcba(2)
            mcbx(3) = mcba(3)
            mcbx(4) = mcba(4)
            mcbx(5) = jprec
            mcbx(6) = 0
            mcbx(7) = 0
            dalpha(1) = 0.0D0
            dalpha(2) = 0.0D0
            dbeta(1) = 0.0D0
            dbeta(2) = 0.0D0
            IF ( jprec==2 ) THEN
               dlmdas = xlmdas
               dalpha(1) = 1.0D0
               dbeta(1) = dlmdas
               itypa = 2
               itypb = 2
            ELSE
               slmdas = xlmdas
               alpha(1) = 1.0
               beta(1) = slmdas
               itypa = 1
               itypb = 1
            ENDIF
            llcore = nz
            CALL sadd(core,core)
            CALL wrttrl(mcbx)
            ifile1 = icr1
            ifile2 = maa
         ENDIF
         CALL factor(ifile1,scr3,-scr4,scr5,scr6,scr7)
!
!     C  IS ON SCR3
!
!     CHANGE SIGNS OF THE OFF-DIAGONAL TERMS OF C AS SDCOMP HAS THEM
!     REVERSED.
!
         ip1 = -5
         file = scr3
         ix(1) = scr3
         CALL rdtrl(ix)
         ix(5) = jprec
         itp1 = ix(5)
         itp2 = itp1
         itu = itp1
         incrp = 1
         incru = 1
         ncol = ix(2)
         ix(1) = scr7
         ix(2) = 0
         ix(6) = 0
         ix(7) = 0
         CALL gopen(scr3,core(ibuf1+1),0)
         CALL gopen(scr7,core(ibuf2+1),1)
         DO l = 1 , ncol
            iiu = 1
            jju = ncol
            CALL unpack(*20,scr3,core)
            IF ( itu==2 ) THEN
               DO k = 1 , ncol
                  dcore(k) = -dcore(k)
               ENDDO
               dcore(l) = -dcore(l)
            ELSE
               DO k = 1 , ncol
                  core(k) = -core(k)
               ENDDO
               core(l) = -core(l)
            ENDIF
            iip = iiu
            jjp = jju
            CALL pack(core,scr7,ix)
         ENDDO
         CALL close(scr3,1)
         CALL close(scr7,1)
         CALL wrttrl(ix)
!
!     C IS NOW ON SCR7
!
!     INVERT  C
!
         CALL invert(scr7,scr5,scr6)
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
         CALL tranp1(scr5,scr6,7,scr4,scr3,scr7,icr1,scr1,scr2,309,0)
!
!     COMPUTE  J
!
         CALL ssg2b(ifile2,scr6,0,scr5,0,jprec,1,scr4)
         CALL ssg2b(scr6,scr5,0,scr4,1,jprec,1,scr3)
!
!     J IS ON SCR4
!
!     EXTRACT EIGENVALUES
!
         CALL valvec
!
!     TRANSFORM
!
         CALL ssg2b(scr6,scr5,0,scr4,0,jprec,1,scr7)
!
!     MERGE MODES AND FREE BODY MODES
!
         CALL read6(icr2,scr4,nfr,phia)
         icore(1) = nnv
         name(3) = end
         CALL conmsg(name,3,0)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE givens
