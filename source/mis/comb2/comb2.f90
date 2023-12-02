!*==comb2.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE comb2
USE C_BLANK
USE C_MPY3TL
USE C_MPYADX
USE C_NAMES
USE C_PACKX
USE C_PARMEG
USE C_SYSTEM
USE C_XMSSG
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL :: addflg
   INTEGER , DIMENSION(7,7) :: amcb
   INTEGER , SAVE :: blank , horg , papp , xxxx
   INTEGER :: buf1 , i , iform , inuse , iprc , irf , it , item , ityp , j , k , nmat , nogo , nsize , pvec , rc , rdsof , rsofar , &
            & scr1 , scr2 , scr3 , scr4 , sof1 , sof2 , sof3
   INTEGER , DIMENSION(7) :: cpv , mcbtrl , rpv
   REAL(REAL64) , DIMENSION(1) :: dbz
   REAL :: den
   INTEGER , DIMENSION(6,7) :: hmcb
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(5) , SAVE :: kmp , kmpitm
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL close , gopen , korsz , makmcb , merge , mesage , mpy3dr , mpyad , mtrxi , pack , rdtrl , smsg , sofcls , sofopn ,      &
          & softrl , sort , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     COMB2 PERFORMS THE TRANSFORMATION AND ADDITION OF STIFFNESS, MASS,
!     OR LOAD MATRICES FOR THE PHASE 2 SUBSTRUCTURE COMBINE OPERATION
!
!     NOVEMBER 1973
!
!
   !>>>>EQUIVALENCE (Dbz(1),Z(1),Iz(1)) , (pvec,kmpitm(3))
   DATA name/4HCOMB , 4H2   /
   DATA horg/4HHORG/
   DATA blank/4H    /
   DATA xxxx/4HXXXX/
   DATA papp/4HPAPP/
   DATA kmp/4HK    , 4HM    , 4HP    , 4HB    , 4HK4  /
   DATA kmpitm/4HKMTX , 4HMMTX , 4HPVEC , 4HBMTX , 4HK4MX/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     INITIALIZE
!
         DO i = 1 , 14
            IF ( Namess(i,1)==xxxx .OR. Namess(i,1)==0 ) Namess(i,1) = blank
         ENDDO
         Acomb = 201
         scr1 = 301
         scr2 = 302
         scr3 = 303
         scr4 = 304
         Scr5 = 305
         Scr6 = 306
         Scr7 = 307
         Signab = 1
         Signc = 1
         Prec = 0
         Mscr = Scr5
         Icode = 0
         Rule = 0
         rdsof = 1
         nogo = 0
         Rfiles(1) = Acomb
         nsize = 0
         Mcbp21(1) = 0
         Mcbp22(1) = 0
         rsofar = 0
         Kn = 1
         Jn = -1
         DO i = 1 , 5
            IF ( Type(1)==kmp(i) ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         WRITE (Nout,99001) Sfm , Type
99001    FORMAT (A25,' 6302, ',2A4,' IS ILLEGAL MATRIX TYPE FOR MODULE ','COMB2')
         IF ( Dry<0 ) RETURN
!
         Dry = -2
         item = 0
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (2)
         item = kmpitm(i)
         IF ( item==pvec ) item = Pora(1)
         IF ( Dry<0 ) RETURN
         spag_nextblock_1 = 3
      CASE (3)
!
         Lcore = korsz(Z) - 1
         Lkore = Lcore
         buf1 = Lcore - Sysbuf + 1
         sof1 = buf1 - Sysbuf
         sof2 = sof1 - Sysbuf - 1
         sof3 = sof2 - Sysbuf
         IF ( sof3>0 ) THEN
!
            CALL sofopn(Z(sof1),Z(sof2),Z(sof3))
!
!     GRAB THE MATRIX CONTROL BLOCKS
!
            nmat = 0
            DO i = 1 , 7
               IF ( Namess(1,i)/=blank ) THEN
                  amcb(1,i) = 100 + i
                  CALL rdtrl(amcb(1,i))
                  IF ( amcb(1,i)<=0 ) THEN
!
!     NO GINO FILE.  CHECK SOF
!
                     CALL softrl(Namess(1,i),item,amcb(1,i))
                     rc = amcb(1,i)
                     IF ( rc==1 ) THEN
!
!     MATRIX FOUND ON SOF
!
                        amcb(1,i) = 0
                        GOTO 2
                     ELSEIF ( rc==3 ) THEN
                        IF ( Type(1)==kmp(3) ) CYCLE
                     ELSEIF ( rc/=4 .AND. rc/=5 ) THEN
                        nogo = 1
                        WRITE (Nout,99002) Sfm , Namess(1,i) , Namess(2,i) , item
!
!     DIAGNOSTICS
!
99002                   FORMAT (A25,' 6301, DATA MISSING IN GO MODE FOR SUBSTRUCTURE ',2A4,' ITEM ',A4)
                        CYCLE
                     ENDIF
                     nogo = 1
                     CALL smsg(rc-2,item,Namess(1,i))
                     CYCLE
                  ENDIF
!
!     GRAB THE MCB OF THE TRANSFORMATION MATRIX
!
 2                CALL softrl(Namess(1,i),horg,mcbtrl)
                  rc = mcbtrl(1)
                  IF ( rc==1 ) THEN
                     DO it = 1 , 6
                        hmcb(it,i) = mcbtrl(it+1)
                     ENDDO
                     nmat = nmat + 1
                     Use(2*nmat-1) = i
                     den = float(amcb(7,i))/10000.
                     Use(2*nmat) = amcb(2,i)*amcb(3,i)*den
!
!     CHECK COMPATIBILITY OF DIMENSIONS
!
                     IF ( nsize==0 ) nsize = hmcb(1,i)
                     IF ( hmcb(1,i)/=nsize .OR. hmcb(2,i)/=amcb(2,i) .OR. hmcb(2,i)/=amcb(3,i) ) THEN
                        IF ( .NOT.(item==pvec .OR. item==papp .AND. hmcb(1,i)==nsize .AND. hmcb(2,i)==amcb(3,i)) ) THEN
                           nogo = 1
                           WRITE (Nout,99005) Sfm , i , Namess(1,i) , Namess(2,i)
                        ENDIF
                     ENDIF
                  ELSEIF ( rc==3 ) THEN
                     nogo = 1
                     WRITE (Nout,99003) Sfm , Namess(1,i) , Namess(2,i)
99003                FORMAT (A25,' 6303, H OR G TRANSFORMATION MATRIX FOR SUBSTRUCTURE',1X,2A4,' CANNOT BE FOUND ON SOF')
                  ELSEIF ( rc==4 .OR. rc==5 ) THEN
                     nogo = 1
                     CALL smsg(rc-2,horg,Namess(1,i))
                  ELSE
                     nogo = 1
                     CALL smsg(1,horg,Namess(1,i))
                  ENDIF
               ENDIF
            ENDDO
            IF ( nogo==0 ) THEN
!
               IF ( nmat/=0 ) THEN
!
!     DETERMINE PRECISION FOR FINAL MATRIX
!
                  iprc = 1
                  ityp = 0
                  DO i = 1 , nmat
                     IF ( amcb(5,i)==2 .OR. amcb(5,i)==4 ) iprc = 2
                     IF ( amcb(5,i)>=3 ) ityp = 2
                  ENDDO
                  Iprec = ityp + iprc
!
                  IF ( item==pvec .OR. item==papp ) THEN
!                                          ******
!                                                    *
!     PROCESS LOAD MATRICES                          *
!                                                    *
!****                                           ******
                     Mcbc(1) = 0
                     Mcba(1) = scr2
                     Tflag = 1
                     Mrgz = Lcore
                     Prec = 0
!
!     SELECT FIRST RESULT FILE SO THAT FINAL RESULT WILL WIND UP ON
!     ACOMB
!
                     Rfiles(2) = scr3
                     Rfiles(3) = scr4
                     irf = 1
                     IF ( nmat==2 .OR. nmat==5 ) irf = 2
                     IF ( nmat==3 .OR. nmat==6 ) irf = 3
                     IF ( nmat==1 ) Mcbp(1) = Acomb
!
!     CREATE COLUMN PARTITIONING VECTOR FOR ALL MERGES
!     (VECTOR IS ALWAYS NULL)
!
                     CALL makmcb(cpv,Scr6,nsize,Rect,Rsp)
                     Typin = Rsp
                     Typout = Rsp
                     Irow = 1
                     Nrow = 1
                     Incr = 1
                     CALL gopen(Scr6,Z(buf1),Wrtrew)
                     CALL pack(0,Scr6,cpv)
                     CALL close(Scr6,Rew)
                     addflg = .TRUE.
!
                     DO Kk = 1 , nmat
                        inuse = Use(2*Kk-1)
!
!     COPY TRANSFORMATION MATRIX TO SCR2
!
                        CALL mtrxi(scr2,Namess(1,inuse),horg,Z(buf1),rc)
!
!     IF LOAD MATRIX IS ON SOF, COPY IT TO SCR1
!
                        Mcbb(1) = 100 + inuse
                        IF ( amcb(1,inuse)<=0 ) THEN
                           Mcbb(1) = scr1
                           CALL mtrxi(scr1,Namess(1,inuse),item,Z(buf1),rc)
                        ENDIF
!
!     MULTIPLY (HT * A) AND STORE RESULT ON RFILES(IRF)
!     (ACOMB,SCR3, OR SCR4)
!
                        CALL sofcls
                        DO j = 2 , 7
                           Mcba(j) = hmcb(j-1,inuse)
                           Mcbb(j) = amcb(j,inuse)
                        ENDDO
                        IF ( Mcbb(6)==0 .OR. Mcba(3)==Mcbb(3) ) THEN
                           CALL makmcb(Mcbd,Rfiles(irf),hmcb(1,inuse),Rect,Iprec)
!
                           CALL mpyad(Z,Z,Z)
!
                           IF ( addflg ) THEN
                              DO j = 2 , 7
                                 Mcbp(j) = Mcbd(j)
                              ENDDO
                           ELSE
!
!     COMPUTE ROW PARTITIONING VECTOR TO MERGE RESULT OF THIS MULTIPLY
!     WITH ALL PREVIOUS RESULTS
!
                              k = amcb(2,inuse)
                              CALL makmcb(rpv,Scr5,rsofar+k,Rect,Rsp)
                              IF ( k>Lcore ) THEN
                                 spag_nextblock_1 = 4
                                 CYCLE SPAG_DispatchLoop_1
                              ENDIF
                              DO j = 1 , k
                                 Z(j) = 1.0E0
                              ENDDO
                              Typin = Rsp
                              Typout = Rsp
                              Irow = rsofar + 1
                              Nrow = rsofar + k
                              Incr = 1
                              CALL gopen(Scr5,Z(buf1),Wrtrew)
                              CALL pack(Z,Scr5,rpv)
                              CALL close(Scr5,Rew)
!
!     MERGE MATRICES   STORE RESULT ON NEXT AVAILABLE RFILE
!
                              j = mod(irf,3) + 1
                              CALL makmcb(Mcbp,Rfiles(j),nsize,Rect,Iprec)
                              Mcbp(2) = rpv(3)
                              j = mod(j,3) + 1
                              Mcbp11(1) = Rfiles(j)
                              Mcbp12(1) = Rfiles(irf)
                              DO j = 2 , 7
                                 Mcbp11(j) = Mcbp(j)
                                 Mcbp12(j) = Mcbd(j)
                              ENDDO
!
                              CALL merge(rpv,cpv,Z)
!
                              irf = mod(irf,3) + 1
                           ENDIF
                           rsofar = rsofar + amcb(2,inuse)
                           addflg = .FALSE.
                           irf = mod(irf,3) + 1
                           CALL sofopn(Z(sof1),Z(sof2),Z(sof3))
                        ELSE
                           i = Kk
                           nogo = 1
                           WRITE (Nout,99005) Sfm , i , Namess(1,i) , Namess(2,i)
                           GOTO 10
                        ENDIF
                     ENDDO
                     CALL wrttrl(Mcbp)
                  ELSE
!                                               ******
!                                                         *
!     PROCESS STIFFNESS, MASS OR DAMPING MATRICES         *
!                                                         *
!                                               ******
!
!     IF NMAT IS ODD, PUT FIRST RESULT ON ACOMB.  IF EVEN, PUT IT ON
!     SCR4.  FINAL RESULT WILL THEN BE ON ACOMB.
!
                     CALL sort(0,0,2,2,Use,2*nmat)
                     irf = 1
                     IF ( (nmat/2)*2==nmat ) irf = 2
                     iform = 6
                     Rfiles(2) = scr4
                     addflg = .FALSE.
!
                     DO Kk = 1 , nmat
                        j = 2*Kk - 1
                        Jn = Jn + 2
                        inuse = Use(Jn)
!
!     MOVE TRANSFORMATION MATRIX TO SCR2
!
                        CALL mtrxi(scr2,Namess(1,inuse),horg,Z(buf1),rc)
!
!     IF INPUT MATRIX IS ON SOF, MOVE IT TO SCR1
!
                        Mcbb2(1) = 100 + inuse
                        IF ( amcb(1,inuse)<=0 ) THEN
                           Mcbb2(1) = scr1
                           CALL mtrxi(scr1,Namess(1,inuse),item,Z(buf1),rc)
                        ENDIF
!
!     PERFORM TRIPLE MULTIPLY  H(T)*INPUT*H
!
                        CALL sofcls
                        Mcba2(1) = scr2
                        Mcbc2(1) = 0
                        IF ( addflg ) Mcbc2(1) = Rfiles(3-irf)
                        addflg = .TRUE.
                        DO j = 2 , 7
                           Mcba2(j) = hmcb(j-1,inuse)
                           Mcbb2(j) = amcb(j,inuse)
                        ENDDO
                        IF ( Mcbb2(4)<=2 ) iform = Mcbb2(4)
                        CALL makmcb(Mcbd2,Rfiles(irf),hmcb(1,inuse),iform,Iprec)
!
                        CALL mpy3dr(Z)
!
                        CALL wrttrl(Mcbd2)
                        DO j = 2 , 7
                           Mcbc2(j) = Mcbd2(j)
                        ENDDO
                        irf = 3 - irf
                        CALL sofopn(Z(sof1),Z(sof2),Z(sof3))
                     ENDDO
                  ENDIF
               ENDIF
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
 10         Dry = -2
            WRITE (Nout,99004) amcb , hmcb
99004       FORMAT ('0*** COMB2 MATRIX TRAILER DUMP',//7(4X,7I10/),/7(11X,6I10/))
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ELSE
            CALL mesage(8,0,name)
            Dry = -2
            RETURN
         ENDIF
      CASE (4)
         CALL mesage(8,0,name)
         spag_nextblock_1 = 5
      CASE (5)
!
!     NORMAL COMPLETION
!
         CALL sofcls
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99005 FORMAT (A25,' 6304, MODULE COMB2 INPUT MATRIX NUMBER ',I2,' FOR SUBSTRUCTURE ,2A4,28H HAS INCOMPATIBLE DIMENSIONS')
END SUBROUTINE comb2
