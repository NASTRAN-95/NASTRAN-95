!*==comb2.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE comb2
   USE c_blank
   USE c_mpy3tl
   USE c_mpyadx
   USE c_names
   USE c_packx
   USE c_parmeg
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   USE iso_fortran_env
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
            IF ( namess(i,1)==xxxx .OR. namess(i,1)==0 ) namess(i,1) = blank
         ENDDO
         acomb = 201
         scr1 = 301
         scr2 = 302
         scr3 = 303
         scr4 = 304
         scr5 = 305
         scr6 = 306
         scr7 = 307
         signab = 1
         signc = 1
         prec = 0
         mscr = scr5
         icode = 0
         rule = 0
         rdsof = 1
         nogo = 0
         rfiles(1) = acomb
         nsize = 0
         mcbp21(1) = 0
         mcbp22(1) = 0
         rsofar = 0
         kn = 1
         jn = -1
         DO i = 1 , 5
            IF ( type(1)==kmp(i) ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         WRITE (nout,99001) sfm , type
99001    FORMAT (A25,' 6302, ',2A4,' IS ILLEGAL MATRIX TYPE FOR MODULE ','COMB2')
         IF ( dry<0 ) RETURN
!
         dry = -2
         item = 0
         spag_nextblock_1 = 3
      CASE (2)
         item = kmpitm(i)
         IF ( item==pvec ) item = pora(1)
         IF ( dry<0 ) RETURN
         spag_nextblock_1 = 3
      CASE (3)
!
         lcore = korsz(z) - 1
         lkore = lcore
         buf1 = lcore - sysbuf + 1
         sof1 = buf1 - sysbuf
         sof2 = sof1 - sysbuf - 1
         sof3 = sof2 - sysbuf
         IF ( sof3>0 ) THEN
!
            CALL sofopn(z(sof1),z(sof2),z(sof3))
!
!     GRAB THE MATRIX CONTROL BLOCKS
!
            nmat = 0
            DO i = 1 , 7
               IF ( namess(1,i)/=blank ) THEN
                  amcb(1,i) = 100 + i
                  CALL rdtrl(amcb(1,i))
                  IF ( amcb(1,i)<=0 ) THEN
!
!     NO GINO FILE.  CHECK SOF
!
                     CALL softrl(namess(1,i),item,amcb(1,i))
                     rc = amcb(1,i)
                     IF ( rc==1 ) THEN
!
!     MATRIX FOUND ON SOF
!
                        amcb(1,i) = 0
                        GOTO 2
                     ELSEIF ( rc==3 ) THEN
                        IF ( type(1)==kmp(3) ) CYCLE
                     ELSEIF ( rc/=4 .AND. rc/=5 ) THEN
                        nogo = 1
                        WRITE (nout,99002) sfm , namess(1,i) , namess(2,i) , item
!
!     DIAGNOSTICS
!
99002                   FORMAT (A25,' 6301, DATA MISSING IN GO MODE FOR SUBSTRUCTURE ',2A4,' ITEM ',A4)
                        CYCLE
                     ENDIF
                     nogo = 1
                     CALL smsg(rc-2,item,namess(1,i))
                     CYCLE
                  ENDIF
!
!     GRAB THE MCB OF THE TRANSFORMATION MATRIX
!
 2                CALL softrl(namess(1,i),horg,mcbtrl)
                  rc = mcbtrl(1)
                  IF ( rc==1 ) THEN
                     DO it = 1 , 6
                        hmcb(it,i) = mcbtrl(it+1)
                     ENDDO
                     nmat = nmat + 1
                     use(2*nmat-1) = i
                     den = float(amcb(7,i))/10000.
                     use(2*nmat) = amcb(2,i)*amcb(3,i)*den
!
!     CHECK COMPATIBILITY OF DIMENSIONS
!
                     IF ( nsize==0 ) nsize = hmcb(1,i)
                     IF ( hmcb(1,i)/=nsize .OR. hmcb(2,i)/=amcb(2,i) .OR. hmcb(2,i)/=amcb(3,i) ) THEN
                        IF ( .NOT.(item==pvec .OR. item==papp .AND. hmcb(1,i)==nsize .AND. hmcb(2,i)==amcb(3,i)) ) THEN
                           nogo = 1
                           WRITE (nout,99005) sfm , i , namess(1,i) , namess(2,i)
                        ENDIF
                     ENDIF
                  ELSEIF ( rc==3 ) THEN
                     nogo = 1
                     WRITE (nout,99003) sfm , namess(1,i) , namess(2,i)
99003                FORMAT (A25,' 6303, H OR G TRANSFORMATION MATRIX FOR SUBSTRUCTURE',1X,2A4,' CANNOT BE FOUND ON SOF')
                  ELSEIF ( rc==4 .OR. rc==5 ) THEN
                     nogo = 1
                     CALL smsg(rc-2,horg,namess(1,i))
                  ELSE
                     nogo = 1
                     CALL smsg(1,horg,namess(1,i))
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
                  iprec = ityp + iprc
!
                  IF ( item==pvec .OR. item==papp ) THEN
!                                          ******
!                                                    *
!     PROCESS LOAD MATRICES                          *
!                                                    *
!****                                           ******
                     mcbc(1) = 0
                     mcba(1) = scr2
                     tflag = 1
                     mrgz = lcore
                     prec = 0
!
!     SELECT FIRST RESULT FILE SO THAT FINAL RESULT WILL WIND UP ON
!     ACOMB
!
                     rfiles(2) = scr3
                     rfiles(3) = scr4
                     irf = 1
                     IF ( nmat==2 .OR. nmat==5 ) irf = 2
                     IF ( nmat==3 .OR. nmat==6 ) irf = 3
                     IF ( nmat==1 ) mcbp(1) = acomb
!
!     CREATE COLUMN PARTITIONING VECTOR FOR ALL MERGES
!     (VECTOR IS ALWAYS NULL)
!
                     CALL makmcb(cpv,scr6,nsize,rect,rsp)
                     typin = rsp
                     typout = rsp
                     irow = 1
                     nrow = 1
                     incr = 1
                     CALL gopen(scr6,z(buf1),wrtrew)
                     CALL pack(0,scr6,cpv)
                     CALL close(scr6,rew)
                     addflg = .TRUE.
!
                     DO kk = 1 , nmat
                        inuse = use(2*kk-1)
!
!     COPY TRANSFORMATION MATRIX TO SCR2
!
                        CALL mtrxi(scr2,namess(1,inuse),horg,z(buf1),rc)
!
!     IF LOAD MATRIX IS ON SOF, COPY IT TO SCR1
!
                        mcbb(1) = 100 + inuse
                        IF ( amcb(1,inuse)<=0 ) THEN
                           mcbb(1) = scr1
                           CALL mtrxi(scr1,namess(1,inuse),item,z(buf1),rc)
                        ENDIF
!
!     MULTIPLY (HT * A) AND STORE RESULT ON RFILES(IRF)
!     (ACOMB,SCR3, OR SCR4)
!
                        CALL sofcls
                        DO j = 2 , 7
                           mcba(j) = hmcb(j-1,inuse)
                           mcbb(j) = amcb(j,inuse)
                        ENDDO
                        IF ( mcbb(6)==0 .OR. mcba(3)==mcbb(3) ) THEN
                           CALL makmcb(mcbd,rfiles(irf),hmcb(1,inuse),rect,iprec)
!
                           CALL mpyad(z,z,z)
!
                           IF ( addflg ) THEN
                              DO j = 2 , 7
                                 mcbp(j) = mcbd(j)
                              ENDDO
                           ELSE
!
!     COMPUTE ROW PARTITIONING VECTOR TO MERGE RESULT OF THIS MULTIPLY
!     WITH ALL PREVIOUS RESULTS
!
                              k = amcb(2,inuse)
                              CALL makmcb(rpv,scr5,rsofar+k,rect,rsp)
                              IF ( k>lcore ) THEN
                                 spag_nextblock_1 = 4
                                 CYCLE SPAG_DispatchLoop_1
                              ENDIF
                              DO j = 1 , k
                                 z(j) = 1.0E0
                              ENDDO
                              typin = rsp
                              typout = rsp
                              irow = rsofar + 1
                              nrow = rsofar + k
                              incr = 1
                              CALL gopen(scr5,z(buf1),wrtrew)
                              CALL pack(z,scr5,rpv)
                              CALL close(scr5,rew)
!
!     MERGE MATRICES   STORE RESULT ON NEXT AVAILABLE RFILE
!
                              j = mod(irf,3) + 1
                              CALL makmcb(mcbp,rfiles(j),nsize,rect,iprec)
                              mcbp(2) = rpv(3)
                              j = mod(j,3) + 1
                              mcbp11(1) = rfiles(j)
                              mcbp12(1) = rfiles(irf)
                              DO j = 2 , 7
                                 mcbp11(j) = mcbp(j)
                                 mcbp12(j) = mcbd(j)
                              ENDDO
!
                              CALL merge(rpv,cpv,z)
!
                              irf = mod(irf,3) + 1
                           ENDIF
                           rsofar = rsofar + amcb(2,inuse)
                           addflg = .FALSE.
                           irf = mod(irf,3) + 1
                           CALL sofopn(z(sof1),z(sof2),z(sof3))
                        ELSE
                           i = kk
                           nogo = 1
                           WRITE (nout,99005) sfm , i , namess(1,i) , namess(2,i)
                           GOTO 10
                        ENDIF
                     ENDDO
                     CALL wrttrl(mcbp)
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
                     CALL sort(0,0,2,2,use,2*nmat)
                     irf = 1
                     IF ( (nmat/2)*2==nmat ) irf = 2
                     iform = 6
                     rfiles(2) = scr4
                     addflg = .FALSE.
!
                     DO kk = 1 , nmat
                        j = 2*kk - 1
                        jn = jn + 2
                        inuse = use(jn)
!
!     MOVE TRANSFORMATION MATRIX TO SCR2
!
                        CALL mtrxi(scr2,namess(1,inuse),horg,z(buf1),rc)
!
!     IF INPUT MATRIX IS ON SOF, MOVE IT TO SCR1
!
                        mcbb2(1) = 100 + inuse
                        IF ( amcb(1,inuse)<=0 ) THEN
                           mcbb2(1) = scr1
                           CALL mtrxi(scr1,namess(1,inuse),item,z(buf1),rc)
                        ENDIF
!
!     PERFORM TRIPLE MULTIPLY  H(T)*INPUT*H
!
                        CALL sofcls
                        mcba2(1) = scr2
                        mcbc2(1) = 0
                        IF ( addflg ) mcbc2(1) = rfiles(3-irf)
                        addflg = .TRUE.
                        DO j = 2 , 7
                           mcba2(j) = hmcb(j-1,inuse)
                           mcbb2(j) = amcb(j,inuse)
                        ENDDO
                        IF ( mcbb2(4)<=2 ) iform = mcbb2(4)
                        CALL makmcb(mcbd2,rfiles(irf),hmcb(1,inuse),iform,iprec)
!
                        CALL mpy3dr(z)
!
                        CALL wrttrl(mcbd2)
                        DO j = 2 , 7
                           mcbc2(j) = mcbd2(j)
                        ENDDO
                        irf = 3 - irf
                        CALL sofopn(z(sof1),z(sof2),z(sof3))
                     ENDDO
                  ENDIF
               ENDIF
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
 10         dry = -2
            WRITE (nout,99004) amcb , hmcb
99004       FORMAT ('0*** COMB2 MATRIX TRAILER DUMP',//7(4X,7I10/),/7(11X,6I10/))
            spag_nextblock_1 = 5
         ELSE
            CALL mesage(8,0,name)
            dry = -2
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
