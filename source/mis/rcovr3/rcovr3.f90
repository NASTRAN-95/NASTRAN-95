!*==rcovr3.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rcovr3
   IMPLICIT NONE
   USE C_BLANK
   USE C_NAMES
   USE C_OUTPUT
   USE C_PACKX
   USE C_SYSTEM
   USE C_UNPAKX
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: buf1 , buf2 , buf3 , buf4 , file , i , j , lcore , n , nc , neigv , nogo , rc
   LOGICAL :: first
   INTEGER , DIMENSION(2) :: fss
   INTEGER , SAVE :: iblank , lama , pg , pgs , po , pos , ps , pss , qas , qvec , scr1 , scr2 , scr3 , soln , srd , uas , uvec ,   &
                   & ys , yss
   INTEGER , DIMENSION(3) :: initm , outdb
   INTEGER , DIMENSION(4) :: ivec , ovec
   INTEGER , DIMENSION(10) :: iz
   INTEGER , DIMENSION(7) :: mcbtrl
   INTEGER , DIMENSION(2) , SAVE :: subr
   EXTERNAL close , fdsub , fname , gopen , korsz , makmcb , mesage , mtrxi , open , pack , rcovsl , rdtrl , sfetch , sjump , smsg ,&
          & sofcls , sofopn , softrl , suread , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THE RCOVR3 MODULE RECOVERS DATA FOR SUBSTRUCTURE PHASE 3.
!
!     DISPLACEMENTS AND REACTIONS ARE COPIED FROM THE SOF TO GINO FILES.
!     FOR NORMAL MODES, LAMA IS CREATED FROM THE SOLN ITEM.
!     FOR STATICS, THE LOADS AND ENFORCED DISPLACEMENTS ARE FACTORED
!     AND COMBINED TO CORRESPOND WITH THE PHASE 2 SOLUTION SUBCASES.
!
!     JANUARY 1974
!
   !>>>>EQUIVALENCE (pg,ivec(1)) , (pgs,ovec(1)) , (ps,ivec(2)) , (pss,ovec(2)) , (po,ivec(3)) , (pos,ovec(3)) , (ys,ivec(4)) ,          &
!>>>>    & (yss,ovec(4)) , (soln,initm(1)) , (lama,outdb(1)) , (uvec,initm(2)) , (uas,outdb(2)) , (qvec,initm(3)) , (qas,outdb(3)) ,     &
!>>>>    & (Z(1),Iz(1))
   DATA pg , ps , po , ys , uas , qas , pgs , pss , pos , yss , lama/101 , 102 , 103 , 104 , 201 , 202 , 203 , 204 , 205 , 206 ,    &
      & 207/
   DATA scr1 , scr2 , scr3/301 , 302 , 303/
   DATA soln , uvec , qvec , iblank/4HSOLN , 4HUVEC , 4HQVEC , 4H    /
   DATA subr , srd/4HRCOV , 4HR3   , 1/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     INITIALIZATION
!
         lcore = korsz(Z)
         buf1 = lcore - Sysbuf + 1
         buf2 = buf1 - Sysbuf - 1
         buf3 = buf2 - Sysbuf
         buf4 = buf3 - Sysbuf
         lcore = buf4 - 1
         IF ( lcore<=0 ) CALL mesage(-8,0,subr)
         nogo = 0
         Itypp = 1
         Otypp = 1
         Irowp = 1
         Incp = 1
         Otypun = 1
         Irowun = 1
         Incun = 1
         first = .FALSE.
         CALL sofopn(Z(buf1),Z(buf2),Z(buf3))
         DO i = 1 , 6
            Here(i) = 0
         ENDDO
!
!     CHECK DATA
!
!     NO EXTRA POINTS
!
         IF ( Noue/=-1 ) THEN
!
!     ABNORMAL MODULE EXITS
!
            WRITE (Nout,99001) Ufm
99001       FORMAT (A23,' 6372, NO EXTRA POINTS ALLOWED IN PHASE 3 ','SUBSTRUCTURING.')
            n = -61
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     SUBSTRUCTURE NAME
!
            CALL fdsub(Name,rc)
            IF ( rc==-1 ) CALL smsg(-2,iblank,Name)
!
!     PAIRS OF INPUT ITEMS AND OUTPUT BLOCKS
!
            CALL sfetch(Name,soln,srd,rc)
            IF ( rc/=1 ) CALL smsg(2-rc,soln,Name)
            IF ( Rfno/=1 .AND. Rfno/=2 ) THEN
               Trl(1) = outdb(1)
               CALL rdtrl(Trl)
               IF ( Trl(1)<=0 ) THEN
                  CALL mesage(1,outdb(1),subr)
                  nogo = 1
               ENDIF
            ENDIF
            DO i = 2 , 3
               IF ( .NOT.(i==1 .AND. (Rfno==1 .OR. Rfno==2)) ) THEN
                  CALL softrl(Name,initm(i),mcbtrl)
                  rc = mcbtrl(1)
                  IF ( rc==1 ) THEN
                     Trl(1) = outdb(i)
                     CALL rdtrl(Trl)
                     IF ( Trl(1)>0 ) THEN
                        Here(i-1) = 1
                     ELSE
                        CALL mesage(1,outdb(i),subr)
                        nogo = 1
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
!
!     PAIRS OF DATA BLOCKS
!
            IF ( Rfno/=3 .AND. Rfno/=8 ) THEN
               DO i = 1 , 4
                  Trl(1) = ivec(i)
                  CALL rdtrl(Trl)
                  IF ( Trl(1)>=0 ) THEN
                     IF ( i/=4 .OR. Trl(6)/=0 ) THEN
                        Trl(1) = ovec(i)
                        CALL rdtrl(Trl)
                        IF ( Trl(1)>0 ) THEN
                           Here(i+2) = 1
                        ELSE
                           CALL mesage(1,ovec(i),subr)
                           nogo = 1
                        ENDIF
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
!
!     TERMINATE IF THERE WERE ERRORS
!
            IF ( nogo/=0 ) THEN
               n = -37
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ELSE
!
!     COPY DISPLACEMENTS AND REACTIONS FROM SOF TO GINO FILES
!
               IF ( Here(1)==1 ) CALL mtrxi(uas,Name,uvec,Z(buf4),rc)
               IF ( Here(2)==1 ) CALL mtrxi(qas,Name,qvec,Z(buf4),rc)
!
!     BRANCH ON RIGID FORMAT NUMBER
!
               IF ( Rfno==3 ) THEN
!
!     RIGID FORMAT  3 -- NORMAL MODES
!     *******************************
!
!     WRITE NULL REACTIONS MATRIX TO PREVENT ERROR 3007 IN UMERGE
!
                  IF ( Here(2)/=1 ) THEN
                     Nrowp = 1
                     CALL makmcb(Trl,qas,1,2,1)
                     CALL gopen(qas,Z(buf4),Wrtrew)
                     CALL pack(0,qas,Trl)
                     CALL close(qas,Rew)
                     CALL wrttrl(Trl)
                  ENDIF
!
!     GENERATE OFP ID RECORD FOR LAMA
!
                  IF ( lcore<146 ) THEN
                     n = -8
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     CALL gopen(lama,Z(buf4),Wrtrew)
                     DO i = 3 , 50
                        iz(i) = 0
                     ENDDO
                     iz(1) = 21
                     iz(2) = 6
                     iz(10) = 7
                     DO i = 1 , 96
                        iz(i+50) = Titles(i)
                     ENDDO
                     CALL write(lama,Z,146,1)
!
!     GET SOLN ITEM AND CHECK GROUP 0 DATA
!
                     CALL sfetch(Name,soln,srd,rc)
                     IF ( rc/=1 ) CALL smsg(2-rc,soln,Name)
                     CALL suread(fss,2,n,rc)
                     WRITE (Nout,99005) Uim , fss , Name
                     CALL suread(Ibuf,-1,n,rc)
                     IF ( Ibuf(1)/=Rfno ) THEN
                        WRITE (Nout,99002) Sfm , Ibuf(1) , Rfno
                        n = -61
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
                     ELSE
                        neigv = Ibuf(2)
                        IF ( neigv>0 ) THEN
                           SPAG_Loop_1_1: DO
!
!     COPY SOLN GROUP 1 TO LAMA RECORD 2 AND WRITE NON-ZERO TRAILER
!
                              CALL suread(Z,lcore,n,rc)
                              CALL write(lama,Z,n,0)
                              IF ( rc/=1 ) THEN
                                 CALL write(lama,0,0,1)
                                 CALL close(lama,Rew)
                                 CALL makmcb(Trl,lama,0,0,0)
                                 Trl(2) = 1
                                 CALL wrttrl(Trl)
                                 EXIT SPAG_Loop_1_1
                              ENDIF
                           ENDDO SPAG_Loop_1_1
                        ELSE
!
!     NO EIGENVALUES.  WRITE ZERO TRAILER TO INDICATE LAMA IS PURGED
!
                           CALL close(lama,Rew)
                           CALL makmcb(Trl,lama,0,0,0)
                           CALL wrttrl(Trl)
                           WRITE (Nout,99003) Uwm
99003                      FORMAT (A25,' 6323, NO EIGENVALUES FOR THIS SOLUTION')
                           spag_nextblock_1 = 3
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                     ENDIF
                  ENDIF
               ELSE
!
!     RIGID FORMAT  1 -- STATIC
!     RIGID FORMAT  2 -- INERTIAL RELIEF
!     RIGID FORMAT  8 -- FREQUENCY RESPONSE
!     RIGID FORMAT  9 -- TRANSIENT RESPONSE
!     *************************************
!
!     FETCH SOLN ITEM AND PROCESS GROUP 0 DATA
!
                  CALL sfetch(Name,soln,srd,rc)
                  IF ( rc/=1 ) CALL smsg(2-rc,soln,Name)
                  CALL suread(fss,2,n,rc)
                  WRITE (Nout,99005) Uim , fss , Name
                  CALL suread(Ibuf,3,n,rc)
                  IF ( Ibuf(1)/=Rfno ) THEN
                     WRITE (Nout,99002) Sfm , Ibuf(1) , Rfno
                     n = -61
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ELSEIF ( Ibuf(2)/=1 ) THEN
                     WRITE (Nout,99004) Ufm , Name
99004                FORMAT (A23,' 6324, PHASE 3 RECOVER ATTEMPTED FOR NON-BASIC ','SUBSTRUCTURE ',2A4)
                     n = -61
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     nc = Ibuf(3)
!
!     WRITE NULL REACTIONS MATRIX TO PREVENT ERROR 3007 IN UMERGE
!
                     IF ( Here(2)/=1 ) THEN
                        Nrowp = 1
                        CALL makmcb(Trl,qas,1,2,1)
                        CALL gopen(qas,Z(buf4),Wrtrew)
                        DO i = 1 , nc
                           CALL pack(0,qas,Trl)
                        ENDDO
                        CALL close(qas,Rew)
                        CALL wrttrl(Trl)
                     ENDIF
!
!     COPY FREQUENCIES ONTO PPF OR TIME STEPS ONTO TOL
!
                     IF ( Rfno>=8 ) THEN
                        j = 1
                        CALL sjump(j)
                        file = lama
                        CALL open(*20,lama,Z(buf4),Wrtrew)
                        CALL fname(lama,Ibuf)
                        CALL write(lama,Ibuf,2,0)
                        SPAG_Loop_1_2: DO
                           CALL suread(Z,lcore,n,rc)
                           CALL write(lama,Z,n,0)
                           IF ( rc/=1 ) THEN
                              CALL write(lama,0,0,1)
!
!     WRITE NULL DYNAMIC LOADS MATRIX ONTO PPF
!
                              CALL makmcb(Trl,lama,1,2,1)
                              IF ( Rfno/=9 ) THEN
                                 DO i = 1 , nc
                                    CALL pack(0,lama,Trl)
                                 ENDDO
                              ENDIF
                              CALL wrttrl(Trl)
                              CALL close(lama,Rew)
                              EXIT SPAG_Loop_1_2
                           ENDIF
                        ENDDO SPAG_Loop_1_2
                     ENDIF
!
!     FOR EACH SUBCASE READ FROM THE SOLN, FORM A COMBINED VECTOR FROM
!     THE VECTORS OF THE APPLIED LOADS OR ENFORCED DISPLACEMENTS DATA
!     BLOCKS
!
                     lcore = buf3 - 1
                     DO i = 1 , 4
                        IF ( Here(i+2)/=0 ) THEN
                           CALL rcovsl(Name,0,ivec(i),scr1,scr2,scr3,ovec(i),Z,Z,lcore,first,Rfno)
                           IF ( ovec(i)/=0 ) first = .TRUE.
                        ENDIF
                     ENDDO
                  ENDIF
               ENDIF
!
!     NORMAL MODULE EXITS
!
               CALL sofcls
               RETURN
            ENDIF
         ENDIF
 20      n = -1
         spag_nextblock_1 = 2
      CASE (2)
         CALL sofcls
         CALL mesage(n,file,subr)
         spag_nextblock_1 = 3
      CASE (3)
         CALL sofcls
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99002 FORMAT (A25,' 6322, SOLN HAS INCORRECT RIGID FORMAT NUMBER.',/32X,'PHASE 2 RIGID FORMAT WAS',I3,' AND PHASE 3 IS',I3)
!
!     FORMAT STATEMENTS FOR DIAGNOSTIC MESSAGES
!
99005 FORMAT (A29,' 6321, SUBSTRUCTURE PHASE 3 RECOVER FOR FINAL SOLUT','ION STRUCTURE ',2A4,/35X,' AND BASIC SUBSTRUCTURE ',2A4)
END SUBROUTINE rcovr3
