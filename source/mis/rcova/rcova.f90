!*==rcova.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rcova
   USE c_blank
   USE c_rcovcm
   USE c_rcovcr
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , rc
   INTEGER , DIMENSION(5) , SAVE :: km , kmu
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , SAVE :: phis , schk , scr1 , soln , uvec
   EXTERNAL delete , korsz , mesage , mtrxi , mtrxo , rcovds , rcovms , rcovss , sfetch , smsg , sofcls , sofopn
!
! End of declarations rewritten by SPAG
!
!
!     RCOVA CREATES THE SOLN ITEM FOR A FINAL SOLUTION STRUCTURE (FSS)
!     IN PHASE 2 OF SUBSTRUCTURING
!
   !>>>>EQUIVALENCE (Z(1),Iz(1))
   DATA soln , uvec , phis/4HSOLN , 4HUVEC , 4HPHIS/
   DATA km/4HKMTX , 4HMMTX , 4HUVEC , 4HBMTX , 4HK4MX/
   DATA kmu/103 , 104 , 106 , 109 , 110/
   DATA schk/3/
   DATA scr1/301/
   DATA name/4HRCOV , 4HA   /
!
!     INITIALIZE
!
   sof1 = korsz(z) - lreq - sysbuf + 1
   sof2 = sof1 - sysbuf - 1
   sof3 = sof2 - sysbuf
   buf1 = sof3 - sysbuf
   buf2 = buf1 - sysbuf
   buf3 = buf2 - sysbuf
   icore = 1
   lcore = buf3 - 1
   IF ( lcore<=0 ) CALL mesage(-8,0,name)
   CALL sofopn(z(sof1),z(sof2),z(sof3))
!
!     COPY KGG, MGG, UVEC, BGG AND K4GG TO THE SOF IF THEY ARNT THERE
!
   SPAG_Loop_1_2: DO i = 1 , 5
      IF ( .NOT.(km(i)==uvec .AND. mrecvr) ) THEN
         IF ( dry>=0 ) THEN
            SPAG_Loop_2_1: DO
               CALL mtrxo(kmu(i),fss,km(i),z(buf1),rc)
               IF ( rc==1 .OR. rc==3 .OR. rc==6 ) THEN
               ELSEIF ( rc==2 ) THEN
                  CALL delete(fss,km(i),rc)
                  CYCLE
               ELSEIF ( rc==4 .OR. rc==5 ) THEN
                  CALL smsg(rc-2,km(i),fss)
               ELSE
                  EXIT SPAG_Loop_2_1
               ENDIF
               CYCLE SPAG_Loop_1_2
            ENDDO SPAG_Loop_2_1
         ENDIF
         rc = 2
         CALL mtrxo(-1,fss,km(i),0,rc)
      ENDIF
   ENDDO SPAG_Loop_1_2
   IF ( dry>=0 ) THEN
!
!     IF MODAL RECOVER, COPY PHIS ITEM TO UVEC
!
      IF ( mrecvr ) THEN
         rfno = 3
         CALL mtrxi(scr1,fss,phis,0,rc)
         IF ( rc==1 ) THEN
            CALL mtrxo(scr1,fss,uvec,0,rc)
         ELSE
            CALL smsg(rc-2,phis,fss)
            CALL spag_block_1
            RETURN
         ENDIF
      ENDIF
!
!     ATTEMPT TO FETCH SOLN ITEM FOR FSS.  IF IT ALREADY EXISTS, RETURN
!
      CALL sfetch(fss,soln,schk,rc)
      IF ( rc/=1 ) THEN
         IF ( rc/=3 ) THEN
            CALL smsg(rc-2,soln,fss)
!
!     CREATE SOLN ITEM FOR PROPER RIGID FORMAT
!
         ELSEIF ( rfno<0 .OR. rfno>9 ) THEN
!
!     DIAGNOSTICS
!
            CALL mesage(7,0,name)
            CALL spag_block_1
            RETURN
         ELSEIF ( rfno==3 ) THEN
!
!     MODAL SOLUTION - R.F. 3
!
            CALL rcovms
         ELSEIF ( rfno==4 .OR. rfno==5 .OR. rfno==6 .OR. rfno==7 ) THEN
            CALL mesage(7,0,name)
            CALL spag_block_1
            RETURN
         ELSEIF ( rfno==8 .OR. rfno==9 ) THEN
!
!     DYNAMIC SOLUTION - R.F. 8 AND 9
!
            CALL rcovds
         ELSE
!
!     STATIC SOLUTION - R.F. 1 AND 2
!
            CALL rcovss
         ENDIF
      ENDIF
   ENDIF
!
!     FINISHED
!
   CALL sofcls
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
      iopt = -1
      CALL sofcls
   END SUBROUTINE spag_block_1
END SUBROUTINE rcova
