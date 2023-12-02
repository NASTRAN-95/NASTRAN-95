!*==rcova.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rcova
   IMPLICIT NONE
   USE C_BLANK
   USE C_RCOVCM
   USE C_RCOVCR
   USE C_SYSTEM
   USE C_ZZZZZZ
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
   Sof1 = korsz(Z) - Lreq - Sysbuf + 1
   Sof2 = Sof1 - Sysbuf - 1
   Sof3 = Sof2 - Sysbuf
   Buf1 = Sof3 - Sysbuf
   Buf2 = Buf1 - Sysbuf
   Buf3 = Buf2 - Sysbuf
   Icore = 1
   Lcore = Buf3 - 1
   IF ( Lcore<=0 ) CALL mesage(-8,0,name)
   CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
!
!     COPY KGG, MGG, UVEC, BGG AND K4GG TO THE SOF IF THEY ARNT THERE
!
   SPAG_Loop_1_2: DO i = 1 , 5
      IF ( .NOT.(km(i)==uvec .AND. Mrecvr) ) THEN
         IF ( Dry>=0 ) THEN
            SPAG_Loop_2_1: DO
               CALL mtrxo(kmu(i),Fss,km(i),Z(Buf1),rc)
               IF ( rc==1 .OR. rc==3 .OR. rc==6 ) THEN
               ELSEIF ( rc==2 ) THEN
                  CALL delete(Fss,km(i),rc)
                  CYCLE
               ELSEIF ( rc==4 .OR. rc==5 ) THEN
                  CALL smsg(rc-2,km(i),Fss)
               ELSE
                  EXIT SPAG_Loop_2_1
               ENDIF
               CYCLE SPAG_Loop_1_2
            ENDDO SPAG_Loop_2_1
         ENDIF
         rc = 2
         CALL mtrxo(-1,Fss,km(i),0,rc)
      ENDIF
   ENDDO SPAG_Loop_1_2
   IF ( Dry>=0 ) THEN
!
!     IF MODAL RECOVER, COPY PHIS ITEM TO UVEC
!
      IF ( Mrecvr ) THEN
         Rfno = 3
         CALL mtrxi(scr1,Fss,phis,0,rc)
         IF ( rc==1 ) THEN
            CALL mtrxo(scr1,Fss,uvec,0,rc)
         ELSE
            CALL smsg(rc-2,phis,Fss)
            CALL spag_block_1
            RETURN
         ENDIF
      ENDIF
!
!     ATTEMPT TO FETCH SOLN ITEM FOR FSS.  IF IT ALREADY EXISTS, RETURN
!
      CALL sfetch(Fss,soln,schk,rc)
      IF ( rc/=1 ) THEN
         IF ( rc/=3 ) THEN
            CALL smsg(rc-2,soln,Fss)
!
!     CREATE SOLN ITEM FOR PROPER RIGID FORMAT
!
         ELSEIF ( Rfno<0 .OR. Rfno>9 ) THEN
!
!     DIAGNOSTICS
!
            CALL mesage(7,0,name)
            CALL spag_block_1
            RETURN
         ELSEIF ( Rfno==3 ) THEN
!
!     MODAL SOLUTION - R.F. 3
!
            CALL rcovms
         ELSEIF ( Rfno==4 .OR. Rfno==5 .OR. Rfno==6 .OR. Rfno==7 ) THEN
            CALL mesage(7,0,name)
            CALL spag_block_1
            RETURN
         ELSEIF ( Rfno==8 .OR. Rfno==9 ) THEN
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
      Iopt = -1
      CALL sofcls
   END SUBROUTINE spag_block_1
END SUBROUTINE rcova
