!*==mred1d.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mred1d
   USE c_blank
   USE c_system
   USE c_two
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: dnamic , eedx , komplx , kreal
   INTEGER , DIMENSION(3,3) , SAVE :: eig
   INTEGER :: eigcp , eigtyp , i , imsg , itest , nowdsr , nwds2r
   INTEGER , DIMENSION(3) :: eigcpr
   INTEGER , DIMENSION(7) :: eigtrl
   INTEGER , DIMENSION(3) , SAVE :: letr
   INTEGER , DIMENSION(2) , SAVE :: modnam
   EXTERNAL close , gopen , locate , mesage , orf , preloc , read , sofcls , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS SUBROUTINE GENERATES THE EEDX DATA BLOCK USING THE EED DATA
!     BLOCK FORMAT FROM THE EIGR OR EIGC AND EIGP BULK DATA FOR THE
!     MRED1 MODULE.
!
!     INPUT  DATA
!     GINO - DYNAMICS - EIGC DATA
!                       EIGP DATA
!                       EIGR DATA
!
!     OUTPUT DATA
!     GINO - EEDX     - EIGC DATA
!                       EIGP DATA
!                       EIGR DATA
!
!     PARAMETERS
!     INPUT  - DNAMIC - DYNAMICS DATA BLOCK INPUT FILE NUMBER
!              GBUF1  - GINO BUFFER
!              EEDX   - EEDX DATA BLOCK OUTPUT FILE NUMBER
!              KORBGN - BEGINNING ADDRESS OF OPEN CORE
!              IEIG   - EIGENVALUE EXTRACTION SET IDENTIFICATION NUMBER
!     OUTPUT - DRY    - MODULE OPERATION FLAG
!     OTHERS - EIGTYP - EIG CARD TYPE PROCESSING FLAG
!                     = 1, PROCESS EIGC DATA
!                     = 2, PROCESS EIGP DATA
!                     = 3, PROCESS EIGR DATA
!              EIGCP  - EIGC AND EIGP DATA ERROR FLAG
!                     = 0, NO EIGC, EIGP DATA - NO ERROR
!                     = 1, EIGC DATA ONLY - NO ERROR
!                     = 2, EIGP DATA ONLY - ERROR
!                     = 3, EIGC AND EIGP DATA - NO ERROR
!              EIGTRL - EEDX TRAILER
!              EIGCPR - DUMMY EIG(C,P,R) ARRAY
!              EIG    - ARRAY OF EIG(C,P,R) CARD TYPES AND HEADER
!                       INFORMATION
!              KORBGN - BEGINNING ADDRESS OF OPEN CORE
!              NWDS2R - NUMBER OF EIG(C,P,R) WORDS TO READ ON DYNAMIC
!                       DATA FILE
!
   DATA dnamic , eig , eedx/103 , 207 , 2 , 0 , 257 , 4 , 0 , 307 , 3 , 0 , 202/
   DATA modnam , letr/4HMRED , 4H1D   , 1HC , 1HP , 1HR/
   DATA komplx , kreal/4HCOMP , 4HREAL/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     OPEN DYNAMICS, EEDX DATA BLOCKS
!
         IF ( dry==-2 ) RETURN
         IF ( usrmod ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL preloc(*40,z(gbuf1),dnamic)
         CALL gopen(eedx,z(gbuf2),1)
!
!     SET PROCESSING FLAGS
!
         eigtyp = 0
         eigcp = 0
         eigtrl(1) = eedx
         DO i = 2 , 7
            eigtrl(i) = 0
         ENDDO
 20      SPAG_Loop_1_1: DO
!
!     INCREMENT EIG PROCESSING FLAG
!     EIGTYP .EQ. 1, PROCESS EIGC DATA
!     EIGTYP .EQ. 2, PROCESS EIGP DATA
!     EIGTYP .EQ. 3, PROCESS EIGR DATA
!
            eigtyp = eigtyp + 1
            IF ( eigtyp==4 ) THEN
!
!     CLOSE DYNAMICS, EEDX DATA BLOCKS
!
               CALL close(dnamic,1)
               CALL close(eedx,1)
!
!     TEST FOR EIG CARD ERRORS
!
               IF ( eigtrl(2)==0 ) THEN
!
!     PROCESS MODULE FATAL ERRORS
!
                  WRITE (iprntr,99001) ufm , ieig , oldnam
99001             FORMAT (A23,' 6628, NO EIGC OR EIGR CARD SPECIFIED FOR SET ID',I9,', SUBSTRUCTURE ',2A4,1H.)
               ELSEIF ( eigcp==2 ) THEN
                  WRITE (iprntr,99002) ufm , ieig , oldnam
99002             FORMAT (A23,' 6629, NO EIGC DATA CARD SPECIFHIED WITH EIGP DATA ','CARD SET ID',I9,', SUBSTRUCTURE ',2A4,1H.)
               ELSE
!
!     WRITE EEDX DATA BLOCK TRAILER
!
                  CALL wrttrl(eigtrl)
                  EXIT SPAG_Loop_1_1
               ENDIF
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
!
!     SELECT EIG MODE
!
            ELSEIF ( type(1)/=kreal .OR. eigtyp>=3 ) THEN
               IF ( type(1)/=komplx .OR. eigtyp/=3 ) THEN
                  DO i = 1 , 3
                     eigcpr(i) = eig(i,eigtyp)
                  ENDDO
!
!     LOCATE EIG(C,P,R) DATA CARD
!
                  CALL locate(*20,z(gbuf1),eigcpr,itest)
!
!     SET UP EEDX DATA RECORD
!
                  DO i = 1 , 3
                     z(korbgn+i-1) = eigcpr(i)
                  ENDDO
!
!     FIND CORRECT EIG(C,P,R) DATA CARD
!
                  IF ( eigtyp==2 ) THEN
                     nwds2r = 4
                  ELSEIF ( eigtyp==3 ) THEN
                     nwds2r = 18
                  ELSE
                     nwds2r = 10
                  ENDIF
                  SPAG_Loop_2_3: DO
                     CALL read(*60,*80,dnamic,z(korbgn+3),nwds2r,0,nowdsr)
                     IF ( z(korbgn+3)==ieig ) THEN
!
!     SELECT EIG PROCESSING MODE
!
                        IF ( eigtyp==2 ) THEN
!
!     WRITE EIGP DATA ONTO EEDX DATA BLOCK
!
                           CALL write(eedx,z(korbgn),7,1)
                           eigcp = eigcp + 2
                           eigtrl(2) = orf(eigtrl(2),4096)
                        ELSEIF ( eigtyp==3 ) THEN
!
!     WRITE EIGR DATA ONTO EEDX DATA BLOCK
!
                           CALL write(eedx,z(korbgn),21,1)
                           eigtrl(2) = orf(eigtrl(2),8192)
                        ELSE
!
!     WRITE EIGC DATA ONTO EEDX DATA BLOCK
!
                           CALL write(eedx,z(korbgn),13,0)
                           eigtrl(2) = orf(eigtrl(2),16384)
                           eigcp = eigcp + 1
                           SPAG_Loop_3_2: DO
                              CALL read(*60,*80,dnamic,z(korbgn),7,0,nowdsr)
                              IF ( z(korbgn)==-1 ) THEN
                                 CALL write(eedx,z(korbgn),7,1)
                                 EXIT SPAG_Loop_3_2
                              ELSE
                                 CALL write(eedx,z(korbgn),7,0)
                              ENDIF
                           ENDDO SPAG_Loop_3_2
                        ENDIF
                        EXIT SPAG_Loop_2_3
                     ELSEIF ( eigtyp/=2 .AND. eigtyp/=3 ) THEN
                        SPAG_Loop_3_4: DO
!
!     READ REST OF EIGC DATA
!
                           CALL read(*60,*80,dnamic,z(korbgn+3),7,0,nowdsr)
                           IF ( z(korbgn+3)==-1 ) EXIT SPAG_Loop_3_4
                        ENDDO SPAG_Loop_3_4
                     ENDIF
                  ENDDO SPAG_Loop_2_3
               ENDIF
            ENDIF
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 2
      CASE (2)
         RETURN
!
!     PROCESS SYSTEM FATAL ERRORS
!
 40      imsg = -1
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 60      imsg = -2
         IF ( eigtyp==2 ) GOTO 20
         WRITE (iprntr,99003) ufm , letr(eigtyp) , ieig , oldnam
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 80      imsg = -3
         IF ( eigtyp==2 ) GOTO 20
         WRITE (iprntr,99003) ufm , letr(eigtyp) , ieig , oldnam
         spag_nextblock_1 = 3
      CASE (3)
         CALL sofcls
         CALL mesage(imsg,dnamic,modnam)
         RETURN
      CASE (4)
         dry = -2
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
99003 FORMAT (A23,' 6627, NO EIG',A1,' DATA CARD ','SPECIFIED FOR SET ID',I9,', SUBSTRUCTURE ',2A4,1H.)
!
END SUBROUTINE mred1d
