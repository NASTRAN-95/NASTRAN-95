!*==comb1.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE comb1
   IMPLICIT NONE
   USE C_BLANK
   USE C_CMB001
   USE C_CMB002
   USE C_CMB003
   USE C_CMB004
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: aaa
   INTEGER :: i , ib1 , ib2 , ib3 , ibuf , ifile , j , nce , ndof , nps , nz
   EXTERNAL bdat01 , bdat02 , bdat03 , bdat04 , bdat05 , bdat06 , close , cmauto , cmcase , cmckcd , cmckdf , cmcomb , cmcont ,     &
          & cmdisc , cmhgen , cmmcon , cmrels , cmsfil , cmsofo , cmtoc , eof , korsz , mesage , open , preloc , sofcls , sofopn
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS IS THE MODULE FOR THE COMBINATION OF SUBSTRUCTURES.
!
!     IT IS PRIMARILY AN INITIALIZER AND DRIVER CALLING THE ROUTINES
!     NECESSARY TO PROCESS THE COMBINE. THE SUBROUTINES ARE
!
!          CMCASE  -  READS THE CASECC DATA BLOCK AND INITIALIZES
!                     PARAMETERS FOR THE COMBINE OPERATION.
!          CMTOC   -  GENERATES THE TABLE OF CONTENTS OF PSEUDO-
!                     STRUCTURES BEING COMBINED AND THEIR COMPONENT
!                     BASIC SUBSTRUCTURES.
!          BDAT01  -  PROCESSES THE CONCT1 BULK DATA.
!          BDAT02  -  PROCESSES THE CONCT  BULK DATA.
!          BDAT03  -  PROCESSES THE TRANS  BULK DATA.
!          BDAT04  -  PROCESSES THE RELES  BULK DATA.
!          BDAT05  -  PROCESSES THE GNEW   BULK DATA.
!          BDAT06  -  PROCESSES THE GTRAN  BULK DATA.
!          CMSFIL  -  GENERATES SUBFIL - THE BASIC FILE USED TO STORE
!                     THE DATA NECESSARY TO AFFECT THE COMBINATION.
!          CMCONT  -  GENERATES THE CONNECTION ENTRIES TO BE USED.
!          CMCKCD  -  CHECKS VALIDITY OF MANUALLY-SPECIFIED CONNECTIONS
!          CMAUTO  -  PROCESSES USERS REQUEST FOR AUTOMATIC
!                     COMBINATION OF SUBSTRUCTURES.
!          CMRELS  -  APPLIES ANY MANUAL RELEASE DATA TO THE SYSTEM.
!          CMCOMB  -  PROCESSES MULTIPLY CONNECTED POINTS.
!          CMDISC  -  PROCESSES GRID POINTS NOT TO BE CONNECTED.
!          CMSOFO  -  GENERATES NEW SOF ITEMS FOR THE RESULTANT
!                     COMBINED STRUCTURE.
!          CMHGEN  -  GENERATES THE DOF TRANSFORMATION MATRIX FOR
!                     EACH COMPONENT TO THE COMBINATION
!
   DATA aaa/4HCOMB , 4H1   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         IF ( Dry==0 .OR. Dry==-2 ) RETURN
         Scr1 = 301
         Scr2 = 302
         Scbdat = 303
         Scsfil = 304
         Scconn = 305
         Scmcon = 306
         Sctoc = 307
         Sccstm = 308
         Scr3 = 309
         Geom4 = 102
         Casecc = 101
         DO i = 1 , 6
            Tdat(i) = .FALSE.
         ENDDO
         DO i = 1 , 7
            DO j = 1 , 3
               Origin(i,j) = 0.0
            ENDDO
         ENDDO
         Lonly = .FALSE.
         Intp = Sys(4)
         Outt = Sys(2)
         ibuf = Sys(1)
!
         nz = korsz(Z(1))
         Buf1 = nz - ibuf - 2
         Buf2 = Buf1 - ibuf
         Buf3 = Buf2 - ibuf
         Buf4 = Buf3 - ibuf
         Buf5 = Buf4 - ibuf
         ib1 = Buf5 - ibuf
         ib2 = ib1 - ibuf
         ib3 = ib2 - ibuf
         Score = 1
         Lcore = ib3 - 1
         IF ( Lcore>0 ) THEN
!
            CALL open(*60,Scconn,Z(Buf2),1)
            CALL close(Scconn,2)
            CALL sofopn(Z(ib1),Z(ib2),Z(ib3))
!
            CALL cmcase
            IF ( Dry==-2 ) THEN
               WRITE (Outt,99001) Ufm
99001          FORMAT (A23,' 6535, MODULE COMB1 TERMINATING DUE TO ABOVE ','SUBSTRUCTURE CONTROL ERRORS.')
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ELSE
               CALL cmtoc
               IF ( .NOT.Lonly ) THEN
!
                  ifile = Geom4
                  CALL preloc(*20,Z(Buf1),Geom4)
                  IF ( Conect ) THEN
                     CALL bdat01
                     CALL bdat02
                  ENDIF
                  ifile = Scbdat
                  CALL open(*60,Scbdat,Z(Buf2),1)
                  CALL bdat05
                  CALL bdat06
                  CALL bdat03
                  CALL close(Geom4,1)
!
                  CALL cmsfil
                  CALL preloc(*40,Z(Buf1),Geom4)
                  CALL bdat04
                  CALL close(Geom4,1)
                  IF ( Dry==-2 ) THEN
                     WRITE (Outt,99002) Ufm
99002                FORMAT (A23,' 6536, MODULE COMB1 TERMINATING DUE TO ABOVE ERRORS',' IN BULK DATA.')
                     spag_nextblock_1 = 5
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ELSE
                  CALL cmsofo
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ELSE
            CALL mesage(8,0,aaa)
            Dry = -2
            RETURN
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         IF ( Tdat(1) .OR. Tdat(2) ) CALL cmcont
         IF ( Dry==-2 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL cmauto
         IF ( Tdat(1) .OR. Tdat(2) ) CALL cmckcd
         IF ( Dry==-2 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( Tdat(4) ) CALL cmrels
         CALL cmmcon(nce)
         nps = Npsub + 1
         ndof = 6
         IF ( Mcon ) CALL cmcomb(nps,nce,ndof,Z)
         IF ( Dry==-2 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
         CALL cmckdf
         IF ( Dry==-2 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL cmdisc
         CALL cmsofo
         CALL cmhgen
         spag_nextblock_1 = 3
      CASE (3)
!
         CALL sofcls
         IF ( Tocopn ) CALL close(Sctoc,1)
         WRITE (Outt,99003) Uim
99003    FORMAT (A29,' 6521, MODULE COMB1 SUCCESSFULLY COMPLETED.')
         RETURN
!
 20      IF ( .NOT.(Conect .OR. Tran) ) THEN
            ifile = Scbdat
            CALL open(*60,Scbdat,Z(Buf2),1)
            CALL eof(Scbdat)
            CALL close(Scbdat,1)
            CALL cmsfil
            IF ( .NOT.Conect ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
!     ERRORS
!
 40      WRITE (Outt,99004) Ufm
99004    FORMAT (A23,' 6510, THE REQUESTED COMBINE OPERATION REQUIRES ','SUBSTRUCTURE BULK DATA WHICH HAS NOT BEEN GIVEN.')
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 60      CALL mesage(1,Scbdat,aaa)
         spag_nextblock_1 = 4
      CASE (4)
         WRITE (Outt,99005) Ufm
99005    FORMAT (A23,' 6537, MODULE COMB1 TERMINATING DUE TO ABOVE ERRORS')
         spag_nextblock_1 = 5
      CASE (5)
         IF ( Tocopn ) CALL close(Sctoc,1)
         spag_nextblock_1 = 6
      CASE (6)
         Dry = -2
         CALL sofcls
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE comb1
