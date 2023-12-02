!*==comb1.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE comb1
   USE c_blank
   USE c_cmb001
   USE c_cmb002
   USE c_cmb003
   USE c_cmb004
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
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
         IF ( dry==0 .OR. dry==-2 ) RETURN
         scr1 = 301
         scr2 = 302
         scbdat = 303
         scsfil = 304
         scconn = 305
         scmcon = 306
         sctoc = 307
         sccstm = 308
         scr3 = 309
         geom4 = 102
         casecc = 101
         DO i = 1 , 6
            tdat(i) = .FALSE.
         ENDDO
         DO i = 1 , 7
            DO j = 1 , 3
               origin(i,j) = 0.0
            ENDDO
         ENDDO
         lonly = .FALSE.
         intp = sys(4)
         outt = sys(2)
         ibuf = sys(1)
!
         nz = korsz(z(1))
         buf1 = nz - ibuf - 2
         buf2 = buf1 - ibuf
         buf3 = buf2 - ibuf
         buf4 = buf3 - ibuf
         buf5 = buf4 - ibuf
         ib1 = buf5 - ibuf
         ib2 = ib1 - ibuf
         ib3 = ib2 - ibuf
         score = 1
         lcore = ib3 - 1
         IF ( lcore>0 ) THEN
!
            CALL open(*60,scconn,z(buf2),1)
            CALL close(scconn,2)
            CALL sofopn(z(ib1),z(ib2),z(ib3))
!
            CALL cmcase
            IF ( dry==-2 ) THEN
               WRITE (outt,99001) ufm
99001          FORMAT (A23,' 6535, MODULE COMB1 TERMINATING DUE TO ABOVE ','SUBSTRUCTURE CONTROL ERRORS.')
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ELSE
               CALL cmtoc
               IF ( .NOT.lonly ) THEN
!
                  ifile = geom4
                  CALL preloc(*20,z(buf1),geom4)
                  IF ( conect ) THEN
                     CALL bdat01
                     CALL bdat02
                  ENDIF
                  ifile = scbdat
                  CALL open(*60,scbdat,z(buf2),1)
                  CALL bdat05
                  CALL bdat06
                  CALL bdat03
                  CALL close(geom4,1)
!
                  CALL cmsfil
                  CALL preloc(*40,z(buf1),geom4)
                  CALL bdat04
                  CALL close(geom4,1)
                  IF ( dry==-2 ) THEN
                     WRITE (outt,99002) ufm
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
            dry = -2
            RETURN
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         IF ( tdat(1) .OR. tdat(2) ) CALL cmcont
         IF ( dry==-2 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL cmauto
         IF ( tdat(1) .OR. tdat(2) ) CALL cmckcd
         IF ( dry==-2 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( tdat(4) ) CALL cmrels
         CALL cmmcon(nce)
         nps = npsub + 1
         ndof = 6
         IF ( mcon ) CALL cmcomb(nps,nce,ndof,z)
         IF ( dry==-2 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
         CALL cmckdf
         IF ( dry==-2 ) THEN
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
         IF ( tocopn ) CALL close(sctoc,1)
         WRITE (outt,99003) uim
99003    FORMAT (A29,' 6521, MODULE COMB1 SUCCESSFULLY COMPLETED.')
         RETURN
!
 20      IF ( .NOT.(conect .OR. tran) ) THEN
            ifile = scbdat
            CALL open(*60,scbdat,z(buf2),1)
            CALL eof(scbdat)
            CALL close(scbdat,1)
            CALL cmsfil
            IF ( .NOT.conect ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
!     ERRORS
!
 40      WRITE (outt,99004) ufm
99004    FORMAT (A23,' 6510, THE REQUESTED COMBINE OPERATION REQUIRES ','SUBSTRUCTURE BULK DATA WHICH HAS NOT BEEN GIVEN.')
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 60      CALL mesage(1,scbdat,aaa)
         spag_nextblock_1 = 4
      CASE (4)
         WRITE (outt,99005) ufm
99005    FORMAT (A23,' 6537, MODULE COMB1 TERMINATING DUE TO ABOVE ERRORS')
         spag_nextblock_1 = 5
      CASE (5)
         IF ( tocopn ) CALL close(sctoc,1)
         spag_nextblock_1 = 6
      CASE (6)
         dry = -2
         CALL sofcls
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE comb1
