
SUBROUTINE comb1
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Buf1 , Buf2 , Buf3 , Buf4 , Buf5 , Casecc , Dry , Geom4 , Intp , Iprint , Isort , Lcore , Nipnew , Npsub , Outt ,        &
         & Restct(7,7) , Scbdat , Scconn , Sccstm , Scmcon , Score , Scr1 , Scr2 , Scr3 , Scsfil , Sctoc , Step , Sys(69)
   REAL Cnam(2) , Combo(7,5) , Conset , Origin(7,3) , Toler , Z(1)
   LOGICAL Conect , Iauto , Lonly , Mcon , Tdat(6) , Tocopn , Tran
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /blank / Step , Dry
   COMMON /cmb001/ Scr1 , Scr2 , Scbdat , Scsfil , Scconn , Scmcon , Sctoc , Geom4 , Casecc , Sccstm , Scr3
   COMMON /cmb002/ Buf1 , Buf2 , Buf3 , Buf4 , Buf5 , Score , Lcore , Intp , Outt
   COMMON /cmb003/ Combo , Conset , Iauto , Toler , Npsub , Conect , Tran , Mcon , Restct , Isort , Origin , Iprint , Tocopn
   COMMON /cmb004/ Tdat , Nipnew , Cnam , Lonly
   COMMON /system/ Sys
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   INTEGER aaa(2) , i , ib1 , ib2 , ib3 , ibuf , ifile , j , nce , ndof , nps , nz
   INTEGER korsz
!
! End of declarations
!
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
!
   IF ( Dry==0 .OR. Dry==-2 ) GOTO 99999
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
      CALL open(*500,Scconn,Z(Buf2),1)
      CALL close(Scconn,2)
      CALL sofopn(Z(ib1),Z(ib2),Z(ib3))
!
      CALL cmcase
      IF ( Dry==-2 ) THEN
         WRITE (Outt,99001) Ufm
99001    FORMAT (A23,' 6535, MODULE COMB1 TERMINATING DUE TO ABOVE ','SUBSTRUCTURE CONTROL ERRORS.')
         GOTO 800
      ELSE
         CALL cmtoc
         IF ( .NOT.Lonly ) THEN
!
            ifile = Geom4
            CALL preloc(*300,Z(Buf1),Geom4)
            IF ( Conect ) THEN
               CALL bdat01
               CALL bdat02
            ENDIF
            ifile = Scbdat
            CALL open(*500,Scbdat,Z(Buf2),1)
            CALL bdat05
            CALL bdat06
            CALL bdat03
            CALL close(Geom4,1)
!
            CALL cmsfil
            CALL preloc(*400,Z(Buf1),Geom4)
            CALL bdat04
            CALL close(Geom4,1)
            IF ( Dry==-2 ) THEN
               WRITE (Outt,99002) Ufm
99002          FORMAT (A23,' 6536, MODULE COMB1 TERMINATING DUE TO ABOVE ERRORS',' IN BULK DATA.')
               GOTO 700
            ENDIF
         ELSE
            CALL cmsofo
            GOTO 200
         ENDIF
      ENDIF
   ELSE
      CALL mesage(8,0,aaa)
      Dry = -2
      GOTO 99999
   ENDIF
 100  IF ( Tdat(1) .OR. Tdat(2) ) CALL cmcont
   IF ( Dry==-2 ) GOTO 600
   CALL cmauto
   IF ( Tdat(1) .OR. Tdat(2) ) CALL cmckcd
   IF ( Dry==-2 ) GOTO 600
   IF ( Tdat(4) ) CALL cmrels
   CALL cmmcon(nce)
   nps = Npsub + 1
   ndof = 6
   IF ( Mcon ) CALL cmcomb(nps,nce,ndof,Z)
   IF ( Dry==-2 ) GOTO 600
!
   CALL cmckdf
   IF ( Dry==-2 ) GOTO 600
   CALL cmdisc
   CALL cmsofo
   CALL cmhgen
!
 200  CALL sofcls
   IF ( Tocopn ) CALL close(Sctoc,1)
   WRITE (Outt,99003) Uim
99003 FORMAT (A29,' 6521, MODULE COMB1 SUCCESSFULLY COMPLETED.')
   GOTO 99999
!
 300  IF ( .NOT.(Conect .OR. Tran) ) THEN
      ifile = Scbdat
      CALL open(*500,Scbdat,Z(Buf2),1)
      CALL eof(Scbdat)
      CALL close(Scbdat,1)
      CALL cmsfil
      IF ( .NOT.Conect ) GOTO 100
   ENDIF
!
!     ERRORS
!
 400  WRITE (Outt,99004) Ufm
99004 FORMAT (A23,' 6510, THE REQUESTED COMBINE OPERATION REQUIRES ','SUBSTRUCTURE BULK DATA WHICH HAS NOT BEEN GIVEN.')
   GOTO 700
 500  CALL mesage(1,Scbdat,aaa)
 600  WRITE (Outt,99005) Ufm
99005 FORMAT (A23,' 6537, MODULE COMB1 TERMINATING DUE TO ABOVE ERRORS')
 700  IF ( Tocopn ) CALL close(Sctoc,1)
 800  Dry = -2
   CALL sofcls
99999 END SUBROUTINE comb1
