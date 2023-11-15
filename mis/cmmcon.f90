
SUBROUTINE cmmcon(Nce)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Buf1 , Buf3 , Inpt , Junk(2) , Junk2(38) , Junk3(2) , Lcore , Npsub , Scconn , Scmcon , Score , Z(1)
   REAL Buf2 , Outt , Scbdat , Scr1 , Scr2 , Scsfil
   LOGICAL Mcon
   COMMON /cmb001/ Scr1 , Scr2 , Scbdat , Scsfil , Scconn , Scmcon
   COMMON /cmb002/ Buf1 , Buf2 , Buf3 , Junk , Score , Lcore , Inpt , Outt
   COMMON /cmb003/ Junk2 , Npsub , Junk3 , Mcon
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER Nce
!
! Local variable declarations
!
   INTEGER aaa(2) , i , ifile , iloc , ist , isub , j , k , loc , ncem1 , nnn , nwd
!
! End of declarations
!
!
!     THIS SUBROUTINE DETERMINES WHETHER MORE THAN ONE CONNECTION ENTRY
!     HAS BEEN SPECIFIED FOR A GIVEN IP NUMBER.
!
   DATA aaa/4HCMMC , 4HON  /
!
!     READ CONNECTION ENTRIES INTO OPEN CORE
!
   nwd = 2 + Npsub
   Mcon = .TRUE.
   ifile = Scconn
   CALL open(*400,Scconn,Z(Buf1),0)
   j = 0
   Nce = 0
 100  CALL read(*300,*200,Scconn,Z(Score+j),10,1,nnn)
 200  Nce = Nce + 1
   Z(Score+j) = Nce
   j = j + nwd
   GOTO 100
 300  CALL close(Scconn,1)
!
!     SWEEP THROUGH CONNECTION ENTRIES AND DETERMINE THOSE THAT
!     REPRESENT MULTIPLE CONNECTIONS.
!
   Mcon = .FALSE.
   ncem1 = Nce - 1
!
   DO k = 1 , ncem1
      DO i = 1 , Npsub
         ist = Score + i + (k-1)*nwd + 1
         IF ( Z(ist)/=0 ) THEN
            DO j = 1 , Nce
               IF ( k/=j ) THEN
                  isub = Score + 1 + i + (j-1)*nwd
                  IF ( Z(ist)==Z(isub) ) THEN
                     iloc = i + 1
                     Z(ist-iloc) = -1*iabs(Z(ist-iloc))
                     Z(isub-iloc) = -1*iabs(Z(isub-iloc))
                     Mcon = .TRUE.
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
      ENDDO
   ENDDO
!
   IF ( .NOT.Mcon ) RETURN
!
!     GENERATE OUTPUT FILE OF CONNECTION ENTRY IDS
!
   ifile = Scmcon
   CALL open(*400,Scmcon,Z(Buf1),1)
   DO i = 1 , Nce
      loc = Score + (i-1)*nwd
      IF ( Z(loc)<0 ) CALL write(Scmcon,iabs(Z(loc)),1,0)
   ENDDO
   CALL write(Scmcon,0,0,1)
   CALL close(Scmcon,1)
   RETURN
!
 400  CALL mesage(-1,ifile,aaa)
END SUBROUTINE cmmcon
