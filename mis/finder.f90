
SUBROUTINE finder(Nam,Subno,Comno)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Buf1 , Buf2 , Buf3 , Buf5 , Casecc , Combo(7,5) , Conect , Conset , Geom4 , Origin(7,3) , Restct(7,7) , Scbdat , Scconn ,   &
      & Scmcon , Score , Scr1 , Scr2 , Scsfil , Toler , Tran , Z(1)
   INTEGER Buf4 , Iauto , Ierr , Inam(2) , Inpt , Iprint , Isort , Lcore , Mcon , Npsub , Outt , Sctoc
   LOGICAL Tocopn
   COMMON /cmb001/ Scr1 , Scr2 , Scbdat , Scsfil , Scconn , Scmcon , Sctoc , Geom4 , Casecc
   COMMON /cmb002/ Buf1 , Buf2 , Buf3 , Buf4 , Buf5 , Score , Lcore , Inpt , Outt
   COMMON /cmb003/ Combo , Conset , Iauto , Toler , Npsub , Conect , Tran , Mcon , Restct , Isort , Origin , Iprint , Tocopn
   COMMON /cmbfnd/ Inam , Ierr
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER Comno , Subno
   INTEGER Nam(2)
!
! Local variable declarations
!
   INTEGER cnam(2) , i , id(3) , ieor , j , ncom , nnn
!
! End of declarations
!
!
!
!     THIS SUBROUTINE READS THE TABLE OF CONTENTS OF SUBSTRUCTURES
!     BEING COMBINED ( SCRATCH FILE SCTOC ) AND FOR ANY GIVEN
!     BASIC SUBSTRUCTURE NAME ( NAM ) RETURNS THE ID NUMBER OF THE
!     PSEUDO-STRUCTURE CONTAINING IT ( SUBNO ) AND ITS POSITION IN
!     THE COMPONENT LIST FOR THAT STRUCTURE ( COMNO ).  IF A NAME
!     DOES NOT APPEAR IN THE SCTOC AN ERROR MESSAGE IS ISSUED.
!
!
!     OPEN SCTOC FILE
!
   Ierr = 0
   IF ( .NOT.Tocopn ) CALL open(*99999,Sctoc,Z(Buf4),0)
   CALL rewind(Sctoc)
!
   DO i = 1 , Npsub
      CALL read(*99999,*99999,Sctoc,id,3,0,nnn)
      ncom = id(3)
      DO j = 1 , ncom
         ieor = 0
         IF ( j==ncom ) ieor = 1
         CALL read(*99999,*99999,Sctoc,cnam,2,ieor,nnn)
         IF ( Nam(1)==cnam(1) .AND. Nam(2)==cnam(2) ) GOTO 100
      ENDDO
   ENDDO
!
!     IERR = 1 MEANS THAT THE SUBSTRUCTURE NAME IS NOT IN THE TOC
!
   Ierr = 1
   RETURN
 100  Subno = i
   Inam(1) = id(1)
   Inam(2) = id(2)
   Comno = j
   IF ( .NOT.Tocopn ) CALL close(Sctoc,1)
99999 END SUBROUTINE finder
