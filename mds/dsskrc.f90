
SUBROUTINE dsskrc
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
!
! Local variable declarations
!
   INTEGER iclr , id , iflg , len
!
! End of declarations
!
 100  DO
      id = iand(Ibase(Indclr),Maskq1)
      IF ( id==Idsrh ) EXIT
      IF ( id==Idssb ) EXIT
      IF ( id==Idsef ) THEN
         Indclr = Indclr + 1
         Indcbp = Indclr
         GOTO 99999
      ELSE
         IF ( id/=Idseb ) CALL dsmsg(103)
         CALL dsrdnb
      ENDIF
   ENDDO
   len = iand(Ibase(Indclr),Maskh2)
   iclr = Indclr + len + 1
   id = iand(Ibase(iclr),Maskq1)
   IF ( id/=Idsrt ) CALL dsmsg(104)
   iflg = iand(Ibase(iclr),Maskq2)
   IF ( iflg==Idsc ) THEN
      Indclr = iclr + 1
      Indcbp = Indclr
   ELSE
      CALL dsrdnb
      GOTO 100
   ENDIF
99999 END SUBROUTINE dsskrc
