
SUBROUTINE dsrdmb(Idata,M)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
!
! Dummy argument declarations
!
   INTEGER M
   INTEGER Idata(2)
!
! Local variable declarations
!
   INTEGER id , idiff , inum , ireq , iwords , k , len
!
! End of declarations
!
   Irword = 0
 100  len = iand(Ibase(Indclr),Maskh2)
   IF ( len/=0 ) THEN
      idiff = Indcbp - Indclr
      iwords = len - idiff
      ireq = iabs(Nwords)
      IF ( ireq<=(iwords+Irword) ) THEN
         inum = ireq - Irword
         IF ( inum/=0 ) THEN
            IF ( Nwords>=0 ) THEN
               DO k = 1 , inum
                  Idata(Irword+k) = Ibase(Indcbp+k)
               ENDDO
            ENDIF
            Indcbp = Indcbp + inum
         ENDIF
         GOTO 99999
      ENDIF
   ENDIF
   id = iand(Ibase(Indclr+len+1),Maskq2)
   IF ( len/=0 ) THEN
      IF ( Nwords>=0 ) THEN
         DO k = 1 , iwords
            Idata(Irword+k) = Ibase(Indcbp+k)
         ENDDO
      ENDIF
      Irword = Irword + iwords
   ENDIF
   IF ( id==Idsc ) THEN
      Iretrn = 2
      Ieor = 1
      M = Irword
   ELSE
      CALL dsrdnb
      GOTO 100
   ENDIF
99999 RETURN
END SUBROUTINE dsrdmb
