!*==dsrdmb.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dsrdmb(Idata,M)
   USE I_DSIOF
   USE I_XNSTRN
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   INTEGER M
   INTEGER Idata(2)
   INTEGER id , idiff , inum , ireq , iwords , k , len
   irword = 0
   SPAG_Loop_1_1: DO
      len = iand(ibase(indclr),maskh2)
      IF ( len/=0 ) THEN
         idiff = indcbp - indclr
         iwords = len - idiff
         ireq = iabs(nwords)
         IF ( ireq<=(iwords+irword) ) THEN
            inum = ireq - irword
            IF ( inum/=0 ) THEN
               IF ( nwords>=0 ) THEN
                  DO k = 1 , inum
                     Idata(irword+k) = ibase(indcbp+k)
                  ENDDO
               ENDIF
               indcbp = indcbp + inum
            ENDIF
            EXIT SPAG_Loop_1_1
         ENDIF
      ENDIF
      id = iand(ibase(indclr+len+1),maskq2)
      IF ( len/=0 ) THEN
         IF ( nwords>=0 ) THEN
            DO k = 1 , iwords
               Idata(irword+k) = ibase(indcbp+k)
            ENDDO
         ENDIF
         irword = irword + iwords
      ENDIF
      IF ( id==idsc ) THEN
         iretrn = 2
         ieor = 1
         M = irword
      ELSE
         CALL dsrdnb
         CYCLE
      ENDIF
      EXIT SPAG_Loop_1_1
   ENDDO SPAG_Loop_1_1
END SUBROUTINE dsrdmb
