!*==dk89.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION dk89(I,A,B,M,N,X)
USE iso_fortran_env
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) :: dk89
   INTEGER :: I
   REAL(REAL64) :: A
   REAL(REAL64) :: B
   INTEGER :: M
   INTEGER :: N
   REAL(REAL64) , DIMENSION(1) :: X
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: amf , ammsf , amn1f , an1 , an2 , anm1f , capx , f89 , s , sf
   INTEGER :: iret , is , kfac , lfac , n1 , n2 , n3 , nfac , spag_nextblock_1
!
! End of declarations rewritten by SPAG
!
!
! Function and Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
         f89 = 0.0D0
         capx = A + B*X(I)
         nfac = M
         ASSIGN 20 TO iret
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 20      amf = kfac
         n1 = M + 1
         n2 = n1 - N
         an1 = n1
         an2 = n2
         is = 0
         s = 0.0D0
         sf = 1.0D0
         ammsf = amf
         spag_nextblock_1 = 2
      CASE (2)
         n3 = n2 - is
         IF ( n3==0 ) THEN
            nfac = n2
            ASSIGN 40 TO iret
            spag_nextblock_1 = 4
         ELSE
            f89 = f89 + amf*((-A)**is)*(capx**n3)/(ammsf*sf*(an2-s))
            spag_nextblock_1 = 3
         ENDIF
         CYCLE
 40      amn1f = kfac
         nfac = N - 1
         ASSIGN 60 TO iret
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 60      anm1f = kfac
         f89 = f89 + amf*((-A)**n2)*dlog(dabs(capx))/(amn1f*anm1f)
         spag_nextblock_1 = 3
      CASE (3)
         IF ( is<M ) THEN
            is = is + 1
            s = is
            sf = sf*s
            ammsf = ammsf/(an1-s)
            spag_nextblock_1 = 2
         ELSEIF ( B==0.0D0 ) THEN
            dk89 = 0.0D0
            RETURN
         ELSE
            f89 = f89/(B**n1)
            dk89 = f89
            RETURN
         ENDIF
      CASE (4)
         kfac = 1
         IF ( nfac>=2 ) THEN
            DO lfac = 2 , nfac
               kfac = kfac*lfac
            ENDDO
         ENDIF
         GOTO iret
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END FUNCTION dk89
