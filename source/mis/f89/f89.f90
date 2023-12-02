!*==f89.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION f89(I,A,B,M,N,X)
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   REAL :: f89
   INTEGER :: I
   REAL :: A
   REAL :: B
   INTEGER :: M
   INTEGER :: N
   REAL , DIMENSION(1) :: X
!
! Local variable declarations rewritten by SPAG
!
   REAL :: amf , ammsf , amn1f , an1 , an2 , anm1f , capx , s , sf
   INTEGER :: ifac , iret , is , lfac , n1 , n2 , n3 , nfac , spag_nextblock_1
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
!
!
         f89 = 0.0
         capx = A + B*X(I)
         nfac = M
         ASSIGN 20 TO iret
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 20      amf = ifac
         n1 = M + 1
         n2 = n1 - N
         an1 = n1
         an2 = n2
         is = 0
         s = 0.0
         sf = 1.0
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
 40      amn1f = ifac
         nfac = N - 1
         ASSIGN 60 TO iret
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 60      anm1f = ifac
         f89 = f89 + amf*((-A)**n2)*alog(abs(capx))/(amn1f*anm1f)
         spag_nextblock_1 = 3
      CASE (3)
         IF ( is<M ) THEN
            is = is + 1
            s = is
            sf = sf*s
            ammsf = ammsf/(an1-s)
            spag_nextblock_1 = 2
         ELSEIF ( B==0.0 ) THEN
!
            f89 = 0.0
            RETURN
         ELSE
            f89 = f89/(B**n1)
            RETURN
         ENDIF
      CASE (4)
!
         ifac = 1
         IF ( nfac>=2 ) THEN
            DO lfac = 2 , nfac
               ifac = ifac*lfac
            ENDDO
         ENDIF
         GOTO iret
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END FUNCTION f89
