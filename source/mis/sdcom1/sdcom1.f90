!*==sdcom1.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sdcom1(P,Ac,Wa,Wb)
   USE c_sdcomx
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: P
   INTEGER , DIMENSION(1) :: Ac
   REAL , DIMENSION(1) :: Wa
   REAL , DIMENSION(1) :: Wb
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , iend , ijmk , ilmk , istart , j , k , k1 , l
   REAL :: pi
!
! End of declarations rewritten by SPAG
!
!
!
   j = 1
   l = 1
   k1 = lastpl + 1
   iend = min0(lastpl,lasti)
   istart = max0(k1,start)
   IF ( c==lastpl ) THEN
!
      IF ( start<=lastpl ) THEN
         DO i = start , iend
            pi = -P(i)/P(1)
            ijmk = j - i
            ilmk = l - i
            DO k = i , lastpl
               Wb(k+ijmk) = pi*P(k) + Wa(k+ilmk)
            ENDDO
            j = ijmk + k1
            l = ilmk + k1
            P(i) = pi
         ENDDO
         IF ( lastpl>=lasti ) RETURN
      ENDIF
      DO i = istart , lasti
         pi = -P(i)/P(1)
         ijmk = j - i
         IF ( Ac(i)<0 ) THEN
            DO k = i , c
               Wb(k+ijmk) = pi*P(k)
            ENDDO
         ELSE
            DO k = i , c
               IF ( Ac(k)>0 ) THEN
                  Wb(k+ijmk) = pi*P(k) + Wa(l)
                  l = l + 1
               ELSE
                  Wb(k+ijmk) = pi*P(k)
               ENDIF
            ENDDO
         ENDIF
         j = ijmk + c + 1
         P(i) = pi
      ENDDO
   ELSE
      IF ( start<=lastpl ) THEN
         DO i = start , iend
            pi = -P(i)/P(1)
            ijmk = j - i
            ilmk = l - i
            DO k = i , lastpl
               Wb(k+ijmk) = pi*P(k) + Wa(k+ilmk)
            ENDDO
            l = ilmk + k1
            DO k = k1 , c
               IF ( Ac(k)>0 ) THEN
                  Wb(k+ijmk) = pi*P(k) + Wa(l)
                  l = l + 1
               ELSE
                  Wb(k+ijmk) = pi*P(k)
               ENDIF
            ENDDO
            j = ijmk + c + 1
            P(i) = pi
         ENDDO
         IF ( lastpl>=lasti ) RETURN
      ENDIF
      DO i = istart , lasti
         pi = -P(i)/P(1)
         ijmk = j - i
         IF ( Ac(i)<0 ) THEN
            DO k = i , c
               Wb(k+ijmk) = pi*P(k)
            ENDDO
         ELSE
            DO k = i , c
               IF ( Ac(k)>0 ) THEN
                  Wb(k+ijmk) = pi*P(k) + Wa(l)
                  l = l + 1
               ELSE
                  Wb(k+ijmk) = pi*P(k)
               ENDIF
            ENDDO
         ENDIF
         j = ijmk + c + 1
         P(i) = pi
      ENDDO
      RETURN
   ENDIF
END SUBROUTINE sdcom1
