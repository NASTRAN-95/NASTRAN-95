!*==sdcom1.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sdcom1(P,Ac,Wa,Wb)
   IMPLICIT NONE
   USE C_SDCOMX
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
   k1 = Lastpl + 1
   iend = min0(Lastpl,Lasti)
   istart = max0(k1,Start)
   IF ( C==Lastpl ) THEN
!
      IF ( Start<=Lastpl ) THEN
         DO i = Start , iend
            pi = -P(i)/P(1)
            ijmk = j - i
            ilmk = l - i
            DO k = i , Lastpl
               Wb(k+ijmk) = pi*P(k) + Wa(k+ilmk)
            ENDDO
            j = ijmk + k1
            l = ilmk + k1
            P(i) = pi
         ENDDO
         IF ( Lastpl>=Lasti ) RETURN
      ENDIF
      DO i = istart , Lasti
         pi = -P(i)/P(1)
         ijmk = j - i
         IF ( Ac(i)<0 ) THEN
            DO k = i , C
               Wb(k+ijmk) = pi*P(k)
            ENDDO
         ELSE
            DO k = i , C
               IF ( Ac(k)>0 ) THEN
                  Wb(k+ijmk) = pi*P(k) + Wa(l)
                  l = l + 1
               ELSE
                  Wb(k+ijmk) = pi*P(k)
               ENDIF
            ENDDO
         ENDIF
         j = ijmk + C + 1
         P(i) = pi
      ENDDO
   ELSE
      IF ( Start<=Lastpl ) THEN
         DO i = Start , iend
            pi = -P(i)/P(1)
            ijmk = j - i
            ilmk = l - i
            DO k = i , Lastpl
               Wb(k+ijmk) = pi*P(k) + Wa(k+ilmk)
            ENDDO
            l = ilmk + k1
            DO k = k1 , C
               IF ( Ac(k)>0 ) THEN
                  Wb(k+ijmk) = pi*P(k) + Wa(l)
                  l = l + 1
               ELSE
                  Wb(k+ijmk) = pi*P(k)
               ENDIF
            ENDDO
            j = ijmk + C + 1
            P(i) = pi
         ENDDO
         IF ( Lastpl>=Lasti ) RETURN
      ENDIF
      DO i = istart , Lasti
         pi = -P(i)/P(1)
         ijmk = j - i
         IF ( Ac(i)<0 ) THEN
            DO k = i , C
               Wb(k+ijmk) = pi*P(k)
            ENDDO
         ELSE
            DO k = i , C
               IF ( Ac(k)>0 ) THEN
                  Wb(k+ijmk) = pi*P(k) + Wa(l)
                  l = l + 1
               ELSE
                  Wb(k+ijmk) = pi*P(k)
               ENDIF
            ENDDO
         ENDIF
         j = ijmk + C + 1
         P(i) = pi
      ENDDO
      RETURN
   ENDIF
END SUBROUTINE sdcom1
