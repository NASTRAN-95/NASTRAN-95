!*==int2a8.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE int2a8(X,A8) !HIDESTARS (*,X,A8)
   USE C_MACHIN
   USE C_SYSTEM
   IMPLICIT NONE
   REAL Dummy(38)
   INTEGER Mach , Nbpc , Nbpw , Ncpw
   COMMON /machin/ Mach
   COMMON /system/ Dummy , Nbpc , Nbpw , Ncpw
   INTEGER A8(2)
   CHARACTER*8 K8(1)
   REAL X(1)
   CHARACTER*1 a(10) , alp(10) , ib , im , ip , pt
   REAL absx , rx , xll , xlu
   CHARACTER*10 alp10 , temp10
   INTEGER int , ip1 , ix , j , jx , ll , lu , n , n1 , nn , np1 , nt , power
   CHARACTER*8 temp , zero , zerox
   INTEGER :: spag_nextblock_1
!
   !>>>>EQUIVALENCE (temp,temp10,a(1)) , (jx,rx) , (alp10,alp(1))
   DATA ip , im , ib , pt , temp , zero , zerox , nn , ll , alp10/'+' , '-' , ' ' , '.' , 'T' , '0' , '0.0' , 0 , 0 , '1234567890'/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     THESE ROUTINES ENCODE AN INTEGER OR F.P. NUMBER IN X, TO AN 8-BYTE
!     BCD WORD IN A8, OR AN 8-CHARACTER WORD IN K8, LEFT ADJUSTED.
!     WITH MAXIMUM NUMBERS OF DIGITS SQUEEZED INTO THE 8-BYTE FORMAT.
!
!     ENTRY POINT    INT 2 A8  (INTEGER-BCD VERSION)
!                    INT 2 K8  (INTEGER-CHARACTER VERSION)
!                    FP  2 A8  (REAL-BCD VERSION)
!                    FP  2 K8  (REAL-CHARACTER VERSION)
!
!     WRITTEN BY G.CHAN/UNISYS IN AUG. 1985
!     PARTICULARLY FOR XREAD ROUTINE, IN SUPPORT OF ITS NEW FREE-FIELD
!     INPUT FORMAT.
!     THIS ROUTINE IS MACHINE INDEPENDENT
!
         nt = +1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
         ENTRY int2k8(X,K8)
                      !HIDESTARS (*,X,K8)
!     =====================
!
         nt = -1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
         ENTRY fp2a8(X,A8)
                     !HIDESTARS (*,X,A8)
!     ====================
!
         nt = +2
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
         ENTRY fp2k8(X,K8)
                     !HIDESTARS (*,X,K8)
!     ======================
!
         nt = -2
         spag_nextblock_1 = 2
      CASE (2)
!
         int = iabs(nt)
         DO j = 1 , 8
            a(j) = ip
         ENDDO
         a(9) = ib
         a(10) = ib
         IF ( int/=1 ) THEN
!
!     F.P. NUMBER
!
            absx = abs(X(1))
            IF ( absx<1.E-20 ) THEN
               temp = zerox
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ELSE
               absx = absx*(1.0+1.E-20)
               lu = 7
               ll = -3
               n = 0
               IF ( X(1)>0. ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               lu = lu - 1
               ll = ll + 1
            ENDIF
         ELSE
!
!     INTEGER
!
            lu = 8
            n = 0
            rx = X(1)
            ix = iabs(jx)
            xll = float(ix) + .01
            absx = abs(xll)
            nn = 0
            IF ( jx<0 .OR. ix>=10**8 ) THEN
               IF ( jx>=0 .OR. ix>=10**7 ) RETURN 1
            ENDIF
            IF ( jx<0 ) THEN
            ELSEIF ( jx==0 ) THEN
!
               temp = zero
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ELSE
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         n = 1
         a(1) = im
         spag_nextblock_1 = 3
      CASE (3)
         n1 = n
         IF ( int/=1 ) THEN
            xll = alog10(absx)
            IF ( xll<0. ) xll = xll - .99998
            IF ( xll>0. ) xll = xll + .00002
            power = ifix(xll)
            np1 = power + 1
            ip1 = iabs(np1)
            xlu = 10.**lu
            xll = 10.**ll
            IF ( absx<xll .OR. absx>xlu ) THEN
!
!     F.P. NUMBER IN .XXXXX+X, .XXXX-XX, -.XXXX-X, OR -.XXX+XX FORMS
!     FOR MAXIMUM NOS. OF DIGITS POSSIBLE IN AN A8 WROD.
!
               int = 3
               n = n + 1
               a(n) = pt
               lu = lu - 2
            ENDIF
!
!     F.P. NUMBER IS SQUEEZED INTO AN EIGHT DIGIT F FORMAT, IF
!     X IS BETWWEN 10**-3 AND 10**7 AND X IS POSITUVE, OR
!          BETWWEN 10**-2 AND 10**6 AND X IS NEGATIVE,
!
            IF ( ip1>=10 ) lu = lu - 1
            IF ( np1==-1 ) lu = lu + 1
            nn = lu - np1
            IF ( int==2 .AND. nn>7 ) nn = 7
            ix = ifix(absx*10.**nn)
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
         SPAG_Loop_1_1: DO
            lu = lu - 1
            IF ( lu<0 .AND. int==3 ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( lu>=0 .OR. n/=7 ) THEN
               power = 10**lu
               IF ( power==0 ) power = 1
               j = ix/power
               IF ( j>=10 ) CYCLE
               ix = mod(ix,power)
               IF ( lu-nn+1<0 ) THEN
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( lu-nn+1/=0 ) EXIT SPAG_Loop_1_1
               IF ( int==3 ) THEN
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
            n = n + 1
            a(n) = pt
            IF ( n<8 ) EXIT SPAG_Loop_1_1
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDDO SPAG_Loop_1_1
         IF ( j==0 .AND. n<=n1 ) THEN
            IF ( int==1 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         IF ( j==0 ) j = 10
         n = n + 1
         a(n) = alp(j)
         IF ( lu==0 .AND. int==1 ) THEN
!
            n = n + 1
            IF ( n<=8 ) THEN
               DO j = n , 8
                  a(j) = ib
               ENDDO
            ENDIF
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( n<8 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)
         SPAG_Loop_1_2: DO j = 1 , 8
            IF ( a(n)==pt ) EXIT SPAG_Loop_1_2
            IF ( a(n)/=alp(10) ) EXIT SPAG_Loop_1_2
            a(n) = ib
            n = n - 1
         ENDDO SPAG_Loop_1_2
         spag_nextblock_1 = 7
      CASE (7)
!
         IF ( nt<0 ) THEN
            K8(1) = temp
         ELSEIF ( nt/=0 ) THEN
!WKBD IF (MACH .EQ. 4) A8(1) = ISWAP(TEMP10)
!     IF (NCPW .GE. 8) A8(2) = LSHIFT(A8(1),4*NBPC)
            IF ( Mach/=4 ) CALL khrbc2(temp,A8(1))
         ENDIF
         RETURN
      CASE (8)
!
         n = n + 1
         IF ( np1>=0 ) a(n) = ip
         IF ( np1<0 ) a(n) = im
         IF ( ip1>=10 ) THEN
            j = ip1/10
            a(n+1) = alp(j)
            j = mod(ip1,10)
            IF ( j==0 ) j = 10
            a(n+2) = alp(j)
         ELSE
            a(n+1) = alp(ip1)
         ENDIF
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE int2a8
