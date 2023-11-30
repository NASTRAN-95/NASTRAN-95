
SUBROUTINE k2b(K,B,N)
   IMPLICIT NONE
   INTEGER N
   INTEGER B(1)
   CHARACTER*1 K(1)
   INTEGER a , i
   CHARACTER*1 c
   CHARACTER*4 c4 , d4
!
!     MOVE ONLY THE APPROPRIATE PORTION OF THIS ROUTINE TO THE MDS GROUP
!
!     VAX, IBM, UNIX AND UNIVAC VERSION
!     =================================
!
!     TO CONVERT CHARACTER STRING TO BCD WORDS, ONE CHARACTER PER WORD
!     AND BLANK FILLED. SAME RESULT AS
!                READ (Kn,10) B
!             10 FORMAT (nA1)      WHERE Kn IS IN CHARACTER*n
!
!     (NOTE - THE INTERNAL FILE READ AND WRITE ARE SLOW IN MOST MACHINES
!             AND IS EXTREMELY SLOW IN CDC)
!
!     CHARACTER*n C4,D4
!         Where n = 4 for IBM, VAX and UNIVAC, 8 for 64-BIT UNIX MACHINE
   !>>>>EQUIVALENCE (a,c,c4)
   DATA d4/'    '/
!
   c4 = d4
   i = 1
   DO
      c = K(i)
      B(i) = a
      i = i + 1
      IF ( i>N ) RETURN
   ENDDO
!
   ENTRY b2k(B,K,N)
!     =================
!
!     TO MERGE FROM ONE-CHARACTER BCD WORDS TO A CHARACTER STRING
!     SAME RESULT AS
!                WRITE (Kn,10) B
!             10 FORMAT (nA1)      WHERE Kn IS IN CHARACTER*n
!
   i = 1
   DO
      a = B(i)
      K(i) = c
      i = i + 1
      IF ( i>N ) EXIT
   ENDDO
!
!
!     SUBROUTINE K2B (K,B,N)
!
!     CDC VERSION
!     ===========
!
!     TO CONVERT CHARACTER STRING TO BCD WORDS, ONE CHARACTER PER WORD
!     AND BLANK FILLED. SAME RESULT AS
!                READ (Kn,10) B
!             10 FORMAT (nA1)      WHERE Kn IS IN CHARACTER*n
!
!     (NOTE - THE INTERNAL FILE READ AND WRITE ARE SLOW IN MOST MACHINES
!             AND IS EXTREMELY SLOW IN CDC)
!
!     INTEGER K(1),B(1)
!     DATA    NBPC,NCPW,NCPWP1 / 6, 10, 11  /
!     DATA    MASK /O"77000000000000000000" /
!
!     IE = 1 + N/NCPW
!     KK = 0
!     DO 40 I = 1,IE
!     KI = K(I)
!     DO 30 J = 1,NCPW
!     KK = KK + 1
!     IF (KK .GT. N) GO TO 50
!     B(KK) = AND(KI,MASK)
!     KI = SHIFT(KI,NBPC)
!30   CONTINUE
!40   CONTINUE
!50   RETURN
!
!
!     ENTRY B2K (B,K,N)
!     =================
!
!     TO MERGE FROM ONE-CHARACTER BCD WORDS TO A CHARACTER STRING
!     SAME RESULT AS
!                WRITE (Kn,10) B
!             10 FORMAT (nA1)      WHERE Kn IS IN CHARACTER*n
!
!     IE = 1 + N/NCPW
!     KK = 0
!     DO 70 I = 1,IE
!     KK = KK + 1
!     KI = AND(B(KK),MASK)
!     DO 60 J = 2,NCPW
!     KI = SHIFT(KI,NBPC)
!     KK = KK + 1
!     IF (KK .GT. N) GO TO 80
!     KI = OR(KI,AND(B(KK),MASK))
!60   CONTINUE
!70   K(I) = SHIFT(KI,NBPC)
!     GO TO 90
!80   J = NCPWP1 - MOD(KK,NCPW)
!     IF (J .GT. NCPW) J = 1
!     K(IE) = SHIFT(KI,J*NBPC)
!90   RETURN
!
END SUBROUTINE k2b