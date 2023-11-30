
SUBROUTINE na12a8(A,N,B,Notuse) !HIDESTARS (*,A,N,B,Notuse)
   IMPLICIT NONE
   INTEGER Mach , Nout
   COMMON /machin/ Mach
   COMMON /xreadx/ Nout
   INTEGER N , Notuse
   INTEGER A(1) , B(2)
   CHARACTER*1 C(1)
   CHARACTER*8 D(1)
   CHARACTER*10 blnk , temp
   INTEGER cdc , i , j
   CHARACTER*1 t(8)
!
   !>>>>EQUIVALENCE (t(1),temp)
   DATA blnk/'          '/ , cdc/4/
!
!     THESE ROUTTNES CONVERT N A1 BCD WORDS IN A, OR N A1 CHARACTERS IN
!     C TO AN 8-BYTE BCD WORD IN B (CDC ONLY), (OR TO TWO 4-BYTE BCD
!     WORDS IN B, ALL OTHER NON-CDC MACHINES), OR AN 8-CHARACTER WORD
!     IN D, LEFT ADJUSTED.
!     CALLING ROUTINE MUST NOT USE LOGICAL*1 FOR A-ARRAY.
!     (NO SYSTEM ENCODE/DECODE FUNCTIONS ARE USED)
!
!     ENTRY POINTS   NA1 2 A8  (BCD-BYTE  VERSION)
!                    NK1 2 K8  (CHARACTER VERSION)
!
!
!     WRITTEN BY G.CHAN/SPERRY IN AUG. 1985
!     PARTICULARLY FOR XREAD ROUTINE, IN SUPPORT OF ITS NEW FREE-FIELD
!     INPUT FORMAT.  THIS SUBROUTINE IS MACHINE INDEPENDENT
!
!     LAST REVISED  8/1988
!
   IF ( N>8 ) GOTO 100
   temp = blnk
   CALL b2k(A,temp,N)
   IF ( Mach/=cdc ) CALL khrbc2(temp,B(1))
!WKBD IF (MACH .EQ. CDC) B(1) = ISWAP(TEMP)
   RETURN
!
   ENTRY nk12k8(C,N,D,Notuse) !HIDESTARS (*,C,N,D,Notuse)
!     ===============================
!
   IF ( N<=8 ) THEN
      temp = blnk
      DO i = 1 , N
         t(i) = C(i)
      ENDDO
      D(1) = temp
      RETURN
   ENDIF
!
 100  WRITE (Nout,99001) N
99001 FORMAT ('   N.GT.8/NA12A8',I6)
   j = Notuse
   RETURN 1
END SUBROUTINE na12a8