
SUBROUTINE xsfadd
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Almsk , Apndmk , Comm(20) , Cursno , Entn1 , Entn2 , Entn3 , Entn4 , Flag , Fnx , Lmsk , Lxmsk , Macsft , Mf(401) ,      &
         & Rmsk , Rxmsk , S , Scornt , Sos(1501) , Tapmsk , Thcrmk , Xfiat(1320) , Zap
   COMMON /xsfa1 / Mf , Sos , Comm , Xfiat
!
! Local variable declarations
!
   INTEGER i
!
! End of declarations
!
!
!XSFABD
!
!     REVISED  8/89 BY G.C./UNISYS
!          1.  THE ORDER OF COMM AND XFIAT IN /XSFA1/ ARE REVERSED IN
!              THIS ROUTINE AND IN THE FOLLOWING 7 SUBROUTINES -
!              XCLEAN, XDPH, XPOLCK, XPUNP, XPURGE, XSFA AND XSOSGN.
!              ANY INCREASE IN SIZE OF XFIAT CAN THEREFORE BE MADE
!              EASILY THROUGH OUT THESE GROUP OF ROUTINES BY JUST
!              CHANGING THE XFIAT DIMENSION HERE.
!          2.  IN THIS GROUP OF ROUTINES, THE ARRAY XFIAT IN /XSFA1/ IS
!              RENAMED TO XFIAT, NOT TO BE CONFUSED WITH THE XFIAT ARRAY
!              IN /XFIAT/
!          3.  ENTN1 MUST EQUAL ICFIAT, THE 24TH WORD OF /SYSTEM/
!              HOWEVER, XSFA AND XPURGE ROUTINES INITIALIZE ENTN1 AGAIN
!              TO ICFIAT, JUST TO BE SURE.
!          4.  THE DIMENSION OF XFIAT SHOULD BE 800 WHEN ENTN1 = 8, OR
!              1100 WHEN ENTN1 IS 11
!
!WKBR COMMON /XSFA1 / MF(401),SOS(1501),COMM(20),XFIAT(1100)
   EQUIVALENCE (Comm(1),Almsk) , (Comm(2),Apndmk) , (Comm(3),Cursno) , (Comm(4),Entn1) , (Comm(5),Entn2) , (Comm(6),Entn3) ,        &
    & (Comm(7),Entn4) , (Comm(8),Flag) , (Comm(9),Fnx) , (Comm(10),Lmsk) , (Comm(11),Lxmsk) , (Comm(12),Macsft) , (Comm(13),Rmsk) , &
    & (Comm(14),Rxmsk) , (Comm(15),S) , (Comm(16),Scornt) , (Comm(17),Tapmsk) , (Comm(18),Thcrmk) , (Comm(19),Zap)
   Entn1 = 11
   Entn2 = 3
   Entn3 = 4
   Entn4 = 3
   Flag = 0
   DO i = 1 , 1320
      Xfiat(i) = 0
   ENDDO
   Tapmsk = 32768
!            TAPMSK = O 000000100000  = Z 00008000
   Apndmk = 1073741824
!            APNDMK = O 010000000000  = Z 40000000
   Rmsk = 32767
!            RMSK   = O 000000077777  = Z 00007FFF
   Rxmsk = 65535
!            RXMSK  = O 000000177777  = Z 0000FFFF
   Lmsk = 1073676288
!            LMSK   = O 007777600000  = Z 3FFF0000
   Lxmsk = 2147418112
!            LXMSK  = O 017777600000  = Z 7FFF0000
   Scornt = 1073708992
!            SCORNT = O 007777677700  = Z 3FFF7FC0
   Zap = 32767
!            ZAP    = O 000000077777  = Z 00007FFF
END SUBROUTINE xsfadd
