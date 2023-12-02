!*==xsfadd.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE xsfadd
   IMPLICIT NONE
   USE C_XSFA1
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: apndmk , entn1 , entn2 , entn3 , entn4 , flag , i , lmsk , lxmsk , rmsk , rxmsk , scornt , tapmsk , zap
!
! End of declarations rewritten by SPAG
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
   !>>>>EQUIVALENCE (Comm(1),Almsk) , (Comm(2),Apndmk) , (Comm(3),Cursno) , (Comm(4),Entn1) , (Comm(5),Entn2) , (Comm(6),Entn3) ,        &
!>>>>    & (Comm(7),Entn4) , (Comm(8),Flag) , (Comm(9),Fnx) , (Comm(10),Lmsk) , (Comm(11),Lxmsk) , (Comm(12),Macsft) , (Comm(13),Rmsk) , &
!>>>>    & (Comm(14),Rxmsk) , (Comm(15),S) , (Comm(16),Scornt) , (Comm(17),Tapmsk) , (Comm(18),Thcrmk) , (Comm(19),Zap)
   entn1 = 11
   entn2 = 3
   entn3 = 4
   entn4 = 3
   flag = 0
   DO i = 1 , 1320
      Xfiat(i) = 0
   ENDDO
   tapmsk = 32768
!            TAPMSK = O 000000100000  = Z 00008000
   apndmk = 1073741824
!            APNDMK = O 010000000000  = Z 40000000
   rmsk = 32767
!            RMSK   = O 000000077777  = Z 00007FFF
   rxmsk = 65535
!            RXMSK  = O 000000177777  = Z 0000FFFF
   lmsk = 1073676288
!            LMSK   = O 007777600000  = Z 3FFF0000
   lxmsk = 2147418112
!            LXMSK  = O 017777600000  = Z 7FFF0000
   scornt = 1073708992
!            SCORNT = O 007777677700  = Z 3FFF7FC0
   zap = 32767
!            ZAP    = O 000000077777  = Z 00007FFF
END SUBROUTINE xsfadd
