
SUBROUTINE strax3(Again)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Block(22,14) , Clock(22,14) , Conphi , Disp(9) , Dum(100) , Dum4(51) , Dum5(33) , Eforc(9) , Estres(9) , Force(25) , Harm , &
      & Savef(75) , Saves(75) , Sinphi , Skip(25) , Stress(100) , Unu(123)
   INTEGER Cangle , Elemid , Iblock(22,14) , Iclock(22,14) , Iforce(25) , Ipart , Isavef(75) , Isaves(75) , Istres(100) , Ktype ,   &
         & N , Nangle , Nelhar , Nphi
   COMMON /isave / Isavef , Isaves
   COMMON /sdr2de/ Dum5 , Ipart
   COMMON /sdr2x4/ Dum4 , Ktype
   COMMON /sdr2x7/ Dum , Stress , Force , Skip , Block , Clock
   COMMON /sdr2x8/ Disp , Eforc , Estres , Harm , N , Sinphi , Conphi , Nphi , Nangle , Elemid , Unu , Nelhar
!
! Dummy argument declarations
!
   LOGICAL Again
!
! Local variable declarations
!
   INTEGER i , j , k
!
! End of declarations
!
!
!
!
!
! SCRATCH BLOCK
!
!
!
!
   EQUIVALENCE (Istres(1),Stress(1)) , (Iforce(1),Force(1)) , (Iblock(1,1),Block(1,1)) , (Iclock(1,1),Clock(1,1)) ,                 &
    & (Isavef(1),Savef(1)) , (Isaves(1),Saves(1)) , (Nangle,Cangle)
!
   IF ( .NOT.(Again) ) THEN
      Again = .TRUE.
      Nangle = 0
   ENDIF
   Nangle = Nangle + 1
!
!
!  BRANCH TO INSERT STRESSES AND FORCES INTO FORCE AND STRESS OR
!                                            SAVEF AND SAVES
!
!    KTYPE=1 - REAL OUTPUT FROM BLOCK IS TRANSFERED TO CLOCK, THEN
!            STORED IN FORCE AND STRESS, NOTHING IN SAVEF AND SAVES
!    KTYPE=2 - COMPLEX OUTPUT
!    IPART=1 - IMAGINARY PART OF COMPLEX OUTPUT FROM BLOCK, STORED
!              IN SAVEF AND SAVES
!    IPART=2 - REAL PART OF COMPLEX OUTPUT FROM CLOCK STORED IN
!              FORCE AND STRESS
!
   IF ( Ktype/=2 ) THEN
      DO i = 1 , 22
         DO j = 1 , 14
            Clock(i,j) = Block(i,j)
         ENDDO
      ENDDO
   ENDIF
!
!  OUTPUT FORCES FOR THIS ANGLE
!
   Iforce(1) = Elemid
   Force(2) = Clock(1,Cangle)
   j = 2
   DO i = 1 , 9
      j = j + 1
      Force(j) = Clock(i+7,Cangle)
!
!  OUTPUT CHARGES
      IF ( (i==3) .OR. (i==6) .OR. (i==9) ) THEN
         j = j + 1
         k = 19 + i/3
         Force(j) = Clock(k,Cangle)
      ENDIF
   ENDDO
!
! OUTPUT STRESSES
   Istres(1) = Elemid
   Stress(2) = Clock(1,Cangle)
   DO i = 1 , 6
      Stress(2+i) = Clock(i+1,Cangle)
   ENDDO
!
!  OUTPUT FLUXES
   DO i = 1 , 3
      Stress(i+8) = Clock(i+16,Cangle)
   ENDDO
!
   IF ( Ktype==2 ) THEN
!
!
!  OUTPUT FORCES FOR THIS ANGLE
!
      Isavef(1) = Elemid
      Savef(2) = Block(1,Nangle)
      j = 2
      DO i = 1 , 9
         j = j + 1
         Savef(j) = Block(i+7,Nangle)
!
!  OUTPUT CHARGES
         IF ( (i==3) .OR. (i==6) .OR. (i==9) ) THEN
            j = j + 1
            k = 19 + i/3
            Savef(j) = Block(k,Nangle)
         ENDIF
      ENDDO
!
! OUTPUT STRESSES
      Isaves(1) = Elemid
      Saves(2) = Block(1,Nangle)
      DO i = 1 , 6
         Saves(2+i) = Block(i+1,Nangle)
      ENDDO
!
!  OUTPUT FLUXES
      DO i = 1 , 3
         Saves(i+8) = Block(i+16,Nangle)
      ENDDO
!
      IF ( Nangle==14 ) THEN
         Again = .FALSE.
         GOTO 99999
      ELSEIF ( Iblock(1,Nangle+1)==1 ) THEN
         Again = .FALSE.
         GOTO 99999
      ENDIF
   ELSEIF ( Cangle==14 ) THEN
      Again = .FALSE.
      GOTO 99999
   ELSEIF ( Iclock(1,Cangle+1)==1 ) THEN
      Again = .FALSE.
      GOTO 99999
   ENDIF
!
   RETURN
99999 RETURN
END SUBROUTINE strax3
