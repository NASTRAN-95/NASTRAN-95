
SUBROUTINE scone3(Again)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Block(9,14) , Conphi , Delta , Dum(5) , Dum2(92) , Elhar , Force(25) , Harm , Iii , Sig(3) , Sig1 , Sig12 , Sig2 , Sinphi , &
      & Stress(100) , Sum(8) , Temp , Theta , Vec(8) , Zoff(2) , Zoveri
   INTEGER Elemid , Iblock(9,14) , Iforce(25) , Ipt , Istres(100) , N , Nangle , Nphi , Npoint
   COMMON /sdr2x7/ Dum , Iii , Zoff , Dum2 , Stress , Force
   COMMON /sdr2x8/ Vec , Sum , Sig , Sig1 , Sig2 , Sig12 , Temp , Delta , Theta , Npoint , Zoveri , Ipt , Block , Elhar , Elemid ,  &
                 & Harm , N , Sinphi , Conphi , Nphi , Nangle
!
! Dummy argument declarations
!
   LOGICAL Again
!
! Local variable declarations
!
   INTEGER i , j
!
! End of declarations
!
!
!
!
!
!
!
   EQUIVALENCE (Istres(1),Stress(1))
   EQUIVALENCE (Iforce(1),Force(1))
   EQUIVALENCE (Iblock(1,1),Block(1,1))
!
   IF ( .NOT.(Again) ) THEN
      Again = .TRUE.
      Nangle = 0
   ENDIF
   Nangle = Nangle + 1
!*****
!     OUTPUT FORCES FOR THIS ANGLE
!*****
   Iforce(1) = Elemid
   Force(2) = Block(1,Nangle)
   Force(3) = Block(5,Nangle)
   Force(4) = Block(6,Nangle)
   Force(5) = Block(7,Nangle)
   Force(6) = Block(8,Nangle)
   Force(7) = Block(9,Nangle)
!*****
! COMPUTE AND OUTPUT STRESSES AND PRINCIPAL STRESSES
!*****
   Istres(1) = Elemid
   Stress(2) = Block(1,Nangle)
   DO i = 1 , 2
      Zoveri = 0.0
      IF ( Iii/=0.0 ) Zoveri = Zoff(i)/Iii
      DO j = 1 , 3
         Sig(j) = Block(j+1,Nangle) + Block(j+4,Nangle)*Zoveri
      ENDDO
      Temp = Sig(1) - Sig(2)
      Sig12 = sqrt((Temp*0.50E0)**2+Sig(3)**2)
      Delta = (Sig(1)+Sig(2))*0.50E0
      Sig1 = Delta + Sig12
      Sig2 = Delta - Sig12
      Delta = 2.0E0*Sig(3)
      IF ( abs(Delta)<1.0E-15 .AND. abs(Temp)<1.0E-15 ) THEN
         Theta = 0.0E0
      ELSE
         Theta = atan2(Delta,Temp)*28.6478898E0
      ENDIF
      Ipt = 8*i - 6
      Stress(Ipt+1) = Zoff(i)
      Stress(Ipt+2) = Sig(1)
      Stress(Ipt+3) = Sig(2)
      Stress(Ipt+4) = Sig(3)
      Stress(Ipt+5) = Theta
      Stress(Ipt+6) = Sig1
      Stress(Ipt+7) = Sig2
      Stress(Ipt+8) = Sig12
   ENDDO
!*****
! SET AGAIN .FALSE. IF SDR2E IS NOT TO CALL THIS ROUTINE AGAIN FOR THIS
! ELEMENT.. E.G. ALL THE ANGLES DESIRED HAVE BEEN PROCESSED...
!*****
   IF ( Nangle==14 ) THEN
      Again = .FALSE.
   ELSEIF ( Iblock(1,Nangle+1)==1 ) THEN
      Again = .FALSE.
   ELSE
      RETURN
   ENDIF
END SUBROUTINE scone3
