
SUBROUTINE rod
   IMPLICIT NONE
   REAL Alpha , Arry(3) , Bgpdt(9) , Bufflg , Core(1) , Costh , Degra , Delta , E1 , Endid , Eorflg , Force(3) , G , Ge , Gpida1(1) &
      & , Gpidb1(1) , Oldel , Pi , Radeg , Rho , S4pisq , Sigmac , Sigmas , Sigmat , Sinth , Space(10) , Stress , Tbar , Temp ,     &
      & Ti(16) , To1 , Twopi , Vect(3) , Vmag , Xl
   INTEGER Eid , Eltype , Gpida , Gpidb , Iarry(97) , Icstma , Icstmb , Idefm , Ideft , In , Inflag , Itemp , L , Matid , Nu
   COMMON /condas/ Pi , Twopi , Radeg , Degra , S4pisq
   COMMON /matin / Matid , Inflag , Temp , Stress , Sinth , Costh
   COMMON /matout/ E1 , G , Nu , Rho , Alpha , To1 , Ge , Sigmat , Sigmac , Sigmas , Space
   COMMON /ssgett/ Eltype , Oldel , Eorflg , Endid , Bufflg , Itemp , Ideft , Idefm
   COMMON /ssgwrk/ Ti , Vect , Force , Bgpdt , Vmag , In , L , Tbar , Delta , Xl
   COMMON /trimex/ Eid , Gpida , Gpidb , Iarry
   COMMON /zzzzzz/ Core
   REAL a
   INTEGER i , nept
!
!     ELEMENT TEMPERATURE AND DEFORMATION LOADING FOR THE ROD, CONROD,
!     TUBE
!
   EQUIVALENCE (Iarry(1),Arry(1)) , (Icstma,Bgpdt(1)) , (Icstmb,Bgpdt(5)) , (Gpida1(1),Bgpdt(2)) , (Gpidb1(1),Bgpdt(6))
!
   nept = 5
   IF ( Eltype==3 ) nept = 4
   a = Arry(2)
!
!     RECOMPUTE AREA IF ELEMENT IS TUBE
!
   IF ( nept==4 ) a = Pi*(a-Arry(3))*Arry(3)
!
   DO i = 1 , 9
      nept = nept + 1
      Bgpdt(i) = Arry(nept)
   ENDDO
!
!     OBTAIN THE MATERIAL DATA
!
   Inflag = 1
   Matid = Iarry(1)
   Temp = Bgpdt(9)
   CALL mat(Eid)
   IF ( Itemp/=0 ) THEN
      CALL ssgetd(Eid,Ti,0)
      Tbar = Ti(1) - To1
   ELSE
      Tbar = 0.0
   ENDIF
   IF ( Ideft/=0 ) THEN
      CALL fedt(Eid,Delta,Idefm)
   ELSE
      Delta = 0.0
   ENDIF
   DO i = 1 , 3
      Vect(i) = Gpida1(i) - Gpidb1(i)
   ENDDO
   CALL norm(Vect(1),Xl)
   Vmag = E1*a*(Delta+Alpha*Xl*Tbar)/Xl
   DO i = 1 , 3
      Vect(i) = -Vect(i)*Vmag
      Force(i) = -Vect(i)
   ENDDO
   IF ( Icstmb/=0 ) CALL basglb(Vect(1),Vect(1),Gpidb1,Icstmb)
   In = Gpidb - 1
   DO i = 1 , 3
      L = In + i
      Core(L) = Core(L) + Vect(i)
   ENDDO
   IF ( Icstma/=0 ) CALL basglb(Force(1),Force(1),Gpida1,Icstma)
   In = Gpida - 1
   DO i = 1 , 3
      L = In + i
      Core(L) = Core(L) + Force(i)
   ENDDO
END SUBROUTINE rod
