
SUBROUTINE tshear
   IMPLICIT NONE
   REAL Alpha , Area , Bufflg , Costh , Csid(4,4) , Diag1(3) , Diag2(3) , E1 , Ecpt(1) , Eltemp , Eltype , Endid , Eorflg , F12(2) ,&
      & Fmu , G , Ge , Oldel , Pa , Pg(1) , Rho , Sigmac , Sigmas , Sigmat , Sinth , Stress , Tbar , Temp , Thick , Ti(16) , To1 ,  &
      & Tsq , Vec(3,4) , Veca(3) , Vecb(3) , Vmag , Xl(4)
   INTEGER I , I12 , Ia , Ib , Idefm , Ideft , In , Inflag , Isils(4) , Itemp , J , Matid , Mid , Ncsid(4,4) , Nu
   COMMON /matin / Matid , Inflag , Temp , Stress , Sinth , Costh
   COMMON /matout/ E1 , G , Nu , Rho , Alpha , To1 , Ge , Sigmat , Sigmac , Sigmas
   COMMON /ssgett/ Eltype , Oldel , Eorflg , Endid , Bufflg , Itemp , Ideft , Idefm
   COMMON /ssgwrk/ Vec , Xl , Diag1 , Diag2 , Ti , Pa , Tsq , Veca , Vecb , Area , Vmag , I , J , Ia , Ib , I12 , Tbar , In
   COMMON /trimex/ Ecpt , Isils , Mid , Thick , Fmu , Csid , Eltemp , F12
   COMMON /zzzzzz/ Pg
   INTEGER igrid2
   REAL vkl
!
!     ELEMENT TEMPERATURE AND DEFORMATION LOADING FOR THE SHEAR PANEL.
!
!     FORMULATION IS THAT OF A PSEUDO-ROD ON EACH EDGE OF THE SHEAR
!     PANEL.
!
!     ECPT( 1)         - ELEMENT ID
!     ECPT( 2 THRU 5)  - 4 GRID SILS
!     ECPT( 6)         - MATERIAL ID
!     ECPT( 7)         - THICKNESS
!     ECPT( 8)         - NON-STRUCTURAL MASS
!     ECPT( 9 THRU 24) - 4 POINTS (CSID,X,Y,Z) REPEATS
!     ECPT(25)         - ELEMENT TEMPERATURE
!     ECPT(26)         - F1 EFFECTIVENESS FACTOR DIRECTION 1, (NOT USED)
!     ECPT(27)         - F2 EFFECTIVENESS FACTOR DIRECTION 2, (NOT USED)
!
   EQUIVALENCE (Ncsid,Csid)
!
   F12(1) = 1.00
   F12(2) = 1.00
!
   IF ( F12(1)==0.0 .AND. F12(2)==0.0 ) RETURN
!
!     MATERIAL DATA ACQUISITION
!
   Inflag = 1
   Matid = Mid
   Temp = Eltemp
   CALL mat(Ecpt(1))
!
!     GRID POINT TEMPERATURES
!
   IF ( Itemp/=0 ) THEN
      CALL ssgetd(Ecpt(1),Ti,4)
!
!     ELEMENT DEFORMATION (NOT USED)
!
!     4 NORMALIZED EDGE VECTORS AND LENGTHS
!
      DO I = 1 , 4
         igrid2 = I + 1
         IF ( I==4 ) igrid2 = 1
!
         DO J = 1 , 3
            Vec(J,I) = Csid(J+1,I) - Csid(J+1,igrid2)
         ENDDO
!
         CALL norm(Vec(1,I),Xl(I))
      ENDDO
!
      IF ( F12(1)<=1.01 .OR. F12(2)<=1.01 ) THEN
!
!     PROJECTED AREA IS NEEDED. FIRST OBTAIN THE DIAGONAL VECTORS.
!
         DO I = 1 , 3
            Diag1(I) = Csid(I+1,3) - Csid(I+1,1)
            Diag2(I) = Csid(I+1,4) - Csid(I+1,2)
         ENDDO
!
!     NORMAL VECTOR (DIAG1 X DIAG2)
!
         CALL saxb(Diag1,Diag2,Diag2)
         CALL norm(Diag2,vkl)
         Pa = 0.5*vkl
      ENDIF
!
!     LOOP THROUGH LOADS ON 4 EDGES.
!
      Tsq = Thick*Thick
      DO I = 1 , 4
         I12 = mod(I,2)
         IF ( I12==0 ) I12 = 2
         Ia = I
         Ib = Ia + 1
         IF ( I==4 ) Ib = 1
!
!     TEMPERATURE
!
         Tbar = (Ti(Ia+1)+Ti(Ib+1))/2.0 - To1
!
!     EXTENSIONAL AREA
!
         IF ( F12(I12)<=1.01 ) THEN
            Area = F12(I12)*Pa*Thick/(Xl(I12)+Xl(I12+2))
         ELSE
            Area = 0.50*F12(I12)*Tsq
         ENDIF
!
         Vmag = E1*Area*Alpha*Tbar
         DO J = 1 , 3
            Veca(J) = Vmag*Vec(J,I)
            Vecb(J) = -Veca(J)
         ENDDO
!
         IF ( Ncsid(1,Ib)/=0 ) CALL basglb(Vecb(1),Vecb(1),Isils(Ib),Csid(1,Ib))
         In = Isils(Ib) - 1
         DO J = 1 , 3
            In = In + 1
            Pg(In) = Pg(In) + Vecb(J)
         ENDDO
!
         IF ( Ncsid(1,Ia)/=0 ) CALL basglb(Veca(1),Veca(1),Isils(Ia),Csid(1,Ia))
         In = Isils(Ia) - 1
         DO J = 1 , 3
            In = In + 1
            Pg(In) = Pg(In) + Veca(J)
         ENDDO
!
      ENDDO
   ENDIF
END SUBROUTINE tshear
