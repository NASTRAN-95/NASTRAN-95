
SUBROUTINE melbow
   IMPLICIT NONE
   REAL A , Betar , Costh , Cp , Dum(14) , Dumm(8) , Dz , Ecpt(9) , Eltemp , Fe , Fj , Nsm , Prop(8) , R , Rho , Sinth , Smallv(3) ,&
      & Stress , Tempel , Z(1)
   INTEGER Bggind , Clsnrw , Clsrw , Eor , Frowic , I1 , I2 , I6x6b , I6x6m , Icssv , Icstm , Idum1 , Idum2 , Idum3 , Idum4 ,       &
         & Idum5 , Iecpt(1) , Ielid , Ifbgg , Ifcstm , Ifdit , Ifecpt , Ifgpct , Ifmgg , Ifmpt , Igbgg , Igecpt , Iggpct , Igmgg ,  &
         & Igpct , Imatid , Inrw , Iopt4 , Ipoint , Isilno(2) , Iz(1) , Jmax , Left , Link(10) , Lrowic , Matflg , Matidc ,         &
         & Mcbbgg(7) , Mcbmgg(7) , N6x6b , N6x6m , Ncstm , Neor , Ngpct , Nlinks , Nogo , Npoint , Npvt , Nrowsc , Outrw , Tnrows
   DOUBLE PRECISION Dela(6) , Delb(6) , Dp(6) , Dumdp , Fl , M(36) , Ta(9) , Tb(9) , Veci(3)
   LOGICAL Heat
   COMMON /hmtout/ Cp
   COMMON /matin / Matidc , Matflg , Eltemp , Stress , Sinth , Costh
   COMMON /matout/ Rho , Prop
   COMMON /sma2bk/ Icstm , Ncstm , Igpct , Ngpct , Ipoint , Npoint , I6x6m , N6x6m , I6x6b , N6x6b
   COMMON /sma2cl/ Iopt4 , Bggind , Npvt , Left , Frowic , Lrowic , Nrowsc , Tnrows , Jmax , Nlinks , Link , Nogo
   COMMON /sma2dp/ Ta , Tb , Dp , Veci , Dela , Delb , Fl , M , Dumdp
   COMMON /sma2et/ Ielid , Isilno , Smallv , Icssv , Imatid , A , I1 , I2 , Fj , Nsm , Fe , Dum , R , Betar , Dumm , Tempel
   COMMON /sma2ht/ Heat
   COMMON /sma2io/ Ifcstm , Ifmpt , Ifdit , Idum1 , Ifecpt , Igecpt , Ifgpct , Iggpct , Idum2 , Idum3 , Ifmgg , Igmgg , Ifbgg ,     &
                 & Igbgg , Idum4 , Idum5 , Inrw , Outrw , Clsnrw , Clsrw , Neor , Eor , Mcbmgg , Mcbbgg
   COMMON /zzzzzz/ Z
   REAL dcr
   DOUBLE PRECISION fm
   INTEGER i
!
!     THIS ROUTINE COMPUTES THE MASS MATRIX M(NPVT,NPVT) FOR AN ELBOW.
!
!     ECPT FOR THE ELBOW
!
!     ECPT( 1)  -  IELID        ELEMENT ID. NUMBER
!     ECPT( 2)  -  ISILNO(2)    * SCALAR INDEX NOS. OF THE GRID POINTS
!     ECPT( 3)  -    ...        *
!     ECPT( 9)  -  A            CROSS-SECTIONAL AREA
!     ECPT(13)  -  NSM          NON-STRUCTURAL MASS
!     ECPT(29)  -  R            RADIUS OF CURVATURE
!     ECPT(30)  -  BETAR        ANGLE FROM GA TO GB
!
!
!     SMA2 VARIABLE CORE BOOKKEEPING PARAMETERS
!
!
!     SMA2 PROGRAM CONTROL PARAMETERS
!
!
!     ECPT COMMON BLOCK
!
!
!     SMA2 LOCAL VARIABLES
!
!
!     INPUT AND OUTPUT BLOCKS FOR SUBROUTINE MAT
!
   !>>>>EQUIVALENCE (Z(1),Iz(1),Dz) , (Ecpt(1),Iecpt(1),Ielid)
   DATA dcr/0.01745329/
!
!     COMPUTE LENGTH OF ELBOW, FL
!
   Dp(1) = R
   Dp(2) = Betar
   Dp(3) = dcr
   Fl = Dp(1)*Dp(2)*Dp(3)
   IF ( Fl==0.0D0 ) THEN
!
      CALL mesage(30,26,Iecpt(1))
!
!     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
!     ACCUMULATE
!
      Nogo = 1
      RETURN
   ELSEIF ( Heat ) THEN
!
!     HEAT FORMULATION
!
!     GET CP USING -HMAT- ROUTINE.
!
      Matidc = Imatid
      Matflg = 4
      CALL hmat(Ielid)
      M(1) = Fl*dble(Ecpt(9))*dble(Cp)/2.0D0
!
!     OUTPUT THE MASS FOR HEAT PROBLEM.
!
      CALL sma2b(M(1),Npvt,Npvt,Ifbgg,Dumdp)
      GOTO 99999
   ENDIF
!
!     GET RHO FROM MPT BY CALLING MAT
!
   Matidc = Imatid
   Matflg = 4
   Eltemp = Tempel
   CALL mat(Ecpt(1))
   DO i = 1 , 36
      M(i) = 0.0D0
   ENDDO
   fm = 0.5*Fl*(Rho*A+Nsm)
!
!     PUT MASS IN M-ARRAY
!
   M(1) = fm
   M(8) = M(1)
   M(15) = M(1)
!
!     INSERT THE 6 X 6
!
   CALL sma2b(M,Npvt,-1,Ifmgg,Dumdp)
   RETURN
99999 RETURN
END SUBROUTINE melbow