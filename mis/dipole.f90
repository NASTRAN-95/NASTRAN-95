
SUBROUTINE dipole(Buf,Ibuf,Xx,Yy,Zz,Hc1,Hc2,Hc3)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   REAL Hc1 , Hc2 , Hc3 , Xx , Yy , Zz
   REAL Buf(50)
   INTEGER Ibuf(50)
!
! Local variable declarations
!
   REAL cx , cy , cz , fpi , max , min , mx , mxa , my , myb , mz , mzc , r3 , r5 , rmr1 , xnum
   INTEGER icid
!
! End of declarations
!
!
! DIPOLE COMPUTES THE MAGNETIC INTENSITY AT THE POINT (XX,YY,ZZ) DUE
! TO A MAGNETIC DIPOLE MOMENT DEFINED ON AN MDIPOLE CARD STORED IN BUF.
! THE FORMULATION COMES FROM DARRELL NIXONS REPORT 27-23 MARCH 1972
!
   DATA fpi/12.566371/
!
   Hc1 = 0.
   Hc2 = 0.
   Hc3 = 0.
!
! ICID IS 0 FOR NOW AND WILL NOT BE USED. COORDS. AND MOMENT
! ARE ASSUMED TO BE IN BASIC COORDS
!
   icid = Ibuf(1)
!
! COORDS OF POINT DIPOLE
!
   cx = Buf(2)
   cy = Buf(3)
   cz = Buf(4)
   mx = Buf(5)
   my = Buf(6)
   mz = Buf(7)
   min = Buf(8)
   max = Buf(9)
!
! H WILL BE COMPUTED ONLY IF DISTANCE FROM (CX,CY,CZ) TO (XX,YY,ZZ) IS
! BETWEEN MIN AND MAX- IF MAX IS 0, COMPUTE FOR ALL POINTS GREATER THAN
! MIN
!
   rmr1 = sqrt((Xx-cx)**2+(Yy-cy)**2+(Zz-cz)**2)
   IF ( min>1.E-6 ) THEN
      IF ( rmr1<min ) GOTO 99999
   ENDIF
   IF ( max>1.E-6 ) THEN
      IF ( rmr1>max ) GOTO 99999
   ENDIF
!
   mxa = 3.*mx*(Xx-cx)
   myb = 3.*my*(Yy-cy)
   mzc = 3.*mz*(Zz-cz)
!
   r3 = rmr1**3
   r5 = r3*rmr1**2
   xnum = (mxa+myb+mzc)/r5
!
   Hc1 = -mx/r3 + xnum*(Xx-cx)
   Hc1 = Hc1/fpi
!
   Hc2 = -my/r3 + xnum*(Yy-cy)
   Hc2 = Hc2/fpi
!
   Hc3 = -mz/r3 + xnum*(Zz-cz)
   Hc3 = Hc3/fpi
!
99999 RETURN
END SUBROUTINE dipole
