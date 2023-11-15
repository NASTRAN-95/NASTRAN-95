
SUBROUTINE getdef(Dfrm,Ph,Mag,Conv,Plttyp,Buf,Gpt,D)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Defc(4) , Defval , Maxdef , Pbufsz , Penpap(30) , Ploter(5) , Scale(4) , Skp11(3) , Skp12(4) , Skp2(6)
   INTEGER Last , Lsil , Msil , Ngp , Ngpset , Siln
   COMMON /blank / Ngp , Lsil , Skp11 , Ngpset , Skp12 , Skp2 , Msil
   COMMON /xxparm/ Pbufsz , Ploter , Penpap , Scale , Maxdef
   COMMON /zntpkx/ Defc , Siln , Last
!
! Dummy argument declarations
!
   REAL Conv , Ph
   INTEGER Dfrm , Mag , Plttyp
   INTEGER Buf(1) , Gpt(1)
   REAL D(3,1)
!
! Local variable declarations
!
   REAL cn , sn
   INTEGER gp , gpx , i , i1 , i2 , inprew , k , rew , sil1 , sil2 , sp , trl(7) , type
!
! End of declarations
!
!
   EQUIVALENCE (Defval,Defc(1))
   DATA inprew , rew/0 , 1/
!
   Last = 0
   k = 3*Ngpset
   DO i = 1 , k
      D(i,1) = 0.0
   ENDDO
   trl(1) = Dfrm
   CALL rdtrl(trl(1))
   IF ( trl(5)<=0 ) RETURN
   sp = trl(5)
   ASSIGN 800 TO type
!
!     NOTE TRANSIENT RESPONSE HAS SP = 1
!
   IF ( sp>=3 ) THEN
      ASSIGN 700 TO type
      IF ( Mag==0 ) THEN
         ASSIGN 600 TO type
         sn = sin(Ph)*Conv
         cn = cos(Ph)*Conv
         IF ( Plttyp==2 ) THEN
!
!     VELOCITY
!
            i1 = sp - 1
            i2 = 1
         ELSE
!
!     DISPLACEMENT OR ACCELERATION
!
            i1 = 1
            i2 = sp - 1
            IF ( Plttyp==3 .OR. Plttyp==4 ) cn = -cn
         ENDIF
      ENDIF
   ENDIF
   Maxdef = 0.
   CALL intpk(*99999,Dfrm,0,sp,0)
   gp = 0
   Siln = 0
   CALL gopen(Msil,Buf(1),inprew)
   CALL fread(Msil,sil2,1,0)
!
!     -GP- = PREVIOUS EXISTENT GRID POINT IN THIS SET. FIND NEXT ONE.
!
 100  k = gp + 1
   DO gpx = k , Ngp
      IF ( Gpt(gpx)/=0 ) GOTO 200
   ENDDO
   sil1 = Lsil + 1
   GOTO 400
 200  DO WHILE ( gpx/=gp+1 )
      gp = gp + 1
      CALL fread(Msil,sil2,1,0)
   ENDDO
   sil1 = sil2
!
!     -SIL1- = SIL NUMBER OF NEXT EXISTENT GRID POINT. READ SIL NUMBER
!              OF NEXT GRID POINT.
!
   gp = gpx
   gpx = iabs(Gpt(gp))
   IF ( gp==Ngp ) sil2 = Lsil + 1
   IF ( gp/=Ngp ) CALL fread(Msil,sil2,1,0)
!
!     READ NEXT DEFORMATION VALUE AT THIS EXISTING GRID POINT.
!
 300  IF ( Siln<=Lsil .AND. Siln>=sil1 ) THEN
      IF ( Siln>sil1+2 .OR. Siln>=sil2 ) GOTO 100
      k = Siln - sil1 + 1
      D(k,gpx) = Defval
      IF ( Last==0 ) GOTO 500
!
      CALL close(Msil,rew)
      GOTO 99999
   ENDIF
 400  IF ( Last/=0 ) THEN
      CALL close(Msil,rew)
      GOTO 99999
   ENDIF
 500  CALL zntpki
   GOTO type
 600  Defval = Defc(i1)*cn - Defc(i2)*sn
   GOTO 800
 700  Defval = Conv*sqrt(Defc(1)**2+Defc(sp-1)**2)
 800  IF ( abs(Defval)>Maxdef ) Maxdef = abs(Defval)
   GOTO 300
99999 END SUBROUTINE getdef
