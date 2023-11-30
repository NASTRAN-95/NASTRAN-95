
SUBROUTINE rodd
   IMPLICIT NONE
   REAL Afact , Alfa , Bgpdt(4,2) , Cfact , Cp , Dum(3) , Dum2(2) , Dumy(12) , E , Eltemp , Est(200) , G , Heat , Jfact , Kcon ,    &
      & Mu , Rho , Tbar , Tsub0
   INTEGER Eid , Elid , Estid , Ge , Icmbar , Iest(13) , Iheat , Inflag , Ioutpt , Iprec , Ixtra , Izr , Kmbgg(3) , Ksystm(63) ,    &
         & Matid , Mid , Nlocs , Nu , Nzr , Sil1 , Sil2
   DOUBLE PRECISION K(1)
   LOGICAL Nogo
   CHARACTER*23 Ufm
   COMMON /emgdic/ Dum2 , Nlocs , Elid , Estid
   COMMON /emgest/ Eid , Sil1 , Sil2 , Mid , Afact , Jfact , Cfact , Mu , Bgpdt , Tbar
   COMMON /emgprm/ Ixtra , Izr , Nzr , Dumy , Kmbgg , Iprec , Nogo , Heat , Icmbar
   COMMON /hmtout/ Kcon
   COMMON /matin / Matid , Inflag , Eltemp , Dum
   COMMON /matout/ E , G , Nu , Rho , Alfa , Tsub0 , Ge
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ K
   INTEGER dict(7) , i , iaa , iaaz , iab , iabz , iba , ibaz , ibb , ibbz , icode , ij , ipart , ipass , iti , itj , iz , izero ,  &
         & izp5 , izpi , j , kz , ldata , ndof , ng , npart , nsq , nz
   DOUBLE PRECISION el , evect(3) , ha(3) , hb(3) , ke , kha(3) , khb(3) , massii(9) , massij(9) , massji(9) , massjj(9) , me ,     &
                  & mijdum(9) , mjidum(9) , scale , ta(9) , tb(9) , te
!
!     THIS ROUTINE PROCESSES ROD ELEMENT DATA TO PRODUCE STIFFNESS AND
!     MASS MATRICES. IF THE HEAT TRANSFER OPTION IS ON, CONDUCTIVITY AND
!     CAPACITY MATRICES ARE PRODUCED
!
!     THIS ROUTINE CAN COMPUTE BOTH CONVENTIONAL AND CONSISTENT
!     MASS MATRICES
!
!     DOUBLE PRECISION VERSION
!
!     THIS VERSION WAS SPECIALLY CODED TO ILLUSTRATE A GENERAL
!     USE OF THE IMPROVED MATRIX GENERATOR.
!
!     THE EST ENTRY FOR THIS ELEMENT CONTAINS
!
!     POSITION     NAME       DESCRIPTION
!     *****        *****      *******************************
!     1             EID       ELEMENT ID NO.
!     2             SIL1      SCALAR INDEX OF POINT A
!     3             SIL2      SCALAR INDEX OF POINT B
!     4             MID       MATERIAL DATA ID
!     5             AFACT     AREA OF CROSS SECTION
!     6             JFACT     TORSIONAL STIFFNESS COEFFICIENT
!     7             CFACT     TORSIONAL STRESS RECOVERY DISTANCE
!     8             MU        NON-STRUCTURAL MASS PER LENGTH
!     9-16          BGPDT     BASIC GRID POINT DATA. COORDINATE SYSTEM
!                             NUMBER AND  X,Y,Z LOCATION FOR 2 POINTS
!     17            TBAR      AVERAGE ELEMENT TEMPERATURE
!
!
!
!     THE VARIABLE K IS OPEN CORE. OPEN SPACE EXISTS FROM Z(IZ) TO Z(NZ)
!     THIS IS INTENDED AS AN EXAMPLE. NORMALLY FOR SMALL ARRAYS
!     LOCAL VARIABLES MAY BE USED.
!
   EQUIVALENCE (Ksystm(2),Ioutpt) , (Ksystm(56),Iheat) , (Eid,Est(1),Iest(1)) , (Cp,Kcon)
!
!     FOR DOUBLE PRECISION THE POINTERS TO OPEN CORE MUST BE MODIFIED.
!
   iz = (Izr-2)/Iprec + 2
   nz = Nzr/Iprec
   IF ( nz-iz<=144 ) THEN
!
      Nogo = .TRUE.
      WRITE (Ioutpt,99001) Ufm
99001 FORMAT (A23,' 3119, INSUFFICIENT CORE TO PROCESS ROD ELEMENTS')
      GOTO 99999
   ELSE
      dict(1) = Estid
!
!     SUBTRACT BASIC LOCATIONS TO OBTAIN LENGTH ETC.
!
      DO i = 1 , 3
         evect(i) = Bgpdt(i+1,2) - Bgpdt(i+1,1)
      ENDDO
!
      el = dsqrt(evect(1)**2+evect(2)**2+evect(3)**2)
      IF ( el<=0.0D0 ) GOTO 400
!
!     IF HEAT TRANSFER PROBLEM TRANSFER.  CALL MATERIAL SUBROUTINE
!
      Inflag = 1
      Matid = Mid
      Eltemp = Tbar
      IF ( Iheat==1 ) THEN
!
!     HEAT TRANSFER CALCULATIONS ARE PERFORMED HERE
!
         Inflag = 1
         dict(2) = 1
         dict(3) = 2
         dict(4) = 1
         dict(5) = 0
         IF ( Kmbgg(1)/=0 ) THEN
            CALL hmat(Eid)
            K(iz) = dble(Afact*Kcon)/el
            IF ( K(iz)/=0.0D0 ) THEN
               K(iz+1) = -K(iz)
               K(iz+2) = -K(iz)
               K(iz+3) = K(iz)
               CALL emgout(K(iz),K(iz),4,1,dict,1,Iprec)
            ENDIF
         ENDIF
         Inflag = 4
         IF ( Kmbgg(1)==0 ) RETURN
         CALL hmat(Eid)
         K(iz) = dble(Afact*Cp)*el/2.0D0
         IF ( K(iz)==0.0D0 ) RETURN
         K(iz+1) = K(iz)
         dict(2) = 2
         CALL emgout(K(iz),K(iz),2,1,dict,3,Iprec)
         RETURN
      ELSE
         CALL mat(Eid)
         ke = dble(E*Afact)/el
         me = (dble(Rho*Afact+Mu))*el/2.0D0
         te = dble(G*Jfact)/el
!
!     PROCESS STIFFNESS HERE
!
         IF ( Kmbgg(1)==0 ) GOTO 300
         IF ( ke==0.0D0 .AND. te==0.0D0 ) GOTO 300
!
!     GENERATE   HA  =  (E*TA)/EL   AND  HB = (E*TB)/EL
!
         IF ( Iest(9)==0 ) THEN
            DO i = 1 , 3
               ha(i) = evect(i)/el
            ENDDO
         ELSE
            CALL transd(Bgpdt(1,1),ta)
            CALL gmmatd(evect,1,3,0,ta,3,3,0,ha)
            DO i = 1 , 3
               ha(i) = ha(i)/el
            ENDDO
         ENDIF
         IF ( Iest(13)==0 ) THEN
            DO i = 1 , 3
               hb(i) = evect(i)/el
            ENDDO
         ELSE
            CALL transd(Bgpdt(1,2),tb)
            CALL gmmatd(evect,1,3,0,tb,3,3,0,hb)
            DO i = 1 , 3
               hb(i) = hb(i)/el
            ENDDO
         ENDIF
!
!     THE GENERAL 12X12  MATRIX FOR THE ROD ELEMENT IS
!                            -                              -
!                            1HA K HA1   0  1HA K HB1       1
!                **   **     1 ------1------1-------1-------1
!                *  K  *   = 1   0   1HA T A1       1HA T HB1
!                **   **     1 ------1------1-------1-------1
!                            1HB K HA1      1HB K HB1       1
!                            1 ------1------1-------1-------1
!                            1       1HB T A1       1HB T HB1
!                            1       1      1       1       1
!                            -                              -
!                      EACH BLOCK  ABOVE IS A 3 BY 3 MATRIX
!
!     TEST AND SET COMPONENT CODE    111= 7     111000=56
!
         icode = 0
         ndof = 0
         IF ( te==0.D0 ) THEN
            icode = 7
            ndof = 6
         ELSEIF ( ke/=0.D0 ) THEN
            icode = 63
            ndof = 12
         ELSE
            icode = 56
            ndof = 6
         ENDIF
         nsq = ndof**2
         ng = ndof/2
         npart = ng*ndof
         izero = iz - 1
         ipass = 1
         DO i = 1 , nsq
            izpi = iz + i - 1
            K(izpi) = 0.0D0
         ENDDO
!
!     EXTENSIONAL STIFFNESS TERMS ARE COMPUTED HERE.
!
         IF ( icode==56 ) GOTO 200
         scale = ke
      ENDIF
   ENDIF
 100  DO i = 1 , 3
      kha(i) = scale*ha(i)
      khb(i) = scale*hb(i)
   ENDDO
!
!     THE MATRIX COLUMNS AND ROWS MUST BE IN THE NUMERICAL ORDER
!     OF TH SIL VALUES. THE POINTERS INTO THE MATRIX ARE VARIABLES.
!
   IF ( Sil2<Sil1 ) THEN
      ibbz = izero
      iabz = izero + ng
      ibaz = izero + npart
      iaaz = ibaz + ng
   ELSEIF ( Sil2==Sil1 ) THEN
      GOTO 400
   ELSE
      iaaz = izero
      ibaz = izero + ng
      iabz = izero + npart
      ibbz = iabz + ng
   ENDIF
   DO j = 1 , 3
      DO i = 1 , 3
         ij = ndof*(j-1) + i
         iaa = ij + iaaz
         K(iaa) = kha(i)*ha(j)
         iba = ij + ibaz
         K(iba) = -khb(i)*ha(j)
         iab = ij + iabz
         K(iab) = -kha(i)*hb(j)
         ibb = ij + ibbz
         K(ibb) = khb(i)*hb(j)
      ENDDO
   ENDDO
!
!     THE TORSIONAL STIFFNESS TERMS ARE FORMED USING TE INSTEAD OF KE
!     THEY ARE INSERTED IN THE MATRIX WITH  A CONSTANT OFFSET, 3*12+3.
!
 200  IF ( ipass/=2 ) THEN
      IF ( ndof==12 ) izero = 38 + iz
      ipass = 2
      scale = te
      IF ( icode/=7 ) GOTO 100
   ENDIF
   ipart = iz
   dict(2) = 1
   dict(3) = ndof
   dict(4) = icode
   dict(5) = Ge
   CALL emgout(K(ipart),K(ipart),nsq,1,dict,1,Iprec)
!
!     THE MASS MATRIX TERMS ARE CALCULATED HERE.
!
 300  IF ( Kmbgg(2)==0 .OR. me==0.0D0 ) RETURN
   dict(3) = 6
   dict(4) = 7
   dict(5) = 0
!
!     CHECK TO SEE IF CONVENTIONAL OR CONSISTENT MASS MATRIX IS REQUIRED
!
   IF ( Icmbar>0 ) THEN
!
!     CONSISTENT MASS MATRIX TERMS ARE COMPUTED HERE
!
      dict(2) = 1
      ldata = 36
      DO i = 1 , 9
         massii(i) = 0.0D0
         massjj(i) = 0.0D0
         massij(i) = 0.0D0
         massji(i) = 0.0D0
         mijdum(i) = 0.0D0
         mjidum(i) = 0.0D0
      ENDDO
      me = 2.0D0*me
      DO i = 1 , 9 , 4
         massii(i) = me/3.0D0
         massjj(i) = me/3.0D0
         massij(i) = me/6.0D0
         massji(i) = me/6.0D0
         mijdum(i) = me/6.0D0
         mjidum(i) = me/6.0D0
      ENDDO
      IF ( Sil2<Sil1 ) THEN
         iti = 13
         itj = 9
      ELSEIF ( Sil2==Sil1 ) THEN
         GOTO 400
      ELSE
         iti = 9
         itj = 13
      ENDIF
      IF ( Iest(iti)/=0 ) THEN
         CALL transd(Iest(iti),ta)
         CALL gmmatd(ta,3,3,1,massii,3,3,0,K(iz))
         CALL gmmatd(K(iz),3,3,0,ta,3,3,0,massii)
         CALL gmmatd(ta,3,3,1,mijdum,3,3,0,massij)
         CALL gmmatd(mjidum,3,3,0,ta,3,3,0,massji)
      ENDIF
      IF ( Iest(itj)/=0 ) THEN
         CALL transd(Iest(itj),ta)
         CALL gmmatd(ta,3,3,1,massjj,3,3,0,K(iz))
         CALL gmmatd(K(iz),3,3,0,ta,3,3,0,massjj)
         CALL gmmatd(massij,3,3,0,ta,3,3,0,mijdum)
         CALL gmmatd(ta,3,3,1,massji,3,3,0,mjidum)
         DO i = 1 , 9
            massij(i) = mijdum(i)
            massji(i) = mjidum(i)
         ENDDO
      ENDIF
      DO i = 1 , 3
         kz = iz + i - 1
         K(kz) = massii(i)
         K(kz+6) = massii(i+3)
         K(kz+12) = massii(i+6)
         K(kz+3) = massij(i)
         K(kz+9) = massij(i+3)
         K(kz+15) = massij(i+6)
         K(kz+18) = massji(i)
         K(kz+24) = massji(i+3)
         K(kz+30) = massji(i+6)
         K(kz+21) = massjj(i)
         K(kz+27) = massjj(i+3)
         K(kz+33) = massjj(i+6)
      ENDDO
   ELSE
!
!     CONVENTIONAL MASS MATRIX TERMS ARE COMPUTED HERE
!
      dict(2) = 2
      ldata = 6
      izp5 = iz + 5
      DO i = iz , izp5
         K(i) = me
      ENDDO
   ENDIF
   CALL emgout(K(iz),K(iz),ldata,1,dict,2,Iprec)
   RETURN
!
 400  Nogo = .TRUE.
   WRITE (Ioutpt,99002) Ufm , Eid
99002 FORMAT (A23,' 3118, ROD ELEMENT NO.',I9,' HAS ILLEGAL GEOMETRY OR CONNECTIONS.')
   RETURN
99999 RETURN
END SUBROUTINE rodd
