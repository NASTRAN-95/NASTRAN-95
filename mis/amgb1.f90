
SUBROUTINE amgb1(Input,Matout,Skj)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Amach , Amachr , Blspc , Bspace , Chord , Degra , Den , Dum(2) , Flowa , Mach , Maxmac , Minmac , Pi , Radeg , Radius ,     &
      & Redf , Refc , Refcrd , Refden , Refflo , Refmac , Refstg , Refvel , Rfreq , S4pisq , Sigma , Stager , Twopi , Vel , Work(1) &
      & , Xsign
   LOGICAL Debug , Tsonic
   INTEGER Ii , Incr , Iout , Iref , Isk , Iti , Ito , Iz(1) , Mcb(7) , Nj , Nk , Nlines , Nn , Nrow , Nsk , Nstns , Nstnsx , Sln , &
         & Sysbuf , Tskj(7)
   CHARACTER*23 Ufm
   COMMON /amgbug/ Debug
   COMMON /amgmn / Mcb , Nrow , Dum , Refc , Sigma , Rfreq , Tskj , Isk , Nsk
   COMMON /bamg1l/ Iref , Minmac , Maxmac , Nlines , Nstns , Refstg , Refcrd , Refmac , Refden , Refvel , Refflo , Sln , Nstnsx ,   &
                 & Stager , Chord , Radius , Bspace , Mach , Den , Vel , Flowa , Amach , Redf , Blspc , Amachr , Tsonic , Xsign
   COMMON /blank / Nk , Nj
   COMMON /condas/ Pi , Twopi , Radeg , Degra , S4pisq
   COMMON /packx / Iti , Ito , Ii , Nn , Incr
   COMMON /system/ Sysbuf , Iout
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Work
!
! Dummy argument declarations
!
   INTEGER Input , Matout , Skj
!
! Local variable declarations
!
   REAL amachl , radii(50) , wfact
   INTEGER ecore , i , ibad , ip1 , ip2 , ip3 , ip4 , ip5 , iptr , irsln , isln , j , k , n , najjc , name(2) , ndata , next ,      &
         & nline , nsns , ntsonx , nw1 , nw2 , nwar
   INTEGER korsz
!
! End of declarations
!
!
!     DRIVER FOR COMPRESSOR BLADE THEORY.
!     COMPUTATIONS ARE FOR THE AJJL AND SKJ MATRICES.
!     FOR COMPRESSOR BLADES K-SET = J-SET = NLINES*NSTNS.
!     SKJ = W*F(INVERS)TRANSPOSE.
!
!
   EQUIVALENCE (Work(1),Iz(1))
   DATA name/4HAMGB , 4H1   /
!
!     READ PARAMETERS IREF,MINMAC,MAXMAC,NLINES AND NSTNS
!
   CALL read(*400,*400,Input,Iref,5,0,n)
   IF ( Debug ) CALL bug1('ACPT-REF  ',5,Iref,5)
!
!     READ REST OF ACPT RECORD INTO OPEN CORE AND LOCATE REFERENCE
!     PARAMETERS REFSTG,REFCRD,REFMAC,REFDEN,REFVEL AND REFFLO
!     STORE STREAMLINE RADIUS FOR ALL STREAMLINES
!
   ecore = korsz(Iz) - 4*Sysbuf
   CALL read(*100,*100,Input,Iz,ecore,1,nwar)
!
!     NOT ENOUGH CORE
!
   CALL mesage(-8,0,name)
   GOTO 400
 100  irsln = 0
   IF ( Debug ) CALL bug1('ACPT-REST ',10,Iz,nwar)
   ntsonx = 0
   ndata = 3*Nstns + 10
   nline = 0
   DO i = 1 , nwar , ndata
!
!     LOCATE REFERENCE STREAMLINE NUMBER (IREF = SLN)
!
      IF ( Iref==Iz(i) ) irsln = i
!
!     STORE AMACH FOR LATER DATA CHECK. COUNT TRANSONIC STREAMLINES
!
      amachl = Work(i+6)*cos(Degra*(Work(i+9)-Work(i+2)))
      IF ( amachl>Maxmac .AND. amachl<Minmac ) ntsonx = ntsonx + 1
      nline = nline + 1
      Work(nwar+nline) = amachl
      radii(nline) = Work(i+4)
   ENDDO
!
!     DETERMINE DIRECTION OF BLADE ROTATION VIA Y-COORDINATES AT TIP
!     STREAMLINE. USE COORDINATES OF FIRST 2 NODES ON STREAMLINE.
!
   iptr = ndata*(Nlines-1)
   Xsign = 1.0
   IF ( Work(iptr+15)<Work(iptr+12) ) Xsign = -1.0
!
   IF ( Debug ) CALL bug1('RADII     ',25,radii,Nlines)
!
!     INPUT CHECKS -
!     (1) AMACH MUST INCREASE FROM BLADE ROOT TO BLADE TIP
!     (2) ALL TRANSONIC AMACH-S ARE NOT ALLOWED AT PRESENT
!
   ibad = 0
   IF ( ntsonx>=Nlines ) THEN
      ibad = 1
      WRITE (Iout,99001) Ufm
!
99001 FORMAT (A23,' -AMG MODULE- ALL TRANSONIC STREAMLINES NOT ALLOWED',/39X,'CHECK MACH ON STREAML2 BULK DATA CARDS OR',/39X,      &
             &'CHANGE PARAMETERS MINMACH AND MAXMACH.')
   ENDIF
   nw1 = nwar + 1
   nw2 = nwar + Nlines - 1
   DO i = nw1 , nw2
      IF ( Work(i)>Work(i+1) ) GOTO 200
   ENDDO
   GOTO 300
 200  ibad = 1
   isln = (i-nwar-1)*ndata + 1
   WRITE (Iout,99002) Ufm , Iz(isln)
99002 FORMAT (A23,' -AMG MODULE- MACH NUMBERS MUST INCREASE FROM BLADE',' ROOT TO BLADE TIP.',/39X,                                 &
             &'CHECK STREAML2 BULK DATA CARD WITH SLN =',I3)
 300  IF ( ibad/=0 ) THEN
!
!     ERROR MESSAGES
!
!     BAD STREAMLINE DATA
!
      CALL mesage(-61,0,0)
      CALL mesage(-8,0,name)
   ELSE
!
!     SET TSONIC IF THERE ARE ANY TRANSONIC STREAMLINES
!
      Tsonic = .FALSE.
      IF ( ntsonx>0 ) Tsonic = .TRUE.
!
!     STORE REFERENCE PARAMETERS
!     DID IREF MATCH AN SLN OR IS THE DEFAULT TO BE TAKEN  (BLADE TIP)
!
      IF ( irsln==0 ) irsln = (Nlines-1)*ndata + 1
      Refstg = Work(irsln+2)
      Refcrd = Work(irsln+3)
      Refmac = Work(irsln+6)
      Refden = Work(irsln+7)
      Refvel = Work(irsln+8)
      Refflo = Work(irsln+9)
!
!     REPOSITION ACPT TO BEGINNING OF COMPRESSOR BLADE DATA
!
      CALL bckrec(Input)
      CALL fread(Input,0,-6,0)
      IF ( Debug ) CALL bug1('BAMG1L    ',47,Iref,26)
!
!     COMPUTE POINTERS AND SEE IF THERE IS ENOUGH CORE
!     IP1 AND IP2 ARE COMPLEX POINTERS
!
      najjc = Nstns
      ntsonx = 1
      IF ( Tsonic ) najjc = Nlines*Nstns
      IF ( Tsonic ) ntsonx = Nlines
      ip1 = 1
      ip2 = ip1 + 2*(Nstns*najjc)
      ip3 = ip2 + 2*Nstns
      ip4 = ip3 + ntsonx
      ip5 = ip4 + ntsonx
      next = ip5 + ntsonx
      IF ( next>ecore ) THEN
         CALL mesage(-8,0,name)
      ELSE
!
!     CALL ROUTINE TO COMPUTE AND OUTPUT AJJL.
!
         Iti = 3
         Ito = 3
!
         CALL amgb1a(Input,Matout,Work(ip1),Work(ip2),Work(ip3),Work(ip4),Work(ip5))
         IF ( Debug ) CALL bug1('AJJL      ',48,Work(ip1),ip2-1)
!
!     COMPUTE F(INVERSE) AND W(FACTOR) FOR EACH STREAMLINE
!
!     COMPUTE POINTERS AND SEE IF THERE IS ENOUGH CORE
!
         nsns = Nstns*Nstns
         ip1 = 1
         ip2 = ip1 + nsns
         next = ip2 + 3*Nstns
         IF ( next>ecore ) THEN
            CALL mesage(-8,0,name)
         ELSE
!
!     REPOSITION ACPT TO BEGINNING OF COMPRESSOR BLADE DATA
!
            CALL bckrec(Input)
            CALL fread(Input,0,-6,0)
!
            Iti = 1
            Ito = 3
!
            Ii = Isk
            Nsk = Nsk + Nstns
            Nn = Nsk
            DO nline = 1 , Nlines
               CALL amgb1s(Input,Work(ip1),Work(ip2),Work(ip2),radii,wfact,nline)
!
!     OUTPUT SKJ (= WFACT*F(INVERS)TRANSPOSE) FOR THIS STREAMLINE
!
               ip3 = ip2 + Nstns - 1
               DO i = 1 , Nstns
                  k = i
                  DO j = ip2 , ip3
                     Work(j) = Work(k)*wfact
                     k = k + Nstns
                  ENDDO
                  CALL pack(Work(ip2),Skj,Tskj)
                  IF ( Debug ) CALL bug1('SKJ       ',55,Work(ip2),Nstns)
               ENDDO
               Ii = Ii + Nstns
               IF ( nline/=Nlines ) Nn = Nn + Nstns
            ENDDO
!
!     UPDATE NROW AND PACK POINTERS
!
            Nrow = Nrow + Nlines*Nstns
            IF ( Debug ) CALL bug1('NEW-NROW  ',110,Nrow,1)
            Isk = Ii
            Nsk = Nn
            RETURN
         ENDIF
      ENDIF
   ENDIF
!
!     INPUT NOT POSITIONED PROPERLY OR INCORRECTLY WRITTEN
!
 400  CALL mesage(-7,0,name)
   RETURN
END SUBROUTINE amgb1