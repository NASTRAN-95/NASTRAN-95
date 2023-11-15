
SUBROUTINE amgt1(Input,Matout,Skj)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Amach , Amachr , Blspc , Bspace , Chord , Dcbdzb , Degra , Den , Dum(2) , Mach , Maxmac , Minmac , Pi , Radeg , Redf ,      &
      & Refc , Refcrd , Refden , Refmac , Refstg , Refswp , Refvel , Rfreq , S4pisq , Sigma , Stager , Sweep , Twopi , Vel , Work(1)&
      & , Xsign
   LOGICAL Debug , Tsonic
   INTEGER Ii , Incr , Iout , Iref , Isk , Iti , Ito , Iz(1) , Mcb(7) , Nj , Nk , Nlines , Nn , Nrow , Nsk , Nstns , Nstnsx , Sln , &
         & Sysbuf , Tskj(7)
   CHARACTER*23 Ufm
   COMMON /amgbug/ Debug
   COMMON /amgmn / Mcb , Nrow , Dum , Refc , Sigma , Rfreq , Tskj , Isk , Nsk
   COMMON /blank / Nk , Nj
   COMMON /condas/ Pi , Twopi , Radeg , Degra , S4pisq
   COMMON /packx / Iti , Ito , Ii , Nn , Incr
   COMMON /system/ Sysbuf , Iout
   COMMON /tamg1l/ Iref , Minmac , Maxmac , Nlines , Nstns , Refstg , Refcrd , Refmac , Refden , Refvel , Refswp , Sln , Nstnsx ,   &
                 & Stager , Chord , Dcbdzb , Bspace , Mach , Den , Vel , Sweep , Amach , Redf , Blspc , Amachr , Tsonic , Xsign
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Work
!
! Dummy argument declarations
!
   INTEGER Input , Matout , Skj
!
! Local variable declarations
!
   INTEGER ecore , i , ibad , ip1 , ip2 , ip3 , ip4 , ip5 , iptr , irsln , j , k , n , najjc , name(2) , ndata , next , nline ,     &
         & nsns , nstns2 , ntsonx , nwar
   INTEGER korsz
!
! End of declarations
!
!
!     DRIVER FOR SWEPT TURBOPROP BLADES (AEROELASTIC THEORY 7).
!
!     COMPUTATIONS ARE FOR THE AJJL AND SKJ MATRICES.
!     FOR SWEPT TURBOPROPS K-SET = J-SET = 2*NSTNS*NLINES.
!     SKJ = F(INVERS)TRANSPOSE.
!
   EQUIVALENCE (Work(1),Iz(1))
   DATA name/4HAMGT , 4H1   /
!
!     READ PARAMETERS IREF,MINMAC,MAXMAC,NLINES AND NSTNS
!
   CALL read(*200,*200,Input,Iref,5,0,n)
   IF ( Debug ) CALL bug1('ACPT-REF  ',5,Iref,5)
!
!     READ REST OF ACPT RECORD INTO OPEN CORE AND LOCATE REFERENCE
!     PARAMETERS REFSTG,REFCRD,REFMAC,REFDEN,REFVEL AND REFSWP
!
   ecore = korsz(Iz) - 4*Sysbuf
   CALL read(*100,*100,Input,Iz,ecore,1,nwar)
!
!     NOT ENOUGH CORE
!
   CALL mesage(-8,0,name)
   GOTO 200
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
!     STORE MACH NUMBERS FOR LATER DATA CHECK.
!
      Mach = Work(i+6)
      IF ( Mach>Maxmac .AND. Mach<Minmac ) ntsonx = ntsonx + 1
      nline = nline + 1
      Work(nwar+nline) = Mach
   ENDDO
!
!     DETERMINE DIRECTION OF BLADE ROTATION VIA Y-COORDINATES AT TIP
!     STREAMLINE. USE COORDINATES OF FIRST 2 NODES ON STREAMLINE.
!
   iptr = ndata*(Nlines-1)
   Xsign = 1.0
   IF ( Work(iptr+15)<Work(iptr+12) ) Xsign = -1.0
!
!     INPUT CHECKS -
!
!     (1) MACH NUMBERS MUST INCREASE FROM BLADE ROOT TO BLADE TIP.
!         NOTE - THIS CHECK WILL NOT BE MADE FOR SWEPT TURBOPROPS.
!     (2) SUPERSONIC CASCADE CODE HAS BEEN INSTALLED IN SUB.AMGT1C
!     (3) LINEAR INTERPOLATION EXISTS FOR TRANSONIC STREAMLINES
!     (4) ALL TRANSONIC STREAMLINES ARE NEVER ALLOWED.
!
!
!     CHECK FOR ALL TRANSONIC STREAMLINES.
!
   ibad = 0
   IF ( ntsonx>=Nlines ) THEN
      ibad = 1
      WRITE (Iout,99001) Ufm
!
99001 FORMAT (A23,' -AMG MODULE- ALL TRANSONIC STREAMLINES NOT ALLOWED',/39X,'CHECK MACH ON STREAML2 BULK DATA CARDS OR',/39X,      &
             &'CHANGE PARAMETERS MINMACH AND MAXMACH.')
   ENDIF
!
!     MACH NUMBERS MUST INCREASE FROM BLADE ROOT TO BLADE TIP.
!
!     NOTE - THIS CHECK WILL NOT BE MADE FOR SWEPT TURBOPROPS.
!
   IF ( ibad/=0 ) THEN
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
      Refswp = Work(irsln+9)
!
!     REPOSITION ACPT TO BEGINNING OF BLADE DATA.
!
      CALL bckrec(Input)
      CALL fread(Input,0,-6,0)
      IF ( Debug ) CALL bug1('TAMG1L    ',46,Iref,26)
!
!     COMPUTE POINTERS AND SEE IF THERE IS ENOUGH CORE.
!     IP1 AND IP2 ARE COMPLEX POINTERS.
!
      nstns2 = 2*Nstns
      najjc = nstns2
      ntsonx = 1
      IF ( Tsonic ) najjc = Nlines*nstns2
      IF ( Tsonic ) ntsonx = Nlines
      ip1 = 1
      ip2 = ip1 + 2*(nstns2*najjc)
      ip3 = ip2 + 1
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
         CALL amgt1a(Input,Matout,Work(ip1),Work(ip3),Work(ip4),Work(ip5),nstns2)
         IF ( Debug ) CALL bug1('AJJL      ',48,Work(ip1),ip2-1)
!
!     COMPUTE F(INVERSE) FOR EACH STREAMLINE
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
!     REPOSITION ACPT TO BEGINNING OF BLADE DATA.
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
               CALL amgt1s(Input,Work(ip1),Work(ip2),Work(ip2))
!
!     OUTPUT SKJ (= F(INVERS)TRANSPOSE) FOR THIS STREAMLINE
!
               ip3 = ip2 + Nstns - 1
               DO i = 1 , Nstns
                  k = i
                  DO j = ip2 , ip3
                     Work(j) = Work(k)
                     k = k + Nstns
                  ENDDO
                  CALL pack(Work(ip2),Skj,Tskj)
                  IF ( Debug ) CALL bug1('SKJ       ',55,Work(ip2),Nstns)
               ENDDO
               Ii = Ii + Nstns
               Nn = Nn + Nstns
               DO i = 1 , Nstns
                  k = i
                  DO j = ip2 , ip3
                     Work(j) = Work(k)
                     k = k + Nstns
                  ENDDO
                  CALL pack(Work(ip2),Skj,Tskj)
                  IF ( Debug ) CALL bug1('SKJ       ',75,Work(ip2),Nstns)
               ENDDO
               Ii = Ii + Nstns
               IF ( nline/=Nlines ) Nn = Nn + Nstns
            ENDDO
!
!     UPDATE NROW AND PACK POINTERS
!
            Nrow = Nrow + Nlines*nstns2
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
 200  CALL mesage(-7,0,name)
   RETURN
END SUBROUTINE amgt1
