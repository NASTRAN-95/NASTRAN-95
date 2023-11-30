
SUBROUTINE ddrmma(Setup)
   IMPLICIT NONE
   REAL A(4) , Lambda , Omega , Rbuf(150) , Rbufa(75) , Rbufb(75)
   INTEGER Buf(150) , Bufa(75) , Bufb(75) , Buff(6) , Bufsav(10) , Complx(1) , Device , Dhsize , Entrys , File , Filnam(2) , Form , &
         & I1 , I2 , Icc , Idout , Idrec(146) , Ieol , Ieor , Ierror , Ilist , Infile , Ipass , Irow , Istlst , Itemp , Itype1 ,    &
         & Itype2 , Jfile , Lsf , Lstlst , Mcb(7) , Ncc , Ncore , Nlambs , Nlist , Npos , Nptsf , Nsols , Nstxtr , Nwds , Nwdsf ,   &
         & Nwords , Outfil , Passes , Phase , Savdat(75) , Savpos(25) , Scrt(7) , Setid , Sets(5,3) , Uvsol
   LOGICAL Col1 , Frstid , Lminor , Sort2 , Trnsnt
   COMMON /clstrs/ Complx
   COMMON /ddrmc1/ Idrec , Buff , Passes , Outfil , Jfile , Mcb , Entrys , Sets , Infile , Lambda , File , Sort2 , Col1 , Frstid ,  &
                 & Ncore , Nsols , Dhsize , Filnam , Rbuf , Idout , Icc , Ncc , Ilist , Nlist , Nwds , Setid , Trnsnt , I1 , I2 ,   &
                 & Phase , Itype1 , Itype2 , Nptsf , Lsf , Nwdsf , Scrt , Ierror , Itemp , Device , Form , Istlst , Lstlst , Uvsol ,&
                 & Nlambs , Nwords , Omega , Ipass
   COMMON /stdata/ Lminor , Nstxtr , Npos , Savdat , Savpos , Bufsav
   COMMON /zntpkx/ A , Irow , Ieol , Ieor
   LOGICAL Setup
   INTEGER elwork(300) , i , icomp , iout , j , k , npt , typout
!*****
!  UNPACKS DATA FROM A TRANSIENT OR FREQUENCY RESPONSE SOLUTION
!  COLUMN AS REQUIRED TO FORM ONE OFP OUTPUT LINE ENTRY.
!
!  BEFORE CALLING FOR ENTRY CONSTRUCTION ONE SETUP CALL IS REQUIRED
!  FOR EACH COLUMN. (SETUP = .TRUE.)
!*****
!
!
!
!
   !>>>>EQUIVALENCE (Buf(1),Rbuf(1),Bufa(1),Rbufa(1)) , (Rbufb(1),Bufb(1),Buf(76))
!*****
!  PERFORM SOLUTION COLUMN SETUP WHEN SETUP = .TRUE.
!*****
   IF ( .NOT.Setup ) THEN
!*****
!  FILL BUFFER WITH REAL AND OR COMPLEX VALUES.
!*****
      k = I1 - 1
      DO i = 1 , k
         Bufb(i) = Bufa(i)
      ENDDO
      DO i = I1 , I2
         IF ( icomp==Irow ) THEN
!
!     NON-ZERO COMPONENT AVAILABLE.
!
            IF ( .NOT.Trnsnt ) THEN
               IF ( Ipass==1 ) THEN
!
!     FREQUENCY RESPONSE FOR DISPLACEMENTS OR SPCFS PASS
!
                  Rbufa(i) = A(1)
                  Rbufb(i) = A(2)
                  IF ( Ieol<=0 ) THEN
                     CALL zntpki
                  ELSE
!
                     Irow = 0
                  ENDIF
               ELSEIF ( Ipass==2 ) THEN
!
!     FREQUENCY RESPONSE VELOCITYS PASS
!
                  Rbufa(i) = -Omega*A(2)
                  Rbufb(i) = Omega*A(1)
                  IF ( Ieol<=0 ) THEN
                     CALL zntpki
                  ELSE
                     Irow = 0
                  ENDIF
               ELSEIF ( Ipass==3 ) THEN
!
!     FREQUENCY RESPONSE ACCELERATIONS PASS
!
                  Rbufa(i) = Omega*A(1)
                  Rbufb(i) = Omega*A(2)
                  IF ( Ieol<=0 ) THEN
                     CALL zntpki
                  ELSE
                     Irow = 0
                  ENDIF
               ELSE
                  GOTO 10
               ENDIF
               GOTO 20
            ENDIF
!
!     TRANSIENT RESPONSE
!
 10         Rbufa(i) = A(1)
            IF ( Ieol<=0 ) THEN
               CALL zntpki
            ELSE
               Irow = 0
            ENDIF
         ELSE
            Rbufa(i) = 0.0
            Rbufb(i) = 0.0
         ENDIF
!
 20      icomp = icomp + 1
!
      ENDDO
!*****
!  IF TRANSIENT (REAL) THEN RETURN. FOR FREQUENCY (COMPLEX) COMBINE DATA
!  FOR OUTPUT AND CONVERT TO MAGNITUDE PHASE IF NECESSARY.
!
!  BUFA CONTAINS THE REAL PART
!  BUFB CONTAINS THE IMAGINARY PART
!*****
      IF ( Trnsnt ) GOTO 200
      IF ( Itype1==4 ) GOTO 300
      IF ( Itype1==5 ) GOTO 200
!
!     POINT DATA
!
      DO k = 1 , 6
         IF ( Form==3 ) CALL magpha(Bufa(k+2),Bufb(k+2))
         Bufa(k+8) = Bufb(k+2)
      ENDDO
      Nwdsf = 14
      RETURN
   ELSE
      typout = 3
      IF ( Trnsnt ) typout = 1
      icomp = 1
      CALL intpk(*100,Scrt(6),0,typout,0)
      CALL zntpki
      RETURN
   ENDIF
 100  Irow = 0
   RETURN
!
!     ELEMENT STRESS OR FORCE DATA
!
 200  IF ( .NOT.(Lminor) ) THEN
      DO k = 1 , Nstxtr
         j = Savpos(Npos+k-1)
         Buf(j) = Bufsav(k)
      ENDDO
   ENDIF
 300  IF ( Trnsnt ) RETURN
   iout = 0
   i = Nptsf
 400  npt = Complx(i)
   IF ( npt<0 ) THEN
      npt = -npt
      IF ( Form/=3 ) GOTO 600
!
!     COMPUTE MAGNITUDE PHASE
!
      CALL magpha(Bufa(npt),Bufb(npt))
   ELSEIF ( npt==0 ) THEN
!
!     MOVE OUTPUT DATA
!
      DO i = 1 , iout
         Buf(i) = elwork(i)
      ENDDO
      Nwdsf = iout
      GOTO 99999
   ELSE
      GOTO 600
   ENDIF
 500  iout = iout + 1
   elwork(iout) = Bufa(npt)
   i = i + 1
   GOTO 400
 600  IF ( npt<=Lsf ) GOTO 500
   npt = npt - Lsf
   iout = iout + 1
   elwork(iout) = Bufb(npt)
   i = i + 1
   GOTO 400
99999 RETURN
END SUBROUTINE ddrmma