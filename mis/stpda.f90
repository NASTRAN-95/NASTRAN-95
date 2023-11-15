
SUBROUTINE stpda(Input,Ajjl,Skj)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Bb(4) , Beta(4) , Bref , Clam , Dum , Ekr(1) , Fm , Fmach , Pi , Refc , Rfk , Tskj(7) , Twopi , Z(1)
   COMPLEX Ekm(4,4)
   INTEGER Ii , Incr , Isk , It0 , Iti , Iz(8) , Mcb(7) , Ncirc , Nd , Ne , Nj , Nk , Nn , Nncirc , Nout , Nrow , Ns , Nsk , Sysbuf
   CHARACTER*23 Ufm
   COMMON /amgmn / Mcb , Nrow , Nd , Ne , Refc , Fmach , Rfk , Tskj , Isk , Nsk
   COMMON /blank / Nk , Nj
   COMMON /condas/ Pi , Twopi
   COMMON /packx / Iti , It0 , Ii , Nn , Incr
   COMMON /stripc/ Ns , Bref , Clam , Fm , Ncirc , Nncirc , Ekr , Dum , Bb , Beta , Ekm
   COMMON /system/ Sysbuf , Nout
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER Ajjl , Input , Skj
!
! Local variable declarations
!
   INTEGER claf , i , i8 , ibloc , ibm , ica , icla , icore , id , idy , igap , igm , insize , ioc , ipm , lcirc , lclaf , n ,      &
         & name(2) , nmach , nnj , nw
   INTEGER korsz
   REAL rm
!
! End of declarations
!
!
!     DRIVER FOR STRIP THEORY
!
   EQUIVALENCE (Iz(1),Z(1))
   DATA name/4HSTPD , 4HA   /
!
   icore = korsz(Iz) - 4*Sysbuf
!
!     BRING IN DATA AND ALLOCATE CORE
!
   CALL fread(Input,Z,8,0)
   nnj = Iz(1)
   claf = Iz(2)
   lclaf = Iz(3)
   Ncirc = Iz(4)
   lcirc = Iz(5)
   Nncirc = Ncirc + 1
   nmach = Iz(6)
   Ns = Iz(7)
   i8 = 8
   Clam = Z(i8)
   Fm = 1.0
   Bref = Refc/2.0
   Ekr(1) = Rfk
   idy = 1
   ibloc = idy + Ns
   id = ibloc + Ns
   ica = id + Ns
   igap = ica + Ns
   insize = igap + Ns
   icla = insize + Ns
   ibm = icla + Ns
   igm = ibm + 16*Ns
   ipm = igm + 12*Ns
   ioc = ipm + 37*Ns
   IF ( ioc>icore ) CALL mesage(-8,0,name)
!
!     READ IN ARRAYS WHICH ARE FIXED
!
   nw = 6*Ns
   CALL fread(Input,Z,nw,0)
!
!     SET CLA ARRAY OR BB AND BETA
!
   IF ( claf==0 ) THEN
      DO i = 1 , Ns
         Z(icla+i-1) = Twopi
      ENDDO
      IF ( Ncirc==0 ) THEN
         CALL fread(Input,Z,0,1)
         GOTO 200
      ELSE
         DO i = 1 , nmach
            CALL fread(Input,rm,1,0)
            IF ( rm==Fmach ) GOTO 50
            CALL fread(Input,Z,-(2*Ncirc+1),0)
         ENDDO
!
!     ERROR MESSAGES
!
         n = lcirc
         GOTO 300
      ENDIF
 50   CALL fread(Input,Bb(1),1,0)
      DO i = 2 , Nncirc
         CALL fread(Input,Bb(i),1,0)
         CALL fread(Input,Beta(i),1,0)
      ENDDO
      CALL fread(Input,Z,0,1)
   ELSE
      IF ( claf<0 ) THEN
         CALL fread(Input,rm,1,0)
         CALL fread(Input,Z(icla),Ns,1)
         DO i = 1 , Ns
            Z(icla+i-1) = Z(icla+i-1)*sqrt((1.0-(rm*rm*Clam*Clam))/(1.0-(Fmach*Fmach*Clam*Clam)))
         ENDDO
         GOTO 200
      ELSE
!
!     FIND MACH NUMBER FOR CLA
!
         DO i = 1 , nmach
            CALL fread(Input,rm,1,0)
            IF ( rm==Fmach ) GOTO 100
            CALL fread(Input,Z,-Ns,0)
         ENDDO
         n = lclaf
         GOTO 300
      ENDIF
!
!     MACH NUMBER NOT INPUT ON AEFACT CARD CLCAF
!
 100  CALL fread(Input,Z(icla),Ns,1)
   ENDIF
!
!     OUTPUT SKJ
!
 200  Iti = 1
   It0 = 3
   Ii = Isk
   Nsk = Nsk + 1
   Nn = Nsk
   rm = 1.0
   DO i = 1 , nnj
      CALL pack(rm,Skj,Tskj)
      Ii = Ii + 1
      IF ( i/=nnj ) Nn = Nn + 1
   ENDDO
   Isk = Ii
   Nsk = Nn
   Iti = 3
   It0 = 3
   CALL stpbg(Z(ibm),Z(igm),Ns,Z(ibloc),Z(id),Z(ica),Z(insize))
   CALL stpphi(Z(ica),Z(ibloc),Z(ipm),Ns)
   CALL stpaic(Z(ibloc),Z(idy),Z(insize),Z(igap),Z(ibm),Z(igm),Z(ipm),Ns,Z(icla),Ajjl)
   Nrow = Nrow + nnj
   RETURN
 300  WRITE (Nout,99001) Ufm , Fmach , n
99001 FORMAT (A23,' 2426, MACH NUMBER ',F10.5,' WAS NOT FOUND ON ','AEFACT CARD',I9)
   CALL mesage(-61,0,name)
END SUBROUTINE stpda
