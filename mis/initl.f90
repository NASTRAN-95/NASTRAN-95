
SUBROUTINE initl(Offset,Deltt)
   IMPLICIT NONE
   REAL Alpha(4) , Beta(4) , Cdp , Csp , Dum(39) , Eofnrw , Power , Rd , Rdrew , Rew , Wrt , Wrtrew , Z(1)
   DOUBLE PRECISION Det , Mindia
   INTEGER Fileb(7) , Filek(7) , Filem(7) , Ia(7) , Ichl , Id(5) , Id1(2) , Ifa(7) , Ifila(7) , Ifilb(7) , Ifilc(7) , Ifl(7) ,      &
         & Ifu(7) , Il(7) , Iopen , Isc1 , Isc2 , Isc3 , Iscr1 , Iscr10 , Iscr2 , Iscr20 , Iscr3 , Iscr30 , Iscr4 , Iscr5 , Iscr6 , &
         & Isym , Itypal , Itypbt , Iu(7) , Mcbs(67) , Nbpw , Nomat , Norew , Nx , Nxx , Nz , Rdp , Rsp , Sqr
   COMMON /dcompx/ Ia , Il , Iu , Iscr10 , Iscr20 , Iscr30 , Det , Power , Nx , Mindia
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp , Rdp , Csp , Cdp , Sqr
   COMMON /saddx / Nomat , Nz , Mcbs
   COMMON /sfact / Ifa , Ifl , Ifu , Isc1 , Isc2 , Nxx , Id , Isc3 , Id1 , Ichl
   COMMON /system/ Dum , Nbpw
   COMMON /trdxx / Filek , Filem , Fileb , Iscr1 , Iscr2 , Iscr3 , Iscr4 , Iscr5 , Iscr6 , Iopen , Isym
   COMMON /zzzzzz/ Z
   REAL Deltt
   INTEGER Offset
   INTEGER file , i , ip1 , iprec , name(2)
   INTEGER korsz
!
!     INITL WILL COMPUTE THE STARTING VALUES FOR THE INTEGRATION ROUTINE
!
!     THIS ROUTINE IS SUITABLE FOR SINGLE PRECISION OPERATION
!
   EQUIVALENCE (Mcbs(1),Ifila(1)) , (Mcbs(8),Itypal) , (Mcbs(9),Alpha(1)) , (Mcbs(13),Ifilb(1)) , (Mcbs(20),Itypbt) ,               &
    & (Mcbs(21),Beta(1)) , (Mcbs(61),Ifilc(1))
   DATA name/4HINIT , 4HL   /
!
   Nomat = 2
   iprec = Rdp
   IF ( Nbpw>=60 ) iprec = Rsp
   Alpha(2) = 0.
   Alpha(3) = 0.
   Alpha(4) = 0.
   Beta(2) = 0.
   Beta(3) = 0.
   Beta(4) = 0.
   Nx = korsz(Z) - Offset
   Nz = Nx
!
!     FORM AND DECOMPOSE THE LEFT HAND MATRIX
!
   Itypal = Rsp
   Itypbt = Rsp
   Alpha(1) = 1./(Deltt**2)
   Beta(1) = .5/Deltt
   Ifilc(4) = 6
   DO i = 1 , 7
      Ifila(i) = Filem(i)
      Ifilb(i) = Fileb(i)
   ENDDO
   Ifilc(2) = Filek(2)
   Ifilc(1) = Iscr2
   IF ( Filek(1)<=0 ) Ifilc(1) = Iscr1
   Ifilc(3) = Filek(2)
   IF ( Ifila(1)/=0 .AND. Ifila(4)/=6 ) Ifilc(4) = Sqr
   IF ( Ifilb(1)/=0 .AND. Ifilb(4)/=6 ) Ifilc(4) = Sqr
   Ifilc(5) = iprec
   IF ( Filem(1)<=0 .AND. Fileb(1)<=0 ) GOTO 500
   CALL sadd(Z,Z)
   IF ( Filek(1)<=0 ) GOTO 200
 100  DO i = 1 , 7
      Ifila(i) = Ifilc(i)
      Ifilb(i) = Filek(i)
   ENDDO
   IF ( Ifilb(4)/=6 ) Ifilc(4) = Sqr
   Ifilc(1) = Iscr1
   Alpha(1) = 1.
   Beta(1) = 1./3.
   CALL sadd(Z,Z)
 200  CALL wrttrl(Ifilc)
   IF ( Ifilc(4)/=6 ) THEN
!
!     SET UP FOR UNSYMMETRIC DECOMPOSITION
!
      Isym = 1
      DO i = 1 , 7
         Ia(i) = Ifilc(i)
      ENDDO
      Il(1) = Iscr2
      Iu(1) = Iscr3
      Iscr10 = Iscr4
      Iscr20 = Iscr5
      Iscr30 = Iscr6
      Il(5) = iprec
      file = Ia(1)
      CALL decomp(*300,Z(1),Z(1),Z(1))
      CALL wrttrl(Il)
      CALL wrttrl(Iu)
   ELSE
!
!     SET UP FOR SYMMETRIC DECOMPOSITION
!
      DO i = 1 , 7
         Ifa(i) = Ifilc(i)
      ENDDO
      Ifl(1) = Iscr2
      Ifu(1) = Iscr3
      Isc1 = Iscr4
      Isc2 = Iscr5
      Isc3 = Iscr6
      Ifl(5) = iprec
      Ichl = 0
      Nxx = Nx
      file = Ifa(1)
      CALL sdcomp(*300,Z,Z,Z)
      CALL wrttrl(Ifl)
      Isym = 0
   ENDIF
!
!     FORM FIRST RIGHT HAND MATRIX
!
   DO i = 1 , 7
      Ifila(i) = Filem(i)
   ENDDO
   Alpha(1) = 2./(Deltt**2)
   Beta(1) = -1.0/3.0
   Ifilc(1) = Iscr1
   CALL sadd(Z,Z)
!
!     FORM SECOND RIGHT HAND MATRIX
!
   Alpha(1) = -1.0/Deltt**2
   Ifilc(1) = Iscr5
   CALL sadd(Z,Z)
   DO i = 1 , 7
      Ifila(i) = Ifilc(i)
      Ifilb(i) = Fileb(i)
   ENDDO
   Alpha(1) = 1.
   Beta(1) = .5/Deltt
   Ifilc(1) = Iscr4
   CALL sadd(Z,Z)
   RETURN
!
!     ERRORS
!
 300  ip1 = -5
 400  CALL mesage(ip1,file,name(1))
!
!     NO BDD OR MDD
!
 500  IF ( Filek(1)<=0 ) THEN
!
!     ILLEGAL INPUT.   NO MATRICES
!
      ip1 = -7
      GOTO 400
   ELSE
      Ifilc(1) = 0
      GOTO 100
   ENDIF
END SUBROUTINE initl
