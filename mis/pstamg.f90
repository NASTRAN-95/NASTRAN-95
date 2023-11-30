
SUBROUTINE pstamg(Input,Ajjl,Skj)
   IMPLICIT NONE
   REAL Fmach , Refc , Rfk , Seclam , Tskj(7) , Z(1)
   INTEGER Ii , Incr , Iout , Isk , Iti , Ito , Iz(1) , Mcb(7) , Nalpha , Nd , Ne , Nmach , Nn , Nnj , Nrow , Nsk , Nstrip , Ntaus ,&
         & Nthick , Nthry , Nxis , Sysbuf
   CHARACTER*23 Ufm
   COMMON /amgmn / Mcb , Nrow , Nd , Ne , Refc , Fmach , Rfk , Tskj , Isk , Nsk
   COMMON /packx / Iti , Ito , Ii , Nn , Incr
   COMMON /pstonc/ Nnj , Nmach , Nthry , Nthick , Nalpha , Nxis , Ntaus , Nstrip , Seclam
   COMMON /system/ Sysbuf , Iout
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
   INTEGER Ajjl , Input , Skj
   INTEGER i , ib , ica , icore , idel , iend , ipalp , ipt , n , name(2) , nhacpt , nhcomm , nt , nw
   INTEGER korsz
   REAL rm
!
!     DRIVER FOR PISTON THEORY
!
   EQUIVALENCE (Z(1),Iz(1))
   DATA nhacpt , nhcomm , name/4HACPT , 4HCOMM , 4HPSTA , 4HMG  /
!
   icore = korsz(Iz) - 4*Sysbuf
!
!     BRING IN DATA AND ALLOCATE CORE
!
   CALL fread(Input,Nnj,9,0)
   idel = 1
   ib = idel + Nstrip
   ica = ib + Nstrip
   ipalp = ica + Nstrip
!
!     READ FIXED ARRAYS
!
   nw = 3*Nstrip
   CALL fread(Input,Z,nw,0)
!
!     READ ALPHA ARRAY AND STUFF  AT END (INTEGRALS OR TAUS)
!
   iend = 0
   DO i = 1 , Nmach
      CALL fread(Input,rm,1,0)
      IF ( rm/=Fmach ) THEN
         CALL fread(Input,Z,-Nalpha,0)
      ELSE
         iend = 1
         CALL fread(Input,Z(ipalp),Nalpha,0)
      ENDIF
   ENDDO
   IF ( iend==0 ) THEN
!
!     ERROR MESSAGE
!
      WRITE (Iout,99001) Ufm , Fmach
99001 FORMAT (A23,' 2428, MACH NUMBER ',F10.5,' WAS NOT FOUND IN ','PISTON THEORY ALPHA ARRAY.')
      CALL mesage(-61,0,name)
      GOTO 99999
   ELSE
      ipt = ipalp + Nalpha
      CALL read(*100,*100,Input,Z(ipt),icore,1,n)
   ENDIF
 100  nt = ipt + n
   CALL bug(nhacpt,30,Z,nt)
   CALL bug(nhcomm,30,Nnj,9)
!
!     OUTPUT SKJ
!
   rm = 1.0
   Iti = 1
   Ito = 3
   Ii = Isk
   Nsk = Nsk + 1
   Nn = Nsk
   DO i = 1 , Nnj
      CALL pack(rm,Skj,Tskj)
      Ii = Ii + 1
      IF ( i/=Nnj ) Nn = Nn + 1
   ENDDO
   Isk = Ii
   Nsk = Nn
   Iti = 3
   Ito = 3
   CALL psta(Z(idel),Z(ib),Z(ica),Z(ipalp),Z(ipt),Ajjl)
   Nrow = Nrow + Nnj
!
99999 RETURN
END SUBROUTINE pstamg
