!*==pstamg.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE pstamg(Input,Ajjl,Skj)
   IMPLICIT NONE
   USE C_AMGMN
   USE C_PACKX
   USE C_PSTONC
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Input
   INTEGER :: Ajjl
   INTEGER :: Skj
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ib , ica , icore , idel , iend , ipalp , ipt , n , nt , nw
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , SAVE :: nhacpt , nhcomm
   REAL :: rm
   EXTERNAL bug , fread , korsz , mesage , pack , psta , read
!
! End of declarations rewritten by SPAG
!
!
!     DRIVER FOR PISTON THEORY
!
   !>>>>EQUIVALENCE (Z(1),Iz(1))
   DATA nhacpt , nhcomm , name/4HACPT , 4HCOMM , 4HPSTA , 4HMG  /
!
   icore = korsz(iz) - 4*Sysbuf
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
      RETURN
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
END SUBROUTINE pstamg
