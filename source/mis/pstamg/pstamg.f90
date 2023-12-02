!*==pstamg.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE pstamg(Input,Ajjl,Skj)
   USE c_amgmn
   USE c_packx
   USE c_pstonc
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
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
   icore = korsz(iz) - 4*sysbuf
!
!     BRING IN DATA AND ALLOCATE CORE
!
   CALL fread(Input,nnj,9,0)
   idel = 1
   ib = idel + nstrip
   ica = ib + nstrip
   ipalp = ica + nstrip
!
!     READ FIXED ARRAYS
!
   nw = 3*nstrip
   CALL fread(Input,z,nw,0)
!
!     READ ALPHA ARRAY AND STUFF  AT END (INTEGRALS OR TAUS)
!
   iend = 0
   DO i = 1 , nmach
      CALL fread(Input,rm,1,0)
      IF ( rm/=fmach ) THEN
         CALL fread(Input,z,-nalpha,0)
      ELSE
         iend = 1
         CALL fread(Input,z(ipalp),nalpha,0)
      ENDIF
   ENDDO
   IF ( iend==0 ) THEN
!
!     ERROR MESSAGE
!
      WRITE (iout,99001) ufm , fmach
99001 FORMAT (A23,' 2428, MACH NUMBER ',F10.5,' WAS NOT FOUND IN ','PISTON THEORY ALPHA ARRAY.')
      CALL mesage(-61,0,name)
      RETURN
   ELSE
      ipt = ipalp + nalpha
      CALL read(*100,*100,Input,z(ipt),icore,1,n)
   ENDIF
 100  nt = ipt + n
   CALL bug(nhacpt,30,z,nt)
   CALL bug(nhcomm,30,nnj,9)
!
!     OUTPUT SKJ
!
   rm = 1.0
   iti = 1
   ito = 3
   ii = isk
   nsk = nsk + 1
   nn = nsk
   DO i = 1 , nnj
      CALL pack(rm,Skj,tskj)
      ii = ii + 1
      IF ( i/=nnj ) nn = nn + 1
   ENDDO
   isk = ii
   nsk = nn
   iti = 3
   ito = 3
   CALL psta(z(idel),z(ib),z(ica),z(ipalp),z(ipt),Ajjl)
   nrow = nrow + nnj
!
END SUBROUTINE pstamg
