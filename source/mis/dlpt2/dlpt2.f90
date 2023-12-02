!*==dlpt2.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dlpt2(Input,W1jk,W2jk)
   USE c_amgmn
   USE c_amgp2
   USE c_packx
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Input
   INTEGER :: W1jk
   INTEGER :: W2jk
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(2) :: a
   INTEGER :: ecore , i , icg , idelx , iee , inb , inc , isg , ixic , ixlam , iys , izs , n , nread , nstrip , ntp
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , DIMENSION(4) :: np
   EXTERNAL korsz , mesage , pack , read
!
! End of declarations rewritten by SPAG
!
!
   !>>>>EQUIVALENCE (np(2),nstrip) , (np(3),ntp)
   !>>>>EQUIVALENCE (Work(1),Iz(1))
   DATA name/4HDLPT , 4H2   /
!
!     READ IN NP,NSIZE,NTP,F
!
   CALL read(*100,*100,Input,np,4,0,n)
!
!     COMPUTE POINTERS AND SEE IF THERE IS ENOUGH CORE
!
   ecore = korsz(iz)
   ecore = ecore - 4*sysbuf
   nn = ii + 1
   inc = 0
   inb = inc + np(1)
   iys = inb + np(1)
   izs = iys + nstrip
   iee = izs + nstrip
   isg = iee + nstrip
   icg = isg + nstrip
   ixic = icg + nstrip
   idelx = ixic + ntp
   ixlam = idelx + ntp
   nread = ixlam + ntp
!
!     FILL IN DATA
!
   IF ( nread>ecore ) THEN
!
!     ERROR MESSAGES
!
!     NOT ENOUGH CORE
      CALL mesage(-8,0,name)
   ELSE
      CALL read(*100,*100,Input,work,nread,1,n)
!
!     COMPUTE TERMS AND PACK
!
      DO i = 1 , ntp
         a(1) = 0.0
         a(2) = 1.0
         CALL pack(a,W1jk,tw1jk)
         a(1) = -(2.0/refc)
         a(2) = work(idelx+i)/(2.0*refc)
         CALL pack(a,W2jk,tw2jk)
!
!     BUMP PACK INDEXES
!
         ii = ii + 2
         IF ( i/=ntp ) nn = nn + 2
      ENDDO
      RETURN
   ENDIF
!     FILE NOT POSITIONED PROPERLY
 100  CALL mesage(-7,0,name)
END SUBROUTINE dlpt2
