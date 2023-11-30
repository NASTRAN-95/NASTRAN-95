
SUBROUTINE dlpt2(Input,W1jk,W2jk)
   IMPLICIT NONE
   REAL Fmach , Refc , Rfk , Work(1)
   INTEGER Ii , Incr , Iti , Ito , Iz(1) , Mcb(7) , Nd , Ne , Nn , Nrow , Sysbuf , Tw1jk(7) , Tw2jk(7)
   COMMON /amgmn / Mcb , Nrow , Nd , Ne , Refc , Fmach , Rfk
   COMMON /amgp2 / Tw1jk , Tw2jk
   COMMON /packx / Iti , Ito , Ii , Nn , Incr
   COMMON /system/ Sysbuf
   COMMON /zzzzzz/ Work
   INTEGER Input , W1jk , W2jk
   REAL a(2)
   INTEGER ecore , i , icg , idelx , iee , inb , inc , isg , ixic , ixlam , iys , izs , n , name(2) , np(4) , nread , nstrip , ntp
   INTEGER korsz
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
   ecore = korsz(Iz)
   ecore = ecore - 4*Sysbuf
   Nn = Ii + 1
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
      CALL read(*100,*100,Input,Work,nread,1,n)
!
!     COMPUTE TERMS AND PACK
!
      DO i = 1 , ntp
         a(1) = 0.0
         a(2) = 1.0
         CALL pack(a,W1jk,Tw1jk)
         a(1) = -(2.0/Refc)
         a(2) = Work(idelx+i)/(2.0*Refc)
         CALL pack(a,W2jk,Tw2jk)
!
!     BUMP PACK INDEXES
!
         Ii = Ii + 2
         IF ( i/=ntp ) Nn = Nn + 2
      ENDDO
      RETURN
   ENDIF
!     FILE NOT POSITIONED PROPERLY
 100  CALL mesage(-7,0,name)
END SUBROUTINE dlpt2