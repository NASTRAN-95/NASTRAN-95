
SUBROUTINE cinvp1
   IMPLICIT NONE
   DOUBLE PRECISION Alpha(2) , Beta(2) , Lambda(2)
   INTEGER Cdp , Csp , Fileb(7) , Filek(7) , Filem(7) , Ifila(7) , Ifilb(7) , Ifilc(7) , Itypal , Itypbt , Mcbs(67) , Nomat , Nz ,  &
         & Scr1 , Scr11 , Scr2 , Sqr , Switch , Sysbuf
   REAL Dum(15) , Dumm(9) , Scr(8) , Z(1)
   COMMON /cinvpx/ Filek , Filem , Fileb , Dum , Scr1 , Scr2 , Scr , Scr11
   COMMON /cinvxx/ Lambda , Switch
   COMMON /names / Dumm , Csp , Cdp , Sqr
   COMMON /saddx / Nomat , Nz , Mcbs
   COMMON /system/ Sysbuf
   COMMON /zzzzzz/ Z
   INTEGER i
   INTEGER korsz
!*******
!     CINVP1 INITIALIZES AND CALLS SUBROUTINE ADD FOR CINVPR
!*******
!
!
   EQUIVALENCE (Mcbs(1),Ifila(1)) , (Mcbs(8),Itypal) , (Mcbs(61),Ifilc(1)) , (Mcbs(13),Ifilb(1)) , (Mcbs(20),Itypbt) ,              &
    & (Mcbs(21),Beta(1)) , (Mcbs(9),Alpha(1))
!*******
!     FORM -(B+LAMBDA*M) ON SCR2
!*******
   Nomat = 2
   DO i = 1 , 7
      Ifila(i) = Filem(i)
      Ifilb(i) = Fileb(i)
   ENDDO
   Alpha(1) = -Lambda(1)
   Alpha(2) = -Lambda(2)
   Beta(1) = -1.D0
   Beta(2) = 0.D0
   Itypal = Cdp
   Itypbt = Cdp
   Nz = korsz(Z)
   IF ( Switch==-204 ) Nz = Nz - 2*Sysbuf
   Ifilc(1) = Scr2
   IF ( Switch/=0 ) Ifilc(1) = Scr11
   Ifilc(2) = Filek(2)
   Ifilc(3) = Filek(3)
   Ifilc(4) = 1
   Ifilc(5) = Cdp
   CALL sadd(Z,Z)
!*******
!     FORM (LAMBDA**2*M+LAMBDA*B+K) ON SCR1
!*******
   DO i = 1 , 7
      Ifila(i) = Filek(i)
   ENDDO
   Ifilb(1) = Ifilc(1)
   Ifilb(2) = Filek(2)
   Ifilb(3) = Filek(3)
   Ifilb(4) = Sqr
   Alpha(2) = 0.D0
   Beta(1) = -Lambda(1)
   Beta(2) = -Lambda(2)
   Ifilb(5) = Cdp
   Alpha(1) = 1.D0
   Ifilc(1) = Scr1
   CALL sadd(Z,Z)
END SUBROUTINE cinvp1
