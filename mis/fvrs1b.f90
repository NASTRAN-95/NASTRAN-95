
SUBROUTINE fvrs1b(Base,W1,Nf)
   IMPLICIT NONE
   REAL Degra , Dum(5) , Dum1(3) , Pi , Radeg , S4piq , Twopi
   INTEGER It(6)
   COMMON /blank / Dum , It , Dum1
   COMMON /condas/ Pi , Twopi , Radeg , Degra , S4piq
   INTEGER Nf
   COMPLEX Base(3,Nf)
   REAL W1(Nf)
   REAL f , phi , rad , xo
   INTEGER i , k , lp , lt
   COMPLEX p , z1
!
!     SUBROUTINE TO COMPUTE BASE(FI)(3X1) FOR MODFRL=FALSE
!
!
!
!
   DO k = 1 , Nf
      f = W1(k)/Twopi
      lt = 1
      lp = 2
      DO i = 1 , 3
         IF ( It(lt)==-1 ) THEN
            Base(i,k) = (0.0,0.0)
         ELSE
            CALL tab(It(lt),f,xo)
            IF ( It(lp)==-1 ) THEN
               p = (1.0,0.0)
            ELSE
               CALL tab(It(lp),f,phi)
               rad = phi*Degra
               z1 = cmplx(0.0,rad)
               p = cexp(z1)
            ENDIF
            Base(i,k) = xo*p
         ENDIF
         lt = lt + 2
         lp = lp + 2
      ENDDO
   ENDDO
END SUBROUTINE fvrs1b
