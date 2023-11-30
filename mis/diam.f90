
SUBROUTINE diam(Nc,Maxdeg,Nl,Nodesl,Idem,Maxlev,Ig,Ic,Ideg,Idis,Iw,Icc,Jg)
   IMPLICIT NONE
   INTEGER Nn
   COMMON /bands / Nn
   INTEGER Idem , Maxdeg , Maxlev , Nc , Nl
   INTEGER Ic(1) , Icc(1) , Ideg(1) , Idis(1) , Ig(1) , Iw(1) , Jg(1) , Nodesl(1)
   INTEGER i , md , ml
   INTEGER idist
!
!     DETERMINE NL STARTING POINTS AND STORE IN NODESL.
!     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
!
!
   Nl = 0
   Maxlev = 600000
   DO i = 1 , Nn
      IF ( Nc==Ic(i) ) THEN
         IF ( Maxdeg>=Ideg(i) ) THEN
            md = idist(i,ml,Maxlev,Ig,Ic,Ideg,Idis,Iw,Icc,Jg)
            IF ( md<=0 ) GOTO 100
            IF ( ml<Maxlev ) THEN
               Maxlev = ml
               Nl = 1
               Nodesl(1) = i
            ELSEIF ( ml==Maxlev ) THEN
               IF ( Nl<Idem ) THEN
                  Nl = Nl + 1
                  Nodesl(Nl) = i
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDDO
   RETURN
!
 100  ml = 1
   Nodesl(1) = i
   Maxlev = 0
END SUBROUTINE diam
