
SUBROUTINE betrns(Tbe,Gg,Kflag,Elid)
   IMPLICIT NONE
   INTEGER Elid , Kflag
   DOUBLE PRECISION Gd(9) , Tbd(9)
   REAL Gg(9) , Tbe(9)
   DOUBLE PRECISION d12(3) , dsstr(3) , dstr(3) , dtemp , dv(3) , led
   INTEGER i
   REAL len , r12(3) , rsstr(3) , rstr(3) , rv(3) , temp
!     &    ENTRY BETRND (TBD,GD,KFLAG,ELID)
!
!*****
!     SUBROUTINE WHICH CALCULATES THE TBE TRANSFORMATION
!     MATRIX WHICH RELATES THE ELEMENT TO THE BASIC C.S.
!
!     GG(9) OR GD(9) IS A 9X1 ARRAY WHICH STORES THE GRID PT. COORD.
!     X(G1),Y(G1),Z(G1),X(G2),Y(G2),Z(G2),X(G3),Y(G3),Z(G3)
!     GG(1),GG(2),GG(3),GG(4),GG(5),GG(6),GG(7),GG(8),GG(9), OR
!     GD(1),GD(2),GD(3),GD(4),GD(5),GD(6),GD(7),GD(8),GD(9)
!
!     KFLAG = 0, TBE (OR TBD) IS OUTPUT WITHOUT TRANSPOSING
!           = 1, TBE (OR TBD) IS OUTPUT AFTER IT IS TRANSPOSED
!*****
!
!
!     SINGLE PRECISION VERSION
!
!*****
!     CALCULATE APPROPRIATE LENGTH QUANTITIES
!*****
   len = sqrt((Gg(4)-Gg(1))**2+(Gg(5)-Gg(2))**2+(Gg(6)-Gg(3))**2)
   IF ( len==0.0 ) THEN
!*****
!     ZERO LENGTH ERROR, BAD GEOMETRY
!*****
      CALL mesage(-30,31,Elid)
      GOTO 99999
   ELSE
!*****
!     CALCULATE APPROPRIATE VECTOR QUANTITIES
!*****
      r12(1) = (Gg(4)-Gg(1))/len
      r12(2) = (Gg(5)-Gg(2))/len
      r12(3) = (Gg(6)-Gg(3))/len
      rv(1) = (Gg(7)-Gg(1))
      rv(2) = (Gg(8)-Gg(2))
      rv(3) = (Gg(9)-Gg(3))
!*****
!     CALCULATE ENTRIES INTO THE TRANSFORMATION MATRIX
!*****
      rstr(1) = (r12(2)*rv(3)-r12(3)*rv(2))
      rstr(2) = (r12(3)*rv(1)-r12(1)*rv(3))
      rstr(3) = (r12(1)*rv(2)-r12(2)*rv(1))
!
      len = sqrt(rstr(1)**2+rstr(2)**2+rstr(3)**2)
      IF ( len==0.0 ) THEN
         CALL mesage(-30,31,Elid)
         GOTO 99999
      ELSE
         DO i = 1 , 3
            rstr(i) = rstr(i)/len
         ENDDO
!
         rsstr(1) = (rstr(2)*r12(3)-rstr(3)*r12(2))
         rsstr(2) = (rstr(3)*r12(1)-rstr(1)*r12(3))
         rsstr(3) = (rstr(1)*r12(2)-rstr(2)*r12(1))
         Tbe(1) = r12(1)
         Tbe(2) = r12(2)
         Tbe(3) = r12(3)
         Tbe(4) = rsstr(1)
         Tbe(5) = rsstr(2)
         Tbe(6) = rsstr(3)
         Tbe(7) = rstr(1)
         Tbe(8) = rstr(2)
         Tbe(9) = rstr(3)
         IF ( Kflag/=0 ) THEN
!*****
!     TRANSPOSE TBE(9) SINCE KFLAG.NE.ZERO
!*****
            temp = Tbe(2)
            Tbe(2) = Tbe(4)
            Tbe(4) = temp
            temp = Tbe(3)
            Tbe(3) = Tbe(7)
            Tbe(7) = temp
            temp = Tbe(6)
            Tbe(6) = Tbe(8)
            Tbe(8) = temp
         ENDIF
         GOTO 100
      ENDIF
   ENDIF
!
   ENTRY betrnd(Tbd,Gd,Kflag,Elid)
!     ================================
!
!     DOUBLE PRECISION VERSION
!
!*****
!     CALCULATE APPROPRIATE LENGTH QUANTITIES
!*****
   led = dsqrt((Gd(4)-Gd(1))**2+(Gd(5)-Gd(2))**2+(Gd(6)-Gd(3))**2)
   IF ( led==0.0D+0 ) THEN
      CALL mesage(-30,31,Elid)
      GOTO 99999
   ELSE
      d12(1) = (Gd(4)-Gd(1))/led
      d12(2) = (Gd(5)-Gd(2))/led
      d12(3) = (Gd(6)-Gd(3))/led
      dv(1) = (Gd(7)-Gd(1))
      dv(2) = (Gd(8)-Gd(2))
      dv(3) = (Gd(9)-Gd(3))
!*****
!     CALCULATE ENTRIES INTO THE TRANSFORMATION MATRIX
!*****
      dstr(1) = (d12(2)*dv(3)-d12(3)*dv(2))
      dstr(2) = (d12(3)*dv(1)-d12(1)*dv(3))
      dstr(3) = (d12(1)*dv(2)-d12(2)*dv(1))
!
      led = dsqrt(dstr(1)**2+dstr(2)**2+dstr(3)**2)
      IF ( led==0.0D+0 ) THEN
         CALL mesage(-30,31,Elid)
         GOTO 99999
      ELSE
         DO i = 1 , 3
            dstr(i) = dstr(i)/led
         ENDDO
!
         dsstr(1) = (dstr(2)*d12(3)-dstr(3)*d12(2))
         dsstr(2) = (dstr(3)*d12(1)-dstr(1)*d12(3))
         dsstr(3) = (dstr(1)*d12(2)-dstr(2)*d12(1))
         Tbd(1) = d12(1)
         Tbd(2) = d12(2)
         Tbd(3) = d12(3)
         Tbd(4) = dsstr(1)
         Tbd(5) = dsstr(2)
         Tbd(6) = dsstr(3)
         Tbd(7) = dstr(1)
         Tbd(8) = dstr(2)
         Tbd(9) = dstr(3)
         IF ( Kflag/=0 ) THEN
!*****
!     TRANSPOSE TBD(9) SINCE KFLAG.NE.ZERO
!*****
            dtemp = Tbd(2)
            Tbd(2) = Tbd(4)
            Tbd(4) = dtemp
            dtemp = Tbd(3)
            Tbd(3) = Tbd(7)
            Tbd(7) = dtemp
            dtemp = Tbd(6)
            Tbd(6) = Tbd(8)
            Tbd(8) = dtemp
         ENDIF
      ENDIF
   ENDIF
 100  RETURN
99999 RETURN
END SUBROUTINE betrns
