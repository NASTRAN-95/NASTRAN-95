
SUBROUTINE hdsolv(Ixr,J,Xxx,Ccc,Ii,Nno,Nit,X21,Y21,Z21,Iia,Nc,Zm,Zmi,Lz)
   IMPLICIT NONE
   INTEGER L0 , L00 , L01 , L1 , L10 , L11 , L12 , L13 , L2 , L3 , L4 , L5 , L6 , L7 , L8 , L9 , Xasolv , Xcc , Yasolv , Zasolv
   REAL Rz(1) , Xdum
   COMMON /go3   / L0 , L1 , L00 , L01 , L2 , L3 , L4 , L5 , L6 , L7 , L8 , L9 , L10 , L11 , L12 , L13
   COMMON /hdptrs/ Xdum , Xcc , Xasolv , Yasolv , Zasolv
   COMMON /zzzzzz/ Rz
   INTEGER Ii , Ixr , J , Lz , Nc , Nit
   REAL Ccc(1) , X21(1) , Xxx(1) , Y21(1) , Z21(1) , Zm(1) , Zmi(1)
   INTEGER Iia(1) , Nno(1)
   REAL a , a1 , a3 , a4 , b , b1 , b3 , b4 , c , c1 , c3 , c4 , d3 , d4 , e , er , ers , exp , exx , f , s , s1 , t , xo , yo , zt
   INTEGER i , ig , is , iv(2) , ix , jb , jj , jt , jv , jx , k , l , m , mt , nk , nr
!
!     THIS SUBROUTINE SOLVES FOR THE LINES OF INTERSECTION RESULTING
!     FROM THE INTERSECTIONS OF THE JTH ELEMENT WITH THE OTHER
!     RELEVANT ELEMENTS.
!
!
!
!
   ers = .015
   er = ers
   exx = .015
   exp = .015
   jt = L12 + (J-1)*5
   jb = L13 + (J-1)*Lz
   IF ( Ii/=0 ) THEN
      a3 = Xxx(1+jt)
      b3 = Xxx(2+jt)
      c3 = Xxx(3+jt)
      d3 = Xxx(4+jt)
      IF ( Xxx(jt+3)/=0. ) THEN
         DO l = 1 , Ii
            k = Nno(L4+l)
!
!     CHECKS TO SEE IF THIS RELEVANT ELEMENT IS TO BE CONSIDERED FOR
!     INTERSECTION
!
            IF ( k>0 ) THEN
               IF ( k>=J ) THEN
                  jx = L12 + (k-1)*5
                  IF ( Zm(L2+J)>=Zmi(L3+k) ) THEN
                     IF ( abs(Xxx(3+jx))>=ers ) THEN
                        mt = 0
                        a4 = Xxx(1+jx)
                        b4 = Xxx(2+jx)
                        c4 = Xxx(3+jx)
                        d4 = Xxx(4+jx)
!
!     DETERMINES THE EQUATION OF LINE OF INTERSECTION.
!
                        b = a3*c4 - a4*c3
                        a = b3*c4 - b4*c3
                        c = d3*c4 - d4*c3
                        IF ( a/=0. .OR. b/=0. ) THEN
                           IF ( a/=0. ) THEN
                              b = b/a
                              c = c/a
                              a = 1
                           ELSE
                              a = 0
                              c = c/b
                              b = 1
                           ENDIF
                           iv(1) = J
                           iv(2) = k
                           DO m = 1 , 2
                              jv = 1
                              i = iv(m)
                              jj = L13 + (i-1)*Lz
                              ig = L12 + (i-1)*5 + 5
                              nk = Xxx(ig)
                              DO ix = 1 , nk
                                 a1 = Ccc(jv+jj)
                                 b1 = Ccc(jv+1+jj)
                                 c1 = Ccc(jv+2+jj)
!
!     CHECK TO BE SURE LINE OF INTERSECTION IS NOT BOUNDARY LINE
!     OF THE JTH SET.
!
                                 s = a1 + b1 + c1
                                 s1 = a + b + c
                                 e = abs(s-s1)
                                 s = a1*50 + b1*50 + c1
                                 s1 = a*50 + b*50 + c
                                 f = abs(s-s1)
                                 IF ( f<exp .AND. e<exp ) GOTO 20
!
!
!     DETERMINES THE POINTS OF INTERSECTIONS OF THE LINE OF INTERSECTION
!     WITH OTHER LINES OF RELEVANT ELEMENTS.
!
!
                                 t = a1*b - b1*a
                                 IF ( abs(t)>=er ) THEN
                                    xo = (c1*a-c*a1)/t
                                    IF ( a/=0. ) THEN
                                       yo = -c - b*xo
                                    ELSE
                                       yo = -c1 - b1*xo
                                    ENDIF
                                    t = xo
                                    IF ( a1==0. ) t = yo
                                    s = t - Ccc(jv+4+jj)
                                    s1 = t - Ccc(jv+3+jj)
                                    IF ( s*s1<=0. ) THEN
                                       mt = mt + 1
!
!     STORE THE PTS OF INTERSECTIONS.
!
                                       Rz(Xasolv-1+mt) = xo
                                       Rz(Yasolv-1+mt) = yo
                                       Rz(Zasolv-1+mt) = -(d3+a3*xo+b3*yo)/c3
                                       zt = -(d4+a4*xo+b4*yo)/c4
                                       IF ( abs(zt-Rz(Zasolv-1+mt))>exx ) GOTO 20
                                    ENDIF
                                 ENDIF
                                 jv = jv + 5
                              ENDDO
                           ENDDO
                           CALL hdstat(mt,Nit,Ixr,X21,Y21,Z21,Iia,iv,a,b,c,J,Rz(Xasolv),Rz(Yasolv),Rz(Zasolv),Ccc,Xxx,Lz)
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
 20      ENDDO
      ENDIF
   ENDIF
   nr = 5*Xxx(5+jt)
   DO is = 1 , nr
      Rz(Xcc-1+is) = Ccc(is+jb)
   ENDDO
   Xxx(5+jt) = Xxx(5+jt) + Nit
END SUBROUTINE hdsolv
