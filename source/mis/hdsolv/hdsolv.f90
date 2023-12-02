!*==hdsolv.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE hdsolv(Ixr,J,Xxx,Ccc,Ii,Nno,Nit,X21,Y21,Z21,Iia,Nc,Zm,Zmi,Lz)
   USE c_go3
   USE c_hdptrs
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ixr
   INTEGER :: J
   REAL , DIMENSION(1) :: Xxx
   REAL , DIMENSION(1) :: Ccc
   INTEGER :: Ii
   INTEGER , DIMENSION(1) :: Nno
   INTEGER :: Nit
   REAL , DIMENSION(1) :: X21
   REAL , DIMENSION(1) :: Y21
   REAL , DIMENSION(1) :: Z21
   INTEGER , DIMENSION(1) :: Iia
   INTEGER :: Nc
   REAL , DIMENSION(1) :: Zm
   REAL , DIMENSION(1) :: Zmi
   INTEGER :: Lz
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a , a1 , a3 , a4 , b , b1 , b3 , b4 , c , c1 , c3 , c4 , d3 , d4 , e , er , ers , exp , exx , f , s , s1 , t , xo , yo , &
         & zt
   INTEGER :: i , ig , is , ix , jb , jj , jt , jv , jx , k , l , m , mt , nk , nr
   INTEGER , DIMENSION(2) :: iv
   EXTERNAL hdstat
!
! End of declarations rewritten by SPAG
!
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
   jt = l12 + (J-1)*5
   jb = l13 + (J-1)*Lz
   IF ( Ii/=0 ) THEN
      a3 = Xxx(1+jt)
      b3 = Xxx(2+jt)
      c3 = Xxx(3+jt)
      d3 = Xxx(4+jt)
      IF ( Xxx(jt+3)/=0. ) THEN
         SPAG_Loop_1_1: DO l = 1 , Ii
            k = Nno(l4+l)
!
!     CHECKS TO SEE IF THIS RELEVANT ELEMENT IS TO BE CONSIDERED FOR
!     INTERSECTION
!
            IF ( k>0 ) THEN
               IF ( k>=J ) THEN
                  jx = l12 + (k-1)*5
                  IF ( Zm(l2+J)>=Zmi(l3+k) ) THEN
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
                              jj = l13 + (i-1)*Lz
                              ig = l12 + (i-1)*5 + 5
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
                                 IF ( f<exp .AND. e<exp ) CYCLE SPAG_Loop_1_1
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
                                       rz(xasolv-1+mt) = xo
                                       rz(yasolv-1+mt) = yo
                                       rz(zasolv-1+mt) = -(d3+a3*xo+b3*yo)/c3
                                       zt = -(d4+a4*xo+b4*yo)/c4
                                       IF ( abs(zt-rz(zasolv-1+mt))>exx ) CYCLE SPAG_Loop_1_1
                                    ENDIF
                                 ENDIF
                                 jv = jv + 5
                              ENDDO
                           ENDDO
                           CALL hdstat(mt,Nit,Ixr,X21,Y21,Z21,Iia,iv,a,b,c,J,rz(xasolv),rz(yasolv),rz(zasolv),Ccc,Xxx,Lz)
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDDO SPAG_Loop_1_1
      ENDIF
   ENDIF
   nr = 5*Xxx(5+jt)
   DO is = 1 , nr
      rz(xcc-1+is) = Ccc(is+jb)
   ENDDO
   Xxx(5+jt) = Xxx(5+jt) + Nit
END SUBROUTINE hdsolv
