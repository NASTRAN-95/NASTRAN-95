!*==hdstus.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE hdstus(Oj,Tmj,Xxx,Tgm,Rv,Rvi,Tgi,Zm,Nno,Ii,H,Im,Jxt,Zj,Nc,Zmi,Ccc,Lz)
   USE c_go3
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL :: Oj
   REAL :: Tmj
   REAL , DIMENSION(1) :: Xxx
   REAL , DIMENSION(1) :: Tgm
   REAL , DIMENSION(1) :: Rv
   REAL , DIMENSION(1) :: Rvi
   REAL , DIMENSION(1) :: Tgi
   REAL , DIMENSION(1) :: Zm
   INTEGER , DIMENSION(1) :: Nno
   INTEGER :: Ii
   REAL , DIMENSION(8) :: H
   INTEGER :: Im
   INTEGER :: Jxt
   REAL :: Zj
   INTEGER :: Nc
   REAL , DIMENSION(1) :: Zmi
   REAL , DIMENSION(1) :: Ccc
   INTEGER :: Lz
!
! Local variable declarations rewritten by SPAG
!
   REAL :: d , dy , ei , ggk , r , s , s1 , t , ve , vx , vx1 , vx2 , yg , zs
   INTEGER :: i , ib , j , jg , jo , js , jt , ns , nsub
!
! End of declarations rewritten by SPAG
!
!
!     THIS SUBROUTINE DETERMINES THE VISIBILITY OF AN ARBITRARY POINT
!     BY DRAWING A LINE FROM THE POINT IN QUESTION TO INFINITY AND
!     COUNTING THE NUMBER OF TIMES IT CROSSES THE BOUNDARIES OF A
!     RELEVANT ELEMENT.
!
!
   ggk = .015
   ei = 0
   Im = 0
   SPAG_Loop_1_2: DO
      IF ( ei<1. ) THEN
         ei = ei + .2
         d = ei*Oj - Tmj
         SPAG_Loop_2_1: DO jo = 1 , Ii
            ggk = .015
            i = 0
            jg = Nno(l4+jo)
            js = l13 + (jg-1)*Lz
            jt = l12 + (jg-1)*5
!
!     PRELIMINARY CHECK TO SEE IF THE POINT IS OUTSIDE THE BOUNDARY
!     BOXES IN THE X,Y,Z DIMENSIONS.
!
            IF ( Tmj<Rv(l7+jg) .AND. Tmj>Rvi(l8+jg) ) THEN
               IF ( Oj<Tgi(l6+jg) .AND. Oj>Tgm(l5+jg) ) THEN
                  IF ( Zj<Zm(l2+jg) ) THEN
                     vx = Xxx(4+jt)
                     vx1 = Xxx(2+jt)*Tmj
                     vx2 = Xxx(1+jt)*Oj
                     zs = -(vx+vx1+vx2)/Xxx(3+jt)
                     IF ( abs(Zj-zs)>=ggk ) THEN
                        IF ( Zj<zs ) THEN
                           ns = Xxx(5+jt)
                           ib = ns*5
                           IF ( H(8)/=1. ) THEN
                              DO j = 1 , ib , 5
                                 ggk = .015
                                 nsub = j + 1 + js
                                 IF ( abs(Ccc(nsub))>=100. ) ggk = alog10(abs(Ccc(nsub)))
                                 ve = Oj
                                 IF ( Ccc(j+js)==0. ) ve = Tmj
                                 s = ve - Ccc(j+3+js)
                                 s1 = ve - Ccc(j+4+js)
                                 yg = Tmj
                                 IF ( Ccc(j+js)/=0. ) THEN
                                    dy = -Ccc(j+2+js) - Ccc(j+1+js)*Oj
                                 ELSE
                                    dy = -Ccc(j+2+js)/Ccc(j+1+js)
                                    yg = Oj
                                 ENDIF
                                 IF ( abs(yg-dy)<ggk .AND. s*s1<=0. ) CYCLE SPAG_Loop_2_1
                              ENDDO
                           ENDIF
!
!     THE FOLLOWING CODE COUNTS THE INTERSECTIONS OF BOUNDARIES
!     OF A GIVEN ELEMENT WITH THE INFINITE LINE AND CHECKS,IF INSIDE
!     OF THE BOUNDARY, WHETHER OR NOT THE POINT IS BEHIND OR IN FRONT
!     OF THE ELEMENT.
!
                           DO j = 1 , ib , 5
                              t = -Ccc(j+2+js) + Ccc(j+js)*d
                              r = ei*Ccc(j+js) + Ccc(j+1+js)
                              IF ( r/=0. ) THEN
                                 t = t/r
                                 IF ( t>=Oj ) THEN
                                    IF ( Ccc(j+js)==0. ) t = ei*t - d
                                    s = t - Ccc(j+3+js)
                                    s1 = t - Ccc(j+4+js)
                                    IF ( s==0. .OR. s1==0. ) CYCLE SPAG_Loop_1_2
                                    IF ( s*s1<0. ) i = i + 1
                                 ENDIF
                              ENDIF
                           ENDDO
                           IF ( mod(i,2)/=0 ) THEN
                              Im = 1
                              EXIT SPAG_Loop_1_2
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDDO SPAG_Loop_2_1
         Im = 0
      ENDIF
      EXIT SPAG_Loop_1_2
   ENDDO SPAG_Loop_1_2
END SUBROUTINE hdstus
