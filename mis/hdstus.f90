
SUBROUTINE hdstus(Oj,Tmj,Xxx,Tgm,Rv,Rvi,Tgi,Zm,Nno,Ii,H,Im,Jxt,Zj,Nc,Zmi,Ccc,Lz)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER L0 , L00 , L01 , L1 , L10 , L11 , L12 , L13 , L2 , L3 , L4 , L5 , L6 , L7 , L8 , L9
   COMMON /go3   / L0 , L1 , L00 , L01 , L2 , L3 , L4 , L5 , L6 , L7 , L8 , L9 , L10 , L11 , L12 , L13
!
! Dummy argument declarations
!
   INTEGER Ii , Im , Jxt , Lz , Nc
   REAL Oj , Tmj , Zj
   REAL Ccc(1) , H(8) , Rv(1) , Rvi(1) , Tgi(1) , Tgm(1) , Xxx(1) , Zm(1) , Zmi(1)
   INTEGER Nno(1)
!
! Local variable declarations
!
   REAL d , dy , ei , ggk , r , s , s1 , t , ve , vx , vx1 , vx2 , yg , zs
   INTEGER i , ib , j , jg , jo , js , jt , ns , nsub
!
! End of declarations
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
   DO
      IF ( ei<1. ) THEN
         ei = ei + .2
         d = ei*Oj - Tmj
         DO jo = 1 , Ii
            ggk = .015
            i = 0
            jg = Nno(L4+jo)
            js = L13 + (jg-1)*Lz
            jt = L12 + (jg-1)*5
!
!     PRELIMINARY CHECK TO SEE IF THE POINT IS OUTSIDE THE BOUNDARY
!     BOXES IN THE X,Y,Z DIMENSIONS.
!
            IF ( Tmj<Rv(L7+jg) .AND. Tmj>Rvi(L8+jg) ) THEN
               IF ( Oj<Tgi(L6+jg) .AND. Oj>Tgm(L5+jg) ) THEN
                  IF ( Zj<Zm(L2+jg) ) THEN
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
                                 IF ( abs(yg-dy)<ggk .AND. s*s1<=0. ) GOTO 20
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
                                    IF ( s==0. .OR. s1==0. ) GOTO 100
                                    IF ( s*s1<0. ) i = i + 1
                                 ENDIF
                              ENDIF
                           ENDDO
                           IF ( mod(i,2)/=0 ) THEN
                              Im = 1
                              GOTO 99999
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
 20      ENDDO
         Im = 0
      ENDIF
      EXIT
 100  ENDDO
99999 RETURN
END SUBROUTINE hdstus
