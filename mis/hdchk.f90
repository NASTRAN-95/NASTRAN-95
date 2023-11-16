
SUBROUTINE hdchk(Xxx,Ccc,Nno,Ii,Xi,Yi,Ngx,Zm,Zmi,Rv,Rvi,Tgm,Tgi,Zi,Lz,Xcc)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Js , Jt , L0 , L00 , L01 , L1 , L10 , L11 , L12 , L13 , L2 , L3 , L4 , L5 , L6 , L7 , L8 , L9 , M , Nn
   REAL Vx , Vx1 , Vx2 , Vx3
   COMMON /go3   / L0 , L1 , L00 , L01 , L2 , L3 , L4 , L5 , L6 , L7 , L8 , L9 , L10 , L11 , L12 , L13
   COMMON /hedg  / Js , M , Jt , Vx , Vx1 , Vx2 , Vx3 , Nn
!
! Dummy argument declarations
!
   INTEGER Ii , Lz
   REAL Ccc(1) , Rv(1) , Rvi(1) , Tgi(1) , Tgm(1) , Xcc(1) , Xi(1) , Xxx(1) , Yi(1) , Zi(1) , Zm(1) , Zmi(1)
   INTEGER Ngx(1) , Nno(1)
!
! Local variable declarations
!
   REAL a , a1 , ac , al , b , b1 , bc , bl , c , c1 , cc , cl , d , eex , eg , egx , egx1 , egy , egy1 , exp , s , s1 , t , tsx ,  &
      & tsz , vt , vu , x1 , x2 , xo , xp , y1 , y2 , yo , yp , z1 , z2 , zp , zx , zx1
   INTEGER jc , je , jm , jr , ju , jv , le , lg , nk
!
! End of declarations
!
!
!
!
!     THIS SUBROUTINE SOLVES FOR THE POINTS OF INTERSECTION ON THE
!     LINES OF THE JTH ELEMENT WITH OTHER LINES AND PLANES(RELEVANT)
!
!
   jm = 1
   eex = .015
   exp = .005
   Ngx(1) = 0
   IF ( Ii==0 ) GOTO 99999
   IF ( Nn/=1 ) THEN
      IF ( Vx3==0. ) THEN
         a = Xxx(Jt+2)
         b = Xxx(Jt+1)
         c = Xxx(Jt+4)
         z1 = Xcc(Js)
         z2 = Xcc(Js+1)
         IF ( a==0. ) THEN
            y1 = Xcc(Js+3)
            y2 = Xcc(Js+4)
            x1 = -c
            x2 = x1
         ELSE
            y1 = -Xcc(Js+3)*b - c
            y2 = -Xcc(Js+4)*b - c
            x1 = Xcc(Js+3)
            x2 = Xcc(Js+4)
         ENDIF
         GOTO 100
      ENDIF
   ENDIF
   a = Xcc(Js)
   b = Xcc(Js+1)
   c = Xcc(Js+2)
   IF ( a==0. ) THEN
      y1 = Xcc(Js+3)
      y2 = Xcc(Js+4)
      x1 = -Xcc(Js+2)
      x2 = x1
   ELSE
      y1 = -Xcc(Js+3)*Xcc(Js+1) - Xcc(Js+2)
      y2 = -Xcc(Js+4)*Xcc(Js+1) - Xcc(Js+2)
      x1 = Xcc(Js+3)
      x2 = Xcc(Js+4)
   ENDIF
   IF ( Nn/=1 ) THEN
      z1 = -(Vx+Vx1*y1+Vx2*x1)/Vx3
      z2 = -(Vx+Vx1*y2+Vx2*x2)/Vx3
   ELSE
      z1 = Xxx(1+Jt)
      z2 = Xxx(2+Jt)
   ENDIF
 100  al = x2 - x1
   bl = y2 - y1
   cl = z2 - z1
   eg = amin1(z1,z2)
   egx = amax1(x1,x2)
   egx1 = amin1(x1,x2)
   egy = amax1(y1,y2)
   egy1 = amin1(y1,y2)
!
!
!     THIS CODE DETERMINES THE POINTS OF INTERSECTIONS ON THE LINES OF
!     JTH ELEMENT RESULTING FROM THE INTERSECTION OF THE PLANES WITH
!     THESE LINES.
!
!
   DO jr = 1 , Ii
      lg = Nno(L4+jr)
      Nno(L4+jr) = iabs(Nno(jr+L4))
      le = Nno(L4+jr)
      je = L13 + Lz*(le-1)
      ju = L12 + 5*(le-1)
      nk = Xxx(5+ju)
      jv = 1
      ac = Xxx(1+ju)
      bc = Xxx(2+ju)
      cc = Xxx(3+ju)
      d = Xxx(4+ju)
      IF ( egx>=Tgm(L5+le) ) THEN
         IF ( egx1<=Tgi(L6+le) ) THEN
            IF ( egy>=Rvi(L8+le) ) THEN
               IF ( egy1<=Rv(L7+le) ) THEN
                  IF ( eg<=Zm(L2+le) ) THEN
                     IF ( lg>=0 ) THEN
                        IF ( (al/=0.) .OR. (bl/=0.) ) THEN
                           IF ( al==0. ) THEN
                              yp = (ac*al/bl)*y1 + (cc*cl/bl)*y1 - d
                              yp = yp - cc*z1 - ac*x1
                              vu = bc + (ac*al/bl) + (cc*cl/bl)
                              IF ( vu==0. ) GOTO 102
                              yp = yp/vu
                              t = (yp-y1)/bl
                              xp = t*al + x1
                              zp = t*cl + z1
                           ELSE
                              xp = ((bc*bl)/al)*x1 + (cc*cl/al)*x1 - d
                              xp = xp - bc*y1 - cc*z1
                              vu = ac + (bc*bl/al) + (cc*cl/al)
                              IF ( vu==0. ) GOTO 102
                              xp = xp/vu
                              t = (xp-x1)/al
                              yp = t*bl + y1
                              zp = t*cl + z1
                           ENDIF
                           s = zp - Zm(L2+le)
                           s1 = zp - Zmi(L3+le)
                           IF ( (abs(s)>=eex) .AND. (abs(s1)>=eex) ) THEN
                              IF ( s*s1>0. ) GOTO 102
                           ENDIF
                           s = xp - Tgm(L5+le)
                           s1 = xp - Tgi(L6+le)
                           IF ( s*s1<=0. ) THEN
                              s = yp - Rv(L7+le)
                              s1 = yp - Rvi(L8+le)
                              IF ( s*s1<=0. ) THEN
                                 t = xp
                                 IF ( a==0. ) t = yp
                                 s = t - Xcc(Js+3)
                                 s1 = t - Xcc(Js+4)
                                 IF ( s*s1<0. ) THEN
                                    M = M + 1
!
!     STORES INTERSECTIONS.
!
                                    Xi(M+1) = xp
                                    Yi(M+1) = yp
                                    Zi(M+1) = zp
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDIF
                     ENDIF
!
!     THIS CODE DETERMINES INTERSECTION POINTS OF LINES WITH LINES.
!
 102                 DO jc = 1 , nk
                        b1 = Ccc(jv+1+je)
                        a1 = Ccc(jv+je)
                        c1 = Ccc(jv+2+je)
                        t = a1*b - b1*a
                        IF ( t/=0. ) THEN
                           xo = (c1*a-c*a1)/t
                           IF ( (abs(b)<=50.) .AND. (a/=0.) ) THEN
                              yo = -c - b*xo
                           ELSE
                              yo = -c1 - b1*xo
                           ENDIF
                           t = xo
                           IF ( a==0. ) t = yo
                           s = t - Xcc(Js+3)
                           s1 = t - Xcc(Js+4)
                           IF ( s*s1<0. ) THEN
                              t = xo
                              IF ( a1==0. ) t = yo
                              s1 = t - Ccc(jv+4+je)
                              s = t - Ccc(jv+3+je)
                              IF ( (abs(s)>eex) .AND. (abs(s1)>eex) ) THEN
                                 IF ( s*s1>0. ) GOTO 104
                              ENDIF
                              IF ( cc/=0. ) THEN
                                 zx = -(ac*xo+bc*yo+d)/cc
                                 IF ( Nn/=1 .AND. Vx3/=0. ) THEN
                                    zx1 = -(Vx+Vx1*yo+Vx2*xo)/Vx3
                                 ELSE
                                    tsz = z2 - z1
                                    tsx = x2 - x1
                                    vt = xo - x1
                                    IF ( tsx==0. ) THEN
                                       vt = yo - y1
                                       tsx = y2 - y1
                                    ENDIF
                                    zx1 = (tsz/tsx)*vt + z1
                                 ENDIF
                                 IF ( abs(zx-zx1)>exp ) THEN
                                    IF ( zx1>zx ) GOTO 104
                                 ENDIF
                                 M = M + 1
!
!     STORES INTERSECTIONS.
!
                                 Xi(M+1) = xo
                                 Yi(M+1) = yo
                                 Zi(M+1) = zx1
                              ENDIF
                           ENDIF
                        ENDIF
 104                    jv = jv + 5
                     ENDDO
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDDO
   Ngx(1) = M
99999 RETURN
END SUBROUTINE hdchk
