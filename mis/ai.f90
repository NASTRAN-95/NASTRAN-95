
FUNCTION ai(I,J,K,L,M,N,Ip,Iq,R,Z)
   IMPLICIT NONE
   INTEGER I , Ip , Iq , J , K , L , M , N
   REAL ai
   REAL R(1) , Z(1)
   REAL abs1 , akkl , akmn , amkl , amm , ammn , arr , rd , xx
   REAL bint , f6211 , f89 , ff100
   INTEGER irr , iss , mm , nn
!
!
   IF ( R(I)==R(J) ) THEN
      ai = 0.0
   ELSE
      rd = R(J)
      IF ( R(J)==0.0 ) rd = R(I)
      abs1 = abs((R(I)-R(J))/rd)
      IF ( abs1<=.0001 ) THEN
         ai = 0.0
      ELSE
         amkl = (R(L)*Z(K)-R(K)*Z(L))/(R(L)-R(K))
         akkl = (Z(L)-Z(K))/(R(L)-R(K))
         ammn = (R(N)*Z(M)-R(M)*Z(N))/(R(N)-R(M))
         akmn = (Z(N)-Z(M))/(R(N)-R(M))
         IF ( akmn/=akkl .OR. ammn/=amkl ) THEN
            iss = iabs(Ip)
            irr = iabs(Iq)
            IF ( Iq+1<0 ) THEN
               IF ( Ip<0 ) THEN
                  mm = iss
                  nn = irr - 1
                  ai = ff100(I,amkl,akkl,mm,nn,R) - ff100(I,ammn,akmn,mm,nn,R) - ff100(J,amkl,akkl,mm,nn,R)                         &
                     & + ff100(J,ammn,akmn,mm,nn,R)
                  arr = irr
                  ai = (1.0/(1.0-arr))*ai
               ELSE
                  mm = Ip
                  nn = irr - 1
                  ai = f89(I,amkl,akkl,mm,nn,R) - f89(I,ammn,akmn,mm,nn,R) - f89(J,amkl,akkl,mm,nn,R) + f89(J,ammn,akmn,mm,nn,R)
                  arr = irr
                  ai = (1.0/(1.0-arr))*ai
               ENDIF
            ELSEIF ( Iq+1==0 ) THEN
               IF ( Ip+1<0 ) THEN
                  mm = iss - 1
                  amm = mm
                  xx = amm*R(I)**mm
                  ai = (-alog(abs(amkl+akkl*R(I)))/xx+akkl/amm*ff100(I,amkl,akkl,mm,1,R)+alog(abs(ammn+akmn*R(I)))                  &
                     & /xx-akmn/amm*ff100(I,ammn,akmn,mm,1,R))
                  xx = amm*R(J)**M
                  ai = (+alog(abs(amkl+akkl*R(J)))/xx-akkl/amm*ff100(J,amkl,akkl,mm,1,R)-alog(abs(ammn+akmn*R(J)))                  &
                     & /xx+akmn/amm*ff100(J,ammn,akmn,mm,1,R)) + ai
               ELSEIF ( Ip+1==0 ) THEN
                  ai = f6211(I,amkl,akkl,R) - f6211(I,ammn,akmn,R) - f6211(J,amkl,akkl,R) + f6211(J,ammn,akmn,R)
               ELSE
                  mm = Ip + 1
                  amm = mm
                  xx = R(I)**mm/amm
                  ai = (+xx*alog(abs(amkl+akkl*R(I)))-akkl/amm*f89(I,amkl,akkl,mm,1,R)-xx*alog(abs(ammn+akmn*R(I)))                 &
                     & +akmn/amm*f89(I,ammn,akmn,mm,1,R))
                  xx = R(J)**mm/amm
                  ai = (-xx*alog(abs(amkl+akkl*R(J)))+akkl/amm*f89(J,amkl,akkl,mm,1,R)+xx*alog(abs(ammn+akmn*R(J)))                 &
                     & -akmn/amm*f89(J,ammn,akmn,mm,1,R)) + ai
               ENDIF
            ELSE
               mm = Ip
               nn = Iq + 1
               ai = bint(I,J,ammn,akmn,mm,nn,R,Z) - bint(I,J,amkl,akkl,mm,nn,R,Z)
            ENDIF
         ELSE
            ai = 0.0
         ENDIF
      ENDIF
   ENDIF
END FUNCTION ai
