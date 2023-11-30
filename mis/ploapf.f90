
SUBROUTINE ploapf(Ecpt,Iecpt,L,Pa,Pb)
   IMPLICIT NONE
   REAL E , G
   COMMON /matout/ E , G
   REAL L
   REAL Ecpt(33) , Pa(1) , Pb(1)
   INTEGER Iecpt(9)
   REAL a , ael , beta , ei1 , ei2 , fj , gak , gjl , i1 , i12 , i2 , k1 , k2 , ke(144) , kep(144) , l2 , l2b3 , l2b6 , l3 , lb ,   &
      & lr1 , lr2 , pe(12) , pep(12) , r1 , r2 , sk1 , sk2 , sk3 , sk4
   INTEGER i , ii , ij , il , ill , ip , ip12 , ipin(10) , j , j12 , ji , jll , k , ka , kb , ll
!
!     THIS ROUTINE IS CALLED ONLY BY PLOAD1 FOR HANDLING PIN FLAGS OF
!     THE CBAR
!
!
   ka = Iecpt(8)
   kb = Iecpt(9)
   IF ( ka/=0 .OR. kb/=0 ) THEN
      DO i = 1 , 6
         pe(i) = Pa(i)
         pe(i+6) = Pb(i)
      ENDDO
      l2 = L**2
      l3 = l2*L
      a = Ecpt(17)
      i1 = Ecpt(18)
      i2 = Ecpt(19)
      fj = Ecpt(20)
      k1 = Ecpt(31)
      k2 = Ecpt(32)
      i12 = Ecpt(33)
      ei1 = E*i1
      ei2 = E*i2
      r1 = 12.0*ei1/l3
      r2 = 12.0*ei2/l3
      IF ( k1/=0.0 .AND. i12==0.0 ) THEN
         gak = G*a*k1
         r1 = (12.0*ei1*gak)/(gak*l3+12.0*L*ei1)
      ENDIF
      IF ( k2/=0.0 .AND. i12==0.0 ) THEN
         gak = G*a*k2
         r2 = (12.0*ei2*gak)/(gak*l3+12.0*L*ei2)
      ENDIF
!
!     COMPUTE THE -SMALL-K-S. SK1, SK2, SK3 AND SK4
!
      sk1 = 0.25*r1*l2 + ei1/L
      sk2 = 0.25*r2*l2 + ei2/L
      sk3 = 0.25*r1*l2 - ei1/L
      sk4 = 0.25*r2*l2 - ei2/L
!
!     COMPUTE THE 12 X 12 MATRIX KE
!
      ael = a*E/L
      lr1 = L*r1/2.0
      lr2 = L*r2/2.0
      gjl = G*fj/L
!
      DO i = 1 , 144
         ke(i) = 0.0
      ENDDO
      ke(1) = ael
      ke(7) = -ael
      ke(14) = r1
      ke(18) = lr1
      ke(20) = -r1
      ke(24) = lr1
      ke(27) = r2
      ke(29) = -lr2
      ke(33) = -r2
      ke(35) = -lr2
      ke(40) = gjl
      ke(46) = -gjl
      ke(51) = -lr2
      ke(53) = sk2
      ke(57) = lr2
      ke(59) = sk4
      ke(62) = lr1
      ke(66) = sk1
      ke(68) = -lr1
      ke(72) = sk3
      ke(73) = -ael
      ke(79) = ael
      ke(86) = -r1
      ke(90) = -lr1
      ke(92) = r1
      ke(96) = -lr1
      ke(99) = -r2
      ke(101) = lr2
      ke(105) = r2
      ke(107) = lr2
      ke(112) = -gjl
      ke(118) = gjl
      ke(123) = -lr2
      ke(125) = sk4
      ke(129) = lr2
      ke(131) = sk2
      ke(134) = lr1
      ke(138) = sk3
      ke(140) = -lr1
      ke(144) = sk1
      IF ( i12/=0.0 ) THEN
         beta = -12.0*E*i12/l3
         lb = L*beta/2.0
         l2b3 = l2*beta/3.0
         l2b6 = l2*beta/6.0
         ke(15) = -beta
         ke(17) = lb
         ke(21) = beta
         ke(23) = lb
         ke(26) = -beta
         ke(30) = -lb
         ke(32) = beta
         ke(36) = -lb
         ke(50) = lb
         ke(54) = l2b3
         ke(56) = -lb
         ke(60) = l2b6
         ke(63) = -lb
         ke(65) = l2b3
         ke(69) = lb
         ke(71) = l2b6
         ke(87) = beta
         ke(89) = -lb
         ke(93) = -beta
         ke(95) = -lb
         ke(98) = beta
         ke(102) = lb
         ke(104) = -beta
         ke(108) = lb
         ke(122) = lb
         ke(126) = l2b6
         ke(128) = -lb
         ke(132) = l2b3
         ke(135) = -lb
         ke(137) = l2b6
         ke(141) = lb
         ke(143) = l2b3
      ENDIF
!
!     SET UP THE IPIN ARRAY
!
      DO i = 1 , 5
         ipin(i) = mod(ka,10)
         ipin(i+5) = mod(kb,10) + 6
         IF ( ipin(i+5)==6 ) ipin(i+5) = 0
         ka = ka/10
         kb = kb/10
      ENDDO
!
!     ALTER KE MATRIX DUE TO PIN FLAGS
!
      DO i = 1 , 10
         ip = ipin(i)
         IF ( ip/=0 ) THEN
            ii = ip*13 - 12
            IF ( ke(ii)/=0.0 ) THEN
               ip12 = (ip-1)*12
               DO j = 1 , 12
                  j12 = (j-1)*12
                  ji = j12 + ip
                  ij = ip12 + j
                  DO ll = 1 , 12
                     jll = j12 + ll
                     ill = ip12 + ll
                     kep(jll) = ke(jll) - (ke(ill)/ke(ii))*ke(ji)
                  ENDDO
                  pep(j) = pe(j) - (ke(ji)/ke(ii))*pe(ip)
                  kep(ij) = 0.0
                  kep(ji) = 0.0
               ENDDO
               pep(ip) = 0.0
               DO k = 1 , 144
                  ke(k) = kep(k)
               ENDDO
               DO k = 1 , 12
                  pe(k) = pep(k)
               ENDDO
            ELSE
               il = ip
               ii = ii - il
               DO j = 1 , 12
                  ii = ii + 1
                  ke(ii) = 0.0
                  ke(il) = 0.0
                  il = il + 12
               ENDDO
            ENDIF
         ENDIF
      ENDDO
!
      DO i = 1 , 10
         ip = ipin(i)
         IF ( ip/=0 ) pe(ip) = 0.0
      ENDDO
      DO i = 1 , 6
         Pa(i) = pe(i)
         Pb(i) = pe(i+6)
      ENDDO
   ENDIF
!
END SUBROUTINE ploapf
