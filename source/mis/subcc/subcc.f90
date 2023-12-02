!*==subcc.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE subcc
   USE c_blk1
   USE c_blk2
   USE c_blk3
   USE c_blk4
   USE c_system
   USE c_xmssg
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   COMPLEX :: c1a , c2a , cexp1 , cexp1a , cexp2 , cexp2a , const , const2 , const3 , const4 , const5 , const6 , ss , t1 , t2 , t3 ,&
            & t4
   REAL :: s1 , s2 , s3 , s4 , s5 , temp
   EXTERNAL akapm , mesage
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     THIS ROUTINE WAS ORIGINALLY CALLED SUBD
!
!
         am6 = 0.0
         f5 = 0.0
         am5 = 0.0
         s1 = sps + sns
         s2 = sigma - sps*del
         s3 = sps/(dstr**2)
         s4 = sns/dstr
         s5 = del*sns + sigma
         ss = cexp(-ai*sigma)
         DO iout = 1 , 200
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  IF ( iout>i7 ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  r5 = iout - 1
                  rq1 = sqrt((r5*pi/sns)**2+scrk**2)
                  rq2 = -rq1
                  c4 = (rq1*s1+s2)/(2.0*pi)
                  c5 = (rq2*s1+s2)/(2.0*pi)
                  bc2 = bc/(2.0*svkl1(iout))*cexp(-ai*(-s2)*(sps+3.0*sns)/(2.0*s1))/(2.0*pi*ai)
                  bc3 = bc2*svkl1(iout)/svkl2(iout)
                  bc4 = bc/(2.0*svkl1(iout))*cexp(ai*(-s2)*(sns-sps)/(2.0*s1))/(2.0*pi*ai)
                  bc5 = bc4*svkl1(iout)/svkl2(iout)
                  f5t = 0.0
                  am5t = 0.0
                  am5tt = 0.0
                  DO jl = 1 , nl
                     qres4(jl) = 0.0
                  ENDDO
                  DO iner = 1 , 200
                     r = iner - 1
                     gamp = 2.0*pi*r - s2
                     gamn = -2.0*pi*r - s2
                     c1p = (gamp/dstr) - scrk
                     c2p = (gamp/dstr) + scrk
                     alp = gamp*s3 - s4*csqrt(c1p)*csqrt(c2p)
                     bkdel3 = sbkde1(iner)
                     IF ( iner>i6 ) THEN
                        CALL akapm(alp,bkdel3)
                        sbkde1(iner) = bkdel3
                     ENDIF
                     t1 = alp*sps - gamp
                     t2 = alp*dstr**2 - gamp*sps
                     sum1 = sumsv1(iout)*cexp(ai*t1)*bkdel3*t1/(t2*svkl1(iout)*(alp-rq1))
                     sum3 = sumsv2(iout)*cexp(ai*t1)*bkdel3*t1/(t2*svkl2(iout)*(alp-rq2))
                     IF ( iner/=1 ) THEN
                        c1n = (gamn/dstr) - scrk
                        c2n = (gamn/dstr) + scrk
                        aln = gamn*s3 - s4*csqrt(c1n)*csqrt(c2n)
                        bkdel3 = sbkde2(iner)
                        IF ( iner>i6 ) THEN
                           CALL akapm(aln,bkdel3)
                           sbkde2(iner) = bkdel3
                        ENDIF
                        t1 = aln*sps - gamn
                        t2 = aln*dstr**2 - gamn*sps
                        sum2 = sumsv1(iout)*cexp(ai*t1)*bkdel3*t1/(t2*svkl1(iout)*(aln-rq1))
                        sum4 = sumsv2(iout)*cexp(ai*t1)*bkdel3*t1/(t2*svkl2(iout)*(aln-rq2))
                     ENDIF
                     IF ( iner==1 ) sum2 = 0.0
                     IF ( iner==1 ) sum4 = 0.0
                     c1p = cexp(-ai*(alp-del)*sps)
                     c2p = cexp(-ai*(alp-del)*sns)
                     c1n = cexp(-ai*(aln-del)*sps)
                     c2n = cexp(-ai*(aln-del)*sns)
                     f5t = f5t + (sum1+sum3)*ai*ss/(alp-del)*(c1p-c2p) + (sum2+sum4)*ss*ai/(aln-del)*(c1n-c2n)
                     am5t = am5t + (sum1+sum3)*ss*(ai*sps*c1p/(alp-del)-ai*sns*c2p/(alp-del)+1.0/((alp-del)**2)*(c1p-c2p)           &
                          & +ai*(2.0-sps)/(alp-del)*(c1p-c2p)) + (sum2+sum4)*ss*(ai*sps*c1n/(aln-del)-ai*sns*c2n/(aln-del)          &
                          & +1.0/((aln-del)**2)*(c1n-c2n)+ai*(2.0-sps)/(aln-del)*(c1n-c2n))
                     temp = (sps-sns)/rl1
                     const = (sum1+sum3)*ss
                     const2 = (sum2+sum4)*ss
                     c1a = -ai*(alp-del)
                     c2a = -ai*(aln-del)
                     cexp1 = cexp(c1a*sns)
                     cexp2 = cexp(c2a*sns)
                     cexp1a = cexp(c1a*temp)
                     cexp2a = cexp(c2a*temp)
                     DO jl = 1 , nl
                        qres4(jl) = qres4(jl) - (const*cexp1+const2*cexp2)
                        cexp1 = cexp1*cexp1a
                        cexp2 = cexp2*cexp2a
                     ENDDO
                     betnp = (2.0*r*pi-s5)/s1
                     betnn = (-2.0*r*pi-s5)/s1
                     c1p = cexp(-2.0*pi*r*ai*sns/s1)
                     c2p = cexp(-2.0*pi*r*ai*sps/s1)
                     c1n = cexp(2.0*pi*r*ai*sns/s1)
                     c2n = cexp(2.0*pi*r*ai*sps/s1)
                     t1 = cexp(-ai*betnp*sps)
                     t2 = cexp(-ai*betnp*sns)
                     t3 = cexp(-ai*betnn*sps)
                     t4 = cexp(-ai*betnn*sns)
                     ca1 = ai*ss/betnp*(t1-t2)
                     ca2 = ai*ss/betnn*(t3-t4)
                     ca3 = ss*(ai*sps/betnp*t1-ai*sns*t2/betnp+(t1-t2)/betnp**2+(2.0-sps)*ai/betnp*(t1-t2))
                     ca4 = ss*(ai*sps*t3/betnn-ai*sns*t4/betnn+(t3-t4)/betnn**2+(2.0-sps)*ai/betnn*(t3-t4))
                     IF ( iner>1 ) THEN
                        f5t = f5t - sumsv1(iout)*((bc2*c1p-bc4*c2p)/(r-c4)*ca1-(bc2*c1n-bc4*c2n)/(r+c4)*ca2) - sumsv2(iout)         &
                            & *((bc3*c1p-bc5*c2p)/(r-c5)*ca1-(bc3*c1n-bc5*c2n)/(r+c5)*ca2)
                        am5t = am5t - sumsv1(iout)*((bc2*c1p-bc4*c2p)/(r-c4)*ca3-(bc2*c1n-bc4*c2n)/(r+c4)*ca4) - sumsv2(iout)       &
                             & *((bc3*c1p-bc5*c2p)/(r-c5)*ca3-(bc3*c1n-bc5*c2n)/(r+c5)*ca4)
                        temp = (sps-sns)/rl1
                        const = (bc2*c1p-bc4*c2p)/(r-c4)
                        const2 = (bc2*c1n-bc4*c2n)/(r+c4)
                        const3 = (bc3*c1p-bc5*c2p)/(r-c5)
                        const4 = (bc3*c1n-bc5*c2n)/(r+c5)
                        const5 = ss*sumsv1(iout)
                        const6 = ss*sumsv2(iout)
                        c1a = -ai*betnp
                        c2a = -ai*betnn
                        cexp1 = cexp(c1a*sns)
                        cexp2 = cexp(c2a*sns)
                        cexp1a = cexp(c1a*temp)
                        cexp2a = cexp(c2a*temp)
                        DO jl = 1 , nl
                           qres4(jl) = qres4(jl) + const5*(const*cexp1-const2*cexp2) + const6*(const3*cexp1-const4*cexp2)
                           cexp1 = cexp1*cexp1a
                           cexp2 = cexp2*cexp2a
                        ENDDO
                     ELSE
                        f5t = f5t - sumsv1(iout)*(bc2*c1p-bc4*c2p)/(r-c4)*ca1 - sumsv2(iout)*(bc3*c1p-bc5*c2p)/(r-c5)*ca1
                        am5t = am5t - sumsv1(iout)*(bc2*c1p-bc4*c2p)/(r-c4)*ca3 - sumsv2(iout)*(bc3*c1p-bc5*c2p)/(r-c5)*ca3
                        temp = (sps-sns)/rl1
                        const = ss*sumsv1(iout)*(bc2*c1p-bc4*c2p)/(r-c4)
                        const2 = ss*sumsv2(iout)*(bc3*c1p-bc5*c2p)/(r-c5)
                        c1a = -ai*betnp
                        cexp1 = cexp(c1a*sns)
                        cexp1a = cexp(c1a*temp)
                        DO jl = 1 , nl
                           qres4(jl) = qres4(jl) + const*cexp1 + const2*cexp1
                           cexp1 = cexp1*cexp1a
                        ENDDO
                     ENDIF
                     IF ( cabs((am5tt-am5t)/am5t)<0.001 ) THEN
                        spag_nextblock_2 = 2
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                     am5tt = am5t
                  ENDDO
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               CASE (2)
                  IF ( iner>i6 ) i6 = iner
                  f5 = f5 + f5t
                  am5 = am5 + am5t
                  DO jl = 1 , nl
                     pres4(jl) = pres4(jl) + qres4(jl)
                  ENDDO
                  alp1 = (2.0*pi*c4-del*sns-sigma)/s1
                  alp2 = (2.0*pi*c5-del*sns-sigma)/s1
                  t1 = 1.0 - cexp(-2.0*pi*ai*c4)
                  t2 = 1.0 - cexp(-2.0*pi*ai*c5)
                  c1p = cexp(-2.0*pi*ai*c4*sns/s1)/(t1)
                  c2p = cexp(2.0*pi*ai*c4*sns/s1)/(t1)
                  c1n = cexp(-2.0*pi*ai*c5*sns/s1)/(t2)
                  c2n = cexp(2.0*pi*ai*c5*sns/s1)/(t2)
                  t1 = cexp(-ai*sps*alp1)
                  t2 = cexp(-ai*sns*alp1)
                  t3 = cexp(-ai*sps*alp2)
                  t4 = cexp(-ai*sns*alp2)
                  ca1 = ai*ss/alp1*(t1-t2)
                  ca2 = ai*ss/alp2*(t3-t4)
                  ca3 = ss*(ai*sps*t1/alp1-ai*sns*t2/alp1+(t1-t2)/alp1**2+(2.0-sps)*ai/alp1*(t1-t2))
                  ca4 = ss*(ai*sps*t3/alp2-ai*sns*t4/alp2+(t3-t4)/alp2**2+(2.0-sps)*ai/alp2*(t3-t4))
                  f5 = f5 - 2.0*pi*ai*sumsv1(iout)*(bc2*c1p-bc4*c2p)*ca1 - 2.0*pi*ai*sumsv2(iout)*(bc3*c1n-bc5*c2n)*ca2
                  am5 = am5 - 2.0*pi*ai*sumsv1(iout)*(bc2*c1p-bc4*c2p)*ca3 - 2.0*pi*ai*sumsv2(iout)*(bc3*c1n-bc5*c2n)*ca4
                  temp = (sps-sns)/rl1
                  const = ss*2.0*pi*ai
                  const2 = const*sumsv1(iout)*(bc2*c1p-bc4*c2p)
                  const3 = const*sumsv2(iout)*(bc3*c1n-bc5*c2n)
                  c1a = -ai*alp1
                  c2a = -ai*alp2
                  cexp1 = cexp(c1a*sns)
                  cexp2 = cexp(c2a*sns)
                  cexp1a = cexp(c1a*temp)
                  cexp2a = cexp(c2a*temp)
                  DO jl = 1 , nl
                     pres4(jl) = pres4(jl) + const2*cexp1 + const3*cexp2
                     cexp1 = cexp1*cexp1a
                     cexp2 = cexp2*cexp2a
                  ENDDO
                  IF ( cabs((am5-am6)/am5)<0.0009 ) THEN
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  am6 = am5
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         WRITE (ibbout,99001) ufm
99001    FORMAT (A23,' - AMG MODULE -SUBROUTINE SUBCC.  AM5 LOOP DID NOT',' CONVERGE.')
         spag_nextblock_1 = 5
      CASE (2)
         clift = f1 + f2 - f2p + f4 + f5
         cmomt = am1 + am2 - am2p + am4 + am5 - amoaxs*clift
         RETURN
      CASE (3)
!
         WRITE (ibbout,99002) ufm
99002    FORMAT (A23,' - AMG MODULE -SUBROUTINE SUBCC.  AM5T LOOP DID NOT',' CONVERGE.')
         spag_nextblock_1 = 5
      CASE (4)
         WRITE (ibbout,99003) ufm , i7
99003    FORMAT (A23,' - AMG MODULE -SUBROUTINE SUBCC.  OUTER LOOP OF AM5',' EXCEEDED I7 (',I6,1H))
         spag_nextblock_1 = 5
      CASE (5)
         CALL mesage(-61,0,0)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE subcc
