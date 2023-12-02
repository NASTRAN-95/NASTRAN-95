!*==subc.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE subc
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
   COMPLEX :: c1a , c2a , c3a , c4a , cexp1 , cexp1a , cexp2 , cexp2a , cexp3 , cexp3a , cexp4 , cexp4a , cexp5 , cexp5a , const ,  &
            & t1 , t2 , t3
   REAL :: s1 , s2 , s3 , s4 , temp , temp1 , temp2
   EXTERNAL akapm , mesage , subcc
!
! End of declarations rewritten by SPAG
!
!
!
   am4tst = 0.0
   s1 = sps*del - sigma
   s2 = sps/(dstr**2)
   s3 = sns/dstr
   s4 = sps + sns
   t3 = cexp(-ai*sigma)
   DO i = 1 , 200
      r = i
      gamp = 2.0*pi*r + s1
      gamn = -2.0*pi*r + s1
      c1p = (gamp/dstr) - scrk
      c2p = (gamp/dstr) + scrk
      alp = gamp*s2 - s3*csqrt(c1p)*csqrt(c2p)
      t1 = alp - del
      CALL akapm(alp,bkdel3)
      sbkde1(i+1) = bkdel3
      sum1 = cexp(ai*(alp*sps-gamp))*(alp*sps-gamp)*bkdel3/((alp*dstr**2-gamp*sps)*t1)                                              &
           & *(f6s*t1/(t1+gl)+f5s+b*ai/(bkdel1*bkap1)*(del-amu)/(alp-del))
      c1n = (gamn/dstr) - scrk
      c2n = (gamn/dstr) + scrk
      aln = gamn*s2 - s3*csqrt(c1n)*csqrt(c2n)
      t2 = aln - del
      CALL akapm(aln,bkdel3)
      sbkde2(i+1) = bkdel3
      sum2 = cexp(ai*(aln*sps-gamn))*(aln*sps-gamn)*bkdel3/((aln*dstr**2-gamn*sps)*t2)*(f6s*(t2)/(t2+gl)+f5s+b*ai/(bkdel1*bkap1)    &
           & *(del-amu)/(t2))
      c1p = cexp(-ai*(t1)*sps)
      c2p = cexp(-ai*(t1)*sns)
      c1n = cexp(-ai*(t2)*sps)
      c2n = cexp(-ai*(t2)*sns)
      f4 = f4 + sum1*t3*ai/(t1)*(c1p-c2p) + sum2*t3*ai/(t2)*(c1n-c2n)
      am4 = am4 + sum1*t3*(ai*sps*c1p/(t1)-ai*sns*c2p/(t1)+1.0/((t1)**2)*(c1p-c2p)+ai*(2.0-sps)/(t1)*(c1p-c2p))                     &
          & + sum2*t3*(ai*sps*c1n/(t2)-ai*sns*c2n/(t2)+1.0/((t2)**2)*(c1n-c2n)+ai*(2.0-sps)/(t2)*(c1n-c2n))
      i6 = i + 1
      temp = (sps-sns)/rl1
      c1a = -ai*t1
      c2a = -ai*t2
      c3a = ai*del
      cexp1 = cexp(c1a*sns)
      cexp2 = cexp(c2a*sns)
      cexp3 = cexp(c3a*sns)
      cexp1a = cexp(c1a*temp)
      cexp2a = cexp(c2a*temp)
      cexp3a = cexp(c3a*temp)
      const = fq7/(2.0*pi)
      temp2 = 2.0*pi*r/s4
      c4a = -ai*s1
      cexp4 = cexp(c4a*(2.0*sns/s4+0.5))
      cexp5 = cexp(c4a*0.5)
      cexp4a = cexp(c4a*temp/s4)
      cexp5a = cexp(c4a*temp/(sps+sns))
      xl = sns
      DO jl = 1 , nl
         pres4(jl) = pres4(jl) - t3*(sum1*cexp1+sum2*cexp2+const*cexp3*(cexp4*sin(temp2*(sns+xl))/r-cexp5*sin(temp2*(sps+xl))/r))
         xl = xl + temp
         cexp1 = cexp1*cexp1a
         cexp2 = cexp2*cexp2a
         cexp3 = cexp3*cexp3a
         cexp4 = cexp4*cexp4a
         cexp5 = cexp5*cexp5a
      ENDDO
      IF ( cabs((am4-am4tst)/am4)<0.0006 ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
      am4tst = am4
   ENDDO
!
   WRITE (ibbout,99001) ufm
99001 FORMAT (A23,' - AMG MODULE -SUBROUTINE SUBC.  AM4 LOOP DID NOT ','CONVERGE.')
   CALL mesage(-61,0,0)
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
      Temp = (Sps-Sns)/Rl1
      Temp1 = 2.0*Sns/S4 + 0.5
      Temp2 = 0.5 - (Sps+Sns)/S4
      C1a = Ai*Del
      C2a = -Ai*S1
      C3a = -C2a
      Cexp1 = cexp(C1a*Sns)
      Cexp2 = cexp(C2a*Temp1)
      Cexp3 = cexp(C3a*Temp2)
      Cexp1a = cexp(C1a*Temp)
      Cexp2a = cexp(C2a*Temp/S4)
      Const = T3*Fq7/2.0
      Xl = Sns
      DO Jl = 1 , Nl
         pres4(Jl) = pres4(Jl) - Const*Cexp1*(Cexp2*((Sns+Xl)/S4-0.5)-Cexp3*((Sps+Xl)/S4-1.5))
         Xl = Xl + Temp
         Cexp1 = Cexp1*Cexp1a
         Cexp2 = Cexp2*Cexp2a
         Cexp3 = Cexp3*Cexp2a
      ENDDO
      CALL subcc
   END SUBROUTINE spag_block_1
END SUBROUTINE subc
