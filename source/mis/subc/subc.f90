!*==subc.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE subc
   IMPLICIT NONE
   USE C_BLK1
   USE C_BLK2
   USE C_BLK3
   USE C_BLK4
   USE C_SYSTEM
   USE C_XMSSG
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
   Am4tst = 0.0
   s1 = Sps*Del - Sigma
   s2 = Sps/(Dstr**2)
   s3 = Sns/Dstr
   s4 = Sps + Sns
   t3 = cexp(-Ai*Sigma)
   DO I = 1 , 200
      R = I
      Gamp = 2.0*Pi*R + s1
      Gamn = -2.0*Pi*R + s1
      C1p = (Gamp/Dstr) - Scrk
      C2p = (Gamp/Dstr) + Scrk
      Alp = Gamp*s2 - s3*csqrt(C1p)*csqrt(C2p)
      t1 = Alp - Del
      CALL akapm(Alp,Bkdel3)
      Sbkde1(I+1) = Bkdel3
      Sum1 = cexp(Ai*(Alp*Sps-Gamp))*(Alp*Sps-Gamp)*Bkdel3/((Alp*Dstr**2-Gamp*Sps)*t1)                                              &
           & *(F6s*t1/(t1+Gl)+F5s+B*Ai/(Bkdel1*Bkap1)*(Del-Amu)/(Alp-Del))
      C1n = (Gamn/Dstr) - Scrk
      C2n = (Gamn/Dstr) + Scrk
      Aln = Gamn*s2 - s3*csqrt(C1n)*csqrt(C2n)
      t2 = Aln - Del
      CALL akapm(Aln,Bkdel3)
      Sbkde2(I+1) = Bkdel3
      Sum2 = cexp(Ai*(Aln*Sps-Gamn))*(Aln*Sps-Gamn)*Bkdel3/((Aln*Dstr**2-Gamn*Sps)*t2)*(F6s*(t2)/(t2+Gl)+F5s+B*Ai/(Bkdel1*Bkap1)    &
           & *(Del-Amu)/(t2))
      C1p = cexp(-Ai*(t1)*Sps)
      C2p = cexp(-Ai*(t1)*Sns)
      C1n = cexp(-Ai*(t2)*Sps)
      C2n = cexp(-Ai*(t2)*Sns)
      F4 = F4 + Sum1*t3*Ai/(t1)*(C1p-C2p) + Sum2*t3*Ai/(t2)*(C1n-C2n)
      Am4 = Am4 + Sum1*t3*(Ai*Sps*C1p/(t1)-Ai*Sns*C2p/(t1)+1.0/((t1)**2)*(C1p-C2p)+Ai*(2.0-Sps)/(t1)*(C1p-C2p))                     &
          & + Sum2*t3*(Ai*Sps*C1n/(t2)-Ai*Sns*C2n/(t2)+1.0/((t2)**2)*(C1n-C2n)+Ai*(2.0-Sps)/(t2)*(C1n-C2n))
      I6 = I + 1
      temp = (Sps-Sns)/Rl1
      c1a = -Ai*t1
      c2a = -Ai*t2
      c3a = Ai*Del
      cexp1 = cexp(c1a*Sns)
      cexp2 = cexp(c2a*Sns)
      cexp3 = cexp(c3a*Sns)
      cexp1a = cexp(c1a*temp)
      cexp2a = cexp(c2a*temp)
      cexp3a = cexp(c3a*temp)
      const = Fq7/(2.0*Pi)
      temp2 = 2.0*Pi*R/s4
      c4a = -Ai*s1
      cexp4 = cexp(c4a*(2.0*Sns/s4+0.5))
      cexp5 = cexp(c4a*0.5)
      cexp4a = cexp(c4a*temp/s4)
      cexp5a = cexp(c4a*temp/(Sps+Sns))
      Xl = Sns
      DO Jl = 1 , Nl
         Pres4(Jl) = Pres4(Jl) - t3*(Sum1*cexp1+Sum2*cexp2+const*cexp3*(cexp4*sin(temp2*(Sns+Xl))/R-cexp5*sin(temp2*(Sps+Xl))/R))
         Xl = Xl + temp
         cexp1 = cexp1*cexp1a
         cexp2 = cexp2*cexp2a
         cexp3 = cexp3*cexp3a
         cexp4 = cexp4*cexp4a
         cexp5 = cexp5*cexp5a
      ENDDO
      IF ( cabs((Am4-Am4tst)/Am4)<0.0006 ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
      Am4tst = Am4
   ENDDO
!
   WRITE (Ibbout,99001) Ufm
99001 FORMAT (A23,' - AMG MODULE -SUBROUTINE SUBC.  AM4 LOOP DID NOT ','CONVERGE.')
   CALL mesage(-61,0,0)
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
      temp = (Sps-Sns)/Rl1
      temp1 = 2.0*Sns/s4 + 0.5
      temp2 = 0.5 - (Sps+Sns)/s4
      c1a = Ai*Del
      c2a = -Ai*s1
      c3a = -c2a
      cexp1 = cexp(c1a*Sns)
      cexp2 = cexp(c2a*temp1)
      cexp3 = cexp(c3a*temp2)
      cexp1a = cexp(c1a*temp)
      cexp2a = cexp(c2a*temp/s4)
      const = t3*Fq7/2.0
      Xl = Sns
      DO Jl = 1 , Nl
         Pres4(Jl) = Pres4(Jl) - const*cexp1*(cexp2*((Sns+Xl)/s4-0.5)-cexp3*((Sps+Xl)/s4-1.5))
         Xl = Xl + temp
         cexp1 = cexp1*cexp1a
         cexp2 = cexp2*cexp2a
         cexp3 = cexp3*cexp2a
      ENDDO
      CALL subcc
      RETURN
   END SUBROUTINE spag_block_1
END SUBROUTINE subc
