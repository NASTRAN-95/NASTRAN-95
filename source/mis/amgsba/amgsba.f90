!*==amgsba.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE amgsba(Ajjl,A0,Ar,Nsbe,A,Yb,Zb)
   IMPLICIT NONE
   USE C_AMGMN
   USE C_CONDAS
   USE C_DLBDY
   USE C_PACKX
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ajjl
   REAL , DIMENSION(1) :: A0
   REAL , DIMENSION(1) :: Ar
   INTEGER , DIMENSION(1) :: Nsbe
   REAL , DIMENSION(1) :: A
   REAL , DIMENSION(1) :: Yb
   REAL , DIMENSION(1) :: Zb
!
! Local variable declarations rewritten by SPAG
!
   REAL :: den
   INTEGER :: i , ib , ibuf1 , ibuf2 , ibuf3 , it , j , nbuf , nfsbeb , nfyb , nl , nlsbeb , ns1 , ns2 , ns5 , ntot
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL close , fread , gopen , mesage , pack , zeroc
!
! End of declarations rewritten by SPAG
!
!
!     BUILD AJJL FOR DOUBLET LATTICE WITH BODIES
!
   DATA name/4HAMGS , 4HBA  /
   Ii = Nrow + 1
   Nn = Nrow + Nj1
   IF ( Next+2*Nj1>Ecore ) CALL mesage(-8,0,name)
   Incr = 1
   Iti = 3
   Ito = 3
   IF ( Nt0/=0 ) THEN
      nbuf = 1
      IF ( Ntzs/=0 ) nbuf = nbuf + 1
      IF ( Ntys/=0 ) nbuf = nbuf + 1
      ibuf1 = Ecore - nbuf*Sysbuf
      ibuf2 = ibuf1
      IF ( Ntzs/=0 ) ibuf2 = ibuf1 + Sysbuf
      ibuf3 = ibuf2
      IF ( Ntys/=0 ) ibuf3 = ibuf2 + Sysbuf
      ns5 = Nt0*2
      ns1 = Ntzs*2
      ns2 = Ntys*2
      ntot = ns5 + ns1 + ns2
      IF ( Next+ntot>ibuf3 ) CALL mesage(-8,0,name)
!
!     BUILD PANEL AND BODY PART OF AJJL
!
      CALL gopen(Scr5,Z(ibuf1),0)
      IF ( Ntzs/=0 ) CALL gopen(Scr1,Z(ibuf2),0)
      IF ( Ntys/=0 ) CALL gopen(Scr2,Z(ibuf3),0)
      DO i = 1 , Nt0
         CALL fread(Scr5,A,ns5,0)
         IF ( Ntzs/=0 ) CALL fread(Scr1,A(ns5+1),ns1,0)
         IF ( Ntys/=0 ) CALL fread(Scr2,A(ns5+ns1+1),ns2,0)
         CALL pack(A,Ajjl,Mcb)
      ENDDO
      CALL close(Scr5,1)
      CALL close(Scr1,1)
      CALL close(Scr2,1)
   ENDIF
   CALL zeroc(A,2*Nj1)
!
!     ADD DIAGIONAL TERMS OF AJJL FOR SLENDER BODIES
!
   IF ( Ntzs/=0 .OR. Ntys/=0 ) THEN
      i = Nt0*2 + 1
      den = Twopi*2.0
      IF ( Ntzs/=0 ) THEN
         nfsbeb = 1
         nlsbeb = 0
         DO ib = 1 , Nbz
            nlsbeb = nlsbeb + Nsbe(ib)
            DO it = nfsbeb , nlsbeb
               A(i) = 1.0/(den*A0(it)**2)
               IF ( abs(Yb(ib))<.00001 ) A(i) = (1.0+float(Nd))*A(i)
               IF ( abs(Zb(ib))<.00001 ) A(i) = (1.0+float(Ne))*A(i)
               CALL pack(A,Ajjl,Mcb)
               A(i) = 0.0
               i = i + 2
            ENDDO
            nfsbeb = nfsbeb + Nsbe(ib)
         ENDDO
      ENDIF
      IF ( Ntys/=0 ) THEN
         nfyb = Nb + 1 - Nby
         nfsbeb = 1
         nlsbeb = 0
         nl = nfyb - 1
         IF ( nl/=0 ) THEN
            DO j = 1 , nl
               nlsbeb = nlsbeb + Nsbe(j)
               nfsbeb = nfsbeb + Nsbe(j)
            ENDDO
         ENDIF
         DO ib = nfyb , Nb
            nlsbeb = nlsbeb + Nsbe(ib)
            DO it = nfsbeb , nlsbeb
               A(i) = 1.0/(den*A0(it)**2)
               IF ( abs(Yb(ib))<.00001 ) A(i) = (1.0-float(Nd))*A(i)
               IF ( abs(Zb(ib))<.00001 ) A(i) = (1.0-float(Ne))*A(i)
               CALL pack(A,Ajjl,Mcb)
               A(i) = 0.0
               i = i + 2
            ENDDO
            nfsbeb = nfsbeb + Nsbe(ib)
         ENDDO
      ENDIF
   ENDIF
END SUBROUTINE amgsba
