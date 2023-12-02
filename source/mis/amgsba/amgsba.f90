!*==amgsba.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE amgsba(Ajjl,A0,Ar,Nsbe,A,Yb,Zb)
   USE c_amgmn
   USE c_condas
   USE c_dlbdy
   USE c_packx
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
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
   ii = nrow + 1
   nn = nrow + nj1
   IF ( next+2*nj1>ecore ) CALL mesage(-8,0,name)
   incr = 1
   iti = 3
   ito = 3
   IF ( nt0/=0 ) THEN
      nbuf = 1
      IF ( ntzs/=0 ) nbuf = nbuf + 1
      IF ( ntys/=0 ) nbuf = nbuf + 1
      ibuf1 = ecore - nbuf*sysbuf
      ibuf2 = ibuf1
      IF ( ntzs/=0 ) ibuf2 = ibuf1 + sysbuf
      ibuf3 = ibuf2
      IF ( ntys/=0 ) ibuf3 = ibuf2 + sysbuf
      ns5 = nt0*2
      ns1 = ntzs*2
      ns2 = ntys*2
      ntot = ns5 + ns1 + ns2
      IF ( next+ntot>ibuf3 ) CALL mesage(-8,0,name)
!
!     BUILD PANEL AND BODY PART OF AJJL
!
      CALL gopen(scr5,z(ibuf1),0)
      IF ( ntzs/=0 ) CALL gopen(scr1,z(ibuf2),0)
      IF ( ntys/=0 ) CALL gopen(scr2,z(ibuf3),0)
      DO i = 1 , nt0
         CALL fread(scr5,A,ns5,0)
         IF ( ntzs/=0 ) CALL fread(scr1,A(ns5+1),ns1,0)
         IF ( ntys/=0 ) CALL fread(scr2,A(ns5+ns1+1),ns2,0)
         CALL pack(A,Ajjl,mcb)
      ENDDO
      CALL close(scr5,1)
      CALL close(scr1,1)
      CALL close(scr2,1)
   ENDIF
   CALL zeroc(A,2*nj1)
!
!     ADD DIAGIONAL TERMS OF AJJL FOR SLENDER BODIES
!
   IF ( ntzs/=0 .OR. ntys/=0 ) THEN
      i = nt0*2 + 1
      den = twopi*2.0
      IF ( ntzs/=0 ) THEN
         nfsbeb = 1
         nlsbeb = 0
         DO ib = 1 , nbz
            nlsbeb = nlsbeb + Nsbe(ib)
            DO it = nfsbeb , nlsbeb
               A(i) = 1.0/(den*A0(it)**2)
               IF ( abs(Yb(ib))<.00001 ) A(i) = (1.0+float(nd))*A(i)
               IF ( abs(Zb(ib))<.00001 ) A(i) = (1.0+float(ne))*A(i)
               CALL pack(A,Ajjl,mcb)
               A(i) = 0.0
               i = i + 2
            ENDDO
            nfsbeb = nfsbeb + Nsbe(ib)
         ENDDO
      ENDIF
      IF ( ntys/=0 ) THEN
         nfyb = nb + 1 - nby
         nfsbeb = 1
         nlsbeb = 0
         nl = nfyb - 1
         IF ( nl/=0 ) THEN
            DO j = 1 , nl
               nlsbeb = nlsbeb + Nsbe(j)
               nfsbeb = nfsbeb + Nsbe(j)
            ENDDO
         ENDIF
         DO ib = nfyb , nb
            nlsbeb = nlsbeb + Nsbe(ib)
            DO it = nfsbeb , nlsbeb
               A(i) = 1.0/(den*A0(it)**2)
               IF ( abs(Yb(ib))<.00001 ) A(i) = (1.0-float(nd))*A(i)
               IF ( abs(Zb(ib))<.00001 ) A(i) = (1.0-float(ne))*A(i)
               CALL pack(A,Ajjl,mcb)
               A(i) = 0.0
               i = i + 2
            ENDDO
            nfsbeb = nfsbeb + Nsbe(ib)
         ENDDO
      ENDIF
   ENDIF
END SUBROUTINE amgsba
