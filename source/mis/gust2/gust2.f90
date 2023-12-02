!*==gust2.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gust2(Fol,Wj,Acpt,X0,V,Cstm,Qhjl)
   IMPLICIT NONE
   USE C_CONDAS
   USE C_SYSTEM
   USE C_ZBLPKX
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Fol
   INTEGER :: Wj
   INTEGER :: Acpt
   REAL :: X0
   REAL :: V
   INTEGER :: Cstm
   INTEGER :: Qhjl
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(13) :: acdr
   REAL :: am , freq , temp
   INTEGER :: buf1 , file , i , iacpt , ib , ib1 , ibs , ic , icg , icore , idelx , ix , ixic , ixs1 , ixs2 , j , jap , jcp , k ,   &
            & ks , meth , nb , nbei , nbes , nbxr , nfreq , nj , njg , nju , np , nr , nstrip , nto , ntp , ntys , ntzs , nwr
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(2) , SAVE :: nam
   INTEGER , SAVE :: nhacj , nhnju
   INTEGER , DIMENSION(7) :: trl
   EXTERNAL bldpk , bldpkn , bug , close , dmpfil , fread , gopen , korsz , mesage , open , rdtrl , read , wrttrl , zblpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     GUST2 MAKE  WJ(W) MATRIX FOR GUST
!
!
!
   !>>>>EQUIVALENCE (Z(1),Iz(1))
!
   DATA nam/4HGUST , 1H2/
   DATA nhnju , nhacj/4HNJU  , 4HACJ /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         icore = korsz(iz) - Sysbuf - 2
         buf1 = icore + 1
!
!     READ IN FREQUENCYS AND CONVERT TO OMEGA
!
         file = Fol
         CALL open(*100,Fol,Z(buf1),0)
         CALL fread(Fol,Z,-2,0)
         CALL read(*80,*20,Fol,Z,icore,0,nfreq)
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 20      DO i = 1 , nfreq
            Z(i) = Z(i)*Twopi
         ENDDO
         CALL close(Fol,1)
!
!     SPACE FOR COLUMN OF W - 2 * J  LONG  1 J FOR A  1 J FOR COEF.
!
         file = Qhjl
         trl(1) = Qhjl
         CALL rdtrl(trl)
         IF ( trl(1)<0 ) GOTO 100
         nj = trl(3)
         jap = nfreq
         jcp = jap + nj
         iacpt = jcp + nj + 1
         IF ( iacpt>icore ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO i = 1 , nj
            Z(jap+i) = 0.0
         ENDDO
!
!     SET UP WJ
!
         trl(1) = Wj
         trl(2) = 0
         trl(3) = nj
         trl(4) = 2
         trl(5) = 3
         trl(6) = 0
         trl(7) = 0
!
!     READ ACPT RECORDS BY METHOD AND FILL IN THE TWO COLUMNS
!     A =  COS G (CG) FOR DLB  1 FOR Z BODIES  0 FOR ALL ELSE
!     COEF =   XM  FOR PANELS AND BODIES
!
         CALL gopen(Acpt,Z(buf1),0)
         nju = 0
         file = Acpt
         DO
            CALL read(*40,*40,Acpt,meth,1,0,nwr)
            IF ( meth==2 ) THEN
!
!     DOUBLET LATTICE WITH BODIES
!
               CALL read(*80,*60,Acpt,acdr,13,0,nwr)
               njg = acdr(1)
               np = acdr(3)
               nb = acdr(4)
               ntp = acdr(5)
               nto = acdr(10)
               ntzs = acdr(11)
               ntys = acdr(12)
               nstrip = acdr(13)
               ic = iacpt
               ib = ic + np
               ib1 = ib + 2*np
               ibs = ib1 + 2*nb
               nr = 3*np + 3*nb
               CALL read(*80,*60,Acpt,Z(iacpt),nr,0,nwr)
               nbei = 0
               nbes = 0
               DO i = 1 , nb
                  nbei = nbei + iz(ib1+i-1)
                  nbes = nbes + iz(ibs+i-1)
               ENDDO
               icg = ib + np
               ix = icg + nstrip - 1
               ixs1 = ix + 4*ntp + 2*nbei + nbes
               ixs2 = ixs1 + nbes
               nr = 11*nb + 4*nstrip
               CALL read(*80,*60,Acpt,Z(icg),-nr,0,nwr)
               nr = nstrip + 4*ntp + 2*nbei + 3*nbes
               IF ( icg+nr>icore ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               CALL read(*80,*60,Acpt,Z(icg),nr,1,nwr)
               IF ( ntp/=0 ) THEN
                  k = 0
                  ks = 0
                  nbxr = iz(ic)
                  DO i = 1 , ntp
                     Z(jap+nju+i) = Z(icg+ks)
                     Z(jcp+nju+i) = Z(ix+i)
                     IF ( i/=ntp ) THEN
                        IF ( i==iz(ib+k) ) k = k + 1
                        IF ( i==nbxr ) THEN
                           ks = ks + 1
                           nbxr = nbxr + iz(ic+k)
                        ENDIF
                     ENDIF
                  ENDDO
               ENDIF
               nju = nju + nto
               IF ( ntzs/=0 ) THEN
                  DO i = 1 , ntzs
                     Z(jap+nju+i) = 1.0
                     Z(jcp+nju+i) = .5*(Z(ixs1+i)+Z(ixs2+i))
                  ENDDO
               ENDIF
               nju = nju + ntzs + ntys
            ELSEIF ( meth==3 .OR. meth==4 .OR. meth==5 ) THEN
!
!     MACH BOX  STRIP  PISTON  THEORIES
!
               CALL read(*80,*60,Acpt,njg,1,1,nwr)
               nju = nju + njg
            ELSE
!
!     DOUBLET LATTICE WITHOUT BODIES
!
               CALL read(*80,*60,Acpt,acdr,4,0,nwr)
               np = acdr(1)
               nstrip = acdr(2)
               njg = acdr(3)
               nr = 2*np + 5*nstrip + 2*njg
               IF ( iacpt+nr>icore ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               CALL read(*80,*60,Acpt,Z(iacpt),nr,1,nwr)
               ixic = iacpt + 2*np + 5*nstrip - 1
               idelx = ixic + njg
               icg = iacpt + 2*np + 4*nstrip
               k = 0
               ks = 0
               nbxr = iz(iacpt)
               DO i = 1 , njg
                  Z(jap+nju+i) = Z(icg+ks)
                  Z(jcp+nju+i) = Z(ixic+i) + .5*Z(idelx+i)
                  IF ( i/=njg ) THEN
                     IF ( i==iz(iacpt+np+k) ) k = k + 1
                     IF ( i==nbxr ) THEN
                        ks = ks + 1
                        nbxr = nbxr + iz(iacpt+k)
                     ENDIF
                  ENDIF
               ENDDO
               nju = nju + njg
            ENDIF
         ENDDO
 40      CALL close(Acpt,1)
         CALL bug(nhnju,100,nju,1)
         CALL bug(nhacj,100,Z(jap+1),2*nj)
         IF ( nju/=nj ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     BUILD WJ LOOP OVER ALL FREQUENCIES WITH AN INNER LOOP ON NJ
!
         CALL gopen(Wj,Z(buf1),1)
         DO i = 1 , nfreq
            freq = Z(i)
            CALL bldpk(3,3,Wj,0,0)
            DO j = 1 , nj
               am = Z(jap+j)
               IF ( am/=0.0 ) THEN
                  Irn = j
                  temp = freq*((Z(jcp+j)-X0)/V)
                  A(1) = cos(temp)*am
                  A(2) = -sin(temp)*am
                  CALL zblpki
               ENDIF
            ENDDO
            CALL bldpkn(Wj,0,trl)
         ENDDO
         CALL close(Wj,1)
         CALL wrttrl(trl)
         CALL dmpfil(-Wj,Z,icore)
         spag_nextblock_1 = 2
      CASE (2)
         RETURN
!
!     ERROR MESSAGES
!
 60      CALL mesage(-3,file,nam)
         spag_nextblock_1 = 3
      CASE (3)
         CALL mesage(-7,0,nam)
         spag_nextblock_1 = 4
      CASE (4)
         CALL mesage(-8,0,nam)
 80      CALL mesage(-2,file,nam)
 100     CALL mesage(-1,file,nam)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE gust2
