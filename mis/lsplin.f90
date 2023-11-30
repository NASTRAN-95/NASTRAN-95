
SUBROUTINE lsplin(Ni,Xyi,Nd,Xyd,Ky,Kd,Kt,Dz,Dtx,Dty,Dtor,G,Ncore,Isng)
   IMPLICIT NONE
   REAL Dtor , Dtx , Dty , Dz
   INTEGER Isng , Kd , Kt , Ky , Ncore , Nd , Ni
   REAL G(1) , Xyd(1) , Xyi(1)
   REAL aym , aymd , ayp , aypd , det , dtor2 , ey , temp , ym , yp
   LOGICAL bnone , bone , both , ikt , nethr , oxr , oyr , sone , spec , stwo
   INTEGER i , ia , ib , ic , icb , icc , ig , ii , ik , is , j , k , kk , mp , name(2) , nc , nca , nca2 , ncb , ncc , needed ,    &
         & nii , nj , nsc , size
!
   DATA name/4HLSPL , 4HIN  /
   spec = .FALSE.
   bone = .FALSE.
   bnone = .FALSE.
   sone = .FALSE.
   stwo = .FALSE.
   oxr = .FALSE.
   oyr = .FALSE.
   both = .FALSE.
   ikt = .FALSE.
   nethr = .TRUE.
   ey = float(Ky)
!
!     KY EFFECTS RIGID BODY ROWS AND COLUMNS OF A AND ROWS OF B
!
!     DTX AND DTY EFFECT ROWS AND COLUMNS OF A AND ROWS OF B
!
!     KD EFFECTS COLUMNS OF B
!
!     SPEC GUARDS AGAINST SINGULAR MATRIX FOR ROTATIONS WITHOUT Y
!     CONSTRAINED
!
   IF ( Kd==0 ) bnone = .TRUE.
   IF ( Kd==1 ) bone = .TRUE.
   IF ( Ky==-1 ) sone = .TRUE.
   IF ( Ky==1 ) stwo = .TRUE.
   IF ( Kt==1 ) ikt = .TRUE.
   IF ( Dtx<0.0 ) oxr = .TRUE.
   IF ( Dty<0.0 ) oyr = .TRUE.
   IF ( oxr .AND. oyr ) both = .TRUE.
   IF ( .NOT.oxr .AND. .NOT.oyr ) nethr = .FALSE.
   dtor2 = Dtor/2.0
   nsc = 3
   IF ( Ky==1 ) nsc = 2
   IF ( Ky==-1 ) nsc = 1
   size = 3
   IF ( oxr ) size = size - 1
   IF ( oyr ) size = size - 1
   IF ( oyr .AND. Ky>-1 ) THEN
      temp = Xyi(1)
      spec = .TRUE.
      nii = 2*Ni
      DO i = 1 , nii , 2
         IF ( Xyi(i)/=temp ) spec = .FALSE.
      ENDDO
   ENDIF
   nca = size*Ni + nsc
   IF ( spec ) nca = nca - 1
   nca2 = 2*nca
   ncb = (Kd+1)*Nd
   ncc = size*Ni
!
!     CORE NEEDED
!                A         G        INVERS
   needed = nca*nca + ncb*ncc + 3*nca
!                                   B
   IF ( ikt ) needed = needed + ncb*nca
!                                        C
   IF ( .NOT.ikt ) needed = needed + ncc*nca
   IF ( needed>Ncore ) CALL mesage(-8,0,name)
   is = Ncore - 3*nca - 1
   ig = 1
!
!     IF KT = 1  COMPUTE  B THEN A  THEN  C IN THE SPACE OF A
!
!     IF KT = 0 COMPUTE  C THEN A  THEN  B IN THE SPACE OF A
!
   IF ( ikt ) THEN
!
!     B MATRIX
!
      ib = ncb*ncc
      mp = ib + 1
      GOTO 200
   ELSE
!
!     FILL IN C MATRIX
!
      ic = ncb*ncc
      mp = ic + 1
   ENDIF
 100  DO i = 1 , ncc
      DO j = 1 , nca
         ic = ic + 1
         G(ic) = 0.0
         IF ( i==j ) G(ic) = 1.0
      ENDDO
   ENDDO
   IF ( ikt ) THEN
      CALL gmmats(G(mp),ncb,nca,0,G(icc),ncc,nca,1,G(ig))
      GOTO 99999
   ELSE
      nc = ncc
      ia = ic
      GOTO 300
   ENDIF
 200  nj = 2*Nd
   nii = 2*Ni
   DO j = 1 , nj , 2
      DO i = 1 , nii , 2
         ym = Xyd(j+1) - Xyi(i+1)
         aym = abs(ym)
         aymd = aym*dtor2
         yp = (Xyd(j+1)+Xyi(i+1))
         ayp = abs(yp)*ey
         aypd = ayp*dtor2
         ib = ib + 1
         G(ib) = aym**3/12.0 - Xyd(j)*Xyi(i)*aymd + ayp**3/12.0 - Xyd(j)*Xyi(i)*aypd
         IF ( .NOT.(bnone) ) THEN
            G(ib+nca) = aym*ym/4.0 + ayp*yp/4.0
            IF ( .NOT.(bone) ) G(ib+nca2) = Xyi(i)*aymd + Xyi(i)*aypd
         ENDIF
         IF ( .NOT.(both) ) THEN
            IF ( .NOT.(oxr) ) THEN
               ib = ib + 1
               G(ib) = -aym*ym/4.0 + ayp*yp/4.0
               IF ( .NOT.(bnone) ) THEN
                  G(ib+nca) = -aym/2.0 + ayp/2.0
                  IF ( .NOT.(bone) ) G(ib+nca2) = 0.0
               ENDIF
               IF ( oyr ) CYCLE
            ENDIF
            ib = ib + 1
            G(ib) = Xyd(j)*aymd + Xyd(j)*aypd
            IF ( .NOT.(bnone) ) THEN
               G(ib+nca) = 0.0
               IF ( .NOT.(bone) ) G(ib+nca2) = -aymd - aypd
            ENDIF
         ENDIF
      ENDDO
      ib = ib + 1
      IF ( sone ) THEN
         G(ib) = Xyd(j+1)
         IF ( .NOT.(bnone) ) THEN
            G(ib+nca) = 1.0
            IF ( .NOT.(bone) ) G(ib+nca2) = 0.0
         ENDIF
      ELSE
         G(ib) = 1.0
         IF ( .NOT.(bnone) ) THEN
            G(ib+nca) = 0.0
            IF ( .NOT.(bone) ) G(ib+nca2) = 0.0
         ENDIF
         IF ( .NOT.(stwo) ) THEN
            ib = ib + 1
            G(ib) = Xyd(j+1)
            IF ( .NOT.(bnone) ) THEN
               G(ib+nca) = 1.0
               IF ( .NOT.(bone) ) G(ib+nca2) = 0.0
            ENDIF
         ENDIF
         IF ( .NOT.(spec) ) THEN
            ib = ib + 1
            G(ib) = -Xyd(j)
            IF ( .NOT.(bnone) ) THEN
               G(ib+nca) = 0.0
               IF ( .NOT.(bone) ) G(ib+nca2) = 1.0
            ENDIF
         ENDIF
      ENDIF
      ib = ib + Kd*nca
   ENDDO
   IF ( .NOT.ikt ) THEN
      CALL gmmats(G(mp),ncc,nca,0,G(icb),ncb,nca,1,G(ig))
      GOTO 99999
   ELSE
      ia = ib
      nc = ncb
   ENDIF
!
!     A MATRIX
!
 300  nii = 2*Ni
   k = ia
!
!     ZERO A
!
   ii = k + 1
   ik = ii + nca*nca
   DO i = ii , ik
      G(i) = 0.0
   ENDDO
   ii = 0
   DO i = 1 , nii , 2
      DO j = i , nii , 2
         k = k + 1
         yp = (Xyi(i+1)+Xyi(j+1))
         ayp = abs(yp)*ey
         aypd = ayp*dtor2
         ym = Xyi(i+1) - Xyi(j+1)
         aym = abs(ym)
         aymd = aym*dtor2
         G(k) = aym**3/12.0 - Xyi(i)*Xyi(j)*aymd + ayp**3/12.0 - Xyi(i)*Xyi(j)*aypd
         IF ( i==j ) G(k) = G(k) + Dz
         IF ( .NOT.(both) ) THEN
            IF ( oxr ) THEN
               G(k+nca) = Xyi(i)*aymd + Xyi(i)*aypd
               k = k + 1
               G(k) = Xyi(j)*aymd + Xyi(j)*aypd
               G(k+nca) = -aymd - aypd
               IF ( i==j ) G(k+nca) = G(k+nca) + Dty
            ELSE
               G(k+nca) = aym*ym/4.0 + ayp*yp/4.0
               IF ( oyr ) THEN
                  k = k + 1
                  G(k) = -aym*ym/4.0 + ayp*yp/4.0
                  G(k+nca) = -aym/2.0 + ayp/2.0
                  IF ( i==j ) G(k+nca) = G(k+nca) + Dtx
               ELSE
                  G(k+nca2) = Xyi(i)*aymd + Xyi(i)*aypd
                  k = k + 1
                  G(k) = -aym*ym/4.0 + ayp*yp/4.0
                  G(k+nca) = -aym/2.0 + ayp/2.0
                  IF ( i==j ) G(k+nca) = G(k+nca) + Dtx
                  G(k+nca2) = 0.0
                  k = k + 1
                  G(k) = Xyi(j)*aymd + Xyi(j)*aypd
                  G(k+nca) = 0.0
                  G(k+nca2) = -aymd - aypd
                  IF ( i==j ) G(k+nca2) = G(k+nca2) + Dty
               ENDIF
            ENDIF
         ENDIF
      ENDDO
      k = k + 1
      IF ( sone ) THEN
         G(k) = Xyi(i+1)
         IF ( .NOT.(both) ) THEN
            IF ( oxr ) G(k+nca) = 0.0
            IF ( oyr ) G(k+nca) = 1.0
            IF ( .NOT.(nethr) ) THEN
               G(k+nca) = 1.0
               G(k+nca2) = 0.0
            ENDIF
         ENDIF
      ELSE
         G(k) = 1.0
         IF ( .NOT.(both) ) THEN
            G(k+nca) = 0.0
            IF ( .NOT.(nethr) ) G(k+nca2) = 0.0
         ENDIF
         IF ( .NOT.(stwo) ) THEN
            k = k + 1
            G(k) = Xyi(i+1)
            IF ( .NOT.(both) ) THEN
               IF ( oxr ) G(k+nca) = 0.0
               IF ( oyr ) G(k+nca) = 1.0
               IF ( .NOT.(nethr) ) THEN
                  G(k+nca) = 1.0
                  G(k+nca2) = 0.0
               ENDIF
            ENDIF
         ENDIF
         IF ( .NOT.(spec) ) THEN
            k = k + 1
            G(k) = -Xyi(i)
            IF ( .NOT.(both) ) THEN
               IF ( oxr ) G(k+nca) = 1.0
               IF ( oyr ) G(k+nca) = 0.0
               IF ( .NOT.(nethr) ) THEN
                  G(k+nca) = 0.0
                  G(k+nca2) = 1.0
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      ii = ii + 1
      k = k + size*ii + (size-1)*nca
   ENDDO
!
!     LOWER TRIANGLE IF A STORED TRANSPOSE INTO UPPER TRIANGLE
!
   k = ia
   DO i = 1 , nca
      DO j = i , nca
         k = k + 1
         kk = k + (nca-1)*(j-i)
         G(kk) = G(k)
      ENDDO
      k = k + i
   ENDDO
!
!     CALL  INVERSE   A-1 C  OR  A-1 B
!
!     REPLACE CALLS TO INVAER WITH CALLS TO INVERS.
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
   Isng = -1
   CALL invers(nca,G(ia+1),nca,G(mp),nc,det,Isng,G(is))
   IF ( Isng/=2 ) THEN
!
!     ADJUST INDEXES TO A AND A-1 RESULT
!
      ib = ia
      icb = ib + 1
      IF ( .NOT.ikt ) GOTO 200
      ic = ia
      icc = ic + 1
      GOTO 100
   ENDIF
99999 RETURN
END SUBROUTINE lsplin
