
SUBROUTINE smc2cd(Zi,Zd,Zil,Zol,Nar,Lasrow,Dtemp,I1,I2,I3)
   IMPLICIT NONE
   INCLUDE 'SMCOMX.COM'
   INTEGER I1 , I2 , I3
   DOUBLE COMPLEX Dtemp(I3) , Zil(I1,I2) , Zol(I2,2)
   INTEGER Lasrow(I2) , Nar(I3) , Zi(10)
   DOUBLE PRECISION Zd(10)
   INTEGER i , iavail , ic1 , ic2 , iilcol , iilrow , iilrow1 , iilrowb , ilim1 , ilim2 , incr , itest , ix2 , j , jdir , jlim1 ,   &
         & jlim2 , jlim4 , jm2 , jmidx , jrowl , jvtmp , k , kbc , kdir , kfrcolg , kfrcolp , kk , klscolp , km2 , kmidx , kr ,     &
         & kridxs , krow1 , krowb , krowsb , kvidx , limit , lrow1 , lrown , missin , nrows , num , nzeros
   DOUBLE COMPLEX zoltmp
!
! ZIL    = INNER LOOP TERMS (SIZE = MAXNAC * (MAXNCOL+NEXTRA)
! ZOL    = OUTER LOOP TERMS (SIZE = (MAXNCOL+NEXTRA) * 2)
! NAR    = SAVE AREA FOR ACTIVE ROWS OF PREVIOUS COLUMN
! I1     = MAXIMUM NUMBER OF ACTIVE ROWS FOR THIS COLUMN
! I2     = NUMBER OF COLUMNS ALLOCATED FOR STORAGE OF INNER AND
!          NUMBER OF ROWS ALLOCATED FOR OUTER LOOP
! I3     = MAXIMUM NUMBER OF WORDS FOR DEFINING THE ACTIVE ROWS FOR
!          ANY COLUMN
! LASROW = LAST NON-ZERO ROW INDEX FOR A GIVEN COLUMN (SIZE = MAXNCOL
!          +NEXTRA)
!
!
! GET ROW VALUES CORRESPONDING TO THE ACTIVE ROWS OF COLUMN K FOR
! EACH COLUMN KFRCOL THROUGH KLSCOL IN ORDER TO FILL INNER LOOP AND
! OUTER LOOP AREAS.
!
! BEGIN TO PROCESS EACH COLUMN
! FOR COLUMN K, GET OUTER LOOP TERMS
!    A(K,J) / A(J,J)
!       K = CURRENT PIVOTAL COLUMN
!       J = RANGES FROM FIRST COLUMN DATA NEEDED FOR COLUMN K TO K-1
!    (E.G.,
!         A(5,1)/A(1,1)
!         A(5,2)/A(2,2)
!         A(5,3)/A(3,3)
!         A(5,4)/A(4,4)
! ALSO, GET INNER LOOP TERMS
!    A(I,J)
!       K = CURRENT PIVOTAL COLUMN
!       I = RANGES FROM K TO LAST ACTIVE ROW OF COLUMN K
!       J = RANGES FROM FIRST COLUMN DATA NEEDED FOR COLUMN K TO K-1
!    (E.G.,
!         A(5,1) A(6,1)  .  A(N,1)
!         A(5,2) A(6,2)  .  A(N,2)
!         A(5,3) A(6,3)  .  A(N,3)
!         A(5,4) A(6,4)  .  A(N,4)
!
!  LOOP 7000 WILL BE ON K
!  LOOP 6000 WILL BE ON J
!
   ic1 = 1
   ic2 = 2
   iilrow1 = 1
!      print *,' i1,i2,i3,maxncol,maxnac=',i1,i2,i3,maxncol,maxnac
   DO k = 1 , Ncol
      kk = mod(k,I2)
      IF ( kk==0 ) kk = I2
      Lasrow(kk) = 0
!      PRINT *,' SMC2CD PROCESSING COLUMN K=',K
      Kcol = k
      kdir = k*4 - 3
      kmidx = Zi(kdir)
!
! SEE IF DATA IS ON IN MEMORY OR ON THE SPILL FILE
!
      IF ( kmidx==0 ) THEN
!
! DATA IS ON THE SPILL FILE
!
         CALL smcspl(Kcol,Zi)
         kmidx = Zi(kdir)
      ENDIF
      kfrcolp = Kfrcol
      klscolp = Klscol
      Kfrcol = Zi(kdir+1)
      km2 = Zi(kmidx+1)
      Kridxn = kmidx + 4 + km2
      Klscol = k - 1
      Kridx = kmidx + 4
      kridxs = Kridx
      krow1 = Zi(Kridx)
      Krown = krow1 + Zi(Kridx+1) - 1
      Karows = 0
      DO kk = 1 , km2 , 2
         Karows = Karows + Zi(Kridx+kk)
      ENDDO
!
! IF THE PREVIOUS COLUMN DID NOT NEED DATA FROM A COLUMN PRECEEDING IT,
! THEN MUST RELOAD THE INNER AND OUTER LOOP ARRAYS
!
      IF ( klscolp>=kfrcolp ) THEN
!
! NOW MUST FIND THE ROW AND COLUMN NUMBER FOR THIS PIVOT COLUMN
! THAT IS NOT ALREADY IN THE INNER LOOP AND OUTER LOOP ARRAYS.
! FIRST CHECK THAT THE FIRST REQUIRED ROW IS STORED, IF NOT THEN WE MUST
! BEGIN AS IF NOTHING STORED.  IF SOME OF THE REQUIRED ROWS ARE PRESENT,
! THEN FIND THE NEXT POSITION AND ROW NUMBER TO BE STORED IN THE INNER
! LOOP ARRAY AND THE NEXT POSITION AND COLUMN NUMBER TO BE STORED IN THE
! OUTER LOOP ARRAY.
!
! IF THE FIRST COLUMN IS LESS THAN FIRST COLUMN OF LAST PIVOT COLUMN
! THEN WE MUST LOAD THE INNER AND OUTER LOOPS FROM THE BEGINNING
!
         IF ( Kfrcol>=kfrcolp ) THEN
            kr = 1
            lrow1 = Nar(1)
            lrown = Nar(1) + Nar(2) - 1
!
!  LROW1 = FIRST ROW OF A STRING OF CONTIGUOUS ROWS OF LAST PIVOT
!          COLUMN PROCESSED
!  LROWN = LAST ROW OF A STRING OF CONTIGUOUS ROWS OF LAST PIVOT COLUMN
!          PROCESSED
!
! FIND FIRST ROW IN INNER LOOP THAT MATCHES THE FIRST ROW REQUIRED
! FOR THIS COLUMN
!
! IF THERE IS NO MATCH FOR THE FIRST COLUMN, THEN GO TO 1350
!
            DO WHILE ( lrow1<=krow1 )
               IF ( krow1<lrown ) THEN
!
! THERE IS AN OVERLAP, SET KROWB, KROWSB, AND IILROW1 TO REFLECT
! THE PROPER ROW NUMBER IN THE INNER LOOP
!
                  incr = krow1 - lrow1
                  krowb = krow1
                  krowsb = Krown - krowb + 1
                  kridxs = Kridx
                  iilrow1 = iilrow1 + incr
                  IF ( iilrow1>I1 ) iilrow1 = iilrow1 - I1
                  lrow1 = krow1
                  iilrow = iilrow1
                  DO WHILE ( lrow1==krow1 )
                     IF ( lrown/=Krown ) THEN
                        IF ( lrown<Krown ) THEN
!
! LAST ROW NUMBERS DO NOT MATCH, KROWN GT LROWN
!
                           incr = lrown - krowb + 1
                           GOTO 2
                        ELSEIF ( lrown>Krown ) THEN
!
! LAST ROW NUMBERS DO NOT MATCH, KROWN LT LROWN
!
                           incr = lrown - lrow1 + 1
                           GOTO 2
                        ENDIF
                     ENDIF
!
! THIS SET OF ROWS MATCHES, GO AND CHECK THE NEXT SET OF ROW NUMBERS
!
                     incr = Krown - krowb + 1
                     iilrow = iilrow + incr
                     IF ( iilrow>I1 ) iilrow = iilrow - I1
                     Kridx = Kridx + 2
                     IF ( Kridx==Kridxn ) THEN
!
! ROWS MATCH FOR INNER LOOP COLUMN VALUES, NOW DETERMINE THE COLUMN INDEX
! FOR THE NEXT COLUMN TO ADD TO THE INNER AND OUTER LOOP ARRAYS.
                        kfrcolg = klscolp + 1
                        iilrow = iilrow1
                        GOTO 50
                     ELSE
                        kr = kr + 2
                        krow1 = Zi(Kridx)
                        krowb = krow1
                        krowsb = Zi(Kridx+1)
                        Krown = krow1 + krowsb - 1
                        kridxs = Kridx
                        lrow1 = Nar(kr)
                        lrown = lrow1 + Nar(kr+1) - 1
                        IF ( lrow1==0 ) THEN
                           kfrcolg = Kfrcol
                           GOTO 50
                        ENDIF
                     ENDIF
                  ENDDO
!
! NOT ALL NEEDED ROW VALUES ARE PRESENT, MUST GET NEEDED ROWS
! FOR ALL COLUMNS REQUIRED FOR THIS PIVOT COLUMN
!
                  kfrcolg = Kfrcol
                  GOTO 50
 2                krowb = krowb + incr
                  krowsb = krowsb - incr
                  kridxs = Kridx
                  iilrow = iilrow + incr
                  IF ( iilrow>I1 ) iilrow = iilrow - I1
                  kfrcolg = Kfrcol
                  GOTO 50
               ELSE
!
! NO OVERLAP WITH THIS STRING, GO AND GET NEXT STRING
! ADJUST 'ILLROW1' WHICH IS THE POINTER TO THE FIRST ROW IN THE INNER
! LOOP THAT CONTAINS THE VALUE OF ROW "KROW1" OF EACH COLUMN.
!
                  incr = lrown - lrow1 + 1
                  iilrow1 = iilrow1 + incr
                  IF ( iilrow1>I1 ) iilrow1 = iilrow1 - I1
                  kr = kr + 2
                  lrow1 = Nar(kr)
                  IF ( lrow1==0 ) EXIT
                  lrown = lrow1 + Nar(kr+1) - 1
               ENDIF
            ENDDO
         ENDIF
      ENDIF
!
! NO MATCH FOUND, WILL START LOADING THE INNER AND OUTER LOOP ARRAYS
! FROM THE BEGINNING
!
      iilrow1 = 1
      iilrow = 1
      krowb = krow1
      krowsb = Krown - krow1 + 1
      kfrcolg = Kfrcol
 50   Kridx = kmidx + 4
      DO j = 1 , km2
         Nar(j) = Zi(Kridx+j-1)
      ENDDO
      Nar(km2+1) = 0
      iilrowb = iilrow
!
! KFRCOL  = FIRST COLUMN NEEDED FOR PIVOT COLUMN "K"
! KLSCOL  = LAST COLUMN NEEDED FOR PIVOT COLUMN "K"
! KFRCOLG = FIRST COLUMN TO BE PLACED IN INNER/OUTER LOOP ARRAYS
! KFRCOLP = FIRST COLUMN OF LAST PIVOT COLUMN PROCESSED
! KLSCOLP = LAST COLUMN OF LAST PIVOT COLUMN PROCESSED
!
!      PRINT *,' KFRCOL,KLSCOL,KFRCOLG,KFRCOLP,KLSCOLP,KAROWS='
!      PRINT *,  KFRCOL,KLSCOL,KFRCOLG,KFRCOLP,KLSCOLP,KAROWS
!      PRINT *,' KROWB,KROWSB,IILROW1,IILROW,kridx='
!      PRINT *,  KROWB,KROWSB,IILROW1,IILROW,kridx
!
! KLSCOL WILL BE LESS THAN KFRCOLG FOR THE FIRST COLUMN AND FOR ANY
! COLUMN THAT DOES NOT NEED A PRECEEDING COLUMN OF DATA
!
      IF ( Klscol>=kfrcolg ) THEN
         DO j = kfrcolg , Klscol
            iilcol = mod(j,I2)
            IF ( iilcol==0 ) iilcol = I2
            Jcol = j
            jdir = j*4 - 3
            jmidx = Zi(jdir)
!
! SEE IF COLUMN DATA IS IN MEMORY OR ON THE SPILL FILE
!
            IF ( jmidx==0 ) THEN
!
! DATA IS ON THE SPILL FILE
!
               CALL smcspl(Jcol,Zi)
               IF ( Zi(jdir)==0 ) jmidx = Ispill
               IF ( Zi(jdir)/=0 ) jmidx = Zi(jdir)
            ENDIF
            Jridx = jmidx + 4
            jm2 = Zi(jmidx+1)
            Jridxn = Jridx + jm2
            jrowl = Zi(Jridx+jm2-2) + Zi(Jridx+jm2-1) - 1
            Jvidx = Jridxn
!
! SAVE DIAGONAL TERM FOR COLUMN J ; (ALWAYS, THE FIRST TERM)
!
            Jvidx = Jvidx/2 + 1
            Zol(iilcol,ic2) = (1.0D0,0.0D0)/cmplx(Zd(Jvidx),Zd(Jvidx+1))
!
! FOR EACH COLUMN J, GET REQUIRED ROWS; I.E, ACTIVE ROWS OF COLUMN K
!
            IF ( j>klscolp ) THEN
!
!  MUST RESET KRIDX, KROW AND KROWS FOR INSERTION OF NEW COLUMN IN INNER LOOP
!
               Kridx = kmidx + 4
               Krow = Zi(Kridx)
               Krows = Zi(Kridx+1)
               iilrow = iilrow1
            ELSE
!
! SET VARIABLES FOR ADDING ROW TERMS TO AN EXISTING COLUMN IN THE INNER LOOP
!
               Kridx = kridxs
               Krow = krowb
               Krows = krowsb
               iilrow = iilrowb
!
! SET LASROW TO ZERO IF THIS COLUMN IS BEING RELOADED INTO ZIL AND NOT
! BEING ADDED TO FROM SOME PREVIOUS COLUMN PROCESSING.
!
               IF ( iilrowb==iilrow1 ) Lasrow(j) = 0
            ENDIF
            Krown = Krow + Krows - 1
!
! JROWL IS LAST ROW TERM IN COLUMN "J".  IF THIS IS BEFORE THE FIRST ROW
! "KROW" TERM NEEDED, THEN NO MORE TERMS ARE NEEDED FROM COLUMN "J" AND
! "LASROW" WILL INDICATE THE LAST VALUE STORED FOR COLUMN "J".
!
            IF ( jrowl<Krow ) CYCLE
 60         Jrow = Zi(Jridx)
            Jrows = Zi(Jridx+1)
            Jrown = Jrow + Jrows - 1
            DO WHILE ( Jrown>=Krow )
               IF ( Jrow>Krown ) THEN
!
! STORE ZEROS FOR CREATED TERMS AND INCREMENT TO THE NEXT SET OF
! OF ROWS FOR THIS PIVOTAL COLUMN.
!
                  iavail = I1 - (iilrow+Krows-1)
                  IF ( iavail<0 ) THEN
                     ilim2 = Krows - (I1-iilrow+1)
                     DO i = iilrow1 , I1
                        Zil(i,iilcol) = (0.0D0,0.0D0)
                     ENDDO
                     DO i = 1 , ilim2
                        Zil(i,iilcol) = (0.0D0,0.0D0)
                     ENDDO
                     iilrow = ilim2 + 1
                  ELSE
                     DO i = 1 , Krows
                        Zil(iilrow+i-1,iilcol) = (0.0D0,0.0D0)
                     ENDDO
                     iilrow = iilrow + Krows
                  ENDIF
               ELSE
                  missin = Krow - Jrow
!
! CHECK TO SEE IF THERE ARE MISSING TERMS, I.E., TERMS CREATED DURING
! THE DECOMPOSITION.  IF THERE ARE MISSING TERMS, THEN SET THEIR VALUES
! TO BE INITIALLY ZERO.
!
                  IF ( missin<0 ) THEN
                     nzeros = iabs(missin)
!
!  STORE "NZEROS" NUMBER OF ZEROS FOR INNER LOOP TERMS
!
                     iavail = I1 - (iilrow+nzeros-1)
                     IF ( iavail<0 ) THEN
                        ilim1 = I1 - iilrow + 1
                        ilim2 = nzeros - ilim1
                        DO i = 1 , ilim1
                           Zil(iilrow+i-1,iilcol) = (0.0D0,0.0D0)
                        ENDDO
                        DO i = 1 , ilim2
                           Zil(i,iilcol) = (0.0D0,0.0D0)
                        ENDDO
                        iilrow = ilim2 + 1
                     ELSE
                        DO i = 1 , nzeros
                           Zil(iilrow+i-1,iilcol) = (0.0D0,0.0D0)
                        ENDDO
                        iilrow = iilrow + nzeros
                     ENDIF
                     Krow = Krow + nzeros
                     Krows = Krows - nzeros
                  ENDIF
                  IF ( missin>0 ) THEN
                     Iskip = Krow - Jrow
                     Jvidx = Jvidx + Iskip*2
                     Jrow = Jrow + Iskip
                  ENDIF
                  Irown = min0(Krown,Jrown)
                  num = Irown - Krow + 1
!
!  MOVE INNER LOOP VALUES FROM IN-MEMORY LOCATION TO
!  THE INNER LOOP AREA
!
                  nrows = Irown - Krow + 1
                  IF ( nrows>(I1-iilrow+1) ) THEN
                     ilim1 = I1 - iilrow + 1
                     ilim2 = nrows - ilim1
                     DO i = 1 , ilim1
                        ix2 = i*2
                        Zil(iilrow+i-1,iilcol) = cmplx(Zd(Jvidx+ix2-2),Zd(Jvidx+ix2-1))
                     ENDDO
                     jvtmp = Jvidx + ilim1*2
                     DO i = 1 , ilim2
                        ix2 = i*2
                        Zil(i,iilcol) = cmplx(Zd(jvtmp+ix2-2),Zd(jvtmp+ix2-1))
                     ENDDO
                     iilrow = ilim2 + 1
                  ELSE
                     DO i = 1 , nrows
                        ix2 = i*2
                        Zil(iilrow+i-1,iilcol) = cmplx(Zd(Jvidx+ix2-2),Zd(Jvidx+ix2-1))
                     ENDDO
                     iilrow = iilrow + nrows
                  ENDIF
                  Lasrow(iilcol) = iilrow
!
! IF ALL OF THE ROWS ARE NON-ZERO, SET LASROW COUNTER TO IILROW1
!
                  IF ( iilrow==iilrow1 ) Lasrow(iilcol) = iilrow1
                  Jvidx = Jvidx + nrows*2
                  Jrow = Jrow + nrows
                  Krow = Irown + 1
                  Krows = Krown - Irown
!
! INCREMENT EITHER KROW OR JROW DEPENDING UPON WHETHER IROWN = JROWN
! OR IROWN = KROWN
!
                  IF ( Irown==Jrown ) GOTO 70
               ENDIF
!
! INCREMENT THE INDEX TO THE NEXT SET OF ROWS FOR COLUMN "K"
!
               Kridx = Kridx + 2
!
! IF THERE ARE NO MORE ROWS FOR THIS COLUMN, THEN COLUMN IS COMPLETE
!
               IF ( Kridx>=Kridxn ) GOTO 80
               Krow = Zi(Kridx)
               Krows = Zi(Kridx+1)
               Krown = Krow + Krows - 1
            ENDDO
!
! INCREMENT "JVIDX" TO POINT TO THE CORRESPONDING VALUE TERM FOR THE
! NEXT ROW OF COLUMN "J"
!
            Jvidx = Jvidx + (Jrown-Jrow+1)*2
!
! INCREMENT THE INDEX TO THE NEXT SET OF ROWS FOR COLUMN "J"
!
 70         Jridx = Jridx + 2
            IF ( Jridx<Jridxn ) GOTO 60
 80      ENDDO
         IF ( k/=1 ) THEN
!
! COMPUTE THE TERMS FOR THE CURRENT COLUMN OF DATA
!
!      do 100 k = 1,n
!         do 10  i = k,n
!         temp = 0.
!         do 5  l = 1,k-1
!            temp = temp + a(i,l)*a(k,l) / a(l,l)
!    5       continue
!         a(i,k) = a(i,k) - temp
!   10    continue
!
!  THE FOLLOWING LAST COMPUTATION TAKES PLACE IN SUBROUTINE SMCOUT.
!  THE RESULTS OF THE DIVISION ARE WRITTEN TO THE OUTPUT FILE BUT
!  THE RESULTS OF THE ABOVE (WITHOUT THE DIVISION BELOW) IS
!  MAINTAINED IN MEMORY FOR REMAINING COLUMN COMPUTATIONS.
!
!         do 11 j = k+1,n
!           a(k,j) = a(j,k) / a( k,k )
!   11      continue
!  100 continue
!
!   NROWS  = NUMBER OF ROWS STORED IN INNER LOOP
!   KCOL   = LAST COLUMN NUMBER STORED IN INNER LOOP
!   KFRCOL = FIRST COLUMN NUMBER STORED IN INNER LOOP
!
            nrows = Karows
            kdir = (Kcol-1)*4 + 1
            kmidx = Zi(kdir)
            Kridx = kmidx + 4
            km2 = Zi(kmidx+1)
            kvidx = Kridx + km2
            kvidx = (kvidx/2) + 1
            ilim1 = iilrow1 + nrows - 1
            ilim2 = 0
            iavail = I1 - ilim1
            IF ( iavail<0 ) THEN
               ilim1 = I1
               ilim2 = nrows - (I1-iilrow1+1)
            ENDIF
            jlim1 = mod(Kfrcol,I2)
            jlim2 = mod(Klscol,I2)
            IF ( jlim1==0 ) jlim1 = I2
            IF ( jlim2==0 ) jlim2 = I2
            jlim4 = 0
            IF ( Kfrcol/=k ) THEN
               IF ( jlim2<jlim1 ) THEN
                  jlim4 = jlim2
                  jlim2 = I2
               ENDIF
!      PRINT *,' JLIM1,JLIM2,JLIM4,IILROW1=',JLIM1,JLIM2,JLIM4,IILROW1
!      PRINT *,' ILIM1,ILIM2,JLIM1,JLIM2,JLIM4,IILROW1,NROWS'
!      PRINT *,  ILIM1,ILIM2,JLIM1,JLIM2,JLIM4,IILROW1,NROWS
               IF ( k/=1 ) THEN
!
! COMPUTE THE OUTER LOOP TERM FOR THIS COLUMN J
! I.E.,   -A(K,J) / A(J,J)
!  where K = current pivot column number; J = column being processed
!
!     KAROWS = NUMBER OF ACTIVE ROWS FOR THE CURRENT PIVOTAL COLUMN
!     JCOL   = COLUMN NUMBER OF CURRENT PIVOTAL COLUMN
!     ZOL(KBC,IC1) = FIRST ACTIVE ROW ("IILROW1") TERM OF COLUMN "KBC"
!     ZOL(KBC,IC2) = DIAGONAL TERM FOR COLUMN "KBC"
!
                  DO kbc = jlim1 , jlim2
                     Zol(kbc,ic1) = Zil(iilrow1,kbc)*Zol(kbc,ic2)
                  ENDDO
                  IF ( jlim4/=0 ) THEN
                     DO kbc = 1 , jlim4
                        Zol(kbc,ic1) = Zil(iilrow1,kbc)*Zol(kbc,ic2)
                     ENDDO
                  ENDIF
               ENDIF
!      CALL KBHELPCD( KFRCOL, KLSCOL, ZOL, ZIL, I1, I2, LASROW )
               DO i = iilrow1 , ilim1
                  Dtemp(i) = (0.0D0,0.0D0)
               ENDDO
!
! PROCESS COLUMNS JLIM1 THROUGH JLIM2
!
               DO j = jlim1 , jlim2
                  limit = ilim1
                  itest = Lasrow(j)
                  IF ( itest/=0 ) THEN
                     IF ( itest>iilrow1 ) limit = itest - 1
!
! PROCESS ROWS IILROW1 THROUGH LIMIT FOR COLUMNS JLIM1 THROUGH JLIM2
!
                     zoltmp = Zol(j,ic1)
                     CALL smcccd(Dtemp(iilrow1),Zil(iilrow1,j),limit-iilrow1+1,zoltmp)
                  ENDIF
!      DO 4020 I = IILROW1, LIMIT
!      DTEMP(I) = DTEMP(I) + ZIL( I, J ) * ZOLTMP
!4020  CONTINUE
               ENDDO
               IF ( jlim4/=0 ) THEN
!
! PROCESS ROWS IILROW1 THROUGH LIMIT FOR COLUMNS 1 THROUGH JLIM4
!
                  DO j = 1 , jlim4
                     itest = Lasrow(j)
                     IF ( itest/=0 ) THEN
                        limit = ilim1
                        IF ( itest>iilrow1 ) limit = itest - 1
                        zoltmp = Zol(j,ic1)
                        CALL smcccd(Dtemp(iilrow1),Zil(iilrow1,j),limit-iilrow1+1,zoltmp)
                     ENDIF
!      DO 4023 I = IILROW1, LIMIT
!      DTEMP(I) = DTEMP(I) + ZIL( I, J ) * ZOLTMP
!4023  CONTINUE
                  ENDDO
               ENDIF
               IF ( ilim2/=0 ) THEN
                  DO i = 1 , ilim2
                     Dtemp(i) = (0.0D0,0.0D0)
                  ENDDO
!
! PROCESS COLUMNS JLIM1 THROUGH JLIM2
!
                  DO j = jlim1 , jlim2
                     itest = Lasrow(j)
                     IF ( itest/=0 .AND. itest<=iilrow1 ) THEN
                        limit = ilim2
                        IF ( itest<=ilim2 ) limit = itest - 1
!
! PROCESS ROWS 1 THROUGH LIMIT FOR COLUMNS JLIM1 THROUGH JLIM2
!
                        zoltmp = Zol(j,ic1)
                        CALL smcccd(Dtemp(1),Zil(1,j),limit,zoltmp)
                     ENDIF
!      DO 4040 I = 1, LIMIT
!      DTEMP(I) = DTEMP(I) + ZIL( I, J ) * ZOLTMP
!4040  CONTINUE
                  ENDDO
                  IF ( jlim4/=0 ) THEN
!
! PROCESS ROWS 1 THROUGH LIMIT FOR COLUMNS 1 THROUGH JLIM4
!
                     DO j = 1 , jlim4
                        itest = Lasrow(j)
                        IF ( itest/=0 .AND. itest<=iilrow1 ) THEN
                           limit = ilim2
                           IF ( itest<=ilim2 ) limit = itest - 1
                           zoltmp = Zol(j,ic1)
                           CALL smcccd(Dtemp(1),Zil(1,j),limit,zoltmp)
                        ENDIF
!      DO 4043 I = 1, LIMIT
!      DTEMP(I) = DTEMP(I) + ZIL( I, J ) * ZOLTMP
!4043  CONTINUE
                     ENDDO
                  ENDIF
               ENDIF
!
! UPDATE EACH ACTIVE ROW TERM FOR COLUMN "K" BY SUBTRACTING "DTEMP"
!
               DO i = iilrow1 , ilim1
                  Zd(kvidx) = Zd(kvidx) - dreal(Dtemp(i))
                  Zd(kvidx+1) = Zd(kvidx+1) - dimag(Dtemp(i))
                  kvidx = kvidx + 2
               ENDDO
               IF ( ilim2/=0 ) THEN
                  DO i = 1 , ilim2
                     Zd(kvidx) = Zd(kvidx) - dreal(Dtemp(i))
                     Zd(kvidx+1) = Zd(kvidx+1) - dimag(Dtemp(i))
                     kvidx = kvidx + 2
                  ENDDO
               ENDIF
            ENDIF
         ENDIF
      ENDIF
!
! CALL SMCOUT TO WRITE OUT THE COLUMN TO THE OUTPUT LOWER TRIANGULAR
! MATRIX FILE
!
      CALL smcout(Zi,Zi,Zd,Zol(1,ic1),Zol(1,ic1))
   ENDDO
END SUBROUTINE smc2cd
