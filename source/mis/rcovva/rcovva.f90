!*==rcovva.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE rcovva(In,Intyp,Outt,Outu,Outv,Outa,Ssnm,Rz,Dz,Cz)
   IMPLICIT NONE
   USE C_BLANK
   USE C_CONDAS
   USE C_NAMES
   USE C_PACKX
   USE C_RCOVCM
   USE C_RCOVCR
   USE C_SYSTEM
   USE C_TYPE
   USE C_UNPAKX
   USE C_XMSSG
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: In
   INTEGER :: Intyp
   INTEGER :: Outt
   INTEGER :: Outu
   INTEGER :: Outv
   INTEGER :: Outa
   INTEGER , DIMENSION(2) :: Ssnm
   REAL , DIMENSION(4) :: Rz
   REAL*8 , DIMENSION(1) :: Dz
   COMPLEX , DIMENSION(2) :: Cz
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: file , i , iprec , item , j , k , n , ncol , nrow , nt , nwcol , nwds , nword , rc
   REAL :: freq , iscale , rscale
   INTEGER , DIMENSION(15) :: inblk , inblk1 , inblk2 , inblk3 , oblk1 , oblk2 , oblk3 , outblk
   REAL*8 :: ival , rval
   INTEGER , DIMENSION(7) :: mcb , mcba , mcbu , mcbv
   INTEGER , DIMENSION(2) , SAVE :: name
   COMPLEX :: scale
   INTEGER , SAVE :: soln , srd
   INTEGER , DIMENSION(4) :: temp
!
! End of declarations rewritten by SPAG
!
!
!     THIS SUBROUTINE COMPUTES THE VELOCITIES AND ACCELERATIONS FOR
!     FOR A GIVEN DISPLACEMENT VECTOR
!
!     INTYP = 0   IN CONTAINS U ONLY AND V AND A ARE CALCULATED
!     INTYP = 1   U CONTAINS U, V AND A SO THEY ARE SPLIT ONTO OUTU,
!                 OUTV AND OUTA
!     INTYP =-1   OUTU, OUTV AND OUTA ARE MERGED ONTO OUTT
!
   !>>>>EQUIVALENCE (temp(1),scale,rscale) , (temp(2),iscale) , (inblk1(1),oblk1(1)) , (inblk2(1),oblk2(1)) , (inblk3(1),oblk3(1)) ,     &
!>>>>    & (outblk(1),inblk(1))
   DATA name/4HRCOV , 4HVA  /
   DATA srd/1/
   DATA soln/4HSOLN/
!
!     GET DISPLACEMENT TRAILER AND DETERMINE TYPE
!
   IF ( Outt/=0 .AND. Outu+Outv+Outa/=0 .AND. Intyp>=0 ) THEN
      n = 7
      CALL mesage(n,file,name)
      GOTO 500
   ELSE
!
      file = In
      IF ( Intyp<0 ) file = Outu
      mcb(1) = file
      CALL rdtrl(mcb)
      IF ( mcb(1)<0 ) THEN
         n = 1
         CALL mesage(n,file,name)
         GOTO 500
      ELSE
         ncol = mcb(2)
         nrow = mcb(3)
         iprec = mcb(5)
         nword = Nwords(iprec)
         nwcol = nrow*nword
!
!     SET UP PACK UNPACK COMMONS
!
         Itinu = iprec
         Iru = 1
         Nru = nrow
         Incru = 1
         Itinp = iprec
         Itoutp = iprec
         Irp = 1
         Nrp = nrow
         Incrp = 1
!
!     BRANCH ON TYPE OF DISPLACEMENTS OR RIGID FORMAT
!
         IF ( Intyp<=0 ) THEN
            IF ( Intyp<0 ) THEN
!
!     THE DISPLACEMENTS, VELOCITIES AND ACCLERATIONS ALREADY EXIST AND
!     ARE TO BE MERGED TOGETHER
!
               IF ( Lcore<0 ) GOTO 300
               CALL gopen(Outu,Rz(Buf1),Rdrew)
               CALL gopen(Outv,Rz(Buf2),Rdrew)
               CALL gopen(Outa,Rz(Buf3),Rdrew)
               CALL gopen(Outt,Rz(Buf4),Wrtrew)
!
               inblk1(1) = Outu
               inblk2(1) = Outv
               inblk3(1) = Outa
               outblk(1) = Outt
!
               j = 1
               DO i = 1 , ncol
                  CALL cpystr(inblk1,outblk,0,j)
                  j = j + 1
                  CALL cpystr(inblk2,outblk,0,j)
                  j = j + 1
                  CALL cpystr(inblk3,outblk,0,j)
                  j = j + 1
               ENDDO
!
               CALL close(Outu,Rew)
               CALL close(Outv,Rew)
               CALL close(Outa,Rew)
               CALL close(Outt,Rew)
               mcb(1) = Outt
               mcb(2) = ncol*3
               CALL wrttrl(mcb)
               GOTO 50
!
            ELSEIF ( Rfno>9 ) THEN
               n = 7
               CALL mesage(n,file,name)
               GOTO 500
            ELSE
               IF ( Rfno==1 .OR. Rfno==2 ) GOTO 50
               IF ( Rfno==4 .OR. Rfno==5 .OR. Rfno==6 .OR. Rfno==7 ) THEN
                  n = 7
                  CALL mesage(n,file,name)
                  GOTO 500
               ELSEIF ( Rfno==8 ) THEN
               ELSEIF ( Rfno==9 ) THEN
                  GOTO 20
!
!     NORMAL MODES
!
!     CHECK IF VECTORS ARE COMPLEX
!
               ELSEIF ( iprec<3 ) THEN
!
!     REAL NORMAL MODES
!
!     V =  U*OMEGA
!     A = -V*OMEGA
!
                  IF ( Lcore<nwcol ) GOTO 300
                  item = soln
                  CALL sfetch(Ssnm,soln,srd,rc)
                  IF ( rc/=1 ) GOTO 100
                  n = 1
                  CALL sjump(n)
                  IF ( n<0 ) THEN
                     CALL smsg(7,item,Ssnm)
                     GOTO 500
                  ELSE
!
                     CALL gopen(In,Rz(Buf1),Rdrew)
                     IF ( Outt/=0 ) CALL gopen(Outt,Rz(Buf2),Wrtrew)
                     IF ( Outu/=0 ) CALL gopen(Outu,Rz(Buf2),Wrtrew)
                     IF ( Outv/=0 ) CALL gopen(Outv,Rz(Buf3),Wrtrew)
                     IF ( Outa/=0 ) CALL gopen(Outa,Rz(Buf4),Wrtrew)
                     CALL makmcb(mcb,Outt,nrow,Rect,iprec)
                     CALL makmcb(mcbu,Outu,nrow,Rect,iprec)
                     CALL makmcb(mcbv,Outv,nrow,Rect,iprec)
                     CALL makmcb(mcba,Outa,nrow,Rect,iprec)
!
!     LOOP THROUGH EACH COLUMN
!
                     DO i = 1 , ncol
!
!     GET SCALE FACTOR FOR THIS COLUMN
!
                        CALL suread(Rz,7,nwds,rc)
                        IF ( rc/=1 ) GOTO 200
                        rscale = Rz(4)
!
                        CALL unpack(*2,In,Rz)
                        GOTO 4
 2                      DO j = 1 , nwcol
                           Rz(j) = 0.0
                        ENDDO
 4                      IF ( Outt/=0 ) CALL pack(Rz(1),Outt,mcb)
                        IF ( Outu/=0 ) CALL pack(Rz(1),Outu,mcbu)
!
                        DO j = 1 , 2
                           IF ( iprec==2 ) THEN
!
                              DO k = 1 , nrow
                                 Dz(k) = rscale*Dz(k)
                              ENDDO
                           ELSE
!
                              DO k = 1 , nrow
                                 Rz(k) = rscale*Rz(k)
                              ENDDO
                           ENDIF
!
                           IF ( Outt/=0 ) CALL pack(Rz(1),Outt,mcb)
                           IF ( Outv/=0 .AND. j==1 ) CALL pack(Rz(1),Outv,mcbv)
                           IF ( Outa/=0 .AND. j==2 ) CALL pack(Rz(1),Outa,mcba)
!
                           rscale = -rscale
                        ENDDO
                     ENDDO
!
!
                     CALL close(In,Rew)
                     IF ( Outt/=0 ) CALL close(Outt,Rew)
                     IF ( Outu/=0 ) CALL close(Outu,Rew)
                     IF ( Outv/=0 ) CALL close(Outv,Rew)
                     IF ( Outa/=0 ) CALL close(Outa,Rew)
                     IF ( Outt/=0 ) CALL wrttrl(mcb)
                     IF ( Outu/=0 ) CALL wrttrl(mcbu)
                     IF ( Outv/=0 ) CALL wrttrl(mcbv)
                     IF ( Outa/=0 ) CALL wrttrl(mcba)
                     GOTO 50
                  ENDIF
               ENDIF
!
!     COMPLEX NORMAL MODES
!
!     V = U*POLE
!     A = V*POLE
!
!     FREQUENCY RESPONSE
!
!     V = U*TWOPHI*FREQ*I
!     A = V*TWOPHI*FREQ*I
!
               IF ( Lcore<nwcol ) GOTO 300
               item = soln
               CALL sfetch(Ssnm,soln,srd,rc)
               IF ( rc/=1 ) GOTO 100
               n = 1
               CALL sjump(n)
               IF ( n<0 ) THEN
                  CALL smsg(7,item,Ssnm)
                  GOTO 500
               ELSE
!
                  CALL gopen(In,Rz(Buf1),Rdrew)
                  IF ( Outt/=0 ) CALL gopen(Outt,Rz(Buf2),Wrtrew)
                  IF ( Outu/=0 ) CALL gopen(Outu,Rz(Buf2),Wrtrew)
                  IF ( Outv/=0 ) CALL gopen(Outv,Rz(Buf3),Wrtrew)
                  IF ( Outa/=0 ) CALL gopen(Outa,Rz(Buf4),Wrtrew)
                  CALL makmcb(mcb,Outt,nrow,Rect,iprec)
                  CALL makmcb(mcbu,Outu,nrow,Rect,iprec)
                  CALL makmcb(mcbv,Outv,nrow,Rect,iprec)
                  CALL makmcb(mcba,Outa,nrow,Rect,iprec)
!
!     LOOP THROUGH EACH COLUMN
!
                  DO i = 1 , ncol
!
!     GET SCALE FACTOR FOR THIS COLUMN
!
                     IF ( Rfno==8 ) THEN
                        CALL suread(freq,1,nwds,rc)
                        IF ( rc/=1 ) GOTO 200
                        scale = Twophi*freq*(0.0,1.0)
!
                        CALL unpack(*6,In,Cz(1))
                        GOTO 8
                     ELSE
                        CALL suread(Cz(1),7,nwds,rc)
                        IF ( rc/=1 ) GOTO 200
                        scale = Cz(2)
                        CALL unpack(*6,In,Cz(1))
                        GOTO 8
                     ENDIF
 6                   DO j = 1 , nwcol
                        Rz(j) = 0.0
                     ENDDO
 8                   IF ( Outt/=0 ) CALL pack(Cz(1),Outt,mcb)
                     IF ( Outu/=0 ) CALL pack(Cz(1),Outu,mcbu)
!
                     DO j = 1 , 2
                        IF ( iprec>3 ) THEN
!
                           nt = nrow*2
                           DO k = 1 , nt , 2
                              rval = Dz(k)
                              ival = Dz(k+1)
                              Dz(k) = rscale*rval - iscale*ival
                              Dz(k+1) = rscale*ival + iscale*rval
                           ENDDO
                        ELSE
!
                           DO k = 1 , nrow
                              Cz(k) = scale*Cz(k)
                           ENDDO
                        ENDIF
!
                        IF ( Outt/=0 ) CALL pack(Cz(1),Outt,mcb)
                        IF ( Outv/=0 .AND. j==1 ) CALL pack(Cz(1),Outv,mcbv)
                        IF ( Outa/=0 .AND. j==2 ) CALL pack(Cz(1),Outa,mcba)
!
                     ENDDO
!
                  ENDDO
!
                  CALL close(In,Rew)
                  IF ( Outt/=0 ) CALL close(Outt,Rew)
                  IF ( Outu/=0 ) CALL close(Outu,Rew)
                  IF ( Outv/=0 ) CALL close(Outv,Rew)
                  IF ( Outa/=0 ) CALL close(Outa,Rew)
                  IF ( Outt/=0 ) CALL wrttrl(mcb)
                  IF ( Outu/=0 ) CALL wrttrl(mcbu)
                  IF ( Outv/=0 ) CALL wrttrl(mcbv)
                  IF ( Outa/=0 ) CALL wrttrl(mcba)
                  GOTO 50
               ENDIF
            ENDIF
         ENDIF
!
!     THE DISPLACEMENT FILE ALREADY CONTAINS THE VELOCITIES AND
!     ACCELERATIONS SO WE JUST SANT TO SPLIT THEM UP
!
 20      IF ( Lcore<0 ) GOTO 300
         CALL gopen(In,Rz(Buf1),Rdrew)
         IF ( Outu/=0 ) CALL gopen(Outu,Rz(Buf2),Wrtrew)
         IF ( Outv/=0 ) CALL gopen(Outv,Rz(Buf3),Wrtrew)
         IF ( Outa/=0 ) CALL gopen(Outa,Rz(Buf4),Wrtrew)
!
         inblk(1) = In
         oblk1(1) = Outu
         oblk2(1) = Outv
         oblk3(1) = Outa
         file = In
         ncol = ncol/3
!
         DO i = 1 , ncol
            IF ( Outu/=0 ) CALL cpystr(inblk,oblk1,0,i)
            IF ( Outu==0 ) CALL fwdrec(*400,In)
            IF ( Outv/=0 ) CALL cpystr(inblk,oblk2,0,i)
            IF ( Outv==0 ) CALL fwdrec(*400,In)
            IF ( Outa/=0 ) CALL cpystr(inblk,oblk3,0,i)
            IF ( Outa==0 ) CALL fwdrec(*400,In)
         ENDDO
!
         CALL close(In,Rew)
         IF ( Outu/=0 ) CALL close(Outu,Rew)
         IF ( Outv/=0 ) CALL close(Outv,Rew)
         IF ( Outa/=0 ) CALL close(Outa,Rew)
         mcb(2) = ncol
         mcb(1) = Outu
         IF ( Outu/=0 ) CALL wrttrl(mcb)
         mcb(1) = Outv
         IF ( Outv/=0 ) CALL wrttrl(mcb)
         mcb(1) = Outa
         IF ( Outa/=0 ) CALL wrttrl(mcb)
      ENDIF
!
!     NORMAL RETURN
!
 50   RETURN
   ENDIF
!
!     ERRORS
!
 100  IF ( rc==6 ) THEN
      CALL mesage(n,file,name)
   ELSE
      CALL smsg(rc-2,item,Ssnm)
   ENDIF
   GOTO 500
 200  CALL smsg(rc+4,item,Ssnm)
   GOTO 500
 300  WRITE (Nout,99001) Swm , Rss
99001 FORMAT (A25,' 6313, INSUFFICIENT CORE FOR RCOVR MODULE WHILE ','TRYING TO PROCESS',/34X,'PRINTOUT DATA BLOCKS FOR ',          &
             &'SUBSTRUCTURE ',2A4)
   GOTO 500
 400  n = 2
   CALL mesage(n,file,name)
 500  In = 0
   CALL close(In,Rew)
   IF ( Outt/=0 ) CALL close(Outt,Rew)
   IF ( Outu/=0 ) CALL close(Outu,Rew)
   IF ( Outv/=0 ) CALL close(Outv,Rew)
   IF ( Outa/=0 ) CALL close(Outa,Rew)
!
END SUBROUTINE rcovva
