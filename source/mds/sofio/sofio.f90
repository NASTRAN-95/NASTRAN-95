!*==sofio.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sofio(Isop,Iblknm,Buf)
   USE i_ginox
   USE i_dsiof
   USE c_sofcom
   USE c_sofdsn
   USE c_sys
   USE c_system
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Isop
   INTEGER :: Iblknm
   INTEGER , DIMENSION(10) :: Buf
!
! Local variable declarations rewritten by SPAG
!
   CHARACTER(80) :: dsname
   INTEGER :: ialloc , iblk , icerr , ifile , iop , isize , istat , k , lasfil , num , numblk
!
! End of declarations rewritten by SPAG
!
   IF ( lensof(1)==0 ) THEN
      numblk = 1
      IF ( lenwpb/=0 ) numblk = isysbf/lenwpb
      DO k = 1 , nfiles
         lensof(k) = 0
         dsname = sofdsn(k)
         CALL dsinqr(dsname,istat,isize)
         IF ( istat/=0 ) lensof(k) = filsiz(k)
      ENDDO
      lasfil = 0
   ENDIF
   IF ( Isop==7 ) THEN
      CALL dsclos(90)
      lasfil = 0
   ELSE
      iblk = Iblknm
      IF ( iblk>0 ) THEN
         ifile = 0
         DO k = 1 , nfiles
            IF ( iblk<=filsiz(k) ) THEN
               ifile = k
               GOTO 20
            ELSE
               iblk = iblk - filsiz(k)
            ENDIF
         ENDDO
         WRITE (iwr,99001) Iblknm
99001    FORMAT (' *** SUBSTRUCTURING ERROR - BLOCK NUMBER OUT OF RANGE ',' -  BLOCK NUMBER IS ',I10)
         WRITE (iwr,99002)
99002    FORMAT (//,' THE FOLLOWING SOF FILES WERE AVAILABLE',//)
         DO k = 1 , nfiles
            WRITE (iwr,99003) sofdsn(k) , filsiz(k) , lensof(k)
99003       FORMAT (' FILE ',A72' HAS ',I10,' BLOCKS - BLOCKS USED ',I10)
         ENDDO
         CALL mesage(-61,0,0)
 20      IF ( lasfil/=ifile ) THEN
            IF ( lasfil/=0 ) CALL dsclos(90)
            ialloc = numblk*filsiz(ifile)
            dsname = sofdsn(ifile)
            iop = 0
            CALL dsopen(dsname,90,iop)
            lasfil = ifile
         ENDIF
         IF ( Isop==1 ) THEN
            CALL dsread(90,Buf(4),nbuff,iblk)
         ELSE
            IF ( (iblk-lensof(ifile))>1 ) THEN
               num = iblk - lensof(ifile) - 1
               IF ( num/=0 ) THEN
                  DO k = 1 , num
                     lensof(ifile) = lensof(ifile) + 1
                     CALL dswrit(90,Buf(4),nbuff,lensof(ifile),icerr)
                     IF ( icerr/=0 ) GOTO 30
                  ENDDO
               ENDIF
            ENDIF
            CALL dswrit(90,Buf(4),nbuff,iblk,icerr)
            IF ( icerr==0 ) THEN
               IF ( iblk>lensof(ifile) ) lensof(ifile) = iblk
               IF ( Iblknm>hiblk ) hiblk = Iblknm
               RETURN
            ENDIF
 30         IF ( icerr==28 ) WRITE (iwr,99004)
99004       FORMAT (///,' INSUFFICIENT SPACE FOR SOF FILE ON DEFAULT',' DEVICE---JOB ABORTED.')
            IF ( icerr/=28 ) WRITE (iwr,99005)
99005       FORMAT (///,' I/O ERROR OCCURRED ON SOF FILE, JOB ABORTED')
            CALL mesage(-61,0,0)
         ENDIF
      ENDIF
   ENDIF
END SUBROUTINE sofio
