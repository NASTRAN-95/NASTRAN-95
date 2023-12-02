!*==sofint.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE sofint(Ib1,Ib2,Numb,Ibl1)
   IMPLICIT NONE
   USE c_itemdt
   USE c_machin
   USE c_sofcom
   USE c_sys
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ib1
   INTEGER :: Ib2
   INTEGER :: Numb
   INTEGER :: Ibl1
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: file , i , ibl , idiff , ihere1 , j , k , last , lastsz , lstsiz , max , maxnxt , maxold , min , n
   INTEGER , SAVE :: iempty , ird , iwrt
   INTEGER , DIMENSION(2) , SAVE :: name
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     CALLED ONCE BY EVERY RUN USING THE SOF UTILITY SUBROUTINES.
!     SHOULD BE CALLED BEFORE ANY OF THEM IS CALLED.  IF THE SOF IS
!     NOT EMPTY, SOME SECURITY CHECKS WILL BE TAKEN CARE OF, AND THE
!     SOF COMMON BLOCKS WILL BE UPDATED AND WRITTEN OUT ON THE FIRST
!     BLOCK OF EACH OF THE SOF FILES.  IF THE SOF IS EMPTY, THE DIT
!     MDI, AND ARRAY NXT WILL BE INITIALIZED AND WRITTEN OUT ON THE
!     THIRD, FOURTH, AND SECOND BLOCKS OF THE FIRST FILE OF THE SOF,
!     AND THE SOF COMMON BLOCKS WILL BE INITIALIZED AND WRITTEN OUT
!     ON THE FIRST BLOCK OF EACH OF THE SOF FILES.
!
!     THE FIRST BLOCK OF EACH OF THE SOF FILES CONTAINS THE FOLLOWING
!     INFORMATION
!       WORD                   WORD                   WORD
!      NUMBER  CONTENTS       NUMBER  CONTENTS       NUMBER  CONTENTS
!      ------  --------       ------  --------       ------  --------
!       1- 2   PASSWORD          26   DIRSIZ            32   MDIBL
!          3   FILE NUMBER       27   SUPSIZ            33   NXTTSZ
!          4   NFILES            28   AVBLKS         34-43   NXTFSZ
!       5-14   FILNAM            29   DITSIZ            44   NXTCUR
!      15-24   FILSIZ            30   DITNSB            45   NXTRST
!         25   BLKSIZ            31   DITBL             46   HIBLK
!                                                       47   IFRST
!
!     STARTING AT LOCATION 100 THE CONTENTS OF THE ITEMDT COMMON BLOCK
!     ARE STORED
!
!
   DATA ird , iwrt/1 , 2/
   DATA iempty , name/4H     , 4HSOFI , 4HNT  /
!
   IF ( ncpw>4 ) THEN
      n = nbpw - nbpc*4
      DO i = 1 , 10
         filnam(i) = lshift(rshift(filnam(i),n),n)
      ENDDO
   ENDIF
   IF ( nfiles<=0 ) THEN
!
!     ERROR MESSAGES.
!
      WRITE (nout,99001) sfm
99001 FORMAT (A25,' 6202.  THE REQUESTED NO. OF FILES IS NON POSITIVE.')
      CALL mesage(-37,0,name(1))
      RETURN
   ELSE
      IF ( status==0 ) THEN
!
!     THE SOF IS EMPTY.  INITIALIZE THE SOF COMMON BLOCKS WHICH ARE
!     STORED IN THE ARRAY A.
!     CHECK IF THE NASTRAN BUFFER SIZE IS LARGE ENOUGH
!
         min = 100 + 7*nitem + (nbuff-nsbuff)
         IF ( nbuff<min ) GOTO 600
         last = nbuff - 4
         hiblk = 0
         ifrst = 3
         DO i = 1 , last
            buf(Ib1+i) = 0
         ENDDO
         buf(Ib1+1) = psswrd(1)
         buf(Ib1+2) = psswrd(2)
         buf(Ib1+25) = nsbuff
         buf(Ib1+26) = nitem + ifrst - 1
         buf(Ib1+27) = 2*(buf(Ib1+25)-1)
         buf(Ib1+28) = -4
         DO i = 1 , nfiles
            buf(Ib1+28) = buf(Ib1+28) + filsiz(i)
         ENDDO
         buf(Ib1+29) = 0
         buf(Ib1+30) = 0
         buf(Ib1+31) = 3
         buf(Ib1+32) = 4
         buf(Ib1+33) = 1
         buf(Ib1+44) = 1
         buf(Ib1+45) = 0
         buf(Ib1+46) = 4
         buf(Ib1+47) = ifrst
!
         buf(Ib1+100) = nitem
         k = 100
         DO i = 1 , nitem
            DO j = 1 , 7
               buf(Ib1+k+j) = item(j,i)
            ENDDO
            k = k + 7
         ENDDO
!
!     INITIALIZE THE ARRAY NXT AND WRITE IT ON THE SECOND BLOCK OF THE
!     FIRST SOF FILE.
!
         DO i = 1 , last
            buf(Ib2+i) = 0
         ENDDO
         IF ( buf(Ib1+27)+1>filsiz(1) ) THEN
            IF ( mod(filsiz(1),2)==1 ) THEN
               max = (filsiz(1)-1)/2
               buf(Ib2+max+1) = lshift(filsiz(1),ihalf)
            ELSE
               max = filsiz(1)/2
            ENDIF
            buf(Ib2+1) = filsiz(1)
         ELSE
            max = buf(Ib1+25) - 1
            buf(Ib2+max+1) = lshift(buf(Ib1+27)+1,ihalf)
            buf(Ib2+1) = buf(Ib1+27) + 1
         ENDIF
         buf(Ib2+1) = orf(buf(Ib2+1),lshift(5,ihalf))
         buf(Ib2+2) = 0
         buf(Ib2+3) = 6
         DO i = 4 , max
            buf(Ib2+i) = 2*i
            buf(Ib2+i) = orf(buf(Ib2+i),lshift(2*i-1,ihalf))
         ENDDO
         CALL sofio(iwrt,1,buf(Ib2-2))
         CALL sofio(iwrt,2,buf(Ib2-2))
!
!     INITIALIZE THE DIT AND WRITE IT ON THE THIRD BLOCK OF THE FIRST
!     SOF FILE.
!
         DO i = 1 , last
            buf(Ib2+i) = iempty
         ENDDO
         CALL sofio(iwrt,3,buf(Ib2-2))
!
!     INITIALIZE THE MDI AND WRITE IT ON THE FOURTH BLOCK OF THE FIRST
!     SOF FILE.
!
         DO i = 1 , last
            buf(Ib2+i) = 0
         ENDDO
         CALL sofio(iwrt,4,buf(Ib2-2))
         Numb = 0
      ELSE
!
!     THE SOF IS NOT EMPTY.  READ THE FIRST BLOCK OF THE FIRST SOF FILE
!     AND VERIFY THE SECURITY VARIABLES.
!
         file = filnam(1)
         CALL sofio(ird,1,buf(Ib1-2))
         IF ( (buf(Ib1+1)/=psswrd(1)) .OR. (buf(Ib1+2)/=psswrd(2)) ) GOTO 100
         IF ( buf(Ib1+3)/=1 ) GOTO 200
         IF ( buf(Ib1+25)/=nsbuff ) THEN
!
            i = (nbuff-nsbuff) + buf(Ib1+25)
            WRITE (nout,99002) ufm , i
99002       FORMAT (A23,' 6205, SUBROUTINE SOFINT - THE BUFFER SIZE HAS BEEN',' MODIFIED.',/30X,                                    &
                   &'THE CORRECT NASTRAN PARAMETER IS BUFFSIZE = ',I6)
            GOTO 500
         ELSE
!
!     CHECK IF THE SPECIFIED NUMBER OF FILES AND THEIR SIZES IS ADEQUATE
!
            IF ( buf(Ib1+4)>=nfiles ) THEN
               max = nfiles - 1
            ELSE
               max = buf(Ib1+4) - 1
            ENDIF
            IF ( max>=1 ) THEN
               DO i = 1 , max
                  IF ( buf(Ib1+14+i)/=filsiz(i) ) THEN
                     file = filnam(i)
                     GOTO 300
                  ENDIF
               ENDDO
!
!     CHECK IF ALL SOF FILES HAVE THE CORRECT PASSWORD AND SEQUENCE
!     NUMBER
!
               max = max + 1
               ibl = 1
               DO i = 2 , max
                  file = filnam(i)
                  ibl = ibl + filsiz(i-1)
                  CALL sofio(ird,ibl,buf(Ib1-2))
                  IF ( (buf(Ib1+1)/=psswrd(1)) .OR. (buf(Ib1+2)/=psswrd(2)) ) GOTO 100
                  IF ( buf(Ib1+3)/=i ) GOTO 200
               ENDDO
               CALL sofio(ird,1,buf(Ib1-2))
               max = max - 1
            ENDIF
            IF ( buf(Ib1+14+max+1)==filsiz(max+1) ) THEN
               Numb = 0
            ELSE
               maxnxt = 0
               IF ( max>=1 ) THEN
                  DO i = 1 , max
                     maxnxt = maxnxt + buf(Ib1+33+i)
                  ENDDO
               ENDIF
               lastsz = (filsiz(max+1)-1)/buf(Ib1+27)
               IF ( filsiz(max+1)-1/=lastsz*buf(Ib1+27) ) lastsz = lastsz + 1
               maxnxt = maxnxt + lastsz
               IF ( buf(Ib1+33)>maxnxt ) GOTO 400
               maxold = maxnxt - lastsz + buf(Ib1+33+max+1)
               IF ( buf(Ib1+33)/=maxold ) THEN
                  Numb = 0
               ELSE
                  IF ( buf(Ib1+14+max+1)>filsiz(max+1) ) GOTO 400
                  lstsiz = mod(buf(Ib1+14+max+1)-2,buf(Ib1+27)) + 1
                  IF ( lstsiz==buf(Ib1+27) ) THEN
                     Numb = 0
                  ELSE
!
!     THE SIZE OF THE LAST SUPERBLOCK THAT WAS USED ON FILE (MAX+1)
!     SHOULD BE INCREASED.
!
                     IF ( filsiz(max+1)-buf(Ib1+14+max+1)>=buf(Ib1+27)-lstsiz ) THEN
                        Numb = buf(Ib1+27) - lstsiz
                     ELSE
                        Numb = filsiz(max+1) - buf(Ib1+14+max+1)
                     ENDIF
                     Ibl1 = 0
                     IF ( max>=1 ) THEN
                        DO i = 1 , max
                           Ibl1 = Ibl1 + filsiz(i)
                        ENDDO
                     ENDIF
                     Ibl1 = Ibl1 + buf(Ib1+14+max+1) + 1
                  ENDIF
               ENDIF
            ENDIF
!
!     UPDATE THE VARIABLE WHICH INDICATES THE NUMBER OF FREE BLOCKS ON
!     THE SOF.
!
            IF ( nfiles<buf(Ib1+4) ) THEN
               idiff = buf(Ib1+14+nfiles) - filsiz(nfiles)
               min = nfiles + 1
               last = buf(Ib1+4)
               DO i = min , last
                  idiff = idiff + buf(Ib1+14+i)
               ENDDO
            ELSEIF ( nfiles==buf(Ib1+4) ) THEN
               idiff = buf(Ib1+14+nfiles) - filsiz(nfiles)
            ELSE
               ihere1 = buf(Ib1+4)
               idiff = buf(Ib1+14+ihere1) - filsiz(ihere1)
               min = buf(Ib1+4) + 1
               DO i = min , nfiles
                  idiff = idiff - filsiz(i)
               ENDDO
            ENDIF
            buf(Ib1+28) = buf(Ib1+28) - idiff
!
!     IF NO ITEM STRUCTURE IS ON THE SOF (THE SOF WAS CREATED BEFORE
!     LEVEL 17.0) THEN USE THE LEVEL 16.0 ITEM STRUCTURE.
!
            IF ( buf(Ib1+100)<=0 .OR. buf(Ib1+100)>100 ) THEN
               WRITE (nout,99003) uwm
!
99003          FORMAT (A25,'6235, THE OLD SOF CONTAINS NO ITEM STRUCTURE ','INFORMATION.',/27X,                                     &
                     & 'THE LEVEL 16.0 ITEM STRUCTURE WILL ','BE USED.')
               buf(Ib1+47) = 3
               buf(Ib1+100) = 18
               k = 100
               DO i = 1 , 18
                  DO j = 1 , 7
                     buf(Ib1+k+j) = item(j,i)
                  ENDDO
                  k = k + 7
               ENDDO
!
!     CHECK IF THE DIRECTORY SIZE HAS BEEN CHANGED
!
            ELSEIF ( nitem/=buf(Ib1+100) ) THEN
               WRITE (nout,99004) uwm
!
99004          FORMAT (A25,' 6233, THE ITEM STRUCTURE HAS BEEN CHANGED FOR THE ','SOF.',/32X,                                       &
                      &'NEW CAPABILITIES USING THESE ITEMS MAY NOT ','BE USED WITH THIS SOF.')
            ENDIF
         ENDIF
      ENDIF
!
!     UPDATE THE COMMON BLOCKS USED BY THE SOF UTILITY SUBROUTINES.
!
      buf(Ib1+4) = nfiles
      DO i = 1 , nfiles
         buf(Ib1+4+i) = filnam(i)
         buf(Ib1+14+i) = filsiz(i)
         buf(Ib1+33+i) = (filsiz(i)-1)/buf(Ib1+27)
         IF ( filsiz(i)-1/=buf(Ib1+33+i)*buf(Ib1+27) ) buf(Ib1+33+i) = buf(Ib1+33+i) + 1
      ENDDO
!
!     WRITE THE UPDATED ARRAY A ON THE FIRST BLOCK OF EACH OF THE SOF
!     FILES.
!
      ibl = 1
      DO i = 1 , nfiles
         buf(Ib1+3) = i
         CALL sofio(iwrt,ibl,buf(Ib1-2))
         ibl = ibl + filsiz(i)
      ENDDO
!
!     PRINT MESSAGE INDICATING THE STATUS OF THE CURRENT SOF FILES.
!
      WRITE (nout,99005) sim , nfiles
99005 FORMAT (A31,' 6201,',I3,' FILES HAVE BEEN ALLOCATED TO THE SOF ','WHERE --')
      DO i = 1 , nfiles
         WRITE (nout,99006) i , filsiz(i)
99006    FORMAT (18H     SIZE OF FILE ,I2,3H = ,I10,7H BLOCKS)
      ENDDO
      WRITE (nout,99007) buf(Ib1+25)
99007 FORMAT (32H     AND WHERE A BLOCK CONTAINS ,I4,6H WORDS)
      RETURN
   ENDIF
!
 100  WRITE (nout,99008) ufm , file
99008 FORMAT (A23,' 6206, SUBROUTINE SOFINT - WRONG PASSWORD ON SOF ','FILE ',A4,1H.)
   GOTO 500
!
 200  WRITE (nout,99009) ufm , file
99009 FORMAT (A23,' 6207, SUBROUTINE SOFINT - THE SOF FILE ',A4,' IS OUT OF SEQUENCE.')
   GOTO 500
!
 300  WRITE (nout,99010) ufm , file
99010 FORMAT (A23,' 6208, SUBROUTINE SOFINT - THE SIZE OF THE SOF FILE ',A4,' HAS BEEN MODIFIED.')
   GOTO 500
!
 400  WRITE (nout,99011) ufm , file
99011 FORMAT (A23,' 6209, SUBROUTINE SOFINT - THE NEW SIZE OF FILE ',A4,' IS TOO SMALL.')
 500  CALL mesage(-61,0,0)
!
 600  WRITE (nout,99012) ufm , min
99012 FORMAT (A23,' 6234, THE NASTRAN BUFFER SIZE IS TO SMALL FOR THE',' SOF FILE.',/30X,'MINIMUM BUFFER SIZE IS ',I10)
   GOTO 500
!
END SUBROUTINE sofint
