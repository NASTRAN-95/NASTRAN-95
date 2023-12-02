!*==flbprt.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE flbprt(Iuset,Ieqex,Ibuf)
   USE c_bitpos
   USE c_flbfil
   USE c_system
   USE c_two
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iuset
   INTEGER :: Ieqex
   INTEGER :: Ibuf
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: astric , blank , dash
   INTEGER :: d32 , d33 , expnt , file , i , ibit , icol , idof , imk , inpnt , inum , itype , iu , juset , k , kk , n , ndof ,     &
            & neqex , nz
   INTEGER , DIMENSION(17) :: msk , sbit , upbit
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , DIMENSION(3,9) , SAVE :: title
   INTEGER , DIMENSION(10) :: zdof , zgrd
   EXTERNAL andf , close , fwdrec , mesage , open , page1 , read , sort , sswtch
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     HYDROELEASTIC USET OUTPUT
!
!     PRINTS DOF VS. DISP SETS IF DIAG 32 IS ON.
!     PRINTS DISP SETS VS. DOF IF DIAG 33 IS ON.
!
   DATA name/4HFLBP , 4HRT  /
   DATA title/4H     , 4H     , 4H MPC , 4H     , 4H     , 4H SPC , 4H     , 4H     , 4HOMIT , 4H     , 4HANAL , 4HYSIS , 4H     ,  &
       &4HPERM , 4H SPC , 4H     , 4HBDRY , 4H SPC , 4H   S , 4HTRUC , 4HTURE , 4H     , 4H   F , 4HLUID , 4HFREE , 4H SUR , 4HFACE/
   DATA blank/1H / , dash/1H-/ , astric/1H*/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
!     DETERMINE IF ANY OUTPUT IS REQUESTED
!
         CALL sswtch(32,d32)
         CALL sswtch(33,d33)
         IF ( d32==0 .AND. d33==0 ) RETURN
!
!     READ EQEXIN INTO CORE
!
         file = eqexin
         CALL open(*40,eqexin,z(Ibuf),0)
         CALL fwdrec(*60,eqexin)
         CALL fwdrec(*60,eqexin)
         nz = Ibuf - Ieqex
         CALL read(*60,*20,eqexin,z(Ieqex),nz,1,neqex)
         WRITE (nout,99001) uwm
99001    FORMAT (A25,' 8011, INSUFFICIENT CORE TO HOLD CONTENTS OF EQEXIN',' DATA BLOCK',/31X,                                      &
                &'HYDROELASTIC USET PRINTOUT TERMINATED.')
         RETURN
 20      CALL close(eqexin,1)
!
!     SORT ON INTERNAL ID
!
         CALL sort(0,0,2,2,z(Ieqex),neqex)
!
!     SET UP USET MASKS FOR DOF VS. DISP SET PRINTOUT
!
         IF ( d32/=0 ) THEN
            msk(1) = two(usb)
            msk(2) = two(usg)
            msk(3) = two(ul)
            msk(4) = two(ua)
            msk(5) = two(uf)
            msk(6) = two(un)
            msk(7) = two(ug)
            msk(8) = two(ur)
            msk(9) = two(uo)
            msk(10) = two(us)
            msk(11) = two(um)
            msk(12) = two(ux)
            msk(13) = two(uy)
            msk(14) = two(ufr)
            msk(15) = two(uz)
            msk(16) = two(uab)
            msk(17) = two(ui)
!
            DO i = 1 , 17
               sbit(i) = 0
            ENDDO
!
!     PASS THROUGH EQEXIN TABLE AND DETERMINE NUMBER OF DOF FOR EACH
!     POINT
!
            juset = Iuset - 1
            line = nlpp
            inpnt = 0
            DO k = 1 , neqex , 2
               itype = mod(z(Ieqex+k),10)
               ndof = 6
               IF ( itype==2 ) ndof = 1
!
!     FOR EACH DOF - GET USET ENTRY AND TEST VARIOUS MACK BITS
!
               DO kk = 1 , ndof
                  juset = juset + 1
                  iu = z(juset)
                  inpnt = inpnt + 1
                  expnt = z(Ieqex+k-1)
                  idof = kk
                  IF ( ndof==1 ) idof = 0
                  DO ibit = 1 , 17
                     IF ( andf(msk(ibit),iu)/=0 ) THEN
                        upbit(ibit) = astric
                        sbit(ibit) = sbit(ibit) + 1
                     ELSE
                        upbit(ibit) = blank
                     ENDIF
                  ENDDO
!
!     PRINT LINE OF OUTPUT
!
                  line = line + 1
                  IF ( line>nlpp ) THEN
                     CALL page1
                     WRITE (nout,99002)
!
!     FORMAT STATEMENTS
!
99002                FORMAT (//12X,'INT DOF  EXT GP. DOF   SB   SG    L    A    F   ',                                              &
                            &'N    G    R    O    S    M    X    Y   FR    Z   AB    I',/1X,131(1H-))
                     line = 1
                  ENDIF
                  WRITE (nout,99003) inpnt , expnt , dash , idof , upbit
99003             FORMAT (10X,I8,1X,I8,1X,A1,I2,1X,17(4X,A1))
               ENDDO
            ENDDO
!
!     PRINT COLUMN TOTALS
!
            WRITE (nout,99004) sbit
99004       FORMAT (1H0,31H-- C O L U M N   T O T A L S --,17I5)
         ENDIF
!
!     SET UP MASKS FOR DISP SET VS. DOF PRINTOUT
!
         IF ( d33==0 ) RETURN
         msk(1) = two(um)
         msk(2) = two(us)
         msk(3) = two(uo)
         msk(4) = two(ua)
         msk(5) = two(usg)
         msk(6) = two(usb)
         msk(7) = two(ux)
         msk(8) = two(uy)
         msk(9) = two(ufr)
!
!     PASS THROUGH EQEXIN TABLE ONCE FOR EACH DISP SET TO BE PRINTED
!
         DO imk = 1 , 9
            inum = -9
            icol = 0
            line = nlpp
            juset = Iuset - 1
            DO k = 1 , neqex , 2
               itype = mod(z(Ieqex+k),10)
               ndof = 6
               IF ( itype==2 ) ndof = 1
!
!     FOR EACH DOF - TEST IF IT IS IN DESIRED SET FOR THIS PASS
!
               expnt = z(Ieqex+k-1)
               DO kk = 1 , ndof
                  juset = juset + 1
                  IF ( andf(z(juset),msk(imk))/=0 ) THEN
                     idof = kk
                     IF ( ndof==1 ) idof = 0
                     icol = icol + 1
                     zgrd(icol) = expnt
                     zdof(icol) = idof
                     IF ( icol>=10 ) THEN
!
!     WE HAVE ACUMULATED 10 POINTS - PRINT THEM
!
                        icol = 0
                        line = line + 1
                        IF ( line>nlpp ) THEN
                           CALL page1
                           WRITE (nout,99005) (title(i,imk),i=1,3)
                           line = 1
                        ENDIF
                        inum = inum + 10
                        WRITE (nout,99006) inum , (zgrd(i),zdof(i),i=1,10)
                     ENDIF
                  ENDIF
!
               ENDDO
            ENDDO
!
!     PRINT ANY REMAINING ENTRIES
!
            IF ( icol/=0 ) THEN
               line = line + 1
               IF ( line>nlpp ) THEN
                  CALL page1
                  WRITE (nout,99005) (title(i,imk),i=1,3)
                  line = 1
               ENDIF
               inum = inum + 10
               WRITE (nout,99006) inum , (zgrd(i),zdof(i),i=1,icol)
            ENDIF
!
         ENDDO
!
!     PRINT OUT COMPLETE
!
         RETURN
!
!     ERROR CONDITIONS - PRINT NON-FATAL MESSAGE
!
 40      n = 1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 60      n = 2
         spag_nextblock_1 = 2
      CASE (2)
         CALL mesage(n,file,name)
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99005 FORMAT (45X,3A4,17H DISPLACEMENT SET,//16X,3H-1-,8X,3H-2-,8X,3H-3-,8X,3H-4-,8X,3H-5-,8X,3H-6-,8X,3H-7-,8X,3H-8-,8X,3H-9-,7X,  &
             &4H-10-,/1H )
99006 FORMAT (1H ,I6,1H=,10(1X,I8,1H-,I1))
END SUBROUTINE flbprt
