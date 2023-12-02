!*==outpt3.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE outpt3
   USE c_blank
   USE c_phdmix
   USE c_system
   USE c_xmssg
   USE c_zntpkx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL :: first
   INTEGER :: i , ibuf , ii , j , jono , lcor , ncards , trl1 , trl2 , trl3 , trl4 , trl5 , trl6 , trl7
   INTEGER , DIMENSION(5) , SAVE :: in
   INTEGER , SAVE :: ityp
   INTEGER , DIMENSION(2) :: name
   INTEGER , DIMENSION(2) , SAVE :: subnam
   INTEGER , DIMENSION(7) :: trl
   EXTERNAL close , fname , gopen , intpk , korsz , mesage , page2 , phdmia , phdmib , phdmic , phdmid , rdtrl , zntpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     PUNCH UP TO 5 MATRIX DATA BLOCK ONTO DMI CARDS
!
!     CALL TO THIS MODULE IS
!
!     OUTPUT3   M1,M2,M3,M4,M5//C,N,PO/C,Y,N1=AB/C,Y,N2=CD/C,Y,N3=EF/
!                                      C,Y,N4=GH/C,Y,N5=IJ   $
!
!               PO = FORTRAN OUTPUT FILE UNIT NO. (DEFAULT = 0)
!                    .GE.0 MEANS NO LISTING OF  CARD IMAGES WILL BE MADE
!                    .LT.0 MEANS LISTING OF DMI CARD IMAGES WILL BE MADE
!                          ON FORTRAN UNIT = IABS(PO).
!
!
!
   !>>>>EQUIVALENCE (trl(1),trl1) , (trl(2),trl2) , (trl(3),trl3) , (trl(4),trl4) , (trl(5),trl5) , (trl(6),trl6) , (trl(7),trl7)
   DATA subnam/4HOUTP , 4HUT3 / , in/101 , 102 , 103 , 104 , 105/
   DATA ityp/1/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
         lcor = korsz(x) - nb
         IF ( lcor<=0 ) CALL mesage(-8,lcor,subnam)
         ibuf = lcor + 1
         jono = 0
         IF ( jo<0 ) jono = iabs(jo)
         noutpt = jono
         itin = 1
         kpp = 2
         nlp = nlpp
!
         DO ii = 1 , 5
            trl1 = in(ii)
            CALL rdtrl(trl)
            IF ( trl1>0 ) THEN
               CALL fname(in(ii),name)
               CALL gopen(in(ii),x(ibuf),0)
               namex(1) = name(1)
               namex(2) = name(2)
               nam = param(1,ii)
               ifo = trl4
               itout = 0
               ir = trl3
               ic = trl2
               CALL phdmia
               IF ( erno/=0 ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
!
               DO j = 1 , trl2
                  CALL intpk(*5,in(ii),0,ityp,0)
                  first = .FALSE.
                  icol = j
!
                  SPAG_Loop_3_1: DO i = 1 , trl3
                     IF ( eol/=0 ) EXIT SPAG_Loop_3_1
                     CALL zntpki
                     iro = iz
                     xx = z(1)
!
!     VAX MAY HAVE A FEW IMBEDED ZEROS
!
                     IF ( xx/=0.0 ) THEN
                        IF ( first ) THEN
                           CALL phdmic
                           IF ( erno/=0 ) THEN
                              spag_nextblock_1 = 2
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                        ELSE
                           first = .TRUE.
                           CALL phdmib
                           IF ( erno/=0 ) THEN
                              spag_nextblock_1 = 2
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDDO SPAG_Loop_3_1
!
                  CALL phdmid
                  IF ( erno/=0 ) THEN
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
 5             ENDDO
!
               ncards = icard1 + 1
               CALL page2(-2)
               WRITE (no,99001) uim , name , ncards
99001          FORMAT (A29,' 4103, OUTPUT3 HAS PUNCHED MATRIX DATA BLOCK ',2A4,' ONTO ',I5,' DMI CARDS.')
               CALL close(in(ii),1)
            ENDIF
         ENDDO
         RETURN
      CASE (2)
!
!     ERROR MESSAGE
!
         CALL page2(-2)
         WRITE (no,99002) ufm
99002    FORMAT (A23,' 4104, ATTEMPT TO PUNCH MORE THAN 99999 DMI CARDS ','FOR A SINGLE MATRIX.')
         CALL mesage(-61,0,0)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE outpt3
