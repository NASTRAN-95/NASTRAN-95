!*==fdit.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fdit(I,K)
   USE c_machin
   USE c_sof
   USE c_sys
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: I
   INTEGER :: K
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: ibl , iblock , icount , iend , inxt , istart , j , ll , ndir
   INTEGER , SAVE :: iempty , indsbr , ird , iwrt
   LOGICAL :: newblk
   INTEGER , DIMENSION(2) , SAVE :: nmsbr
   EXTERNAL andf , chkopn , errmkn , fnxt , getblk , mesage , rshift , sofcls , sofio
!
! End of declarations rewritten by SPAG
!
!
!     FETCHES FROM THE RANDOM ACCESS STORAGE DEVICE THE BLOCK OF THE
!     DIT CONTAINING THE ITH SUBSTRUCTURE NAME, AND STORES IT IN THE
!     ARRAY BUF STARTING AT LOCATION (DIT+1) AND EXTENDING TO LOCATION
!     (DIT+BLKSIZ).  THE OUTPUT K INDICATES THAT THE SUBSTRUCTURE HAS
!     THE KTH ENTRY IN BUF.
!
   DATA ird , iwrt/1 , 2/
   DATA iempty/4H    /
   DATA indsbr/5/ , nmsbr/4HFDIT , 4H    /
!
   CALL chkopn(nmsbr(1))
!
!     NDIR IS THE NUMBER OF SUBSTRUCTURE NAMES IN ONE BLOCK OF THE DIT
!
   ndir = blksiz/2
!
!     COMPUTE THE LOGICAL BLOCK NUMBER, AND THE WORD NUMBER WITHIN
!     BUF IN WHICH THE ITH SUBSTRUCTURE NAME IS STORED.  STORE THE BLOCK
!     NUMBER IN IBLOCK, AND THE WORD NUMBER IN K.
!
   iblock = I/ndir
   IF ( I/=iblock*ndir ) iblock = iblock + 1
   K = 2*(I-(iblock-1)*ndir) - 1 + dit
   IF ( ditlbn==iblock ) RETURN
!
!     THE DESIRED DIT BLOCK IS NOT PRESENTLY IN CORE, MUST THEREFORE
!     FETCH IT.
!
   newblk = .FALSE.
!
!     FIND THE PHYSICAL BLOCK NUMBER OF THE BLOCK ON WHICH THE LOGICAL
!     BLOCK IBLOCK IS STORED.
!
   j = ditbl
   icount = 1
   SPAG_Loop_1_1: DO WHILE ( icount/=iblock )
      icount = icount + 1
      CALL fnxt(j,inxt)
      IF ( mod(j,2)==1 ) THEN
         ibl = andf(buf(inxt),jhalf)
      ELSE
         ibl = rshift(buf(inxt),ihalf)
      ENDIF
      IF ( ibl==0 ) THEN
!
!     WE NEED A FREE BLOCK FOR THE DIT.
!
         CALL getblk(j,ibl)
         IF ( ibl/=-1 ) THEN
            newblk = .TRUE.
            j = ibl
            IF ( icount==iblock ) EXIT SPAG_Loop_1_1
!
!     ERROR MESSAGES.
!
            CALL errmkn(indsbr,7)
         ENDIF
         WRITE (nout,99001) ufm
99001    FORMAT (A23,' 6223, SUBROUTINE FDIT - THERE ARE NO MORE FREE ','BLOCKS AVAILABLE ON THE SOF')
         CALL sofcls
         CALL mesage(-61,0,0)
         RETURN
      ELSE
         j = ibl
      ENDIF
   ENDDO SPAG_Loop_1_1
   IF ( ditpbn==0 ) THEN
      IF ( nxtpbn/=0 ) THEN
!
!     THE IN CORE BLOCK SHARED BY THE DIT AND THE ARRAY NXT IS NOW
!     OCCUPIED BY NXT.  WRITE OUT NXT IF IT HAS BEEN UPDATED.
!
         IF ( nxtup ) THEN
            CALL sofio(iwrt,nxtpbn,buf(nxt-2))
            nxtup = .FALSE.
         ENDIF
         nxtpbn = 0
         nxtlbn = 0
      ENDIF
!
!     THE IN CORE BLOCK SHARED BY THE DIT AND THE ARRAY NXT IS NOW
!     OCCUPIED BY THE DIT.  WRITE IT OUT IF IT HAS BEEN UPDATED.
!
   ELSEIF ( ditup ) THEN
      CALL sofio(iwrt,ditpbn,buf(dit-2))
   ENDIF
!
!     READ THE DESIRED DIT BLOCK INTO CORE.
!
   ditpbn = j
   ditlbn = iblock
   IF ( newblk ) THEN
!
      istart = dit + 1
      iend = dit + blksiz
      DO ll = istart , iend
         buf(ll) = iempty
      ENDDO
      RETURN
   ELSE
      CALL sofio(ird,j,buf(dit-2))
      RETURN
   ENDIF
END SUBROUTINE fdit
