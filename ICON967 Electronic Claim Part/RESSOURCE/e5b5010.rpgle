      /copy ressource/copysrc,hxxcntrl
      **************************************************************************
      **                                                                      **
      **            Electronic Billing Ansi x12 837 Format (5010)             **
      **                                                                      **
      **            © Copyright 2011 - Health Care Software, Inc.             **
      **************************************************************************
      **                         Abstract                                     **
      **   2 Modes -                                                          **
      **   B -program used to create electronic claim                         **
      **   P -program used to display a single claim                          **
      **************************************************************************
      **                        Modification Summary                        **
      **                                                                      **
      ** Change Date   Programmer / Comments              Install Date  **
      **    9/29/2011     Charles Nagy                                        **
      **                 -Changed provider variable for Submitter ID to       **
      **                  be SBMID                                            **
      **                                                                      **
      **  10/04/2011    Charles Nagy                                          **
      **               -Added file EBPSEQ for 277 processing                  **
      **  10/07/2011   -Lose Cat 1 and Cat 2 parms and add Address parm       **
      **                in XFXPRV call                                        **
      **  10/19/2011   -added clear to totseg in case you need to create      **
      **                a seperate transaction set for each claim             **
      **  10/20/2011   -1500N pgm breaking at 6 lines per claim, causing      **
      **                CLM segment to reject because totchg amount in B528   **
      **                too low - added sr gettotchgp                         **
      **  10/26/2011   -handling for bill exception 508                       **
      **                                                                      **
      **  11/09/2011    Brett Miller                                          **
      **               -changes for loops 2410A                               **
      **                (installed by MMCCLELLAN)                             **
      **                                                                      **
      **  12/22/2011    Brett Miller                                          **
      **               -printing changes                                      **
      **                                                                      **
      **   1/05/2012    Brett Miller  ticket 270012                           **
      **               -prof change for provider varaiables by proc           **
      **                                                                      **
      **   1/19/2012    Charles Nagy - Ticket 272334                          **
      **               -Multiple ST/SE segment handling                       **
      **               -Additional updates                                    **
      **                                                                      **
      **   1/19/2012    Brett Miller - Ticket 271962                          **
      **               -dependent changes and new patterns                    **
      **                                                                      **
      **  01/26/2012    Eric Bjorge - Ticket 273819                           **
      **               -Data dependencies didn't look at the pattern          **
      **                                                                      **
      **   1/31/2012    Charles Nagy - Ticket 273819R                         **
      **               -Changes for max charges                               **
      **                                                                      **
      **   2/23/2013    Brett Miller                                          **
      **               -changes for ICN                                       **
      **               -Pulled from HNH by Eric Bjorge - Ticket 279069        **
      **               -Customized for RES                                    **
      **                                                                      **
      **   2/29/2012    Charles Nagy - Ticket 274186                          **
      **               -Recompiled for changes to E5XVAULES                   **
      **                                                                      **
      **  03/06/2012    Eric Bjorge - Ticket 280751                           **
      **               -Place of Service override at clm level                **
      **                                                                      **
      **  03/09/2012    Eric Bjorge - Ticket 281448                           **
      **               -Add up totals of the co-pay bucket while running      **
      **                through the charges for the bill                      **
      **                                                                      **
      **  10/31/2011    Brett Miller                                          **
      **               -New Relationship table BIR5                           **
      **               -Pulled from HCS by Eric Bjorge - Ticket 294083        **
      **                                                                      **
      **  06/13/2012    Eric Bjorge - Ticket 302022                           **
      **               -Removed hardcoded max LX segments and use the ETRMLX  **
      **                field in its place.  If it is 0, it assumes no max.   **
      **               -Added breakout of Amount and Dates per claim to       **
      **                accommodate the fact it might be broken up based on   **
      **                the number of charges and maximum # of LX segments    **
      **               -Change the position of the return loops when the      **
      **                maximum number of charges has been hit                **
      **                                                                      **
      **  07/25/2012    Eric Bjorge - Ticket 312797                           **
      **               -If the second flag in e5pebvar is set to a 'Y',       **
      **                specific loops flagged as a 'T' will only go through  **
      **                when the value in bbprov changes between bills        **
      **                                                                      **
      **  08/16/2012    Eric Bjorge - Ticket 316568R1                         **
      **               -Recompile for ticket                                  **
      **                                                                      **
      **  10/16/2012    Eric Bjorge - Ticket 325649                           **
      **               -Add additional check to the INS_ pattern.  If it is   **
      **                an *, uses the rank of the bill to determine which    **
      **                insurance is billing currently                        **
      **                                                                      **
      **  05/17/2012    Eric Bjorge - Ticket 296953            10/19/2012     **
      **               -Lengthened WKST02, STCNTR to 9,0 from 4,0             **
      **               -Lengthened BHT03 and ST02 to 9 from 4                 **
      **                                                                      **
      **  03/01/2013    bgates - Ticket 346238                 03/05/2013     **
      **               -Commented BIR5 logic; will be handled in HBBPRDT      **
      **                                                                      **
      **  05/10/2013    Eric Bjorge - Ticket 357700                           **
      **               -Add trimfield to handle trimming when getting values  **
      **                                                                      **
      **  01/07/2014    Will Pedicone - Ticket 396669                         **
      **               -Updated pattern logic to function for icpatt as well  **
      **                as ifpatt                                             **
      **                                                                      **
      **  02/19/2014    Will Pedicone - Ticket 406574                         **
      **               -Added pattern #DOCT to print physician taxonomy code. **
      **                                                                      **
      **  02/19/2014    Will Pedicone - Ticket 406574                         **
      **               -Added pattern #DOCR_ to print rendering provider code.**
      **                                                                      **
      **   1/22/2014    Brett Miller                                          **
      **               -changes for ICD10                                     **
      **  06/16/2015    Justin Sarnak                                         **
      **               -bug fix for ICD-10: always append 'A' to diag segment **
      **               **bgates - Pulled changes from HCS                     **
      **                                                                      **
      **  04/16/2014    bgates - Project 688                   07/14/2015     **
      **               -Populate tracking number in E5P5010                   **
      **  05/07/2015    bgates - Ticket 403644                                **
      **               -Repulled/updated                                      **
      **                                                                      **
      **  10/1/2015     Will Pedicone - Ticket 542720                         **
      **               -If type of bill changed in this program, revert back  **
      **                to what it was before updating HBPBILLH record.       **
      **                                                                      **
      **  04/18/2016    Will Pedicone - Ticket 566968                         **
      **               -When paper bill has moved fields up in boxes because  **
      **                there is no primary on account, ebill will operate    **
      **                the same way to pull the correct fields               **
      **                                                                      **
      **   9/01/2016    Charles Nagy - Ticket 573461 - HDEV-10566             **
      **               -Added code to return charge description               **
      **                                                                      **
      **  10/12/2017    Charles Nagy - Ticket 596299 / HDEV-18979             **
      **               -Added code to the #DOCN and #DOCL patterns to pull    **
      **                supervisor name and NPI when bill exception 861 is on **
      **                (B5FFL1) and the resource has the SNPI license code.  **
      **                                                                      **
      **   8/12/2019    Charles Nagy - Ticket 2199535 / HDEV-32893            **
      **               -Added new pattern #LOCAMT to reflect Level of Care    **
      **                information                                           **
      **                                                                      **
      **   6/22/2022    Charles Nagy - CTASK0105382                           **
      **               -Modified pattern #LOCAMT to retrieve Level of Care    **
      **                for the bill From Date from Transfer History          **
      **                                                                      **
      **   9/12/2022    Charles Nagy                                          **
      **               -Bug fix : moved BGD508 into position 39               **
      **                                                                      **
      **   9/05/2023    Charles Nagy - CTASK0117879                           **
      **               -Added new pattern #NOTPOSD to reflect the POS on the  **
      **                charge record in SV105 when it differs from the       **
      **                default POS value in CLM05-1                          **
      **                                                                      **
      **************************************************************************
     fhblbilnm  uf   e           k disk    rename(hbfbill:hbfbillm)
     fhblbilhc  if   e           k disk
     fhbpmast   if   e           k disk
     fhbptobf   if   e           k disk
     fhxplvl6   if   e           k disk
     fhxplvl5   if   e           k disk
     fhxplvl4   if   e           k disk
     fhxplvl3   if   e           k disk
     fhxplvl2   if   e           k disk
     fhxplvl1   if   e           k disk
     fhmpmams   if   e           k disk
     fhmpnpi    if   e           k disk
     fhmllice   if   e           k disk
     fhxpbnfit  if   e           k disk
     fhxpprocc  if   e           k disk
     fe5pebvar  if   e           k disk
     fe5pub04   if   e           k disk    rename(hbfub04:e5fub04)
     fe5p1500   if   e           k disk
     fe5pubdet  if   e           k disk    rename(hbfubdet:e5fubdet)
     fe5pchr15  if   e           k disk
     fe5ploops  if   e           k disk
     fe5lloops  if   e           k disk    rename(e5floops:e5floops2)
     fe5pfield  if   e           k disk
     fe5lfield  if   e           k disk    rename(e5ffield:e5ffield2)
     fe5pcmpnt  if   e           k disk
     fe5lcmpnt  if   e           k disk    rename(e5fcmpnt:e5fcmpnt2)
     f                                     prefix(x:1)
     febpebtri  if   e           k disk
     fe5povrdt  if   e           k disk
     fe5pFub04  if   e           k disk
     fe5pF1500  if   e           k disk
     fe5pdepnd  if   e           k disk
     febpseq    uf a e           k disk
     fe5p5010   o    e           k disk    rename(e5f837:e5f5010)
     fe5pelhtml uf a e           k disk
     fprinter   o    f  132        printer oflind(*inof) usropn
      *------------------------------------------------------------------*
     d ary2000         s              1    dim(2000)
     d totchga         s             11  2 dim(1000)
     d BillFromDate    s              8  0 dim(1000)
     d BillToDate      s              8  0 dim(1000)

     d                 ds
     d hrundt                        14  0
     d  hruntme                       6  0 overlay(hrundt)

     d ldads           ds          1024
     d ldausr                 54     63

     d                 ds
     d bgdfil                        39
     d****  bgd508                        1    overlay(bgdfil:24)
     d  bgd508                        1    overlay(bgdfil:39)
     d                sds
     d  pgmnam           *proc
     d                 ds
     d  curtim                 1     14  0
     d  runtme                 1      6  0
     d  rundte                 7     14  0
     d hx6tid          ds
     d  taxid                  1     10
      **************************************************************************
     d acct#           s             12  0
     d bht03           s              9
     d billtrack       s             14
     d billtrack20     s             20
     d clm05_1         s              2
     d countr          s              3  0
     d counter2        s              3  0
     d SCounter2       s              3  0
     d clmseq          s              5  0 inz(1)
     d date6           s              6
     d dataflag        s               n
     d datafield       s           2000
     d datafields      s           2000
     d dataline        s           2000
     d depfield        s             10    inz('FIELD')
     d depcomp         s             10    inz('COMP ')
     d depdata         s             10    inz('DATA ')
     d depdataN        s             10    inz('NOTDATA')
     d doc#            s              9  0
     d fielddsc        s             30
     d frdate          s              8  0
     d todate          s              8  0
     d icndcn          s             23
     d index           s              5  0
     d header          c                   'CLAIMS SENT TO '
     d hldsep          s              1
     d hold1           s              1
     d hold2           s              1
     d hold3           s              1
     d hlddft          s             30
     d hldhcs          s              6
     d i               s              5  0
     d insdsc          s             15
     d lcode           s              5
     d lv1nam          s             40
     d lvlnam          s             30
     d lvlphn          s             10  0
     d lvladr          s             20
     d lvlcty          s             15
     d lvlsta          s              2
     d lvlzp1          s              5
     d lvlzp2          s              4
     d lvltax          s             15
     d lv6#            s              6  0
     d maxcharge       s               n
     d mdate           s              8  0
     d mode            s              1
     d ohtml           s          32000
     d once            s              1    inz(' ')
     d jj              s              5  0
     d len             s              5  0
     d namein          s             26
     d newsep          s              1
     d last            s             18
     d first           s             12
     d monday          s              4
     d middle          s              1
     d jrorsr          s              3
     d weuser          s             10
     d wedate          s              8
     d wetime          s              6
     d ovrtype         s              1
     d payor           s              6  0
     d plan            s              5  0
     d prmlv6          s              6  0
     d prmmem          s             10
     d prmpln          s              5  0
     d prmprc          s              8
     d prmpyr          s              6  0
     d prmrev          s              4  0
     d prmsct          s              1
     d ProvActive      s               n
     d provchange      s               n
     d prvadr          s            112
     d prvct1          s              3
     d prvct2          s              3
     d prvvar          s             70
     d prvdte          s              8  0
     d proc#           s              8
     d prtfdt          s              8  0
     d prvid           s              5
     d rqsdte          s              8  0
     d reqfrm          s              2
     d reqtyp          s              2
     d rpthdr          s             40
     d rqaccn          s             12  0
     d rqcat1          s              3
     d rqcat2          s              3
     d rqcat3          s              3
     d rqchng          s              1
     d rqdate          s              8  0
     d rqlvl6          s              6  0
     d rqproc          s              8
     d rqrcls          s              2
     d rqroom          s             10
     d rqsct1          s              3
     d rqsct2          s              3
     d rqsct3          s              3
     d rqspln          s              5  0
     d rqsprc          s              7
     d rqspyr          s              6  0
     d rqpos           s              2
     d savfrm          s              2
     d savlop          s              6
     d savprov         s                   like(bbprov) inz(*blanks)
     d savseq          s              5  0
     d savtyp          s              2
     d sav510          s              1
     d sbmlvl          s              6  0
     d sfdseq          s              5  0
     d sfdlop          s              6
     d sfdseg          s              5
     d sfdfsq          s              5  0
     d sfname          s              6
     d sfddsc          s             70
     d sfddlt          s              1
     d sfsmin          s              4  0
     d sfsmax          s              4  0
     d sformt          s              3
     d sfpatt          s             15
     d sfhcsf          s              6
     d sfdeft          s             30
     d sftsep          s              1
     d sfusge          s              1
     d snpi            s             10
     d status          s              2
     d stotal          s              6  0
     d st02            s              9
     d stcntr          s              9  0 inz(0)
     d svtorb          s              1
     d npos            s              5  0
     d ppos            s              5  0
     d spos            s              5  0
     d special         s               n
     d taxid2          s             20
     d taxlevel        s              1
     d taxlevelid      s              6  0
     d totchg          s              9  2
     d totchgx         s              9  2
     d totcpy          s              9  2
     d totseg          s             10  0
     d trknm2          s              2  0
     d trknum2         s              3  0
     d time4           s              4
     d tmptim          s              6  0
     d tmp508          s              4  0
     d trimfield       s               n
     d xa              s              5  0
     d xc              s              5  0
     d xodfrm          s              2
     d xodtyp          s              2
     d xod510          s              1
     d xodubc          s              6  0
     d xodpln          s              5  0
     d xodseq          s              5  0
     d xodlop          s              6
     d xodseg          s              5
     d xodfsq          s              5  0
     d xodcsq          s              5  0
     d xodtbl          s              4
     d xodata          s             30
     d ydate           s              8  0
     d year            s              2
     d cyear           s              4
     d wsaccn          s             17
     d wkst02          s              9  0 inz(0)
     d workcat2        s                   like(bgcat2)
     d workcat3        s                   like(bgcat3)
     d worktobf        s              2
     d yudate          s              8
     d yutime          s              4
     d zeroseq         s              5  0 inz(0)
     d zip4            s              4    inz('9998')
      *-------------------------------------------------------------*
      /copy ressource/copysrc,hxxtable
      *-------------------------------------------------------------*

     c     *entry        plist
     c                   parm                    reqfrm
     c                   parm                    reqtyp
     c                   parm                    billtrack
     c                   parm                    mode
     c                   parm                    weuser
     c                   parm                    wedate
     c                   parm                    wetime
     c                   parm                    weseqn

     c                   eval      i8bfrm = reqfrm
     c                   eval      i8btyp = reqtyp
     c                   time                    curtim
     c                   eval      stotal = 0
      **
     c                   eval      rpthdr = *blanks
     c                   movel(p)  header        rpthdr
     c                   exsr      srctab
     c                   movel     'BELB'        tcode
     c                   movel     reqtyp        ecode
     c                   exsr      srtabl
     c                   if        tind <> 'E'
     c                   eval      rpthdr = header + %trim(ldesc)
     c                   endif
      **

     c                   eval      billtrack20 = billtrack
     c                   time                    hrundt
     c                   move      hrundt        mdate
     c                   exsr      srcmdy
     c                   move      ydate         yudate
     c                   movel     hruntme       yutime

     c     frmtyp        chain     e5febvar
     c                   eval      ProvActive = (%found and e5vf02 = 'Y')

     c     frmtyp        chain     ebfebtri                           79
     c     *in79         cabeq     *on           skip
     c     frmtyp        chain     e5floops                           79
     c     *in79         cabeq     *on           skip
      **
     c                   if        mode = 'P'
      **
     c     *dtaara       define    ebaicn        icnsq             9 0
     c     *dtaara       define    ebagcn        gcnseq            9 0
     c                   add       1             gcnseq
     c                   if        gcnseq = 0
     c                   add       1000          gcnseq
     c                   endif
     c                   endif

     c                   eval      totchg = 0
     c                   eval      totchgx = 0

      **************************************************************************
      // Mode P - Display a single claim //
     c                   if        mode = 'P'
     c     billtrack     chain     hbfbill                            79
     c     *in79         cabeq     *on           skip
     c     acctky        chain     hbfmast                            79
     c     *in79         cabeq     *on           skip
     c     bbplv6        chain     hxflvl6
     c                   eval      totseg = 0
     c                   eval      lvlnam = hx6nam
     c                   eval      lvlphn = hx6phn
     c                   eval      lvladr = hx6a11
     c                   eval      lvlcty = hx6ct1
     c                   eval      lvlsta = hx6st1
     c                   eval      lvlzp1 = hx6z11
     c                   eval      lvlzp2 = hx6z12
      **
     c                   call      'XFXTAXID'
     c                   parm      bbplv6        sbmlvl
     c                   parm      *blanks       taxid2
     c                   parm      '6'           taxlevel
     c                   parm                    taxlevelid
      **
     c                   eval      lvltax = taxid2
     c                   goto      noloop
     c                   endif

      **************************************************************************
      // Mode B - Create Electronic Claim //
     c     *loval        setll     hbfbillm
     c                   dou       *in71 = *on
     c                   eval      svtorb = *blanks
     c                   read      hbfbillm                               71
     c     *in71         cabeq     *on           skip
     c                   eval      totseg = 0
     c                   eval      payor = bbpayr
     c                   eval      plan = bbplan
     c     frmtyppp      chain     e5floops                           79
     c                   if        *in79 = *on
     c                   eval      payor = 0
     c                   eval      plan = 0
     c                   endif
     c     acctky        chain     hbfmast                            79
     c                   if        *in79 = *on
     c                   iter
     c                   else
     c     bbplv6        chain     hxflvl6
     c                   eval      totseg = 0
     c                   eval      lvlnam = hx6nam
     c                   eval      lvlphn = hx6phn
     c                   eval      lvladr = hx6a11
     c                   eval      lvlcty = hx6ct1
     c                   eval      lvlsta = hx6st1
     c                   eval      lvlzp1 = hx6z11
     c                   eval      lvlzp2 = hx6z12
      **
     c                   call      'XFXTAXID'
     c                   parm      bbplv6        sbmlvl
     c                   parm      *blanks       taxid2
     c                   parm      '6'           taxlevel
     c                   parm                    taxlevelid
      **
     c                   eval      lvltax = taxid2
     c                   endif
     c                   eval      totchg = 0

      **************************************************************************
     c     noloop        tag

     c                   eval      i8bpyr = bbpayr
     c                   eval      i8bpln = bbplan
     c                   eval      i8trak = bbtrak

     c                   eval      provchange = (savprov <> bbprov)
     c                   eval      savprov = bbprov
     c                   eval      i8blpr = bbprov
     c                   movel(p)  ETRSID        i8blid
     c****               eval      prvid = 'SBMTR'
     c                   eval      prvid = 'SBMID'
     c                   exsr      srprov
     c                   if        prvvar <> *blanks
     c                   movel(p)  prvvar        i8blid
     c                   movel(p)  prvvar        i8blpr
     c                   endif

     c     bbplv6        chain     hxflvl6
     c                   if        not %found
     c                   move      *blanks       hx6tid
     c                   eval      hx6flg = 'C'
     c                   eval      lvlnam = *blanks
     c                   else
     c                   z-add     hx6num        i8bll6
     c                   Z-ADD     HX6L5A        i8bll5
     c                   eval      lvlnam = hx6nam
     c                   endif
     c                   exsr      chainlevels
     c                   exsr      geticn

     c                   eval      billtrack20 = bbtrak
     c                   if        mode = 'P'
     c     billtrack     cabeq     *blanks       skip
     c                   endif
     c                   if        ETR510 = 'I'
     c     bbtrak        chain     e5fub04                            79
     c                   else
     c     bbtrak        chain     e5f1500                            79
     c                   endif
     c                   if        *in79 = *on
     c                   iter
     c                   endif

     c****               if        etr510 = 'I'
      * rel 1
     c****               exsr      srctab
     c****               eval      tcode = 'BIR5'
     c****               eval      ecode = FL59A
     c****               exsr      srtabl
     c****               if        tind <> 'E' and %subst(hmap:1:2) <> *blanks
     c****               eval      FL59A = hmap
     c****               endif

      * rel 2
     c****               exsr      srctab
     c****               eval      tcode = 'BIR5'
     c****               eval      ecode = FL59B
     c****               exsr      srtabl
     c****               if        tind <> 'E' and %subst(hmap:1:2) <> *blanks
     c****               eval      FL59B = hmap
     c****               endif

      * rel 3
     c****               exsr      srctab
     c****               eval      tcode = 'BIR5'
     c****               eval      ecode = FL59C
     c****               exsr      srtabl
     c****               if        tind <> 'E' and %subst(hmap:1:2) <> *blanks
     c****               eval      FL59C = hmap
     c****               endif
     c****               endif

     c                   eval      totchga = *zeros
     c                   eval      BillFromDate = *all'9'
     c                   eval      BillToDate = *zeros
     c                   eval      totcpy = 0
     c                   if        ETR510 = 'I'
     c                   exsr      gettotchg
     c                   else
     c                   exsr      gettotchgp
     c                   endif
     c                   eval      trknm2 = 0
     c                   eval      countr = 0
     c                   eval      counter2 = 0
     c                   eval      clmseq += 1

     c                   add       1             stotal
     c                   eval      wkst02 += 1
     c                   move(p)   wkst02        st02
     c                   eval      bht03 = st02
      **
     c                   z-add     bbfrdt        ydate
     c                   exsr      srcymd
     c                   z-add     mdate         frdate
      **
     c                   z-add     bbtodt        ydate
     c                   exsr      srcymd
     c                   z-add     mdate         todate
     c                   move      *blanks       insdsc
     c     bilkey        chain     xffbnfit
     c                   if        %found
     c                   movel     xfbnam        insdsc
     c                   endif
     c                   movel(p)  bbtrak        wsaccn
     c                   if        mode <> 'P'
     c                   if        once = *blanks
     c                   eval      once = 'X'
      **
     c                   move      *blanks       lv1nam
     c     bbplv1        chain     hxflvl1
     c                   if        %found
     c                   movel     hx1nam        lv1nam
     c                   call      'XFXCNTR'
     c                   parm                    lv1nam
     c                   endif
     c                   open      printer
     c                   eval      *inof = *on
      **
     c                   endif
     c                   except    detail
     c                   endif
     c                   if        mode = 'P'
     c     frmtyppp      setll     e5floops
     c                   dou       *in78 = *on
     c     reread2       tag
     c     frmtyppp      reade     e5floops                               78
     c                   if        *in78 = *off
     c                             and lsddlt <> 'Y'

     c     lsdhdr        cabne     'H'           reread2
     c*    mode          cabne     'P'           reread2

     c                   eval      dataline = *blanks
     c                   exsr      datadepend
     c     dataflag      cabeq     *off          reread2

     c*                  if        mode = 'P'
     c                   exsr      modecol1
     c*                  else
     c*                  eval      dataline = %trim(lsdseg) + %trim(etrsep)
     c*                  endif
     c     unqkey        setll     e5ffield                                     =BUILD
     c                   dou       *in70 = *on                                  =SUBFILE
     c     unqkey        reade(n)  e5ffield                               70
     c                   if        *in70 = *off
     c                             and IFDDLT <> 'Y'
     c                   exsr      datadependF
     c                   if        dataflag = *off
     c                   exsr      addsep
     c                   iter
     c                   endif

      ** DEPEND FIELD
      *
     c     depkeyF       setll     e5fdepnd
     c     checkother    tag
     c     depkeyF       reade     e5fdepnd                               79
     c                   if        *in79 = *off
     c                   if        depdep <> depfield
     c                             and depdep <> depcomp
     c                   goto      checkother
     c                   endif
      * look at fields
     c                   if        DENCSQ = 0
     c     depfld        chain     e5ffield                           79
     c                   if        *in79 = *on
     c                   eval      len = 0
     c                   eval      len = %len(%trim(dataline))
     c                   if        len <> 0
     c                   if        %subst(dataline:len:1) = etrsep
     c                   eval      %subst(dataline:len:1) = etrssp
     c                   endif
     c                   endif
     c                   if        mode = 'P'
     c                   exsr      writepreview
     c                   else
     c                   exsr      writeelcdata
     c                   endif
     c                   goto      reread2
     c                   else
     c                   eval      datafield = *blanks
     c                   eval      hldhcs = ifhcsf
     c                   eval      ovrtype = 'F'
     c                   exsr      getvalues
     c                   exsr      override
     c                   if        datafield = *blanks
     c                   eval      len = 0
     c                   eval      len = %len(%trim(dataline))
     c                   if        len <> 0
     c                   if        %subst(dataline:len:1) = etrsep
     c                   eval      %subst(dataline:len:1) = etrssp
     c                   endif
     c                   endif
     c                   if        mode = 'P'
     c                   exsr      writepreview
     c                   else
     c                   exsr      writeelcdata
     c                   endif
     c                   goto      reread2
     c                   endif
     c                   endif
     c                   endif
     c                   endif
      ** DEPEND FIELD
     c     comkey        chain     e5fcmpnt                           79
     c                   if        *in79 = *off
     c                   exsr      components
     c                   else
     c                   if        ifhcsf <>  *blanks
     c                   eval      fielddsc = *blanks
     c                   if        ETR510 = 'I'
     c     ifhcsf        chain     e5fFub04                           79
     c                   else
     c     ifhcsf        chain     e5fF1500                           79
     c                   endif
     c                   if        *in79 = *off
     c                   if        ETR510 = 'I'
     c                   eval      fielddsc = %trim(u4name) + '-' + u4desc
     c                   else
     c                   eval      fielddsc = %trim(b5name) + '-' + b5desc
     c                   endif
     c                   endif
     c                   eval      hldhcs = ifhcsf
     c                   eval      ovrtype = 'F'
     c                   exsr      getvalues
     c                   exsr      override
     c                   if        datafield = *blanks
     c                   eval      datafield = ifdeft
     c                   endif
     c                   if        ifpatt <> *blanks
     c                   exsr      pattern
     c                   endif
     c                   if        mode <> 'P'
     c                   eval      datafield = %subst(datafield:1:ifsmax)
     c                   endif
     c                   if        mode = 'P'
     c                   exsr      modecol2
     c                   else
     c                   eval      dataline = %trim(dataline) +
     c                                         %trim(datafield)
     c                   endif
     c                   else
     c                   eval      special = *off
     c                   eval      hlddft = ifdeft
     c                   exsr      specialval
     c                   if        ifpatt <> *blanks
     c                   exsr      pattern
     c                   endif
     c                   if        mode <> 'P'
     c                   eval      datafield = %subst(datafield:1:ifsmax)
     c                   endif
     c                   eval      dataline = %trim(dataline) +
     c                                         %trim(datafield)
     c                   endif
     c                   if        ifsmin = ifsmax and ifsmin <> 1
     c                             and mode <> 'P'
     c                   Eval      index = %size(dataline)-1
     c                   dow       index > 0
     c                   if        %subst(dataline:index:1)= etrsep or
     c                             %subst(dataline:index:1)= etrssp or
     c                             %subst(dataline:index:1)= etrces
     c                   leave
     c                   endif
     c                   eval      index = index - 1
     c                   enddo
     c                   if        index <> 0
     c                   select
     c                   when      iftsep = 'D'
     c                   eval      %subst(dataline:index+ifsmax+1:1) = etrsep
     c                   when      iftsep = 'S'
     c                   eval      %subst(dataline:index+ifsmax+1:1) = etrssp
     c                   when      iftsep = 'C'
     c                   eval      %subst(dataline:index+ifsmax+1:1) = etrces
     c                   endsl
     c                   endif
     c
     c                   else
     c                   exsr      addsep
     c                   endif
      ** END COM
     c                   endif
     c                   endif
     c                   enddo
     c                   if        mode = 'P'
     c                   exsr      writepreview
     c                   else
      ** SKIP all header on batch run
     c***                exsr      writeelcdata
     c                   endif
     c                   endif
     c                   enddo
      ** end header section
     c                   endif

     c                   eval      maxcharge = *off
     c     toomanylines  tag
     c                   if        maxcharge = *On

     c                   eval      provchange = *off
      // Make Sure Another Charge Exists //
     c                   eval      scounter2 = counter2 + 1
     c                   if        ETR510 = 'I'
     c     Strkchg       chain     e5fubdet                           60
     c                   else
     c     Strakchg      chain     e5fchr15                           60
     c                   endif
     c                   if        *in60 = *on
     c                   goto      reread
     c                   endif

     c                   if        etr510 = 'I'
     c                   eval      lsdseq = 420
     c                   else
     c                   eval      lsdseq = 450
     c                   endif
     c     begingloop    setll     e5floops
     c                   eval      maxcharge = *off
     c                   goto      reread
     c                   endif
     c     frmtyppp      setll     e5floops
     c                   dou       *in78 = *on
     c     reread        tag
     c     frmtyppp      reade     e5floops                               78
     c                   if        *in78 = *off
     c                             and lsddlt <> 'Y'
     c                   if        lsdhdr = 'T' and not(provchange) and
     c                             ProvActive
     c                   iter
     c                   endif
     c                   if        lsdhdr <> 'F'  and lsdhdr <> 'C' and
     c                             lsdhdr <> 'T'
     c                   iter
     c                   endif
     c*****              if        ETR510 = 'P'
     c                   eval      totchg = totchga(trknm2 + 1)
     c                   if        ETR510 = 'I'
     c                   eval      ydate = BillFromDate(trknm2 + 1)
     c                   exsr      srcymd
     c                   eval      fl06a = mdate
     c                   eval      ydate = BillToDate(trknm2 + 1)
     c                   exsr      srcymd
     c                   eval      fl06b = mdate
     c                   endif
     c*****              endif

     c     lsdhdr        cabeq     'C'           chargeskip

     c                   exsr      datadepend
     c                   if        dataflag = *off
     c                   goto      reread
     c                   endif
      *
     c     depkey        setll     e5fdepnd
     c     segdeptag     tag
     c     depkey        reade     e5fdepnd                               79
     c                   if        *in79 = *off
     c                             and depdep <> depfield
     c                             and depdep <> depcomp
     c                             and depdep <> depdataN
     c                             and depfsq = 0
      * look at fields
     c                   if        DENCSQ = 0
     c     depfld        chain     e5ffield                           79
     c                   if        *in79 = *on
     c                   iter
     c                   else
     c                   eval      datafield = *blanks
     c                   eval      hldhcs = ifhcsf
     c                   eval      ovrtype = 'F'
     c                   exsr      getvalues
     c                   exsr      override
     c                   if        %subst(ifpatt:1:4) = 'PRV_'
     c                             or %subst(ifpatt:1:4) = '#DOC'
     c                             or %subst(ifpatt:1:4) = '#FLD'
     c                             or %subst(ifpatt:1:4) = '#PRV'
     c                             or %subst(ifpatt:1:7) = '#LOCAMT'
     c                   exsr      pattern
     c                   endif
     c                   if        datafield = *blanks
     c                   iter
     c                   endif
     c                   endif
     c                   else
     c     depcom        chain     e5fcmpnt                           79
     c                   if        *in79 = *on
     c                   iter
     c                   else
     c                   eval      datafield = *blanks
     c                   eval      hldhcs = ichcsf
     c                   eval      ovrtype = 'C'
     c                   exsr      getvalues
     c                   exsr      override
     c                   if        datafield = *blanks
     c                   iter
     c                   endif
     c                   endif
      * look at compents
     c                   endif
     c                   goto      segdeptag
     c                   endif
      *
     c                   eval      dataline = *blanks
     c                   if        mode = 'P'
     c                   exsr      modecol1
     c                   else
     c                   eval      dataline = %trim(lsdseg) + %trim(etrsep)
     c                   endif

     c     chargeskip    tag
     c     unqkey        setll     e5ffield                                     =BUILD
     c                   dou       *in70 = *on                                  =SUBFILE
     c     unqkey        reade(n)  e5ffield                               70
     c                   if        *in70 = *off
     c                             and IFDDLT <> 'Y'
     c                   if        ifpatt = 'REPEAT_CHARGES'
     c                   exsr      repeatCHARGE
     c                   if        maxcharge = *on
     c                   goto      toomanylines
     c                   else
     c                   goto      reread
     c                   endif
     c***                leave
     c                   endif
     c                   exsr      datadependF
     c                   if        dataflag = *off
     c                   exsr      addsep
     c                   iter
     c                   endif
     c                   if        lsdhdr = 'C'
     c**                 iter
     c                   goto      reread
     c                   endif
      ** DEPEND FIELD
      *
     c     depkeyF       setll     e5fdepnd
     c     checkother2   tag
     c     depkeyF       reade     e5fdepnd                               79
     c                   if        *in79 = *off
     c                   if        depdep <> depfield
     c                   goto      checkother2
     c                   endif
      * look at fields
     c                   if        DENCSQ = 0
     c     depfld        chain     e5ffield                           79
     c                   if        *in79 = *on
     c                   eval      len = 0
     c                   eval      len = %len(%trim(dataline))
     c                   if        len <> 0
     c                   if        %subst(dataline:len:1) = etrsep
     c                   eval      %subst(dataline:len:1) = etrssp
     c                   endif
     c                   endif
      **
     c                   if        mode = 'P'
     c                   exsr      writepreview
     c                   else
     c                   exsr      writeelcdata
     c                   endif
      **
     c                   goto      reread
     c                   else
     c                   eval      datafield = *blanks
     c                   eval      hldhcs = ifhcsf
     c                   eval      ovrtype = 'F'
     c                   exsr      getvalues
     c                   exsr      override
     c                   if        datafield = *blanks
     c                   eval      len = 0
     c                   eval      len = %len(%trim(dataline))
     c                   if        len <> 0
     c                   if        %subst(dataline:len:1) = etrsep
     c                   eval      %subst(dataline:len:1) = etrssp
     c                   endif
     c                   endif
      **
     c                   if        mode = 'P'
     c                   exsr      writepreview
     c                   else
     c                   exsr      writeelcdata
     c                   endif
      **
     c                   goto      reread
     c                   endif
     c                   endif
     c                   endif
     c                   endif
      ** DEPEND FIELD

     c     comkey        chain     e5fcmpnt                           79
     c                   if        *in79 = *off
     c                   exsr      components
     c                   else
     c                   if        ifhcsf <>  *blanks
     c                   eval      fielddsc = *blanks
     c                   if        ETR510 = 'I'
     c     ifhcsf        chain     e5ffub04                           79
     c                   else
     c     ifhcsf        chain     e5ff1500                           79
     c                   endif
     c                   if        *in79 = *off
     c                   if        ETR510 = 'I'
     c                   eval      fielddsc = %trim(u4name) + '-' + u4desc
     c                   else
     c                   eval      fielddsc = %trim(b5name) + '-' + b5desc
     c                   endif
     c                   endif
     c                   eval      hldhcs = ifhcsf
     c                   eval      ovrtype = 'F'
     c                   exsr      getvalues
     c                   exsr      override
     c                   if        datafield = *blanks
     c                   eval      datafield = ifdeft
     c                   endif
     c                   if        ifpatt <> *blanks
     c                   exsr      pattern
     c                   endif
     c                   if        mode <> 'P'
     c                   eval      datafield = %subst(datafield:1:ifsmax)
     c                   endif
     c                   if        mode = 'P'
     c                   exsr      modecol2
     c                   else
     c                   eval      dataline = %trim(dataline) +
     c                                         %trim(datafield)
     c                   endif
     c                   else
     c                   eval      special = *off
     c                   eval      hlddft = ifdeft
     c                   exsr      specialval
     c                   if        ifpatt <> *blanks
     c                   exsr      pattern
     c                   endif
     c                   if        mode <> 'P'
     c                   eval      datafield = %subst(datafield:1:ifsmax)
     c                   endif
     c                   eval      dataline = %trim(dataline) +
     c                                         %trim(datafield)
     c                   endif
     c                   exsr      addsep
      ** END COM
     c                   endif
     c                   endif
     c                   enddo
     c                   if        mode = 'P'
     c                   exsr      writepreview
     c                   else
     c                   exsr      writeelcdata
     c                   endif
     c                   endif
     c                   enddo
      ** END LOOP -

     c                   if        mode = 'P'
     c     frmtyppp      setll     e5floops
     c                   dou       *in78 = *on
     c     reread3       tag
     c     frmtyppp      reade     e5floops                               78
     c                   if        *in78 = *off
     c                             and lsddlt <> 'Y'
     c                   if        lsdhdr <> 'E'
     c                   iter
     c                   endif
     c*                  if        mode <> 'P'
     c*                  iter
     c*                  endif
     c                   exsr      datadepend
     c                   if        dataflag = *off
     c                   goto      reread3
     c                   endif
     c                   eval      dataline = *blanks
     c*                  if        mode = 'P'
     c                   exsr      modecol1
     c*                  else
     c*                  eval      dataline = %trim(lsdseg) + %trim(etrsep)
     c*                  endif
     c     unqkey        setll     e5ffield                                     =BUILD
     c                   dou       *in70 = *on                                  =SUBFILE
     c     unqkey        reade(n)  e5ffield                               70
     c                   if        *in70 = *off
     c                             and IFDDLT <> 'Y'
     c                   exsr      datadependF
     c                   if        dataflag = *off
     c                   exsr      addsep
     c                   iter
     c                   endif
     c     comkey        chain     e5fcmpnt                           79
     c                   if        *in79 = *off
     c                   exsr      components
     c                   else
     c                   if        ifhcsf <>  *blanks
     c                   eval      fielddsc = *blanks
     c                   if        ETR510 = 'I'
     c     ifhcsf        chain     e5ffub04                           79
     c                   else
     c     ifhcsf        chain     e5ff1500                           79
     c                   endif
     c                   if        *in79 = *off
     c                   if        ETR510 = 'I'
     c                   eval      fielddsc = %trim(u4name) + '-' + u4desc
     c                   else
     c                   eval      fielddsc = %trim(b5name) + '-' + b5desc
     c                   endif
     c                   endif
     c                   eval      hldhcs = ifhcsf
     c                   eval      ovrtype = 'F'
     c                   exsr      getvalues
     c                   exsr      override
     c                   if        datafield = *blanks
     c                   eval      datafield = ifdeft
     c                   endif
     c                   if        ifpatt <> *blanks
     c                   exsr      pattern
     c                   endif
     c*                  if        mode <> 'P'
     c*                  eval      datafield = %subst(datafield:1:ifsmax)
     c*                  endif
     c*                  if        mode = 'P'
     c                   exsr      modecol2
     c*                  else
     c*                  eval      dataline = %trim(dataline) +
     c*                                        %trim(datafield)
     c*                  endif

     c                   else
     c                   eval      special = *off
     c                   eval      hlddft = ifdeft
     c                   exsr      specialval
     c                   if        ifpatt <> *blanks
     c                   exsr      pattern
     c                   endif
     c*                  if        mode <> 'P'
     c*                  eval      datafield = %subst(datafield:1:ifsmax)
     c*                  endif
     c                   eval      dataline = %trim(dataline) +
     c                                         %trim(datafield)
     c                   endif
     c                   exsr      addsep
      ** END COM
     c                   endif
     c                   endif
     c                   enddo
     c*                  if        mode = 'P'
     c                   exsr      writepreview
     c*                  else
     c*                  exsr      writeelcdata
     c*                  endif
     c                   endif
     c                   enddo
      ** end footer section
     c                   endif
      ** END LOOP -
     c     mode          cabeq     'P'           skip

     c                   move      'Y'           bbelct
     c                   IF        bbaafl <> *blanks
     c                   movel(P)  '*SYSTEM'     bbtsnm
     c                   else
     c                   movel     LDAUSR        BBTSNM
     c                   endif
     c                   time                    tmptim
     c                   movel     tmptim        BBTSTM
     c                   z-add     *date         mdate
     c                   exsr      srcmdy
     c                   z-add     ydate         BBTSDT
     c
     c                   if        svtorb <> *blanks
     c                   eval      bbtorb = svtorb
     c                   eval      svtorb = *blanks
     c                   endif
     c
     c                   update    hbfbillm

     c                   enddo

     c     skip          tag
     c                   if        mode <> 'P' and stotal <> 0
     c                   except    prvtot
     c                   except    lastr
     c                   close     printer
     c                   endif

     c                   eval      *inlr = *on
     c********************************************************************
     c     modecol1      begsr
     c                   if        lsduse = 'R'
     c                   eval      dataline =
     c                             '<span style=' + '''color:blue'''+
     c                             ' title='''+%trim(lsdlop)     +
     c                             ' : '+%trim(lsdseg)     +
     c                               '-' + %trim(lsddsc) +'''>' +
     c                               %trim(lsdseg) +
     c                              '</span>' + %trim(etrsep)
     c                   else
     c                   eval      dataline =
     c                             '<span style=' + '''color:purple'''+
     c                             ' title='''+%trim(lsdlop)     +
     c                             ' : '+%trim(lsdseg)     +
     c                               '-' + %trim(lsddsc) +'''>' +
     c                               %trim(lsdseg) +
     c                              '</span>' + %trim(etrsep)
     c                   endif
     c                   endsr
     c********************************************************************
     c     modecol2      begsr
     c                   eval      dataline = %trim(dataline) +
     c                             '<span style=' + '''color:blue'''+
     c                             ' title='''+%trim(ifname) +
     c                             '-' + %trim(ifddsc) + '::' +
     c                             %trim(fielddsc) + '''>' +
     c                             %trim(datafield) + '</span>'
     c                   endsr
     c*****************************************************************
     c     *inzsr        begsr

     c     *dtaara       define    *lda          ldads
     c                   in        *dtaara

     c     liceky        klist
     c                   kfld                    doc#
     c                   kfld                    lcode

     c     licky2        klist
     c                   kfld                    hmddr#
     c                   kfld                    lcode

     c     bilkey        klist
     c                   kfld                    bbpayr
     c                   kfld                    bbplan

     c     ovrkey        klist
     c                   kfld                    xODFRM
     c                   kfld                    xODTYP
     c                   kfld                    xOD510
     c                   kfld                    xODUBC
     c                   kfld                    xODPLN
     c                   kfld                    xODSEQ
     c                   kfld                    xODLOP
     c                   kfld                    xODSEG
     c                   kfld                    xODFSQ
     c                   kfld                    xODCSQ
     c                   kfld                    xODATA


     c     trkchg        klist
     c                   kfld                    billtrack20
     c                   kfld                    counter2

     c     Strakchg      klist
     c                   kfld                    bbtrak
     c                   kfld                    Scounter2

     c     Strkchg       klist
     c                   kfld                    billtrack20
     c                   kfld                    Scounter2

     c     acctky        klist
     c                   kfld                    bbplv6
     c                   kfld                    bbaccn

     c     trakchg       klist
     c                   kfld                    bbtrak
     c                   kfld                    counter2

     c     looprpt       klist
     c                   kfld                    savFRM
     c                   kfld                    savTYP
     c                   kfld                    sav510
     c                   kfld                    payor
     c                   kfld                    plan
     c                   kfld                    savSEQ
     c                   kfld                    savLOP

     c     looprpt2      klist
     c                   kfld                    savFRM
     c                   kfld                    savTYP
     c                   kfld                    sav510
     c                   kfld                    payor
     c                   kfld                    plan

     c     comkey        klist
     c                   kfld                    reqfrm
     c                   kfld                    reqtyp
     c                   kfld                    etr510
     c                   kfld                    payor
     c                   kfld                    plan
     c                   kfld                    IFDSEQ
     c                   kfld                    IFDLOP
     c                   kfld                    IFDSEG
     c                   kfld                    IFDFSQ

     c     begingloop    klist
     c                   kfld                    reqfrm
     c                   kfld                    reqtyp
     c                   kfld                    etr510
     c                   kfld                    payor
     c                   kfld                    plan
     c                   kfld                    lsdseq

     c     billtp        klist
     c                   kfld                    bbpayr
     c                   kfld                    bbplan
     c                   kfld                    bgcat1
     c                   kfld                    workcat2
     c                   kfld                    workcat3
     c                   kfld                    worktobf

     c     depkey        klist
     c                   kfld                    LSD510
     c                   kfld                    LSDFRM
     c                   kfld                    LSDTYP
     c                   kfld                    payor
     c                   kfld                    plan
     c                   kfld                    LSDSEQ
     c                   kfld                    LSDLOP
     c                   kfld                    LSDSEG

     c     depkeyF       klist
     c                   kfld                    LSD510
     c                   kfld                    LSDFRM
     c                   kfld                    LSDTYP
     c                   kfld                    payor
     c                   kfld                    plan
     c                   kfld                    LSDSEQ
     c                   kfld                    LSDLOP
     c                   kfld                    LSDSEG
     c                   kfld                    IFDFSQ

     c     depkeydata    klist
     c                   kfld                    LSD510
     c                   kfld                    LSDFRM
     c                   kfld                    LSDTYP
     c                   kfld                    payor
     c                   kfld                    plan
     c                   kfld                    LSDSEQ
     c                   kfld                    LSDLOP
     c                   kfld                    LSDSEG
     c                   kfld                    zeroseq
     c                   kfld                    depdata

     c     depkeydataN   klist
     c                   kfld                    LSD510
     c                   kfld                    LSDFRM
     c                   kfld                    LSDTYP
     c                   kfld                    payor
     c                   kfld                    plan
     c                   kfld                    LSDSEQ
     c                   kfld                    LSDLOP
     c                   kfld                    LSDSEG
     c                   kfld                    zeroseq
     c                   kfld                    depdataN

     c     depkeydataf   klist
     c                   kfld                    IFD510
     c                   kfld                    IFDFRM
     c                   kfld                    IFDTYP
     c                   kfld                    payor
     c                   kfld                    plan
     c                   kfld                    IFDSEQ
     c                   kfld                    IFDLOP
     c                   kfld                    IFDSEG
     c                   kfld                    IFDFSQ
     c                   kfld                    depdata

     c     depkeydatafN  klist
     c                   kfld                    IFD510
     c                   kfld                    IFDFRM
     c                   kfld                    IFDTYP
     c                   kfld                    payor
     c                   kfld                    plan
     c                   kfld                    IFDSEQ
     c                   kfld                    IFDLOP
     c                   kfld                    IFDSEG
     c                   kfld                    IFDFSQ
     c                   kfld                    depdataN

     c     depkeyc       klist
     c                   kfld                    IFD510
     c                   kfld                    IFDFRM
     c                   kfld                    IFDTYP
     c                   kfld                    payor
     c                   kfld                    plan
     c                   kfld                    IFDSEQ
     c                   kfld                    IFDLOP
     c                   kfld                    IFDSEG
     c                   kfld                    IFDFSQ

     c     depcom        klist
     c                   kfld                    DENFRM
     c                   kfld                    DENTYP
     c                   kfld                    DEN510
     c                   kfld                    payor
     c                   kfld                    plan
     c                   kfld                    DENSEQ
     c                   kfld                    DENLOP
     c                   kfld                    DENSEG
     c                   kfld                    DENFSQ
     c                   kfld                    DENCSQ
     c
     c     depfld        klist
     c                   kfld                    DENFRM
     c                   kfld                    DENTYP
     c                   kfld                    DEN510
     c                   kfld                    payor
     c                   kfld                    plan
     c                   kfld                    DENSEQ
     c                   kfld                    DENLOP
     c                   kfld                    DENSEG
     c                   kfld                    DENFSQ
     c
     c     wehkey        klist
     c                   kfld                    weuser
     c                   kfld                    wedate
     c                   kfld                    wetime
     c                   kfld                    weseqn

     c     wehkey2       klist
     c                   kfld                    weuser
     c                   kfld                    wedate
     c                   kfld                    wetime

     c     unqkey        klist
     c                   kfld                    lsdfrm
     c                   kfld                    lsdtyp
     c                   kfld                    lsd510
     c                   kfld                    payor
     c                   kfld                    plan
     c                   kfld                    lsdseq
     c                   kfld                    lsdlop
     c                   kfld                    lsdseg

     c     frmtyp        klist
     c                   kfld                    reqfrm
     c                   kfld                    reqtyp

     c     frmtyppp      klist
     c                   kfld                    reqfrm
     c                   kfld                    reqtyp
     c                   kfld                    etr510
     c                   kfld                    payor
     c                   kfld                    plan

     c     seqkey        klist
     c                   kfld                    bbtrak
     c                   kfld                    trknum2

     c     lvl6proc      klist
     c                   kfld                    bgdlv6
     c                   kfld                    bgdtpr
     c

     c                   endsr
     c*****************************************************************
     c     datadepend    begsr
     c** DATA DEPENDENT LOOP CHECK

     c                   eval      dataflag = *on

     c     depkeydata    chain     e5fdepnd                           79
     c                   if        *in79 = *off
     c     depfld        chain     e5ffield                           79
     c                   if        *in79 = *on
     c                   eval      dataflag = *off
     c                   else
     c                   if        DENCSQ = 0
     c     depfld        chain     e5ffield                           79
     c                   if        *in79 = *on
     c                   eval      dataflag = *off
     c                   else
     c                   eval      datafield = *blanks
     c                   eval      hldhcs = ifhcsf
     c                   eval      ovrtype = 'G'
     c                   exsr      getvalues
     c                   exsr      override
     c                   if        %subst(ifpatt:1:4) = 'PRV_'
     c                             or %subst(ifpatt:1:4) = '#DOC'
     c                             or %subst(ifpatt:1:4) = '#FLD'
     c                             or %subst(ifpatt:1:4) = '#PRV'
     c                             or %subst(ifpatt:1:7) = '#LOCAMT'
     c                   exsr      pattern
     c                   endif
     c                   if        datafield = *blanks
     c                   eval      dataflag = *off
     c                   endif
     c                   endif
     c                   else
     c     depcom        chain     e5fcmpnt                           79
     c                   if        *in79 = *on
     c                   eval      dataflag = *off
     c                   else
     c                   eval      datafield = *blanks
     c                   eval      hldhcs = ichcsf
     c                   eval      ovrtype = 'D'
     c                   exsr      getvalues
     c                   exsr      override
     c                   if        %subst(icpatt:1:4) = 'PRV_'
     c                             or %subst(icpatt:1:4) = '#DOC'
     c                             or %subst(icpatt:1:4) = '#FLD'
     c                             or %subst(icpatt:1:4) = '#PRV'
     c                             or %subst(icpatt:1:5) = '#BOX4'
     c                             or %subst(icpatt:1:5) = '#ICDV'
     c                   if        icpatt <> *blanks
     c                   eval      ifpatt = icpatt
     c                   endif
     c                   exsr      pattern
     c                   endif
     c                   if        datafield = *blanks
     c                   eval      dataflag = *off
     c                   endif
     c                   endif
      * look at compents
     c                   endif
     c                   endif

     c                   if        (DEDAT1 = '#NOTBL' or
     c                             DEDAT2 = '#NOTBL' or
     c                             DEDAT3 = '#NOTBL' or
     c                             DEDAT4 = '#NOTBL' or
     c                             DEDAT5 = '#NOTBL')
     C                             and datafield = *blanks
     c                   eval      dataflag = *off
     c                   endif

     c                   if        datafield <> DEDAT1
     c                             and datafield <> DEDAT2
     c                             and datafield <> DEDAT3
     c                             and datafield <> DEDAT4
     c                             and datafield <> DEDAT5
     c                             and DEDAT1 <> '#NOTBL'
     c                             and DEDAT2 <> '#NOTBL'
     c                             and DEDAT3 <> '#NOTBL'
     c                             and DEDAT4 <> '#NOTBL'
     c                             and DEDAT5 <> '#NOTBL'
     c                   eval      dataflag = *off
     c                   endif

     c                   endif
     c** EMD DATA CHECK
      ** Check for NOT DATA depend
     c     depkeydataN   chain     e5fdepnd                           79
     c                   if        *in79 = *off
     c     depfld        chain     e5ffield                           79
     c                   if        *in79 = *on
     c                   eval      dataflag = *off
     c                   else
     c                   if        DENCSQ = 0
     c     depfld        chain     e5ffield                           79
     c                   if        *in79 = *on
     c                   eval      dataflag = *off
     c                   else
     c                   eval      datafield = *blanks
     c                   eval      hldhcs = ifhcsf
     c                   eval      ovrtype = 'G'
     c                   exsr      getvalues
     c                   exsr      override
     c                   if        %subst(ifpatt:1:4) = 'PRV_'
     c                             or %subst(ifpatt:1:4) = '#DOC'
     c                             or %subst(ifpatt:1:4) = '#FLD'
     c                             or %subst(ifpatt:1:4) = '#PRV'
     c                             or %subst(ifpatt:1:7) = '#LOCAMT'
     c                   exsr      pattern
     c                   endif
     c                   endif
     c                   else
     c     depcom        chain     e5fcmpnt                           79
     c                   if        *in79 = *on
     c                   eval      dataflag = *off
     c                   else
     c                   eval      datafield = *blanks
     c                   eval      hldhcs = ichcsf
     c                   eval      ovrtype = 'D'
     c                   exsr      getvalues
     c                   exsr      override
     c                   if        %subst(icpatt:1:4) = 'PRV_'
     c                             or %subst(icpatt:1:4) = '#DOC'
     c                             or %subst(icpatt:1:4) = '#FLD'
     c                             or %subst(icpatt:1:4) = '#PRV'
     c                             or %subst(icpatt:1:5) = '#BOX4'
     c                             or %subst(icpatt:1:5) = '#ICDV'
     c                   if        icpatt <> *blanks
     c                   eval      ifpatt = icpatt
     c                   endif
     c                   exsr      pattern
     c                   endif
     c                   endif
      * look at compents
     c                   endif
     c                   endif

     c                   if        (DEDAT1 = '#NOTBL' or
     c                             DEDAT2 = '#NOTBL' or
     c                             DEDAT3 = '#NOTBL' or
     c                             DEDAT4 = '#NOTBL' or
     c                             DEDAT5 = '#NOTBL')
     C                             and datafield <> *blanks
     c                   eval      dataflag = *off
     c                   endif

     c                   if        (datafield = DEDAT1
     c                             or datafield = DEDAT2
     c                             or datafield = DEDAT3
     c                             or datafield = DEDAT4
     c                             or datafield = DEDAT5)
     c                             and datafield <> *blanks
     c                   eval      dataflag = *off
     c                   endif

     c                   endif
     c                   endsr
     c*****************************************************************
     c     datadependF   begsr
     c** DATA DEPENDENT LOOP CHECK

     c                   eval      sfdseq = ifdseq
     c                   eval      sfdlop = ifdlop
     c                   eval      sfdseg = ifdseg
     c                   eval      sfdfsq = ifdfsq
     c                   eval      sfname = ifname
     c                   eval      sfddsc = ifddsc
     c                   eval      sfddlt = ifddlt
     c                   eval      sfsmin = ifsmin
     c                   eval      sfsmax = ifsmax
     c                   eval      sformt = iformt
     c                   eval      sfpatt = ifpatt
     c                   eval      sfhcsf = ifhcsf
     c                   eval      sfdeft = ifdeft
     c                   eval      sftsep = iftsep
     c                   eval      sfusge = ifusge
    
     c                   eval      dataflag = *on

     c     depkeydataF   chain     e5fdepnd                           79
     c                   if        *in79 = *off
     c     depfld        chain     e5ffield2                          79
     c                   if        *in79 = *on
     c                   eval      dataflag = *off
     c                   else
     c                   if        DENCSQ = 0
     c     depfld        chain     e5ffield2                          79
     c                   if        *in79 = *on
     c                   eval      dataflag = *off
     c                   else
     c                   eval      datafield = *blanks
     c                   eval      hldhcs = ifhcsf
     c                   eval      ovrtype = 'G'
     c                   exsr      getvalues
     c                   exsr      override
     c                   if        %subst(ifpatt:1:4) = 'PRV_'
     c                             or %subst(ifpatt:1:4) = '#DOC'
     c                             or %subst(ifpatt:1:4) = '#FLD'
     c                             or %subst(ifpatt:1:4) = '#PRV'
     c                             or %subst(ifpatt:1:7) = '#LOCAMT'
     c                   exsr      pattern
     c                   endif
     c                   if        datafield = *blanks
     c                   eval      dataflag = *off
     c                   endif
     c                   endif
     c                   else
     c     depcom        chain     e5fcmpnt                           79
     c                   if        *in79 = *on
     c                   eval      dataflag = *off
     c                   else
     c                   eval      datafield = *blanks
     c                   eval      hldhcs = ichcsf
     c                   eval      ovrtype = 'D'
     c                   exsr      getvalues
     c                   exsr      override
     c                   if        %subst(icpatt:1:4) = 'PRV_'
     c                             or %subst(icpatt:1:4) = '#DOC'
     c                             or %subst(icpatt:1:4) = '#FLD'
     c                             or %subst(icpatt:1:4) = '#PRV'
     c                             or %subst(icpatt:1:5) = '#BOX4'
     c                   if        icpatt <> *blanks
     c                   eval      ifpatt = icpatt
     c                   endif
     c                   exsr      pattern
     c                   endif
     c                   if        datafield = *blanks
     c                   eval      dataflag = *off
     c                   endif
     c                   endif
      * look at compents
     c                   endif
     c                   endif

     c                   if        (DEDAT1 = '#NOTBL' or
     c                             DEDAT2 = '#NOTBL' or
     c                             DEDAT3 = '#NOTBL' or
     c                             DEDAT4 = '#NOTBL' or
     c                             DEDAT5 = '#NOTBL')
     C                             and datafield = *blanks
     c                   eval      dataflag = *off
     c                   endif

     c                   if        datafield <> DEDAT1
     c                             and datafield <> DEDAT2
     c                             and datafield <> DEDAT3
     c                             and datafield <> DEDAT4
     c                             and datafield <> DEDAT5
     c                             and DEDAT1 <> '#NOTBL'
     c                             and DEDAT2 <> '#NOTBL'
     c                             and DEDAT3 <> '#NOTBL'
     c                             and DEDAT4 <> '#NOTBL'
     c                             and DEDAT5 <> '#NOTBL'
     c                   eval      dataflag = *off
     c                   endif

     c                   endif
     c** EMD DATA CHECK
      ** Check for NOT DATA depend
     c     depkeydataFN  chain     e5fdepnd                           79
     c                   if        *in79 = *off
     c     depfld        chain     e5ffield2                          79
     c                   if        *in79 = *on
     c                   eval      dataflag = *off
     c                   else
     c                   if        DENCSQ = 0
     c     depfld        chain     e5ffield2                          79
     c                   if        *in79 = *on
     c                   eval      dataflag = *off
     c                   else
     c                   eval      datafield = *blanks
     c                   eval      hldhcs = ifhcsf
     c                   eval      ovrtype = 'G'
     c                   exsr      getvalues
     c                   exsr      override
     c                   if        %subst(ifpatt:1:4) = 'PRV_'
     c                             or %subst(ifpatt:1:4) = '#DOC'
     c                             or %subst(ifpatt:1:4) = '#FLD'
     c                             or %subst(ifpatt:1:4) = '#PRV'
     c                             or %subst(ifpatt:1:7) = '#LOCAMT'
     c                   exsr      pattern
     c                   endif
     c                   endif
     c                   else
     c     depcom        chain     e5fcmpnt                           79
     c                   if        *in79 = *on
     c                   eval      dataflag = *off
     c                   else
     c                   eval      datafield = *blanks
     c                   eval      hldhcs = ichcsf
     c                   eval      ovrtype = 'D'
     c                   exsr      getvalues
     c                   exsr      override
     c                   if        %subst(icpatt:1:4) = 'PRV_'
     c                             or %subst(icpatt:1:4) = '#DOC'
     c                             or %subst(icpatt:1:4) = '#FLD'
     c                             or %subst(icpatt:1:4) = '#PRV'
     c                             or %subst(icpatt:1:5) = '#BOX4'
     c                   if        icpatt <> *blanks
     c                   eval      ifpatt = icpatt
     c                   endif
     c                   exsr      pattern
     c                   endif
     c                   endif
      * look at compents
     c                   endif
     c                   endif

     c                   if        (DEDAT1 = '#NOTBL' or
     c                             DEDAT2 = '#NOTBL' or
     c                             DEDAT3 = '#NOTBL' or
     c                             DEDAT4 = '#NOTBL' or
     c                             DEDAT5 = '#NOTBL')
     c                             and datafield <> *blanks
     c                   eval      dataflag = *off
     c                   endif

     c                   if        datafield = DEDAT1
     c                             or datafield = DEDAT2
     c                             or datafield = DEDAT3
     c                             or datafield = DEDAT4
     c                             or datafield = DEDAT5
     c                   eval      dataflag = *off
     c                   endif

     c                   endif
    
     c                   eval      ifdseq = sfdseq
     c                   eval      ifdlop = sfdlop
     c                   eval      ifdseg = sfdseg
     c                   eval      ifdfsq = sfdfsq
     c                   eval      ifname = sfname
     c                   eval      ifddsc = sfddsc
     c                   eval      ifddlt = sfddlt
     c                   eval      ifsmin = sfsmin
     c                   eval      ifsmax = sfsmax
     c                   eval      iformt = sformt
     c                   eval      ifpatt = sfpatt
     c                   eval      ifhcsf = sfhcsf
     c                   eval      ifdeft = sfdeft
     c                   eval      iftsep = sftsep
     c                   eval      ifusge = sfusge

     c                   endsr
     c*****************************************************************
     c     pattern       begsr

     c                   eval      spos = 0
     c                   eval      spos = %scan('#MODS':ifpatt)
     c                   if        spos <> 0

     c                   eval      hold1 = %subst(ifpatt:7:1)

     c                   select
     c                   when      hold1 = '1'
     c                   eval      datafield = %subst(datafield:1:2)
     c                   when      hold1 = '2'
     c                   eval      datafield = %subst(datafield:3:2)
     c                   when      hold1 = '3'
     c                   eval      datafield = %subst(datafield:5:2)
     c                   when      hold1 = '4'
     c                   eval      datafield = %subst(datafield:7:2)
     c                   endsl
     c                   endif

     c                   eval      spos = 0
     c                   eval      spos = %scan('#NAME':ifpatt)
     c                   if        spos <> 0
     c                   eval      namein = datafield
     c                   call      'XFXBNAM'
     c                   parm                    namein
     c                   parm                    last
     c                   parm                    first
     c                   parm                    middle
     c                   parm                    jrorsr

     c                   eval      hold1 = %subst(ifpatt:7:1)

     c                   eval      datafield = first
     c                   select
     c                   when      hold1 = 'F'
     c                   eval      datafield = first
     c                   when      hold1 = 'M'
     c                   eval      datafield = middle
     c                   when      hold1 = 'L'
     c                   eval      datafield = last
     c                   when      hold1 = 'S'
     c                   eval      datafield = jrorsr
     c                   when      hold1 = 'P'
     c                   eval      datafield = *blanks
     c                   endsl
     c                   endif

     c                   eval      spos = 0
     c                   eval      spos = %scan('#BOX4':ifpatt)
     c                   if        spos <> 0
     c                   eval      hold1 = %subst(ifpatt:7:1)
     c                   eval      hold2 = %subst(ifpatt:8:1)
     c                   eval      hold3 = %subst(ifpatt:9:1)
     c                   select
     c                   when      hold1 = '1'
     c                   eval      hold1 = %subst(datafield:2:1)
     c                   when      hold1 = '2'
     c                   eval      hold1 = %subst(datafield:3:1)
     c                   when      hold1 = '3'
     c                   eval      hold1 = %subst(datafield:4:1)
     c                   endsl
     c                   select
     c                   when      hold2 = '1'
     c                   eval      hold2 = %subst(datafield:2:1)
     c                   when      hold2 = '2'
     c                   eval      hold2 = %subst(datafield:3:1)
     c                   when      hold2 = '3'
     c                   eval      hold2 = %subst(datafield:4:1)
     c                   endsl
     c                   select
     c                   when      hold3 = '1'
     c                   eval      hold3 = %subst(datafield:2:1)
     c                   when      hold3 = '2'
     c                   eval      hold3 = %subst(datafield:3:1)
     c                   when      hold3 = '3'
     c                   eval      hold3 = %subst(datafield:4:1)
     c                   endsl
     c                   eval      datafield = %trim(hold1) +
     c                                %trim(hold2) + %trim(hold3)
     c                   endif

     c                   eval      spos = 0
     c                   eval      spos = %scan('*CONCAT':ifpatt)                            UBTDAT
     c                   if        spos <> 0
     c                   eval      newsep = %subst(ifpatt:8:1)
     c                   eval      datafields = datafield
     c                   eval      hldhcs = %subst(ifpatt:9:6)
     c                   eval      ovrtype = 'F'
     c                   exsr      getvalues
     c                   exsr      override
     c                   if        newsep = '*'
     c                   eval      datafield = %trim(datafields) + ' '
     c                                         + %trim(datafield)
     c                   else
     c                   eval      datafield = %trim(datafields) + %trim(newsep)
     c                                         + %trim(datafield)
     c                   endif
     c                   endif

     c                   eval      spos = 0
     c                   eval      spos = %scan('PRV_':ifpatt)
     c                   if        spos <> 0
     c                   eval      prvid = %subst(ifpatt:5:5)
     c                   exsr      srprov
     c                   if        prvvar <> *blanks
     c                   eval      datafield = %trim(prvvar)
     c                   endif
     c                   endif

     c                   eval      spos = 0
     c                   eval      spos = %scan('YYYYMMDD':ifpatt)
     c                   if        spos <> 0
     c                   movel     datafield     mdate
     c                   exsr      srcmdy
     c                   eval      datafield = %char(ydate)
     c                   endif

     c                   eval      spos = 0
     c                   eval      spos = %scan('#POSD' : ifpatt)
     c                   if        spos <> 0
     c                   exsr      srposd
     c                   endif

     c                   eval      spos = 0
     c                   eval      spos = %scan('#NOTPOSD' : ifpatt)
     c                   if        spos <> 0
     c                   if        bgdpos <> *blanks
     c                             and bgdpos <> clm05_1
     c                   eval      datafield = %trim(bgdpos)
     c                   else
     c                   eval      datafield = *blanks
     c                   endif
     c                   endif

     c                   eval      spos = 0
     c                   eval      spos = %scan('STRIP':ifpatt)                              UBTDAT
     c                   if        spos <> 0
     c****8              eval      newsep = %subst(ifpatt:6:1)
     c                   eval      newsep = %subst(ifpatt:spos+5:1)
     c     nextcheck     tag
     c                   eval      spos = 0
     c                   eval      spos = %scan(newsep:datafield)                            UBTDAT
     c                   if        spos <> 0
     c                   eval      npos = spos - 1
     c                   eval      ppos = spos + 1
     c                   eval      datafield = %subst(datafield:1:npos) +
     c                              %subst(datafield:ppos:2000 - spos)
     c                   goto      nextcheck
     c                   endif
     c                   endif

     c                   if        datafield <> *blanks
     c                             and mode = 'P'
     c                   eval      datafield = '<span style=' +
     c                             '''color:green'''+ ' title='''+%trim(ifname)+
     c                             '-'+%trim(ifddsc) + '''>' +
     c                             %trim(datafield) + '</span>'
     c                   endif

     c                   eval      spos = 0
     c                   eval      spos = %scan('#DOCN':ifpatt)
     c                   if        spos <> 0
     c                   movel     datafield     doc#
     c                   eval      datafield = *blanks
     c                   eval      namein = *blanks
     c                   if        b5ffl1 = 'Y'                                 =bilexc 861
     c                   eval      lcode = 'SNPI '
     c     liceky        chain     hmllice
     c                   if        %found(hmllice)
     c                   eval      snpi = hlcnum
     c     snpi          chain     hmfnpi
     c                   if        %found(hmpnpi)
     c                   eval      namein = npname
     c                   endif
     c                   endif
     c                   else
     c     doc#          chain     hmfmams
     c                   if        %found(hmpmams)
     c                   eval      namein = hmdnam
     c                   endif
     c                   endif
     c                   if        namein <> *blanks
     c                   call      'XFXBNAM'
     c                   parm                    namein
     c                   parm                    last
     c                   parm                    first
     c                   parm                    middle
     c                   parm                    jrorsr

     c                   eval      hold1 = %subst(ifpatt:7:1)

     c                   eval      datafield = first
     c                   select
     c                   when      hold1 = 'F'
     c                   eval      datafield = first
     c                   when      hold1 = 'M'
     c                   eval      datafield = middle
     c                   when      hold1 = 'L'
     c                   eval      datafield = last
     c                   when      hold1 = 'S'
     c                   eval      datafield = jrorsr
     c                   when      hold1 = 'P'
     c                   eval      datafield = *blanks
     c                   endsl
     c                   endif
     c                   endif

     c                   eval      spos = 0
     c                   eval      spos = %scan('#DOCL':ifpatt)
     c                   if        spos <> 0
     c                   movel     datafield     doc#
     c                   eval      datafield = *blanks
     c                   if        b5ffl1 = 'Y'                                 =bilexc 861
     c                   eval      lcode = 'SNPI '
     c     liceky        chain     hmllice
     c                   if        %found(hmllice)
     c                   eval      datafield = hlcnum
     c                   endif
     c                   else
     c     doc#          chain     hmfmams
     c                   if        %found(hmpmams)
     c                   eval      datafield = *blanks
     c                   eval      lcode = 'NPI'
     c     licky2        chain     hmllice
     c                   if        %found(hmllice)
     c                   eval      datafield = hlcnum
     c                   endif
     c                   endif
     c                   endif
     c                   endif

     c                   eval      spos = 0
     c                   eval      spos = %scan('#DOCT':ifpatt)
     c                   if        spos <> 0
     c                   call      'XFXMASGN'
     c                   parm      bbplv6        lv6#
     c                   parm      bbaccn        acct#
     c                   parm      'AT'          status
     c                   parm      bbtodt        rqsdte
     c                   parm                    doc#
     c                   if        doc# <> 0
     c                   eval      datafield = *blanks
     c     doc#          chain     hmfmams
     c                   if        %found(hmpmams)
     c                   eval      datafield = *blanks
     c                   eval      lcode = 'TAXON'
     c     licky2        chain     hmllice
     c                   if        %found(hmllice)
     c                   eval      datafield = hlcnum
     c                   endif
     c                   endif
     c                   endif
     c                   endif

     c                   eval      spos = 0
     c                   eval      spos = %scan('#DOCR_':ifpatt)
     c                   if        spos <> 0 and datafield = *blanks
     c                   if        bgdtth <> 0
     c                   eval      datafield = *blanks
     c     bgdtth        chain     hmfmams
     c                   if        %found(hmpmams)
     c                   eval      datafield = *blanks
     c                   eval      lcode = %subst(ifpatt:7:5)
     c     licky2        chain     hmllice
     c                   if        %found(hmllice)
     c                   eval      datafield = hlcnum
     c                   endif
     c                   endif
     c                   endif
     c                   endif

     c                   eval      spos = 0
     c                   eval      spos = %scan('#PRV_':ifpatt)
     c                   if        spos <> 0 and datafield = *blanks
     c                   eval      prvid = %subst(ifpatt:6:5)
     c                   exsr      srprov
     c                   if        prvvar <> *blanks
     c                   eval      datafield = %trim(prvvar)
     c                   endif
     c                   endif

     c                   eval      spos = 0
     c                   eval      spos = %scan('#FLD_':ifpatt)
     c                   if        spos <> 0 and datafield = *blanks
     c                   eval      hldhcs = %subst(ifpatt:6:6)
     c                   exsr      getvalues
     c                   exsr      override
     c                   endif

     c                   eval      spos = 0
     c                   eval      spos = %scan('#ICDV':ifpatt)
     c                   if        spos <> 0
     c                   if        (ETR510 = 'I' and fl66 = '0') or
     c                             (ETR510 = 'P' and b21icd = '0')
     c                   eval      datafield = 'A' + %trim(ifdeft)
     c                   endif
     c                   endif

     c                   eval      spos = 0
     c                   eval      spos = %scan('#LOCAMT' : ifpatt)
     c                   if        spos <> 0
     c                   eval      rqdate = bbfrdt
     c                   exsr      srtnsf
     c                   eval      datafield = 'LOCAMT='
     c                             + %subst(rqcat3 : 3 : 1)
     c                             + ';'
     c                             + %char(flcaa1)
     c                   endif

     c                   endsr
     c*****************************************************************
     c     srtnsf        begsr

     c                   call      'XFXTRNSF'
     c                   parm      bbplv6        rqlvl6
     c                   parm      bbaccn        rqaccn
     c                   parm                    rqdate
     c                   parm      *blanks       rqcat1
     c                   parm      *blanks       rqcat2
     c                   parm      *blanks       rqcat3
     c                   parm      *blanks       rqroom
     c                   parm      *blanks       rqrcls
     c                   parm      *blanks       rqproc
     c                   parm                    rqchng
      **
     c                   endsr
      *****************************************************************
     c     srprov        begsr
      **   ======        =====
     c                   call      'XFXPRV'
     c                   parm                    bbplv6
     c                   parm                    bbaccn
     c                   parm                    bbpayr
     c                   parm                    bbplan
     c                   parm                    prvid
     c                   parm                    prmprc
     c                   parm      0             prmrev
     c                   parm      bbtodt        prvdte
     c                   parm      *blanks       prvvar
     c                   parm      *blanks       prvadr
      **
     c                   endsr
      *****************************************************************
     c     components    begsr

     c** Code below compensates for user error - if they put fields in the wrong order
      ** the components will self correct

     c                   eval      len = 0
     c                   eval      len = %len(%trim(dataline))
     c                   if        len <> 0
     c                   if        %subst(dataline:len:1) = etrssp
     c                   eval      %subst(dataline:len:1) = etrsep
     c                   endif
     c                   endif

     c     comkey        setll     e5fcmpnt                                     =BUILD
     c                   dou       *in74 = *on                                  =SUBFILE
     c     comkey        reade(n)  e5fcmpnt                               74
     c                   if        *in74 = *off

     c     depkeyc       chain     e5fdepnd                           79
     c                   if        *in79 = *off
      * look at fields
     c                   if        DENCSQ = 0
     c                   else
     c     depcom        chain     e5fcmpnt2                          79
     c                   if        *in79 = *on
     c                   iter
     c                   else
     c                   eval      datafield = *blanks
     c                   eval      hldhcs = xchcsf
     c                   eval      ovrtype = 'C'
     c                   exsr      getvalues
     c                   exsr      override
     c                   if        datafield = *blanks
     c                   eval      len = 0
     c                   eval      len = %len(%trim(dataline))
     c                   if        len <> 0
     c                   if        %subst(dataline:len:1) = etrsep
     c                   if        depdep <> depfield
     c                   eval      %subst(dataline:len:1) = etrssp
     c                   else
     c                   eval      %subst(dataline:len+1:1) = etrsep
     c                   leave
     c                   endif
     c                   endif
     c                   endif
     c                   iter
     c                   endif
     c                   endif
      * look at compents
     c                   endif
     c                   endif

     c                   if        ichcsf <>  *blanks
     c                   eval      fielddsc = *blanks
     c                   if        ETR510 = 'I'
     c     ichcsf        chain     e5ffub04                           79
     c                   else
     c     ichcsf        chain     e5ff1500                           79
     c                   endif
     c                   if        *in79 = *off
     c                   if        ETR510 = 'I'
     c                   eval      fielddsc = %trim(u4name) + '-' + u4desc
     c                   else
     c                   eval      fielddsc = %trim(b5name) + '-' + b5desc
     c                   endif
     c                   endif
     c                   eval      hldhcs = ichcsf
     c                   eval      ovrtype = 'C'
     c                   exsr      getvalues
     c                   exsr      override
     c                   if        datafield = *blanks
     c                   eval      datafield = icdeft
     c                   endif
     c                   if        icpatt <> *blanks
     c                   eval      ifpatt = icpatt
     c                   eval      ifdeft = icdeft
     c                   exsr      pattern
     c                   endif
     c                   if        mode <> 'P'
     c                   eval      datafield = %subst(datafield:1:icsmax)
     c                   endif
     c                   if        mode = 'P'
     c                   exsr      modecol2
     c                   else
     c                   eval      dataline = %trim(dataline) +
     c                                         %trim(datafield)
     c                   endif
     c                   else
     c                   eval      special = *off
     c                   eval      hlddft = icdeft
     c                   exsr      specialval
     c                   if        icpatt <> *blanks
     c                   eval      ifpatt = icpatt
     c                   eval      ifdeft = icdeft
     c                   exsr      pattern
     c                   endif
     c                   if        mode <> 'P'
     c                   eval      datafield = %subst(datafield:1:icsmax)
     c                   endif
     c                   eval      dataline = %trim(dataline) +
     c                                         %trim(datafield)
     c                   endif
     c                   select
     c                   when      ictsep = 'D'
     c                   eval      dataline = %trim(dataline) + etrsep
     c                   when      ictsep = 'S'
     c                   eval      dataline = %trim(dataline) + etrssp
     c                   when      ictsep = 'C'
     c                   eval      dataline = %trim(dataline) + etrces
     c                   endsl
     c                   endif
     c                   enddo

     c                   movea     dataline      ary2000
      /free
           for i = %len(%trim(dataline)) downto 1;
              if i = %len(%trim(dataline)) ;
                 hldsep = ary2000(i);
              else;
                 if ary2000(i) <> etrces;
                    leave;
                 endif;
              endif;
                ary2000(i) = *blanks;
           endfor;
                ary2000(i + 1) = hldsep ;
      /end-free

     c                   movea     ary2000       dataline

     c                   endsr
     c*****************************************************************
     c     specialval    begsr

     c                   select
     c                   when      hlddft = '*BLANKS'
     c                   eval      datafield = *blanks
     c                   when      hlddft = '*DATE'
     c                   eval      datafield = yudate
     c                   eval      special = *on
     c                   when      hlddft = '*DATE6'
     c                   move      yudate        date6
     c                   eval      datafield = date6
     c                   eval      special = *on
     c                   when      hlddft = '*TIME'
     c                   eval      datafield = yutime
     c                   eval      special = *on
     c                   when      hlddft = '*TIME4'
     c                   movel     yutime        time4
     c                   eval      datafield = time4
     c                   eval      special = *on
     c                   other
     c                   eval      datafield = hlddft
     c                   endsl

     c                   if        special = *on
     c                   if        mode = 'P'
     c                   eval      datafield =
     c                             '<span style=' + '''color:red'''+
     c                             ' title='''+%trim(hlddft)  +
     c                               '''>' +
     c                               %trim(datafield) +
     c                              '</span>'
     c                   endif
     c                   else
     c                   if        hlddft <> '*BLANKS'
     c                             and mode = 'P'
     c                   eval      datafield = '<span style=' +
     c                             '''color:red'''+ ' title='''+%trim(ifname)+
     c                             '-'+%trim(ifddsc) + '''>' +
     c                             %trim(datafield) + '</span>'
     c                   endif
     c                   endif

     c                   endsr
     c*****************************************************************
     c     repeatCHARGE  begsr

     c                   eval      savFRM = LSDFRM
     c                   eval      savTYP = LSDTYP
     c                   eval      sav510 = LSD510
     c                   eval      savSEQ = LSDSEQ
     c                   eval      savLOP = LSDLOP
     c


     c                   eval      countr = 0
     c********           eval      counter2 = 0
     c                   eval      trknm2 += 1

     c     nxtchg        tag
     c                   eval      countr += 1
     c                   eval      counter2 += 1
     c                   if        ETR510 = 'I'
     c     trkchg        chain     e5fubdet                           60
     c                   else
     c     trakchg       chain     e5fchr15                           60
     c                   endif
     c     *in60         cabeq     *on           endrpt

     c                   eval      trknum2 = trknm2
     c                   if        countr = 1
     c     seqkey        setll     ebfseq
     c                   dou       *in76 = '1'
     c     seqkey        reade     ebfseq                                 76
     c                   if        *in76 = '0'
     c                   delete    ebfseq
     c                   endif
     c                   enddo
     c                   endif

     c                   eval      sqtrak = bbtrak
     c                   eval      sqtrk2 = trknm2
     c                   eval      sqseq = countr
     c                   if        ETR510 = 'I'
     c                   eval      sqchrg = UBDTC4
     c                   if        UBDTSD <> 0
     c                   eval      mdate = UBDTSD
     c                   exsr      srcmdy
     c                   eval      sqfrdt = ydate
     c                   eval      sqtodt = ydate
     c                   else
     c                   eval      sqfrdt = UBFDAT
     c                   eval      sqtodt = UBTDAT
     c                   endif
     c                   else
     c                   eval      sqchrg = BGDTC4
     c                   eval      sqfrdt = bgdtsd
     c                   eval      sqtodt = bgdtsd
     c                   endif
     c                   write     ebfseq

     c     looprpt       setll     e5floops2
     c                   dou       *in68 = *on
     c     looprpt2      reade     e5floops2                              68
     c                   if        *in68 = *off
     c                             and lsddlt <> 'Y'
     c*****              if        lsdlop <> savlop
     c*****              leave
     c*****              endif
     c                   if        lsdhdr <> 'C'
     c*****              iter
     c                   leave
     c                   endif
      **DEP
      *
     c     depkey        setll     e5fdepnd
     c     trydepagn     tag
     c     depkey        reade     e5fdepnd                               79
     c                   if        *in79 = *off
     c                             and depdep <> depfield
      * look at fields
     c                   if        DENCSQ = 0
     c     depfld        chain     e5ffield                           79
     c                   if        *in79 = *on
     c                   iter
     c                   else
     c                   eval      datafield = *blanks
     c                   eval      hldhcs = ifhcsf
     c                   eval      ovrtype = 'F'
     c                   exsr      getvalues
     c                   exsr      override
     c                   if        %subst(ifpatt:1:4) = 'PRV_'
     c                             or %subst(ifpatt:1:4) = '#DOC'
     c                             or %subst(ifpatt:1:4) = '#FLD'
     c                             or %subst(ifpatt:1:4) = '#PRV'
     c                             or %subst(ifpatt:1:7) = '#LOCAMT'
     c                   exsr      pattern
     c                   endif
     c                   if        datafield = *blanks
     c                   iter
     c                   endif
     c                   endif
     c                   else
     c     depcom        chain     e5fcmpnt                           79
     c                   if        *in79 = *on
     c                   iter
     c                   else
     c                   eval      datafield = *blanks
     c                   eval      hldhcs = ichcsf
     c                   eval      ovrtype = 'C'
     c                   exsr      getvalues
     c                   exsr      override
     c                   if        datafield = *blanks
     c                   iter
     c                   endif
     c                   endif
      * look at compents
     c                   endif
     c                   goto      trydepagn
     c                   endif
      **DEP
     c                   eval      dataline = *blanks
     c                   if        mode = 'P'
     c                   exsr      modecol1
     c                   else
     c                   eval      dataline = %trim(lsdseg) + %trim(etrsep)
     c                   endif
     c     unqkey        setll     e5ffield                                     =BUILD
     c                   dou       *in67 = *on                                  =SUBFILE
     c     unqkey        reade(n)  e5ffield                               67
     c                   if        *in67 = *off
     c                             and IFDDLT <> 'Y'
     c                   exsr      datadependF
     c                   if        dataflag = *off
     c                   exsr      addsep
     c                   iter
     c                   endif
     c     comkey        chain     e5fcmpnt                           79
     c                   if        *in79 = *off
     c                   exsr      components
     c                   else
     c                   if        ifhcsf <>  *blanks
     c                   eval      fielddsc = *blanks
     c                   if        ETR510 = 'I'
     c     ifhcsf        chain     e5ffub04                           79
     c                   else
     c     ifhcsf        chain     e5ff1500                           79
     c                   endif
     c                   if        *in79 = *off
     c                   if        ETR510 = 'I'
     c                   eval      fielddsc = %trim(u4name) + '-' + u4desc
     c                   else
     c                   eval      fielddsc = %trim(b5name) + '-' + b5desc
     c                   endif
     c                   endif
     c                   eval      hldhcs = ifhcsf
     c                   eval      ovrtype = 'F'
     c                   exsr      getvalues
     c                   exsr      override
     c                   if        datafield = *blanks
     c                   eval      datafield = ifdeft
     c                   endif
     c                   if        ifpatt <> *blanks
     c                   exsr      pattern
     c                   endif
     c                   if        mode <> 'P'
     c                   eval      datafield = %subst(datafield:1:ifsmax)
     c                   endif
     c                   if        mode = 'P'
     c                   exsr      modecol2
     c                   else
     c                   eval      dataline = %trim(dataline) +
     c                                         %trim(datafield)
     c                   endif
     c                   else
     c                   eval      special = *off
     c                   eval      hlddft = ifdeft
     c                   exsr      specialval
     c                   if        ifpatt <> *blanks
     c                   exsr      pattern
     c                   endif
     c                   if        mode <> 'P'
     c                   eval      datafield = %subst(datafield:1:ifsmax)
     c                   endif
     c                   eval      dataline = %trim(dataline) +
     c                                         %trim(datafield)
     c                   endif
     c                   exsr      addsep
      ** END COM
     c                   endif
     c                   endif
     c                   enddo
     c                   if        mode = 'P'
     c                   exsr      writepreview
     c                   else
     c                   exsr      writeelcdata
     c                   endif
     c                   endif
     c                   enddo

     c*****              if        (ETR510 = 'P' and countr = 50)
     c                   if        etrmlx <> 0  and countr >= etrmlx
     c                   eval      maxcharge = *on
     c                   goto      endrpt
     c                   endif
     c                   goto      nxtchg

     c     endrpt        endsr
      *****************************************************************
     c     geticn        begsr

     c                   eval      svtorb = bbtorb

      *** Gets the type of bill back depending on how it is defined in the Type of bill maintenance
     c                   eval      workcat2 = bgcat2
     c                   eval      workcat3 = bgcat3
     c                   movel     bbtorb        worktobf
     c     billtp        chain     hbftobf
     c                   if        not %found
     c                   eval      workcat3 = *blanks
     c     billtp        chain     hbftobf
     c                   if        not %found
     c                   eval      workcat2 = *blanks
     c     billtp        chain     hbftobf
     c                   endif
     c                   endif
     c                   if        %found
     c                   movel     tobfvl        bbtorb
     c                   endif

     c                   eval      icndcn = *blanks
     c                   if        bbtorb = '6' or bbtorb = '7' or bbtorb = '8'
B002 c                   if        icndcn = *blanks
     c                             and bbtcn <> *blanks
     c                   movel     bbtcn         icndcn
     c                   endif
     c                   endif

     c                   if        icndcn = *blanks
     c                   eval      bbtorb = '1'
     c                   endif

     c                   endsr
      *****************************************************************
     c     gettotchg     begsr

     c                   eval      xc = 0
     c                   eval      xa = 1
     c                   eval      prmprc = *blanks
     c     billtrack20   setll     e5fubdet
     c                   dou       %eof(e5pubdet)
     c     billtrack20   reade     e5fubdet
     c                   if        not %eof(e5pubdet)
     c*****              eval      totchg += ubdttc
     c                   if        xc = etrmlx and etrmlx <> 0
     c                   eval      xa += 1
     c                   eval      xc = 0
     c                   endif
     c                   eval      xc += 1
     c                   eval      totchga(xa) += ubdttc
     c                   if        ubfdat < BillFromDate(xa)                    Get earliest from dt
     c                   eval      BillFromDate(xa) = ubfdat
     c                   endif
     c                   if        ubtdat > BillToDate(xa)                      Get latest to date
     c                   eval      BillToDate(xa) = ubtdat
     c                   endif
     c                   eval      totchgx += ubdttc
     c                   endif
     c                   enddo

     c                   endsr
     c*****************************************************************
     c     gettotchgp    begsr

     c                   eval      xc = 0
     c                   eval      xa = 1
     c                   eval      prmprc = *blanks
     c     bbtrak        setll     e5fchr15
     c                   dou       %eof(e5pchr15)
     c     bbtrak        reade     e5fchr15
     c                   if        not %eof(e5pchr15)
     c                   if        prmprc = *blanks
     c                   eval      prmprc = bgdtpr
     c                   endif
     c*******************eval      totchg += bgdtam
     c*****              if        xc = 50
     c                   if        xc = etrmlx and etrmlx <> 0
     c                   eval      xa += 1
     c                   eval      xc = 0
     c                   endif
     c                   eval      xc += 1
     c                   eval      totchga(xa) += bgdtam
     c                   eval      totchgx += bgdtam
     c                   select
     c                   when      bbrank = 1
     c                   eval      totcpy += bgdcp1
     c                   when      bbrank = 2
     c                   eval      totcpy += bgdcp2
     c                   when      bbrank = 3
     c                   eval      totcpy += bgdcp3
     c                   endsl
     c                   endif
     c                   enddo

     c                   if        totcpy < 0
     c                   eval      totcpy *= -1
     c                   endif

     c                   endsr
     c*****************************************************************
     c     parse100      begsr

     c                   add       1             totseg

     c                   eval      len = %len(%trimr(ohtml))
     c
     c                   if        len <> 0
     c     1             do        len           jj
     c
     c                   eval      wehtml = %subst(ohtml:jj:100)
     c
     c                   if        wehtml <> *blanks
     c                   eval      weseqn = weseqn + 1
     c                   write     e5felhtml
     c                   endif
     c
     c                   eval      jj = jj + 99
     c                   enddo
     c                   endif
     c
     c                   endsr
      *****************************************************************
     c     writepreview  begsr
     c                   eval      ohtml = *blanks
     c                   if        ifhcsf <> *blanks
     c                   eval      ohtml = '<tr><td colspan="7">'+
     c                                ' '+ %trim(dataline) +
     c                                '</td></tr>'
     c                   exsr      parse100
     c                   else
     c                   eval      ohtml = '<tr><td colspan="7">' +
     c                                %trim(dataline) +
     c                                '</td></tr>'
     c                   exsr      parse100
     c                   endif
     c                   endsr
     c*****************************************************************
     c     addsep        begsr
     c                   select
     c                   when      iftsep = 'D'
     c                   eval      dataline = %trim(dataline) + etrsep
     c                   when      iftsep = 'S'
     c                   eval      dataline = %trim(dataline) + etrssp
     c                   when      iftsep = 'C'
     c                   eval      dataline = %trim(dataline) + etrces
     c                   endsl
     c                   endsr
     c*****************************************************************
     c     writeelcdata  begsr

     c                   movea     dataline      ary2000
      /free
           for i = %len(%trim(dataline)) downto 1;
              if i = %len(%trim(dataline)) ;
                 hldsep = ary2000(i);
              else;
                 if ary2000(i) <> etrsep;
                    leave;
                 endif;
              endif;
                ary2000(i) = *blanks;
           endfor;
                ary2000(i + 1) = hldsep ;
      /end-free

     c                   movea     ary2000       dataline

     c                   eval      i8bldt = %trim(dataline)
     c                   eval      i8sgmt = lsdseg
     c                   add       1             totseg
     c                   write     e5f5010

     c                   endsr
     c*****************************************************************
     c     override      begsr
     c                   eval      xodfrm = LSDFRM
     c                   eval      xodtyp = LSDTYP
     c                   eval      xod510 = LSD510
     c                   eval      xodubc = payor
     c                   eval      xodpln = plan
     c                   eval      xodata = %trim(datafield)

     c                   select
     c                   when      ovrtype = 'G'
     c                   eval      xodseq = DENSEQ
     c                   eval      xodlop = DENLOP
     c                   eval      xodseg = DENSEG
     c                   eval      xodfsq = DENFSQ
     c                   eval      xodcsq = 0
     c                   when      ovrtype = 'F'
     c                   eval      xodseq = LSDSEQ
     c                   eval      xodlop = LSDLOP
     c                   eval      xodseg = LSDSEG
     c                   eval      xodfsq = IFDFSQ
     c                   eval      xodcsq = 0
     c                   when      ovrtype = 'C'
     c                   eval      xodseq = ICDSEQ
     c                   eval      xodlop = ICDLOP
     c                   eval      xodseg = ICDSEG
     c                   eval      xodfsq = ICDFSQ
     c                   eval      xodcsq = ICDCSQ
     c                   when      ovrtype = 'D'
     c                   eval      xodseq = DENSEQ
     c                   eval      xodlop = DENLOP
     c                   eval      xodseg = DENSEG
     c                   eval      xodfsq = DENFSQ
     c                   eval      xodcsq = DENCSQ
     c                   endsl

     c     ovrkey        chain     e5fovrdt
     c                   if        %found(e5povrdt)
     c                             and IOODTA <> *blanks
     c                   eval      datafield = %trim(ioodta)
     c                   else
     c                   eval      xODATA = '*ALL'
     c     ovrkey        chain     e5fovrdt
     c                   if        %found(e5povrdt)
     c                             and IOODTA <> *blanks
     c                   eval      datafield = %trim(ioodta)
     c                   endif
     c                   endif

     c                   endsr
     c*****************************************************************
     c     chainlevels   begsr

     c     hx6l5a        chain     hxflvl5
     c     hx5l4a        chain     hxflvl4
     c     hx4l3a        chain     hxflvl3
     c     hx3l2a        chain     hxflvl2
     c     hx2l1a        chain     hxflvl1
     c                   endsr
     c*****************************************************************
     c     srcmdy        begsr

     c                   call      'XFXCMDY'
     c                   parm                    mdate
     c                   parm                    ydate

     c                   endsr
     c*****************************************************************
     c     srcymd        begsr

     c                   call      'XFXCYMD'
     c                   parm                    ydate
     c                   parm                    mdate

     c                   endsr
      *****************************************************************
     c     srtabl        begsr
     c                   call      'XFXTABL'
     c                   parm                    tcode
     c                   parm                    ecode
     c                   parm                    hmap
     c                   parm                    edate
     c                   parm                    sdesc
     c                   parm                    ldesc
     c                   parm                    tind
     c                   endsr
      **************************************************************************
     c     srctab        begsr
     c                   eval      tcode = *blanks
     c                   eval      ecode = *blanks
     c                   eval      hmap = *blanks
     c                   eval      edate = 0
     c                   eval      sdesc = *blanks
     c                   eval      ldesc = *blanks
     c                   eval      tind = *blanks
     c                   endsr
      *****************************************************************
     c     srposd        begsr

     c                   eval      clm05_1 = *blanks
     c     bbtrak        chain     e5fchr15
     c                   if        %found(e5pchr15)
     c                   call      'HBXPOSD'
     c                   parm      bbpayr        rqspyr
     c                   parm      bbplan        rqspln
     c                   parm      bgdct1        rqsct1
     c                   parm      bgdct2        rqsct2
     c                   parm      bgdct3        rqsct3
     c                   parm      bgdtpr        rqsprc
     c                   parm                    rqpos
     c                   eval      clm05_1 = rqpos
     c                   eval      datafield = %trim(rqpos)
     c                   else
     c                   eval      datafield = *blanks
     c                   endif

     c                   endsr
      **********************************************************************************************
     c     getvalues     begsr

     c                   if        ovrtype = 'F' or ovrtype = 'G'
     c                   eval      spos = %scan('INS_':ifpatt)
     c                   if        spos <> 0
     c                   eval      hold1 = %subst(ifpatt:spos+4:1)
     c                   if        hold1 <> ' '
     c                   select
     c                   when      hold1 = 'A'
     c                   eval      spos = %scan('A':FL05A)
     c                   when      hold1 = 'B'
     c                   eval      spos = %scan('B':FL05A)
     c                   when      hold1 = 'C'
     c                   eval      spos = %scan('C':FL05A)
     c                   when      hold1 = '*'
     c                   if        %subst(FL05A:1:1) = 'A'
     c                   eval      spos = 1
     c                   elseif    %subst(FL05A:1:1) = 'B'
     c                   eval      spos = 2
     c                   elseif    %subst(FL05A:1:1) = 'C'
     c                   eval      spos = 3
     c                   else
     c                   eval      spos = bbrank
     c                   endif
     c                   endsl
     c                   select
     c                   when      spos = 1
     c                   eval      hold1 = 'A'
     c                   when      spos = 2
     c                   eval      hold1 = 'B'
     c                   when      spos = 3
     c                   eval      hold1 = 'C'
     c                   endsl
     c                   eval      hldhcs = %xlate('q':hold1:ifhcsf)
     c                   endif
     c                   endif
     c                   endif

     c                   if        ifname = 'SE01'
     c                   eval      totseg += 1
     c                   endif

      /copy ressource/copysrc,e5xvalues

     c                   endsr
     o**************************************************************************
     oprinter   h    of                     1 03
     o                                           10 'RUN DATE: '
     o                       rundte              20 '  /  /    '
     o                       lv1nam              86
     o                                          127 'PAGE '
     o                       page          z    132
     o*------------------------------------------------------------------------*
     o          h    of                     2
     o                                           10 'RUN TIME: '
     o                       runtme              20 '0 :  :  '
     o                       rpthdr              86
     o                       pgmnam             132
     o*------------------------------------------------------------------------*
     o          h    of                     0
     o                                           24 '   _________ ____       '
     o                                           48 '                 _______'
     o                                           72 '___ __________ ______ __'
     o                                           96 '___ _____________       '
     o                                          120 ' _______________   _____'
     o                                          132 '__________  '
     o*------------------------------------------------------------------------*
     o          h    of                     2
     o                                           24 '   ACCOUNT # NAME       '
     o                                           48 '                 FROM DA'
     o                                           72 'TE  TO DATE    PAYOR# PL'
     o                                           96 'AN# POLICY NUMBER       '
     o                                          120 ' INS DESCRIPTION   TRACK'
     o                                          132 'ING NUMBER  '
     o*------------------------------------------------------------------------*
     o          ef           detail         1
     o                       bbaccn        4     12
     o                       bgname              39
     o                       frdate              51 '  /  /    '
     o                       todate              62 '  /  /    '
     o                       bbpayr        4     69
     o                       bbplan        4     75
     o                       bbplcy              96
     o                       insdsc             113
     o                       wsaccn             132
     o*------------------------------------------------------------------------*
     o          ef           prvtot      1  1
     o                                           15 'TOTAL CHARGES: '
     o                       totchgx       l     40
     o*------------------------------------------------------------------------*
     o          ef           lastr       1  1
     o                                           18 'TOTAL CLAIMS SENT:'
     o                       stotal        l     26
     o**************************************************************************
