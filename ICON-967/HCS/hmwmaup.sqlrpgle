     H SrtSeq(*LangIDShr) AltSeq(*Ext)
     hCOPYRIGHT('(C) Copyright Health Care Software, INC. 2001')
     H BNDDIR('HMEMAMS')
       ctl-opt bnddir('HXEAPPPRF');
      *****************************************************************
      ****                                                         ****
      ****        M/R MEDICAL ADMINISTRATION MASTER UPDATE         ****
      ****                                                         ****
      ****        COPYRIGHT - 2024 - WELLSKY                       ****
      *****************************************************************
      **                     ABSTRACT                                **
      **                                                             **
      **    THIS PROGRAM IS THE PHYSICIAN AND STAFF MEMBER           **
      **    MAINTENANCE DRIVER.                                      **
      **                                                             **
      *****************************************************************
      **                  MODIFICATION SUMMARY                       **
      **                                                             **
      **  CHANGE DATE  USER/COMMENTS                    INSTALL DATE **
      **                                                             **
      **  9/9/2004     JJEROME - Clear REQOPT on        9/10/2004    **
      **               return from DRPLN (ID)                        **
      **                                                             **
      **  01/12/2005   Mark Sanford                                  **
      **               Changed length of email address parm for      **
      **               HXXEMLK and HXXEMUP                           **
      **                                                             **
      **  02/14/2005   bgates                                        **
      **               Commented employee search code (NIU)          **
      **                                                             **
      **  03/03/2005   bgates                                        **
      **               If return from F1 window without selecting    **
      **               an option, clear the line # (for web)         **
      **                                                             **
      **  03/14/2005   Mark Sanford                                  **
      **               Changed option 'AP', 'AR', and 'AT' to be     **
      **               function keys F4, F5, and F6 respectively     **
      **                                                             **
      **  05/11/2005   Rick Nielson                     05/13/2005   **
      **               Move 'Pract/Group' fields.                    **
      **               Replace old F1 functionality with new.        **
      **               -installed by Michael Kundla         .        **
      **                                                             **
      **  06/06/2005   rnielson                         06/13/2005   **
      **               -Add option 'CR Certifications.'              **
      **               Shell HMWCERCD.                               **
      **               -Installed by Michael Kundla                  **
      **                                                             **
      **  06/09/2006   Brian Shore                                   **
      **               -Relaxed edits on other staff types           **
      **               -Added option 'TC' from GSHS                  **
      **                                                             **
      **  07/20/2007   Brian Shore                                   **
      **               -Pacer SD registration 21                     **
      **                change allow to admit patients edit to use   **
      **                new table MADP (update HXPWINH to reference) **
      **                See OPRG IPRG for related changes            **
      **               -Fixed error message bug                      **
      **                                                             **
      **  09/14/2007   Brian Shore                                   **
      **               -Fixed lookup window for format HMFTHRPS      **
      **                                                             **
      **  11/28/2007   Brian Shore                                   **
      **               -Pacer SD registration 26                     **
      **               -Added option IM for immunization maint       **
      **               -Added option ED for education maint          **
      **                                                             **
      **  06/18/2009   Frank DAngelo                                 **
      **               -Added LV6 Association to workflow            **
      **                                                             **
      **  01/26/2010   Michael Kundla                   01/26/2010   **
      **               - level 5 name getting populated with a       **
      **                 previous value in some cases                **
      **               - Also, was not edit checking level 5 on      **
      **                 format HMFPHUP2                             **
      **                                                             **
      **  01/06/2011   Amy Lowry                                     **
      **               Don't lock HMPMAMS when chaining to file      **
      **               to verify doc # for immunizations option      **
      **                                                             **
      **  02/08/2011   Michael Kundla                   02/08/2011   **
      **               - Avoid recursive call by sending flag to     **
      **               lookup program                                **
      **                                                             **
      **  02/16/2011   Amy Lowry                        02/16/2011   **
      **               Remove logic for F5 employee search b/c       **
      **               subroutine was commented out in 2001          **
      **                                                             **
      **  02/18/2011   Michael Kundla                   02/18/2011   **
      **               - Added edit check for Medicare Participating **
      **               flag.                                         **
      **                                                             **
      **  05/21/2014   Justin Sarnak                                 **
      **              - Changed size of address fields               **
      **                                                             **
      **  05/23/2014   Casey Antczak                                 **
      **               -Added NPI and Level 6 to front page          **
      **               -User now needs to specify if he or she       **
      **               wishes to view additional info                **
      **               -Removed addoad variable                      **
      **               -Installed by Steve Ferguson                  **
      **                                                             **
      **  07/17/2014   Justin Sarnak                                 **
      **              - Changed reads to (n) to prevent locks        **
      **              - Fixes for 5/23/14 change                     **
      **                                                             **
      **  07/30/2014   Casey Antczak                    07/30/2014   **
      **              - Will automatically fill in office address    **
      **                and tel/fax fields if they are blank         **
      **                and with values from NPI File                **
      **               - Installed by Michael Kundla                 **
      **                                                             **
      **  08/18/2014   Casey Antczak                    08/18/2014   **
      **              - Degree field no longer required              **
      **               - Installed by Michael Kundla                 **
      **                                                             **
      **  07/30/2014   Casey Antczak                    07/30/2014   **
      **              - Will automatically fill in office address    **
      **                they are blank with values from NPI File     **
      **                on other staff screen                        **
      **                                                             **
      **  08/11/2016   Matthew Viola HDEV-9995                       **
      **              - Added practice/group field to 'other' staff  **
      **                screen                                       **
      **                                                             **
      **  04/06/2017   Matthew Viola HDEV-15786                      **
      **              - Fixed bug were the home zip code extension   **
      **                was not being saved.                         **
      **                                                             **
      **  11/08/2017   Catherine Dougherty HDEV-18014                **
      **              - Added Cosigner Required field to display     **
      **                screen. Cosigner field should be init.       **
      **                to N for new and previously created records  **
      **                Field stored in substring for HMDFUT in      **
      **                position 6 of length 1.                      **
      **                                                             **
      **  06/21/2018   Zach Heushckel HDEV-23984                     **
      **              - Add comment parm to HXXPHNU                  **
      **                                                             **
      **  09/04/2018   Jay Walsh  HDEV-24551                         **
      **              - Allows the ability to add multiple groups to **
      **                a resource                                   **
      **                                                             **
      **  10/18/2018   Christina Scott HDEV-26463                    **
      **              - Add Supervising Clinician when adding new    **
      **                physician or other staff type                **
      **              - Remove 5th Covering Physician                **
      **                                                             **
      **  11/19/2018   Tucker Schmidt  HDEB-22719                    **
      **              - Clear out the group description before       **
      **                inserting the group code description         **
      **                                                             **
      **  01/21/2018   Jay Walsh  HDEV-20965                         **
      **              - Added the ordering priveleges flag           **
      **                                                             **
      **  04/27/2020   Michelle Palmieri  HDEV-36560                 **
      **              - Removed logic for hmdadp and hmdorp at       **
      **                staff level, instead is at lv6 level         **
      **                                                             **
      **  07/22/2020   Matt Kurtanick HDEV-38040                     **
      **               -Changed MGRP table lookup to TMPHYSGRP.      **
      **                                                             **
      **  08/05/2020   Cesar Lopez    HDEV-36906                     **
      **               -Remove Non Numeric characters for telephone  **
      **                numbers pulled from NPI file.                **
      **                                                             **
      **  09/15/2020   Matt Kurtanick HDEV-38984                     **
      **               -Added window to give option to sync phys     **
      **                cell number with user cell number            **
      **                                                             **
      **  09/26/2020   Chris Shull HDEV-39359                        **
      **               -retire views in UTL                          **
      **                                                             **
      **  02/02/2021   Matt Kurtanick HDEV-39410                     **
      **               -Do not allow delete or reinstate of phys if  **
      **                linked user is not deleted/reinstated first  **
      **                                                             **
      **  02/23/2021   Nick Ela    HDEV-41468                        **
      **               -clear out physician group number on new      **
      **               entries                                       **
      **                                                             **
      **  02/24/2021   Camila Barbini HDEV-41427                     **
      **               -Don't write HMPTHRCL if the level 6 is zero  **
      **                                                             **
      ** 08/11/2021    Camila Barbini   ICEN-423                     **
      **               -Replace references to NPI database with calls**
      **                to new NPI API (fix)                        **
      **                                                             **
      **  10/20/2021    Camila Barbini ICEN-120                      **
      **                -Retire the references to XZIP               **
      **                                                             **
      **  12/31/2021    Camila Barbini ICEN-625                      **
      **                 Don't Allow Multiple Physicians to have the **
      **                 same NPI number                             **
      **                                                             **
      ** 07/24/2023     Mohammad Abbas   ICPOE-1321                  **
      **                -From Name filed remove any Non Break        **
      **                 Characters.                                 **
      **                                                             **
      **  03/28/2024    Shaun Schillinger ICEN-1735                  **
      **                -Add app pref reqPhysicianNPI and            **
      **                 reqPhysicianDegree                          **
      **                                                             **
      **  12/16/2024    Leonel Montanez ICPOE-2434                   **
      **                -Call DeleteRapiUser                         **
      **                                                             **
      **  04/23/2025    Matt Viola ICEN-1793                         **
      **                -allow duplicate phone numbers with different**
      **                 types                                       **
      **                                                             **
      *****************************************************************
     fhmpmams   uf a e           k disk
     fhmllice   uf a e           k disk
     f*hmpnpi    if   e           k disk
     fhmpmamg   uf a e           k disk
     fhmlmamg   if   e           k disk    rename(hmfmamg:hmfmamga)
     fhmlmamsn  if   e           k disk
     f                                     rename(hmfmams:hmfmamsn)
     fhmlmamsg  if   e           k disk
     f                                     rename(hmfmams:hmfmamsg)
     fhmlmamst  if   e           k disk
     f                                     rename(hmfmams:hmfmamst)
     fhmlmamsi  if   e           k disk    prefix('I':1)
     f                                     rename(hmfmams:hmfmamsi)
     fhmlmamsz  if   e           k disk
     f                                     rename(hmfmams:hmfmamsz)
     fhmpplnpr  uf a e           k disk
     fhmlplnpr  uf   e           k disk
     f                                     rename(hmfplnpr:hmfplnpp)
     fhmlplnpa  uf   e           k disk
     f                                     rename(hmfplnpr:hmfplnpa)
     fhmpphyad  uf a e           k disk
     fhmpphyrf  if   e           k disk
     fhmpthrcl  uf a e           k disk    rename(hmfthrcl:hmfthrco)
     fhmlthrca  if a e           k disk
     fhmlthrcp  uf   e           k disk
     f                                     rename(hmfthrcl:hmfthrcp)
     fhxplvl5   if   e           k disk
     fhxplvl6   if   e           k disk
     fhxpuser   if   e           k disk
     fhxpbnfit  if   e           k disk
     fhxlctzpz  if   e           k disk
     fhxlwinh   if   e           k disk
     f**hppmast   if   e           k disk
     f**hplmasta  if   e           k disk
     f**                                     rename(hpfmast:hpfmasta)
     fhmdmaup   cf   e             workstn
     f                                     sfile(hmfdrupr:recnum)
     f                                     sfile(hmfdruer:errnum)
     f                                     sfile(hmfthrps:recno2)
     f**                                   sfile(hmfphyps:recno3)
     f                                     sfile(hmfphyas:recno6)
     f****                                 sfile(hmfemppr:recno7)
     f                                     sfile(hmfaddgr:recno7)
     f                                     infds(infods)
      *****************************************************************
      /EJECT
      /copy hcssource/copysrc,hxxlda
      /copy copysrc,hmemams
      /copy copysrc,cfxstzp
      /copy copysrc,hxxappprfp
     d nef             s             70    dim(3) ctdata perrcd(1)
     d rnf             s             70    dim(1) ctdata perrcd(1)
     d eod             s             25    dim(1) ctdata perrcd(1)
     d eos             s             70    dim(1) ctdata perrcd(1)
     d npf             s             70    dim(3) ctdata perrcd(1)
     d rqe             s             70    dim(2) ctdata perrcd(1)
     d iop             s             70    dim(3) ctdata perrcd(1)
     d iln             s             70    dim(1) ctdata perrcd(1)
     d ilc             s             70    dim(1) ctdata perrcd(1)
      *                   IVR     1   1 70               INVALID REFRSH
     d dtp             s             70    dim(1) ctdata perrcd(1)
     d*pnd             s             70    dim(3) ctdata perrcd(1)
     d pnd             s             70    dim(4) ctdata perrcd(1)
     d rnp             s             70    dim(1) ctdata perrcd(1)
     d sms             s             70    dim(2) ctdata perrcd(1)
     d cms             s             70    dim(3) ctdata perrcd(1)
     d fiu             s             70    dim(1) ctdata perrcd(1)
     d ntf             s             70    dim(1) ctdata perrcd(1)
     d nnf             s             70    dim(1) ctdata perrcd(1)
     d nao             s             70    dim(1) ctdata perrcd(1)
     d ipp             s             70    dim(1) ctdata perrcd(1)
     d ems             s             60    dim(60)
     d ems1            s             60    dim(56) ctdata perrcd(1)
     d ems2            s             60    dim(44) ctdata perrcd(1)
     d ems3            s             60    dim(56) ctdata perrcd(1)
     d ems4            s             60    dim(2) ctdata perrcd(1)
     d dlt             s             60    dim(2) ctdata perrcd(1)
     d prt             s             70    dim(2) ctdata perrcd(1)
     d tad             s             70    dim(1) ctdata perrcd(1)
     d tup             s             70    dim(1) ctdata perrcd(1)
     d updt            s             70    dim(2) ctdata perrcd(1)
     d mpr             s             70    dim(1) ctdata perrcd(1)
     d dae             s             70    dim(2) ctdata perrcd(1)
     d nke             s             70    dim(1) ctdata perrcd(1)
     d tcm             s             70    dim(6) ctdata perrcd(1)
     d tnc             s             70    dim(1) ctdata perrcd(1)
     d der             s              1  0 dim(24)
     d err             s              1    dim(7)
     d wnam            s              1    dim(27)
     d wnm2            s              1    dim(16)
     d wnm3            s              1    dim(16)
     d nme             s              1    dim(16)
     d mse             s              1    dim(60)
     d sbs             s              2    dim(3)
     d spt             s              2    dim(20)
     d sptd            s             17    dim(20)
     d tti             s              8    dim(3)
     d cvp             s              5  0 dim(4)
     d cmt             s             75    dim(6)
     d sta             s              2    dim(20)
     d arr             s              4    dim(20)
     d cknm            s              1    dim(26)
     d pyr             s              6  0 dim(999)
     d pln             s              5  0 dim(999)
     d lcn             s              6  0 dim(999)
     d tmpr            s              6  0 dim(999)
     d tmpn            s              5  0 dim(999)
     d tmpl            s              6  0 dim(999)
     d nam             s              1    dim(26)
     d thnm            s              1    dim(26)
     d mams          e ds                  extname(hmpmams)
     d  zmdhz1       e                     extfld(hmdhz1)
     d  zmdhe1       e                     extfld(hmdhe1)
     d plnpr         e ds                  extname(hmpplnpr)
     d phyad         e ds                  extname(hmpphyad)
     d  zmaoz1       e                     extfld(hmaoz1)
     d  zmaoz2       e                     extfld(hmaoz2)
     d                sds
     d  pgmnam           *proc
     d wsdnam          ds
     d fstlet                         1
     d wsenam          ds
     d fstlt2                         1
     d                 ds
     d arstct                         4
     d  arsta                         2    overlay(arstct)
     d  arct2                         2    overlay(arstct:3)
     d                 ds
     d wrkspc                        24
     d  outspc                       20    overlay(wrkspc:5)
     d                 ds
     d  prd                    1    120
     d                                     dim(6)
     d  wspm1                  1     20
     d  wspm2                 21     40
     d  wspm3                 41     60
     d  wspm4                 61     80
     d  wspm5                 81    100
     d                 ds
     d  bsc                    1    150
     d                                     dim(5)
     d  wsds1                  1     30
     d  wsds2                 31     60
     d  wsds3                 61     90
     d                 ds
     d date6                          6  0
     d  mm6                           2  0 overlay(date6)
     d  dd6                           2  0 overlay(date6:3)
     d  yy6                           2  0 overlay(date6:5)
     d hmdtas          ds
     d  tap                    1     45
     d                                     dim(3)
     d  tap1                   1     15
     d  tap2                  16     30
     d  tap3                  31     45
     d                 ds
     d wlgs                          45
     d  wlgs1                        15    overlay(wlgs)
     d  wlgs2                        15    overlay(wlgs:16)
     d  wlgs3                        15    overlay(wlgs:31)
     d                 ds
     d pis                           54
     d  wsdti1                        6  0 overlay(pis)
     d  wsdti2                        6  0 overlay(pis:7)
     d  wsdti3                        6  0 overlay(pis:13)
     d  wsdpi1                        6  0 overlay(pis:19)
     d  wsdpi2                        6  0 overlay(pis:25)
     d  wsdpi3                        6  0 overlay(pis:31)
     d  wsdfi1                        6  0 overlay(pis:37)
     d  wsdfi2                        6  0 overlay(pis:43)
     d  wsdfi3                        6  0 overlay(pis:49)
     d                 ds
     d pex                           36
     d  wsdte1                        6  0 overlay(pex)
     d  wsdte2                        6  0 overlay(pex:7)
     d  wsdte3                        6  0 overlay(pex:13)
     d  wsdpe1                        6  0 overlay(pex:19)
     d  wsdpe2                        6  0 overlay(pex:25)
     d  wsdpe3                        6  0 overlay(pex:31)
     d infods          ds
     d dspfnm                         8
     d rcdfmt                 38     45
     d fil                   370    370
     d col                            1
     d errmsg          ds
     d  msg                    1     60
     d                                     dim(60)
     d tmpdr#          ds
     d  tmp                    1      9
     d                                     dim(9)
     d                 ds
     d curtim                        14  0
     d  runtme                        6  0 overlay(curtim)
     d  rundte                        8  0 overlay(curtim:7)
     d   tmmdd                        4  0 overlay(rundte)
     d    tmm                         2  0 overlay(tmmdd)
     d   tyyyy                        4  0 overlay(rundte:5)
     d                 ds
     d thhphn                        15
     d  thhph1                        3    overlay(thhphn)
     d  thhph2                        3    overlay(thhphn:5)
     d  thhph3                        4    overlay(thhphn:9)
     d                 ds
     d phone#                        10  0
     d  phone1                        3  0 overlay(phone#)
     d  phone2                        3  0 overlay(phone#:4)
     d  phone3                        4  0 overlay(phone#:7)
     d hmrmdr          ds
     d  mdr                    1     48  0
     d                                     dim(12)
     d string          ds
     d  lev6                   1     30
     d                                     dim(30)
     d strnga          ds
     d  lvla                   1      3
     d                                     dim(3)
     d level           ds           297
     d level4                100    129
     d lv4abc                         1
     d lv4abl                         2
     d level5                        30
     d lv5abc                         1
     d lv5abl                         2
     d level6                        30
     d lv6abc                         1
     d lv6abl                         2
     d ctgry2                232    261
     d cat2cp                         1
     d cat2lw                         2
     d asc             c                   const('associations:')
     d high            c                   const('ABCDEFGHIJKLMNOPQRST-
     d                                     UVWXYZ')
     d lo              c                   const('abcdefghijklmnopqrst-
     d                                     uvwxyz')
     d namein          ds
     d nami                          29
     d  lastnm                       16    overlay(nami)
     d  frstnm                       13    overlay(nami:17)
     d prmzip          ds
     d wszip1                         5
     d wszip2                  5      9

     d nplzip          ds            20
     d  npzip1                        5
     d  npzip2                       15

     d nplfax          ds            20
     d  npifax                       10

     d npiDS           ds                  qualified
     d name                          70
     d address                       55
     d address2                      55
     d city                          40
     d state                         40
     d zip                           10
     d phone                         20
     d fax                           20

     d zipCode         ds
     d zip1                           5
     d zip2                    6      9
      *---------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *---------------------------------------------------------------------
     d addth           s              1
     d adrchn          s              1
     d adrseq          s              3  0
     d authnum         s              7  0
     d b               s              2  0
     d begin           s              2  0
     d blkfld          s             27
     d c               s              3  0
     d centerdsc       s            100
     d centerfil       s              1    inz(' ')
     d centerlen       s              3  0 inz(56)
     d chg             s              1
     d chnrcd          s              5  0
     d cknumb          s              6  0
     d cktype          s              1
     d ckzip           s              5
     d d               s              4  0
     d data            s             11
     d dblchk          s              1
     d drnum           s              9  0
     d dspnam          s             26
     d ecode           s             10
     d edate           s              8  0
     d epsd            s             12  0
     d errexs          s              1
     d errid           s              1
     d extensionNumber...
     d                 s              6
     d filenm          s             10
     d fldnam          s              6
     d fldhlp          s              1
     d flg             s              1
     d found           s               n
     d frstme          s              1
     d g               s              4  0
     d goodPhys        s               n
     d hldtyp          s              2
     d hmap            s             10
     d id              s              2  0
     d duplID#         s              9  0
     d in              s              2  0
     d inbc            s              1
     d inc             s              2  0
     d indd            s              1
     d indice          s             99
     d inir            s              1
     d inli            s              1
     d inopt           s             12
     d inpa            s              1
     d inpp            s              1
     d inps            s              1
     d inpu            s              1
     d inqopt          s              1
     d insp            s              1
     d inta            s              1
     d inzi            s              1
     d inzl            s              1
     d inzs            s              1
     d incercd         s               n
     d j               s              3  0
     d k               s              3  0
     d l               s              2  0
     d ldesc           s             50
     d lstchr          s              1
     d l6              s              6  0
     d maxrec          s              3  0
     d mdate           s              8  0
     d n               s              2  0
     d name            s             26
     d nameou          s             31
     d npiDuplicated   s               n
     d npiDuplMessage  s             45
     d npicod          s              5
     d numericPhoneNumber...
     d                 s             10  0
     d nxtseq          s              3  0
     d oldlcn          s              6  0
     d oldpln          s              5  0
     d oldpyr          s              6  0
     d once            s              1
     d once2           s              1
     d one             s              1
     d on2             s              1
     d option          s              3
     d p               s              2  0
     d pccode          s              1
     d phupdate        s               n
     d phynam          s             26
     d pm4             s              2  0
     d pname           s             16
     d primary         s              1    inz('X')
     d pryear          s              4  0
     d rc              s              1  0
     d reqcmt          s             38
     d reqlv6          s              6  0
     d reqmap          s              6  0
     d reqnm2          s             12  0
     d reqnum          s             12  0
       dcl-s reqPhysicianDegree ind;
       dcl-s reqPhysicianNPI ind;
     d reqpgm          s              1
     d reqsrc          s              2
     d result          s             30
     d ropt            s              1
     d rphy            s              9  0
     d rqcode          s              1
     d rqlev6          s              6
     d rqline          s              4  0
     d rqstop          s              2
     d rtnlv1          s              2  0
     d rtnlv2          s              2  0
     d rtnlv3          s              2  0
     d rtnlv4          s              4  0
     d rtnlv5          s              6  0
     d rtnnam          s             26
     d rtrncd          s              1
     d savem#          s              9  0
     d savemp          s             31
     d savgp#          s              4  0
     d savlv5          s              6  0
     d savlv6          s              6  0
     d savnam          s             26
     d savopt          s              1
     d savrc7          s              4  0
     d savrec          s              4  0
     d savtyp          s              2
     d screen          s              3  0
     d sdesc           s             20
     d sfkey           s              3  0
     d slcmde          s              1
     d strnum          s              4  0
     d svaoad          s             40
     d svaoa2          s             40
     d svaoct          s             30
     d svaofx          s             10  0
     d svaost          s              2
     d svaotl          s             10  0
     d svextn          s              6
     d svaoz1          s              5
     d svaoz2          s              4
     d svjdte          s              8  0
     d system          s              1
     d t               s              3  0
     d tchgauth        s               n
     d tcode           s              4
     d tempct          s              2
     d tind            s              1
     d tmplcn          s              6  0
     d tmprc#          s              4  0
     d tmp3            s              3
     d today           s              8  0
     d tst7            s              7
     d typ             s              2
     d type            s              1
     d userPhone       s             10  0
     d verify          s              1
     d w               s              3  0
     d x               s              2  0
     d xx              s              2
     d ydate           s              8  0
     d year            s              4  0
     d wscosn          s              1
     d zrofld          s             59
     d reqext          s              6
     d reqphn          s             10  0
     d reqSeq          s              3  0
     d rqstyp          s              2
     d rtnext          s              6
     d rtnphn          s             10  0
     d drlvl6          s              6  0
     d reqeml          s             50
     d rtneml          s             50
     d mmsrc           s              2
     d mmseq           s             12  0
      *---------------------------------------------------------------------
      * Stand Alone Fields - BOTTOM
      *---------------------------------------------------------------------
      *****************************************************************
      **
     c     start         tag
     c                   move      '1'           reqpgm
     c                   exsr      clrsfl
     c                   eval      reqtyp = *blanks
     c                   eval      reqid# = 0
     c                   eval      reqnam = *blanks
     c                   eval      reqgp# = 0
     c                   eval      reqopt = *blanks
     c                   eval      reqlv5 = 0
     c                   eval      reqlv6 = 0
     c                   eval      reqlno = 0
     c                   move      'N'           wspcon
     c                   eval      wsddld = 0
     c                   movea     '0000000'     *in(01)
     c*                  if        rqcode = 'W'
     c*                  exsr      phyadd
     c*                  goto      exit
     c*                  endif
      *****************************************************************
     c     dsplst        tag
      **
     c                   movel(p)  lv5abl        strnga
     c     high:lo       xlate     strnga        tempct
     c                   movel(p)  lv5abc        lvl5
     c                   cat       tempct:0      lvl5
     c                   cat       ':':0         lvl5
      **
     c                   movel(p)  lv6abl        strnga
     c     high:lo       xlate     strnga        tempct
     c                   movel(p)  lv6abc        lvl6
     c                   cat       tempct:0      lvl6
     c                   cat       ':':0         lvl6
      **
     c                   movel     lv6abc        lv6abr
     c                   move      lv6abl        lv6abr
      **
     c                   movea     '0000000'     *in(01)
     c                   movea     '0'           *in(08)
     c                   movea     '0'           *in(64)
      **
     c**                 write     hxffkey
     c                   eval      w5dpfd = 0
     c                   eval      w5dptd = 0
     c**
     c                   eval      mrinfo = 'Show More Information'
     c                   eval      centerdsc = 'MEDICAL ADMINISTRATION ' +
     c                                         'MAINTENANCE'
     c                   call      'XFXCNTG'
     c                   parm                    centerdsc
     c                   parm                    centerlen
     c                   parm                    centerfil
     c                   eval      wshdr = centerdsc
     c**
     c                   exfmt     hmfdrupc
      **
     c                   if        csrrrn <> 0
     c                   eval      reqlno = csrrrn
     c                   endif
      **
     c                   if        *inkb = *on
     c                   movea     *in(01)       indice
     c                   call      'XFCFKEY'
     c                   parm                    dspfnm
     c                   parm                    rcdfmt
     c                   parm                    indice
     c                   parm                    xx
     c     xx            cabeq     '  '          dsplst
     c                   exsr      srfkey
     c                   endif
      **
     c                   if        *inkc = *on
     c                             or *inkl = *on
     c                   exsr      unlock
     c                   goto      exit
     c                   endif
      **
     c                   if        reqlno <> 0
     c                             and reqopt = *blanks
     c                             and *inkd <> *on
     c                             and *inke <> *on
     c                             and *inkf <> *on
     c                   move      *on           *inka
     c                   movel(p)  'HMFDRUPC'    rcdfmt
     c                   endif
      **
     c                   if        *inka = *on
     c                   move      *on           *in90
     c                   if        csrfld = 'REQOPT'
     c                             or csrrrn <> 0
     c                   movea     *in(01)       indice
     c                   call      'XFCOPTN'
     c                   parm                    dspfnm
     c                   parm                    rcdfmt
     c                   parm                    indice
     c                   parm                    option
     c                   parm      reqlno        rqline
     c                   if        option = *blanks
     c                   eval      reqlno = 0
     c                   goto      dsplst
     c                   endif
     c                   movel     option        reqopt
     c                   eval      reqlno = rqline
     c                   else
     c                   exsr      chklin
     c     slcmde        cabeq     ' '           dsplst
     c                   endif
     c                   endif
      **
     c                   if        *in98 = *on
     c                   call      'HXWHLP '
     c                   parm                    dspfnm
     c                   parm                    rcdfmt
     c                   goto      dsplst
     c                   endif
      **
     c                   eval      dspmsg = *blanks
     c                   movea     '0000000'     *in(01)
      **
     c                   if        *in97 = *on
     c                   movel     savnam        reqnam
     c                   eval      reqgp# = savgp#
     c                   eval      reqlv5 = savlv5
     c                   eval      reqlv6 = savlv6
     c     reqnam        casne     *blanks       slcnam
     c     reqgp#        casne     0             slcgp#
     c     reqtyp        casne     *blanks       slctyp
     c                   endcs
     c                   goto      dsplst
     c                   endif
      **                                                    INFO
     c                   if        reqopt <> *blanks
     c                             or *inkd = *on
     c                             or *inke = *on
     c                             or *inkf = *on
     c                   movel     savnam        reqnam
     c                   eval      reqgp# = savgp#
     c                   eval      reqlv5 = savlv5
     c                   eval      reqlv6 = savlv6
     c                   eval      addth = ' '
     c     reqopt        caseq     'CH'          drchng
     c     reqopt        caseq     'IN'          drinqr
     c**** reqopt        caseq     'AR'          dradd
     c     *inke         caseq     *on           dradd
     c**** reqopt        caseq     'AP'          phyadd
     c     *inkd         caseq     *on           phyadd
     c     reqopt        caseq     'DL'          drdlt
     c     reqopt        caseq     'RI'          drrns
     c     reqopt        caseq     'PR'          drprt
     c     reqopt        caseq     'RC'          drcor
     c     reqopt        caseq     'RL'          drlet
     c     reqopt        caseq     'ID'          drpln
     c**** reqopt        caseq     'AT'          srstaf
     c     *inkf         caseq     *on           srstaf
     c     reqopt        caseq     'PS'          spcpro
     c     reqopt        caseq     'PU'          spcpun
     c     reqopt        caseq     'PP'          spcppr
     c     reqopt        caseq     'TA'          spctap
     c     reqopt        caseq     'PA'          spcpah
     c     reqopt        caseq     'SP'          spcprv
     c     reqopt        caseq     'DD'          spcddd
     c     reqopt        caseq     'L6'          srlvl6
     c     reqopt        caseq     'BC'          brdcer
     c     reqopt        caseq     'IR'          spcirf
     c     reqopt        caseq     'LI'          spclic
     c     reqopt        caseq     'PH'          sphone
     c     reqopt        caseq     'CR'          srcercd
     c     reqopt        caseq     'TC'          srtypchg
     c     reqopt        caseq     'IM'          srimmu
     c     reqopt        caseq     'ED'          sreduc
     c     reqopt        caseq     'AG'          srAddGroup
     c                   cas                     invreq
     c                   endcs
     c                   eval      reqopt = *blanks
     c                   goto      next
     c                   endif
      **
     c                   if        reqid# = 0
     c                   if        reqnam = *blanks
     c                   if        reqgp# = 0
     c                   if        reqtyp = *blanks
     c****               if        reqopt <> 'AR'
     c****                         and reqopt <> 'AT'
     c****                         and reqopt <> 'AP'
     c                   if        *inkd <> *on
     c                             and *inke <> *on
     c                             and *inkf <> *on
     c                   movel     rqe(1)        dspmsg
     c                   movea     '11'          *in(01)
     c                   move      *on           *in07
     c                   goto      dsplst
     c                   endif
     c                   endif
     c                   endif
     c                   endif
     c                   endif
      **
     c     next          tag
     c                   exsr      clrsfl
     c                   if        reqid# <> 0
     c                   exsr      slcid#
     c                   else
1    c                   if        reqnam = *blanks
2    c                   if        reqgp# = 0
3    c                   if        reqtyp <> *blanks
     c                   exsr      slctyp
E3   c                   endif
X2   c                   else
     c                   exsr      slcgp#
E2   c                   endif
X1   c                   else
     c                   exsr      slcnam
E1   c                   endif
E1   c                   endif
      **
     c                   eval      reqopt = *blanks
     c                   eval      reqlno = 0
     c                   movel     reqnam        savnam
     c                   eval      savgp# = reqgp#
     c                   eval      savlv5 = reqlv5
     c                   eval      savlv6 = reqlv6
     c                   movel     reqtyp        savtyp
      **
     c                   goto      dsplst
      **
     c     exit          tag
     c                   move      *on           *inlr
      *****************************************************************
      /EJECT
     c     slcid#        begsr
      **
     c     reqid#        chain     hmfmams                            89
     c     *in89         cabeq     *on           outid#
      **
     c                   eval      recnum = 1
     c                   eval      wsdgp# = getPrimaryGroup(hmddr#)
     c                   exsr      bldsfl
      **
     c     outid#        tag
     c                   if        *in96 = *on
     c                   eval      recnum = recnum + 1
     c                   movel     eod(1)        wsdnam
     c                   eval      hmdtyp = *blanks
     c                   eval      wsaddr = *blanks
     c                   eval      wsidno = 0
     c                   eval      wscity = *blanks
     c                   eval      wsdsvs = *blanks
     c                   eval      wsprl6 = 0
     c                   eval      hmddel = *blanks
     c                   move      *on           *in88
     c                   write     hmfdrupr
     c                   move      *off          *in88
     c                   else
     c                   movel     npf(2)        dspmsg
     c                   endif
     c                   endsr
      *****************************************************************
      /EJECT
     c     slcnam        begsr
      **
     c                   move      *off          *in94
      **
     c                   eval      n = 27
     c                   movea     reqnam        wnam(1)
     c                   dou       wnam(n) <> ' '
     c                   eval      n = n - 1
     c                   enddo
     c                   eval      n = n + 1
      **
     c                   if        *in97 = *off
     c                   eval      hmdnam = *blanks
     c                   movel     reqnam        hmdnam
     c                   if        hmdnam = '*ALL'
     c     *loval        setll     hmfmamsn
     c                   else
     c     hmdnam        setll     hmfmamsn
     c                   endif
     c                   else
     c                   eval      srecno = srecno + 15
     c                   endif
      **
     c                   eval      maxrec = srecno + 14
      **
     c     srecno        do        maxrec        recnum
     c     nxtnam        tag
     c                   read      hmfmamsn                               89
     c     *in89         cabeq     *on           outnam
      **
     c                   if        reqnam = '*ALL'
     c     hmddr#        cabeq     0             nxtnam
     c                   endif
      **
     c                   if        reqgp# <> 0
     c                             and not checkGroupMatch(hmddr# : reqgp#)
     c***     hmdgp#        cabne     reqgp#        nxtnam
     c                   iter
     c                   endif

      **
     c                   if        reqtyp <> *blanks
     c     hmdtyp        cabne     reqtyp        nxtnam
     c                   endif
      **
     c                   movea     hmdnam        wnam(1)
     c                   movea     blkfld        wnam(n)
     c                   movea     wnam(1)       name
     c                   if        reqnam <> '*ALL'
     c     reqnam        cabne     name          outnam
     c                   endif
     c                   if        wspcon = 'Y'
     c     hmdefl        cabne     'C'           nxtnam
     c                   endif
     c                   if        reqlv5 <> 0
     c     reqlv5        cabne     hmdlv5        nxtnam
     c                   endif
     c                   if        reqlv6 <> 0
     c     clkey         chain     hmfthrcl                           89
     c     *in89         cabeq     *on           nxtnam
     c                   endif
      **
     c                   eval      wsdgp# = getPrimaryGroup(hmddr#)
     c                   exsr      bldsfl
     c                   enddo
      **
     c                   if        recnum >= 990
     c                   movel     eos(1)        dspmsg
     c                   else
     c                   move      *on           *in94
     c                   endif
     c                   eval      savrec = recnum
     c                   goto      endnam
      **
     c     outnam        tag
     c                   if        *in96 = *on
     c                   eval      savrec = recnum
     c                   movel     eod(1)        wsdnam
     c                   eval      hmdtyp = *blanks
     c                   eval      wsaddr = *blanks
     c                   eval      wsidno = 0
     c                   eval      wscity = *blanks
     c                   eval      wsdsvs = *blanks
     c                   eval      wsprl6 = 0
     c                   eval      hmddel = *blanks
     c                   move      *on           *in88
     c                   write     hmfdrupr
     c                   move      *off          *in88
     c                   else
     c                   if        dspmsg = *blanks
     c                   if        reqtyp <> 'RF'
     c                             and reqtyp <> 'PH'
     c                   movel     npf(1)        dspmsg
     c                   else
     c                   movel     npf(2)        dspmsg
     c                   endif
     c                   endif
     c                   endif
     c     endnam        endsr
      *****************************************************************
      /EJECT
     c     slcgp#        begsr
      **
     c                   eval      reqnam = *blank
     c                   move      *off          *in94
     c                   if        *in97 = *off
     c***     reqgp#        setll     hmfmamsg
     c     reqgp#        setll     hmfmamga
     c                   else
     c                   eval      srecno = srecno + 15
     c                   endif
      **
     c                   eval      maxrec = srecno + 14
      **
     c     srecno        do        maxrec        recnum
     c     nxtgp#        tag
     c***     reqgp#        reade     hmfmamsg                               89
     c     reqgp#        reade     hmfmamga                               89
     c     *in89         cabeq     *on           outgp#
      **
     c     hgddr#        chain     hmpmams
     c                   if        not%found(hmpmams)
     c                   iter
     c                   endif

     c                   if        reqtyp <> *blanks
     c                   if        hmdtyp <> *blanks
     c     hmdtyp        cabne     reqtyp        nxtgp#
     c                   endif
     c                   endif
      **
     c                   if        wspcon = 'Y'
     c     hmdefl        cabne     'C'           nxtgp#
     c                   endif
      **
     c                   if        reqlv5 <> 0
     c     reqlv5        cabne     hmdlv5        nxtgp#
     c                   endif
      **
     c                   if        reqlv6 <> 0
     c     clkey         chain     hmfthrcl                           89
     c     *in89         cabeq     *on           nxtgp#
     c                   endif
      **
     c                   eval      wsdgp# = getPrimaryGroup(hmddr#)
     c                   exsr      bldsfl
     c                   enddo
      **
     c                   if        recnum >= 990
     c                   movel     eos(1)        dspmsg
     c                   else
     c                   move      *on           *in94
     c                   endif
     c                   eval      savrec = recnum
     c                   goto      endgp#
      **
     c     outgp#        tag
     c                   if        *in96 = *on
     c                   eval      savrec = recnum
     c                   movel     eod(1)        wsdnam
     c                   eval      hmdtyp = *blanks
     c                   eval      wsaddr = *blanks
     c                   eval      wsidno = 0
     c                   eval      wscity = *blanks
     c                   eval      wsdsvs = *blanks
     c                   eval      wsprl6 = 0
     c                   eval      hmddel = *blanks
     c                   move      *on           *in88
     c                   write     hmfdrupr
     c                   move      *off          *in88
     c                   else
     c                   if        dspmsg = *blanks
     c                   if        reqtyp <> 'RF'
     c                             and reqtyp <> 'PH'
     c                   movel     npf(1)        dspmsg
     c                   else
     c                   movel     npf(2)        dspmsg
     c                   endif
     c                   endif
     c                   endif
     c     endgp#        endsr
      *****************************************************************
      /EJECT
     c     slctyp        begsr
      **
     c                   eval      reqnam = *blank
     c                   eval      reqgp# = 0
     c                   move      *off          *in94
      **
     c     again         tag
     c                   if        *in97 = *off
     c     reqtyp        setll     hmfmamst
     c                   else
     c                   eval      srecno = srecno + 15
     c                   endif
      **
     c                   eval      maxrec = srecno + 14
      **
     c     srecno        do        maxrec        recnum
     c     nxttyp        tag
     c     reqtyp        reade     hmfmamst                               89
     c     *in89         cabeq     *on           outtyp
      **
     c                   if        wspcon = 'Y'
     c     hmdefl        cabne     'C'           nxttyp
     c                   endif
      **
     c                   if        reqlv5 <> 0
     c     reqlv5        cabne     hmdlv5        nxttyp
     c                   endif
      **
     c                   if        reqlv6 <> 0
     c     clkey         chain     hmfthrcl                           89
     c     *in89         cabeq     *on           nxttyp
     c                   endif
      **
     c                   eval      wsdgp# = getPrimaryGroup(hmddr#)
     c                   exsr      bldsfl
     c                   enddo
     c*                  if        *in90 = *off
     c*                            and dspmsg = *blanks
     c*                  if        maxrec < savrec
     c*                  eval      *in97 = *on
     c*                  goto      again
     c*                  endif
     c*                  else
     c*                  eval      savrec = 1
     c*                  endif
      **
     c                   if        recnum >= 990
     c                   movel     eos(1)        dspmsg
     c                   else
     c                   move      *on           *in94
     c                   endif
     c                   eval      savrec = recnum
     c                   goto      endtyp
      **
     c     outtyp        tag
     c                   if        *in96 = *on
     c                   eval      savrec = recnum
     c                   movel     eod(1)        wsdnam
     c                   eval      hmdtyp = *blanks
     c                   eval      wsaddr = *blanks
     c                   eval      wsidno = 0
     c                   eval      wscity = *blanks
     c                   eval      wsdsvs = *blanks
     c                   eval      wsprl6 = 0
     c                   eval      hmddel = *blanks
     c                   move      *on           *in88
     c                   write     hmfdrupr
     c                   move      *off          *in88
     c                   else
     c                   if        dspmsg = *blanks
     c                   if        reqtyp <> 'RF'
     c                             and reqtyp <> 'PH'
     c                   movel     npf(1)        dspmsg
     c                   else
     c                   movel     npf(2)        dspmsg
     c                   endif
     c                   endif
     c                   endif
     c     endtyp        endsr
      *****************************************************************
      /EJECT
     c     drchng        begsr
      **
     c                   if        *in70 = *off
     c                   movel     nao(1)        dspmsg
     c                   move      *on           *in04
     c                   goto      dsplst
     c                   endif
      **
     c                   if        reqlno = 0
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c     reqlno        chain     hmfdrupr                           89
     c                   if        *in89 = *on
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c                   if        fstlet = '*'
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   endif
     c                   endif
     c                   endif
      **
     c                   eval      rc = 2
     c                   select
     c                   when      hmdtyp = 'RF'
     c                   move      *on           *in92
     c                   exsr      drinfo
     c                   when      hmdtyp = 'PH'
     c                   move      *off          *in92
     c                   exsr      drinfo
     c                   other
     c                   exsr      srstaf
     c                   endsl
     c                   endsr
      *****************************************************************
      /EJECT
     c     dradd         begsr
      **
     c                   if        *in67 = *off
     c                   movel     nao(1)        dspmsg
     c                   move      *on           *in04
     c                   goto      dsplst
     c                   endif
      **
     c                   move      'RF'          hldtyp
     c                   move      *on           *in92
     c                   move      *on           *in64
     c                   eval      rc = 1
     c                   eval      wsdgp# = 0
     c                   exsr      phnclr
     c                   exsr      drinfo
     c                   endsr
      *****************************************************************
      /SPACE 3
     c     phyadd        begsr
      **
     c                   if        *in60 = *off
     c                   movel     nao(1)        dspmsg
     c                   move      *on           *in04
     c                   goto      dsplst
     c                   endif
      **
     C                   move      '1'           *in64
     c                   move      'PH'          hldtyp
     c                   move      *off          *in92
     c                   eval      rc = 1
     c                   eval      wsdgp# = 0
     c                   exsr      phnclr
     c                   exsr      drinfo
     c                   endsr
      *****************************************************************
      /EJECT
     c     srstaf        begsr
      **
     c                   eval      hmdhe2 = *blanks
     c                   if        inzs = '0'
     c                   movel     nao(1)        dspmsg
     c                   move      *on           *in04
     c                   goto      dsplst
     c                   endif
      **
     c****               if        reqopt = 'AT'
     c                   if        *inkf = *on
     c                   movea     '00'          *in(85)
     c                   eval      rc = 1
     c                   eval      wsdgp# = 0
     c                   eval      hmddr# = 0
     c                   move      'E'           hmdefl
     c                   move      'N'           wsdefl
     c                   eval      addth = 'Y'
     c                   else
     c                   if        reqopt = 'CH'
     c                   movea     '10'          *in(85)
     c                   else
     c                   movea     '11'          *in(85)
     c                   endif
     c                   endif
      **
     c****               if        reqopt = 'AT'
     c                   if        addth = 'Y'
     c                   clear                   hmfmams
     c                   movea     '00'          *in(78)
     c****               eval      hmdorp = 'N'
     c                   else
     c     hmddr#        chain     hmfmams                            7978
     c                   eval      hmdhe2 = hmdhe1
     c                   endif
      **
     c                   if        *in78 = *on
     c                   exsr      lckrec
     c                   endif
     c                   if        *in79 = *on
     c                   movel     rnf(1)        dspmsg
     c                   goto      dsplst
     c                   endif
      **
     c                   if        reqopt = 'IN' or reqopt = 'CH'
     c                   eval      wscosn = %subst(hmdfut:6:1)
     c                   else
     c                   eval      wscosn = 'N'
     c                   endif
      **
     c                   clear                   hmfphyad
     c                   move      1             adrseq
     c     adrkey        chain     hmfphyad                           87
      **
     c****               if        reqopt = 'AT'
     c                   if        addth = 'Y'
     c                   movel     reqtyp        hmdtyp
     c                   exsr      phnclr
     c                   endif
      **
     c                   if        wspcon = 'Y'
     c                   move      *on           *in80
     c                   else
     c                   move      *off          *in80
     c                   endif
      **
     c                   eval      lv5nam = *blanks
     c     hmdlv5        chain     hxflvl5                            79
     c                   if        *in79 = *off
     c                   movel     hx5nam        lv5nam
     c                   endif
      **
     c                   move      ' '           once
      **
     c****               if        reqopt <> 'AT'
     c                   if        addth <> 'Y'
      **                                                    ADD
     c                   if        hmdefl = 'E'
     c                   move      'N'           wsdefl
     c                   move      *off          *in80
     c                   else
     c                   move      'Y'           wsdefl
     c                   move      *on           *in80
     c                   endif
      **
     c                   endif
      **
     c                   exsr      phload
     c                   exsr      clrerr
     c                   move      'Y'           errid
      **
     c                   eval      wsnpic = *blanks
     c                   eval      wsnpid2 = *blanks
     C                   eval      npicod = 'NPI'
     C     npikey        chain(n)  hmflice                            79
     C                   if        *in79 = *off
     C                   movel     hlcnum        wsnpic
     C*    wsnpic        chain     hmfnpi
     C*                  movel     npname        wsnpid2
     c                   exsr      NPIsr
     c                   eval      wsnpid2 = npiDS.name
     C                   endif
      **
     c                   eval      lvlnum = 0
     c                   eval      lvl6ds2 = *blanks
     c     hmddr#        setll     hmfthrcp
     c     hmddr#        reade(n)  hmfthrcp                               79
     c                   if        *in79 = *off
     c                             and hmtlv6 <> 0
     c                   eval      lvlnum = hmtlv6
     c     lvlnum        chain     hxplvl6                            79
     c                   movel     hx6nam        lvl6ds2
     c                   endif
     c                   eval      shw2nd = ' '
     c     displs        tag
      **
     c                   if        dspmsg = *blanks
     c                   move      *off          *in83
     c                   else
     c                   move      *on           *in83
     c                   endif
      **
     c                   movel     level6        string
     c     high:lo       xlate     string        result
     c                   movel     lev6(1)       result
     c                   movel(p)  lv6abl        strnga
     c     high:lo       xlate     strnga        tempct
     c                   movel(p)  lv6abc        lvl6nm
     c                   cat       tempct:0      lvl6nm
     c                   cat       ':':0         lvl6nm
      **
     c                   movel     level5        string
     c     high:lo       xlate     string        result
     c                   movel     lev6(1)       result
      **
     c***                   eval      homel5 = *blanks
     c***                   movel     result        homel5
     c***                   cat       ':':0         homel5
     c                   movel(p)  lv5abl        strnga
     c     high:lo       xlate     strnga        tempct
     c                   movel(p)  lv5abc        lvl5nm
     c                   cat       tempct:0      lvl5nm
     c                   cat       ':':0         lvl5nm
      **
     c**                 write     hxffkey
     c**
     c                   eval      centerdsc = 'STAFF MAINTENANCE'
     c                   call      'XFXCNTG'
     c                   parm                    centerdsc
     c                   parm                    centerlen
     c                   parm                    centerfil
     c                   eval      wshdr = centerdsc
     c**
     c                   exfmt     hmfthrup
      **
     c                   if        *inkb = *on
     c                   movea     *in(01)       indice
     c                   call      'XFCFKEY'
     c                   parm                    dspfnm
     c                   parm                    rcdfmt
     c                   parm                    indice
     c                   parm                    xx
     c     xx            cabeq     '  '          displs
     c                   exsr      srfkey
     c                   endif
      **
     c                   if        *inkc = *on
     c                   exsr      unlock
     c                   goto      exit
     c                   endif
      **
     c                   if        *inka = *on
     c                   if        csrfld = 'HMDTYP    '
     c                   if        *in85 = *on or *in86 = *on
     c                             or *in64 = *on
     c                   move      'Y'           errid
     c                   goto      displs
     c                   endif
     c                   endif
     c                   exsr      chklin
     c                   move      *on           *in90
     c                   endif
      **
     c                   if        *in98 = *on
     c                   if        *in96 = *on
     c                   exfmt     hmfdruec
     c                   else
     c                   call      'HXWHLP '
     c                   parm                    dspfnm
     c                   parm                    rcdfmt
     c                   endif
     c                   goto      displs
     c                   endif
      **
     c****               if        *inke = *on
     c****               move      *on           *in90
     c**** rc            cabeq     2             chgchk
     c****               goto      displs
     c****               endif
      **
     c                   movea     zrofld        *in(01)
     c                   eval      dspmsg = *blanks
     c                   if        once = ' '
     c                   move      'X'           once
     c                   move      *on           *in90
     c                   endif
      **
     c     *inkl         cabeq     *on           stafnd
     c**** *in86         cabeq     *on           stafnd
     c     *in86         cabeq     *on           try1t
      **
     c     chgchk        tag
B1   c                   if        *in90 = *on
     c                             or errid = 'Y'
      **
     c                   exsr      clrerr
      **
B2   c                   if        *in85 = *off
      **
B3   c                   if        wsdefl <> 'Y'
     c                   move      hmddr#        tmpdr#
      **
B4   c     1             do        9             x
B5   c                   if        tmp(x) = '0'
     c                   eval      tmp(x) = *blanks
X5   c                   else
     c                   goto      gotdr#
E5   c                   endif
E4   c                   enddo
      **
     c     gotdr#        tag
E3   c                   endif
      **
E2   c                   endif
      **
B2   c                   if        hmdnam = *blanks
     c                   move      *on           *in02
X2   c                   else
                         hmdnam = ReplaceNonBreakCharacters(hmdnam); // Replace with regular space
     c                   movea     hmdnam        cknm(1)
     c                   exsr      chknam
     c                   move      *in79         *in02
E2   c                   endif

     c****               if        hmdorp = *blanks or (hmdorp <> *blanks
     c****                           and hmdorp <> 'Y' and hmdorp <> 'N')
     c****               move      *on           *in37
     c****               else
     c****               move      *off          *in37
     c****               endif
      **
     c                   if        hmdss# = 0
     c*******************move      *on           *in03
     c                   endif
      **
     c                   if        hmdusr <> *blanks
     c     hmdusr        chain     hxfuser                            79
     c                   if        *in79 = *on
     c                   move      *on           *in13
     c                   else
     c     hmdusr        setll     hmfmamsi                           79
     c                   dou       *in79 = *on
     c     hmdusr        reade     hmfmamsi                               79
     c                   if        *in79 = *off
     c                             and hmddr# <> imddr#
     c                   move      *on           *in13
     c                   endif
     c                   enddo
     c                   endif
     c                   endif
      **
     c                   exsr      clrtbl
     c                   movel     'XYON'        tcode
     c                   movel     wsdefl        ecode
     c                   exsr      srtabl
B4   c                   if        tind = 'E'
     c                   move      *on           *in04
X2   c                   else
B3   c                   if        wsdefl = 'Y'
     c                   move      'C'           hmdefl
X3   c                   else
     c                   move      'E'           hmdefl
E3   c                   endif
E2   c                   endif
      **
      ****               if        hmaotl = 0
     c                   if        wswphn = 0
     c*******************move      *on           *in09
     c                   endif
      **
     c                   if        hmdha1 = *blanks
     c*******************move      *on           *in05
     c                   endif
      **
        //check zip when is not *blanks, pull city and state if are available
       if hmdhz1 <> *blanks;
         xfxstzp( hmdhs1 : hmdhz1 : hmdhe2 : *blanks
                : errStZp : city : county : country : state );
         if errStZp = 2 or errStZp = 3;
           *in08 = *on;
         else;
           if hmdhc1 = *blanks and city <> *blanks;
             hmdhc1 = city;
           endif;
           if state <> *blanks;
             hmdhs1 = state;
           endif;
         endif;
       endif;
B2   c*                  if        hmdhz1 = *blanks
     c*******************move      *on           *in08
X2   c*                  else
B3   c*                  if        hmdhs1 <> *blanks
     c*                  movel     hmdhs1        ckzip
     c*                  movel     hmdhz1        tmp3
     c*                  move      tmp3          ckzip
     c*                  exsr      clrtbl
     c*                  movel     'XZIP'        tcode
     c*                  movel     ckzip         ecode
     c*                  exsr      srtabl
B4   c*                  if        tind = 'E'
     c*                  move      *on           *in08
X4   c*                  else
B5   c*                  if        hmdhc1 = *blanks
     c*    hmdhz1        chain     hxfctzp                            79
B6   c*                 *if        *in79 = *off
B7   c*                 *if        hxcity <> *blanks
     c*                  movel     hxcity        hmdhc1
E7   c*                  endif
B7   c*                 *if        hxstat <> *blanks
     c*                  movel     hxstat        hmdhs1
E6   c*                  endif
E7   c*                  endif
E5   c*                  endif
E4   c*                  endif
X3   c*                  else
     c*    hmdhz1        chain     hxfctzp                            79
B4   c*                  if        *in79 = *off
B5   c*                  if        hxcity <> *blanks
     c*                  movel     hxcity        hmdhc1
E5   c*                  endif
B5   c*                  if        hxstat <> *blanks
     c*                  movel     hxstat        hmdhs1
E5   c*                  endif
E4   c*                  endif
E3   c*                  endif
E2   c*                  endif
      **
B2   c                   if        hmdhc1 = *blanks
     c*******************move      *on           *in06
E2   c                   endif
      **
B2   c*                  if        hmdhs1 = *blanks
     c*******************move      *on           *in07
X2   c*                  else
B2   c                   if        hmdhs1 <> *blanks
     c                   exsr      clrtbl
     c                   movel     'XSTA'        tcode
     c                   movel     hmdhs1        ecode
     c                   exsr      srtabl
B3   c                   if        tind = 'E'
     c                   move      *on           *in07
E3   c                   endif
E2   c                   endif
      **
     c                   if        wshphn = 0
     c*******************move      *on           *in12
     c                   endif
      **
     c                   eval      sta = *blanks
     c                   eval      arr = *blanks
     c                   eval      in = 0
      **
     c                   eval      lv5nam = *blanks
     c     hmdlv5        chain     hxflvl5                            89
B2   c                   if        *in89 = *on and hmdlv5 <> 0
     c                   move      *on           *in11
X2   c                   else
     c                   if        *in89 = *off
     c                   movel     hx5nam        lv5nam
E2   c                   endif
E2   c                   endif
      **
     c*                  movel     *blanks       wsnpid2
     c*                  if        wsnpic <> *blanks
     c*    wsnpic        chain     hmpnpi                             79
     c*                  if        *in79
     c*                  eval      *in32 = *on
     c*                  else
     c*                  movel     npname        wsnpid2
     c*                  if        hmdha1 = *blanks and hmdhc1 = *blanks
     c*                            and hmdhs1 = *blanks and hmdhz1 = *blanks
     c*                            and hmdhe2 = *blanks and wshphn = 0
     c*                  eval      hmdha1 = nplad1
     c*                  eval      hmdhc1 = nplcty
     c*                  eval      hmdhs1 = nplst
     c*                  movel     npzip1        hmdhz1
     c*                  movel     npzip2        hmdhe2
      /free
      // RemoveNonNumericCharacters(
      //     nplphn :
      //     numericPhoneNumber :
      //     extensionNumber);
      // wshphn = numericPhoneNumber;
      /end-free
     c*                  endif

     c*                  endif
     c*                  endif
       //NPI info
       wsnpid2 = *blanks;
       if wsnpic <> *blanks;
         exsr NPIsr;
         if *in32 = *off;
           wsnpid2 = npiDS.name;
           if hmdha1 = *blanks and hmdhc1 = *blanks
              and hmdhs1 = *blanks and hmdhz1 = *blanks
              and hmdhe2 = *blanks and wshphn = 0;
             hmdha1 = npiDS.address;
             hmdhc1 = npiDS.city;
             hmdhs1 = npiDS.state;
             hmdhz1 = zip1;
             hmdhe2 = zip2;
             RemoveNonNumericCharacters(npids.phone:
                                        numericPhoneNumber :
                                        extensionNumber);
             wshphn = numericPhoneNumber;
           endif;
         endif;
       else;
         exsr checkNpiRequired;
       endif;
      **
     c                   movel     *blanks       lvl6ds2
     c                   if        lvlnum <> 0
     c     lvlnum        chain     hxplvl6                            79
     c                   if        *in79
     c                   eval      *in33 = *on
     c                   else
     c                   movel     hx6nam        lvl6ds2
     c                   endif
     c                   endif
      **
     c                   if        shw2nd <> *blanks
     c                             and shw2nd <> 'X'
     c                   move      *on           *in34
     c                   endif
      **
B2   c                   if        *in85 = *off
B3   c                   if        hmdtyp = *blanks
     c                   move      *on           *in24
X3   c                   else
     c                   exsr      clrtbl
     c                   movel     'MMTP'        tcode
     c                   movel     hmdtyp        ecode
     c                   exsr      srtabl
B4   c                   if        tind = 'E' or hmdtyp = 'RF' or hmdtyp = 'PH'
     c                   move      *on           *in24
E4   c                   endif
      ** HMAP is now used to control
      ** the type of license # that
      ** is required.
B4   c**                 if        hmap <> ' '
     c**                 move      *on           *in24
E4   c**                 endif
E3   c                   endif
E2   c                   endif
      **
     c                   eval      *in81 = *off
     c***                   if        hmdgp# <> 0
     c                   if        wsdgp# <> 0
     c                   exsr      clrtbl
     c***                   movel     'MGRP'        tcode
     c***                   movel     hmdgp#        ecode
     c                   movel     wsdgp#        ecode
     c                   exsr      srphysgrp
     c                   if        tind = 'E'
     c                   eval      *in44 = *on
     c                   eval      hmdgpn = *blanks
     c                   else
     c                   eval      hmdgpn = ''
     c                   movel     sdesc         hmdgpn
     c                   eval      *in81 = *on                                  =protect field
     c                   endif
     c                   else
     c                   eval      hmdgpn = *blanks
     c                   endif
      **
     c                   exsr      clrtbl
     c                   eval      ecode = wscosn
     c                   eval      tcode = 'XYON'
     c                   exsr      srtabl
     c                   if        tind = 'E'
     c                   move      *on           *in35
     c                   endif
     c                   if        wscosn = 'Y'
     c                             and hmdgpn = *blanks
     c                   eval      *in15 = *on
     c                   move      *on           *in36
     c                   endif

      /free
       // Supervising Clinician - cannot be self, cannot be another NP, cannot be deleted //
       *in40 = *off;
       if hmdcp5 <> 0;
          *in40 = (hmdcp5 = hmddr#);
          if not *in40;
             goodPhys = *off;
             exec sql select '1' into :goodPhys from HMPMAMS
                       where hmddr# = :hmdcp5 and hmdcp5= 0 and
                       HMDDLD = 0;
             *in40 = (not goodPhys);
          endif;
       endif;
      /end-free
     c                   eval      screen = 2
     c                   exsr      error
      **
     c     errid         cabeq     'Y'           displs
      **
B2   c****               if        reqopt = 'AT'
     c                   if        addth = 'Y'
     c                   movel     nef(1)        dspmsg
X2   c                   else
     c                   movel     nef(2)        dspmsg
E2   c                   endif
     c                   goto      displs
E1   c                   endif
      **
     c                   eval      hmdchd = today
     c                   movel     ldausr        hmdchb
     c                   eval      hmadr# = hmddr#
     c                   movel     hmdnam        hmanam
     c                   eval      hmdhe1 = hmdhe2
     c                   eval      hmdfut = %replace(wscosn:hmdfut:6:1)
      **
     c****               if        reqopt = 'AT'
     c                   if        addth = 'Y'
     c     try1          tag
      **
     c                   if        hmddr# = 0
     c     *lock         in        nxtnum
     c                   eval      hmddr# = nxtnum
     c                   eval      hmadr# = hmddr#
     c                   eval      nxtnum = nxtnum + 1
     c                   out       nxtnum
     c                   endif
     c                   if        hmdtyp = 'TH'
     c                   else
     c                   endif
     c     hmddr#        chain(n)  hmfmams                            79
     c                   if        *in79 = *off
     c                   movel     dae(1)        dspmsg
     c                   goto      displs
     c                   endif

     c                   exsr      writeGroups

     c                   write     hmfmams
     c                   write     hmfphyad
     c                   exsr      phupdt
     c                   movel(p)  tad(1)        dspmsg
     c                   else
     c                   exsr      updateGroup
     c                   update    hmfmams
     c                   if        *in87 = *off
     c                   update    hmfphyad
     c                   else
     c                   write     hmfphyad
     c                   endif
      **
     c*****npikey        chain     hmflice                            79
     c*****              eval      hlcphy = hmddr#
     c*****              eval      hlccod = 'NPI'
     c*****              eval      hlcnum = wsnpic
     c*****              if        wsnpic <> *blanks
     c*****              if        *in79
     c*****              write     hmflice
     c*****              else
     c*****              update    hmflice
     c*****              endif
     c*****              else
     c*****              if        *in79 = *off
     c*****              delete    hmflice
     c*****              endif
     c*****              endif
      **
     c*****hmddr#        setll     hmfthrcp
     c*****hmddr#        reade     hmfthrcp                               79
     c*****              if        *in79 = *off
     c*****              eval      hmtprm = 'N'
     c*****              update    hmfthrcp
     c*****              endif
     c*****clky2         chain     hmfthrco                           79
     c*****              eval      hmtprm = 'Y'
     c*****              if        hmtdlt = 'D'
     c*****              eval      hmtdlt = ' '
     c*****              endif
     c*****              if        *in79
     c*****              eval      hmtth# = hmddr#
     c*****              eval      hmtlv6 = lvlnum
     c*****              write     hmfthrco
     c*****              else
     c*****              update    hmfthrco
     c*****              endif
      **
     c                   exsr      phupdt
     c                   movel(p)  tup(1)        dspmsg
     c                   endif
     c                   exsr      NPI_Lv6
      **
     c     try1t         tag
      **
     c                   select
     c                   when      rc = 1
     c                   move      'A'           ropt
     c                   when      rc = 2
     c                   move      'C'           ropt
     c                   when      rc = 3
     c                   move      'I'           ropt
     c                   other
     c                   move      'I'           ropt
     c                   endsl
      **
     c                   if         shw2nd = 'X'
     c                   call      'HMWLICE'
     c                   parm                    ropt
     c                   parm      hmddr#        rphy
     c                   parm      hmdtyp        typ
     c                   parm      hmdnam        phynam
      **
     c                   call      'HMWLVASC'
     c                   parm      hmddr#        drnum
     c                   parm                    hmdtyp
     c                   parm                    ropt
     c                   endif
      **
     c     stafnd        tag
     c                   movea     zrofld        *in(01)
     c                   endsr
      *****************************************************************
      /EJECT
     c     drinqr        begsr
      **
     c                   if        inzi = '0'
     c                   movel     nao(1)        dspmsg
     c                   move      *on           *in04
     c                   goto      dsplst
     c                   endif
      **
     c                   if        reqlno = 0
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c     reqlno        chain     hmfdrupr                           05
     c                   if        *in05 = *on
     c                   movel     iln(1)        dspmsg
     c                   goto      dsplst
     c                   else
     c                   if        fstlet = '*'
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   endif
     c                   endif
     c                   endif
     c                   eval      rc = 3
     c                   movea     zrofld        *in(01)
     c                   if        hmdtyp = 'PH'
     c                             or hmdtyp = 'RF'
     c                   select
     c                   when      hmdtyp = 'RF'
     c                   move      *on           *in92
     c                   exsr      drinfo
     c                   when      hmdtyp = 'PH'
     c                   move      *off          *in92
     c                   exsr      drinfo
     c                   endsl
     c                   else
     c                   exsr      srstaf
     c                   endif
     c                   endsr
      *****************************************************************
      /EJECT
     c     drdlt         begsr
      **
     c                   if        *in68 = *off
     c                   movel     nao(1)        dspmsg
     c                   move      *on           *in04
     c                   goto      dsplst
     c                   endif
      **
     c                   if        reqlno = 0
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c     reqlno        chain     hmfdrupr                           05
     c                   if        *in05 = *on
     c                   movel     iln(1)        dspmsg
     c                   goto      dsplst
     c                   else
     c                   if        fstlet = '*'
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   endif
     c                   endif
     c                   endif
      **
     c                   eval      wsddld = 0
     c                   move      *on           *in91
     c     agn           tag
      **
     c**                 write     hxffkey
      **
     c                   eval      centerdsc = 'MEDICAL ADMINISTRATION ' +
     c                                         'MAINTENANCE'
     c                   call      'XFXCNTG'
     c                   parm                    centerdsc
     c                   parm                    centerlen
     c                   parm                    centerfil
     c                   eval      wshdr = centerdsc
     c**
     c                   exfmt     hmfdrupc
      **
     c                   if        *inkb = *on
     c                   movea     *in(01)       indice
     c                   call      'XFCFKEY'
     c                   parm                    dspfnm
     c                   parm                    rcdfmt
     c                   parm                    indice
     c                   parm                    xx
     c     xx            cabeq     '  '          agn
     c                   exsr      srfkey
     c                   endif
      **
     c                   move      *off          *in93
      **
     c                   if        *inkc = *on
     c                   movel(p)  pnd(1)        dspmsg
     c                   goto      enddlt
     c                   endif
      **
     c                   if        *inkl = *on
     c                   movel(p)  pnd(1)        dspmsg
     c                   goto      enddlt
     c                   endif
      **
           // Check for related user //

           clear found;
           exec sql
             SELECT '1'
             INTO :found
             FROM txpuser
             JOIN hmpmams ON hxuser = hmdusr
               WHERE hxudlt = ''
                 AND hmdusr <> ''
                 AND hmddr# = :hmddr#;

           if found;
     c                   movel(p)  pnd(2)        dspmsg
     c                   goto      enddlt
           endif;

      **
     c                   if        wsddld = 0
     c                   move      *on           *in93
     c                   movel     dlt(1)        dspmsg
     c                   goto      agn
     c                   else
     c                   eval      mdate = wsddld
     c                   exsr      srcmdy
     c                   if        ydate = 0
     c                   move      *on           *in93
     c                   movel     dlt(1)        dspmsg
     c                   goto      agn
     c                   else
     c                   if        ydate < today
     c                   move      *on           *in93
     c                   movel     dlt(2)        dspmsg
     c                   goto      agn
     c                   endif
     c                   endif
     c                   endif
      **
     c     hmddr#        chain     hmfmams                            7978
      **
     c                   move      'X'           hmddel
     c                   eval      hmddld = ydate
     c                   movel     ldausr        hmddlb
      **
     c                   update    hmfmams
     c                   exsr      phupdt
      **
     c                   movel(p)  dtp(1)        dspmsg

        //Delete Rapi User
        DeleteRapiUser(ldausr: 'HMWMAUP': hmdusr);

     c     enddlt        tag
     c                   move      *off          *in91
     c                   endsr
      *****************************************************************
      /EJECT
     c     drrns         begsr
      **
     c                   if        *in73 = *off
     c                   movel     nao(1)        dspmsg
     c                   move      *on           *in04
     c                   goto      dsplst
     c                   endif
      **
     c                   if        reqlno = 0
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c     reqlno        chain     hmfdrupr                           05
     c                   if        *in05 = *on
     c                   movel     iln(1)        dspmsg
     c                   goto      dsplst
     c                   else
     c                   if        fstlet = '*'
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   endif
     c                   endif
     c                   endif
      **
           // Check for related user //

           clear found;
           exec sql
             SELECT '1'
             INTO :found
             FROM txpuser
             JOIN hmpmams ON hxuser = hmdusr
               WHERE hxudlt <> ''
                 AND hmdusr <> ''
                 AND hmddr# = :hmddr#;

           if found;
             clear reqopt;
     c                   movel(p)  pnd(3)        dspmsg
     c                   goto      dsplst
           endif;

     c
      **
     c     hmddr#        chain     hmfmams                            7978
      **
     c                   exsr      isNPIduplSR
     c                   if        npiDuplicated
     c                   movel(p)  pnd(4)        dspmsg
     c                   eval      dspmsg = %trim(dspmsg)+
     c                             %char(duplID#)
     c                   goto      dsplst
     c                   endif
      **
     c                   move      ' '           hmddel
     c                   eval      hmddld = 0
     c                   eval      hmddlb = *blanks
      **
     c                   update    hmfmams
     c                   exsr      phupdt
      **
     c                   movel     rnp(1)        dspmsg
     c                   endsr
      *****************************************************************
      /EJECT
     c     drprt         begsr
      **
     c                   if        inzl = '0'
     c                   movel     nao(1)        dspmsg
     c                   move      *on           *in04
     c                   goto      dsplst
     c                   endif
      **
     c                   if        reqlno = 0
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c     reqlno        chain     hmfdrupr                           05
     c                   if        *in05 = *on
     c                             or fstlet = '*'
     c                   movel     iln(1)        dspmsg
     c                   goto      dsplst
     c                   endif
     c                   endif
      **
     c                   if        hmdtyp = 'RF'
     c                             or hmdtyp = 'PH'
     c                   call      'HMCMAPR'
     c                   parm      hmddr#        drnum
     c                   parm                    ldasoq
     c                   else
     c                   call      'HMCTHPR'
     c                   parm      hmddr#        drnum
     c                   parm                    ldasoq
     c                   endif
      **
     c                   if        hmdtyp = 'RF'
     c                             or hmdtyp = 'PH'
     c                   movel     prt(1)        dspmsg
     c                   else
     c                   movel     prt(2)        dspmsg
     c                   endif
      **
     c                   eval      reqopt = *blanks
     c                   eval      reqlno = 0
     c                   goto      dsplst
      **
     c                   endsr
      *****************************************************************
      /EJECT
     c     drcor         begsr
      **
     c                   if        *in75 = *off
     c                   movel     nao(1)        dspmsg
     c                   move      *on           *in04
     c                   goto      dsplst
     c                   endif
      **
     c                   if        reqlno = 0
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c     reqlno        chain     hmfdrupr                           05
     c                   if        *in05 = *on
     c                             or fstlet = '*'
     c                   movel     iln(1)        dspmsg
     c                   goto      dsplst
     c                   else
     c                   if        hmdtyp <> 'RF'
     c                             and hmdtyp <> 'PH'
     c                   movel     iop(2)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   endif
     c                   endif
     c                   endif
      **
     c                   call      'HMCCMNT'
     c                   parm      hmddr#        drnum
     c                   parm      'M'           type
      **
     c                   endsr
      *****************************************************************
      /EJECT
     c     srlvl6        begsr
      **
     c                   if        *in61 = *off
     c                   movel     nao(1)        dspmsg
     c                   move      *on           *in04
     c                   goto      dsplst
     c                   endif
      **
     c                   if        reqlno = 0
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c     reqlno        chain     hmfdrupr                           05
     c                   if        *in05 = *on
     c                             or fstlet = '*'
     c                   movel     iln(1)        dspmsg
     c                   goto      dsplst
     c                   endif
     c                   endif
      **
     c                   move      'A'           ropt
      **
     c                   call      'HMWLVASC'
     c                   parm      hmddr#        drnum
     c                   parm                    hmdtyp
     c                   parm                    ropt
      **
     c                   endsr
      *****************************************************************
      /EJECT
     c     drlet         begsr
      **
     c                   if        *in76 = *off
     c                   movel     nao(1)        dspmsg
     c                   move      *on           *in04
     c                   goto      dsplst
     c                   endif
      **
     c                   if        reqlno = 0
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c     reqlno        chain     hmfdrupr                           05
     c                   if        *in05 = *on
     c                             or fstlet = '*'
     c                   movel     iln(1)        dspmsg
     c                   goto      dsplst
     c                   else
     c                   if        hmdtyp <> 'RF'
     c                             and hmdtyp <> 'PH'
     c                   movel     iop(2)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   endif
     c                   endif
     c                   endif
      **
     c                   call      'HMWLETR'
     c                   parm      hmddr#        drnum
     c                   parm      0             l6
     c                   parm      0             epsd
      **
     c                   endsr
      *****************************************************************
      /SPACE 3
     c     invreq        begsr
     c                   movel     iop(1)        dspmsg
     c                   move      *on           *in04
     c                   goto      dsplst
     c                   endsr
      *****************************************************************
      /EJECT
     c     drinfo        begsr
      **
     c                   exsr      inzscn
     c                   exsr      phload
     c                   exsr      clrerr
      **
     c                   move      'Y'           errid
      *****************************************************************
     c     displ1        tag
      **
     c                   if        dspmsg = *blanks
     c                   move      *off          *in83
     c                   else
     c                   move      *on           *in83
     c                   endif
      **
     c                   movel     level4        string
     c     high:lo       xlate     string        result
     c                   movel     lev6(1)       result
     c**
     c                   eval      centerdsc = 'MEDICAL ADMINISTRATION ' +
     c                                         'MAINTENANCE'
     c                   call      'XFXCNTG'
     c                   parm                    centerdsc
     c                   parm                    centerlen
     c                   parm                    centerfil
     c                   eval      wshdr = centerdsc
      **
     c**                 write     hxffkey
     c                   if        *in92 = *on
     c                   exfmt     hmfphyup
     c                   else
     c                   exfmt     hmfphup2
     c                   endif
      **
     c                   if        *inkb = *on
     c                   movea     *in(01)       indice
     c                   call      'XFCFKEY'
     c                   parm                    dspfnm
     c                   parm                    rcdfmt
     c                   parm                    indice
     c                   parm                    xx
     c     xx            cabeq     '  '          displ1
     c                   exsr      srfkey
     c                   endif
      **
     c                   if        *inkc = *on
     c                   exsr      unlock
     c                   goto      exit
     c                   endif
      **
     c                   if        *inka = *on

     c                   if        csrfld = 'HMDTYP    '
     c                   if        *in85 = *on or *in86 = *on
     c                             or *in64 = *on
     c                   move      'Y'           errid
     c                   goto      displ1
     c                   endif
     c                   endif
     c                   exsr      chklin                                       =window help
     c                   move      'Y'           errid
     c                   endif
      **
     c****               if        *inke = *on
     c********           exsr      empsrh
     c****               move      *on           *in90
     c**** rc            cabeq     2             chgck2
     c****               goto      displ1
     c****               endif
      **
     c                   if        *in98 = *on
     c                   if        *in96 = *on
     c                   exfmt     hmfdruec
     c                   else
     c                   if        *in92 = *on
     c                   call      'HXWHLP'
     c                   parm                    dspfnm
     c                   parm                    rcdfmt
     c                   else
     c                   call      'HXWHLP'
     c                   parm                    dspfnm
     c                   parm                    rcdfmt
     c                   endif
     c                   endif
     c                   goto      displ1
     c                   endif
      **
     c                   eval      dspmsg = *blanks
     c     *inkl         cabeq     *on           enddru
      **
     c     chgck2        tag
     c                   if        rc = 3
     c**** addoad        cabeq     'Y'           addr
     c                   move      *off          *in83
     c                   if        *in92 = *off
     c                   if        shw2nd = 'X'
     c                   goto      dspl01
     c                   else
     c                   goto      enddru
     c                   endif
     c                   endif
     c***     addoad        cabeq     'Y'           addr
     c***                   goto      lic#
     c     shw2nd        cabeq     'X'           addr
     c                   goto      enddru
     c                   endif
      **
     c                   if        *in90 = *on
     c                             or errid = 'Y'
     c                   exsr      clrerr
     c                   exsr      check1
     c                   goto      displ1
 Q   c                   endif
     c                   move      *on           *in90
     c                   move      '1'           frstme
      **
     c                   if        hmdmdp = *blanks
     c                   eval      hmdmdp = 'N'
     c                   endif
      **
     c                   if        shw2nd = 'X'
     c     dspl01        tag
      **
     c                   if        *in92 = *off
      **
     c                   if        dspmsg = *blanks
     c                   move      *off          *in83
     c                   else
     c                   move      *on           *in83
     c                   endif
      **
     c                   movel     level4        string
     c     high:lo       xlate     string        result
     c                   movel     lev6(1)       result
      **
     c**                 write     hxffkey
     c**
     c                   eval      centerdsc = 'MEDICAL ADMINISTRATION ' +
     c                                         'MAINTENANCE'
     c                   call      'XFXCNTG'
     c                   parm                    centerdsc
     c                   parm                    centerlen
     c                   parm                    centerfil
     c                   eval      wshdr = centerdsc
      **
     c                   exfmt     hmfphup3
      **
     c                   if        *inkb = *on
     c                   movea     *in(01)       indice
     c                   call      'XFCFKEY'
     c                   parm                    dspfnm
     c                   parm                    rcdfmt
     c                   parm                    indice
     c                   parm                    xx
     c     xx            cabeq     '  '          dspl01
     c                   exsr      srfkey
     c                   endif
      **
     c                   if        *inkc = *on
     c                   exsr      unlock
     c                   goto      exit
     c                   endif
      **
     c                   if        *inka = *on
     c                   exsr      chklin
     c                   move      'Y'           errid
     c                   endif
      **
     c****               if        *inke = *on
     c*********          exsr      empsrh
     c****               move      *on           *in90
     c**** rc            cabeq     2             chgck3
     c****               goto      dspl01
     c****               endif
      **
     c                   if        *in98 = *on
     c                   if        *in96 = *on
     c                   exfmt     hmfdruec
     c                   else
     c                   call      'HXWHLP'
     c                   parm                    dspfnm
     c                   parm                    rcdfmt
     c                   endif
     c                   goto      dspl01
     c                   endif
      **
     c                   eval      dspmsg = *blanks
     c     *inkl         cabeq     *on           displ1
      **
     c     chgck3        tag
     c                   if        rc <> 3
     c                   if        *in90 = *on
     c                             or errid = 'Y'
     c                             or frstme = '1'
     c                   exsr      clrerr
     c                   exsr      chck01
     c                   move      '0'           frstme
     c*******************movel     updt(2)       dspmsg
     c                   goto      dspl01
     c                   endif
     c                   else
     c                   move      *off          *in83
     c                   goto      addr
     c****               goto      lic#
     c                   endif
     c                   endif
     c                   endif
      *****************************************************************
     c                   eval      hmdchd = today
     c                   move      ldausr        hmdchb
     c                   eval      hmadr# = hmddr#
     c                   movel     hmdnam        hmanam
     c                   eval      hmaosq = 1
     c                   eval      hmdhe1 = hmdhe2
     c                   eval      hmdfut = %replace(wscosn:hmdfut:6:1)
      **
     c                   if        rc = 1
     c     try3          tag
     c                   if        hmddr# = 0
     c     *lock         in        nxtnum
     c                   eval      hmddr# = nxtnum
     c                   eval      nxtnum = nxtnum + 1
     c                   out       nxtnum
     c                   endif
     c     hmddr#        chain(n)  hmfmams                            79
     c                   if        *in79 = *off
     c                   movel     dae(1)        dspmsg
     c                   goto      displ1
     c                   endif

     c                   exsr      writeGroups

     c                   write     hmfmams
     c*                  if        rqcode = 'W'
     c*                  eval      hmtth# = hmddr#
     c*                  move      rqlev6        hmtlv6
     c*                  movel     'N'           hmtprm
     c*                  eval      hmtdlt = *blanks
     c*                  eval      hmtddt = 0
     c*                  eval      hmtdby = *blanks
     c*                  write     hmfthrcl
     c*                  endif
     c     adrkey        chain(n)  hmfphyad                           79
     c     *in79         cabeq     *off          try3t
     c                   eval      hmadr# = hmddr#
     c                   write     hmfphyad
     c                   exsr      phupdt
     c                   movel     cms(3)        dspmsg
     c                   else
     c                   exsr      updateGroup
     c                   update    hmfmams
      **
     c*****npikey        chain     hmflice                            79
     c*****              eval      hlcphy = hmddr#
     c*****              eval      hlccod = 'NPI'
     c*****              eval      hlcnum = wsnpic
     c*****              if        wsnpic <> *blanks
     c*****              if        *in79
     c*****              write     hmflice
     c*****              else
     c*****              update    hmflice
     c*****              endif
     c*****              else
     c*****              if        *in79 = *off
     c*****              delete    hmflice
     c*****              endif
     c*****              endif
      **
     c*****hmddr#        setll     hmfthrcp
     c*****hmddr#        reade     hmfthrcp                               79
     c*****              if        *in79 = *off
     c*****              eval      hmtprm = 'N'
     c*****              update    hmfthrcp
     c*****              endif
     c*****clky2         chain     hmfthrco                           79
     c*****              eval      hmtprm = 'Y'
     c*****              if        hmtdlt = 'D'
     c*****              eval      hmtdlt = ' '
     c*****              endif
     c*****              if        *in79
     c*****              eval      hmtth# = hmddr#
     c*****              eval      hmtlv6 = lvlnum
     c*****              write     hmfthrco
     c*****              else
     c*****              update    hmfthrco
     c*****              endif
      **
     c                   eval      phupdate = *on
     c                   if        *in87 = *off
     c                   update    hmfphyad
     c                   else
     c                   write     hmfphyad
     c                   endif
     c                   exsr      phupdt
     c                   movel     cms(2)        dspmsg
     c                   endif
      **
     c                   exsr      NPI_Lv6
      **
     c                   if        shw2nd = ' '
     c                   goto      enddru
     c                   endif
      **
     c     try3t         tag
     c                   if        adrchn = 'Y'
     c                   movel     hmdnam        hmanam
     c                   else
     c                   eval      hmaosq = 1
     c                   eval      hmadr# = hmddr#
     c                   movel     hmdnam        hmanam
     c                   endif
      *****************************************************************
      *****   ADDITIONAL OFFICE ADDRESSES
      *****************************************************************
     c     addr          tag
      **
     c***                   if        addoad <> 'Y'
     c***                   move      *off          *in83
     c***                   goto      lic#
     c***                   endif
      **
     c                   move      *off          *in83
     c                   move      'N'           verify
     c     dspadr        tag
     c                   move      'N'           errexs
     c                   move      *on           *in96
     c**                 write     hxffkey
     c**
     c                   if        *in92 = *on
     c                   eval      centerdsc = 'REFERRING PHYSICIAN OFFICE ' +
     c                                         'ADDRESSES'
     c                   else
     c                   eval      centerdsc = 'OFFICE ADDRESSES'
     c                   endif
     c                   call      'XFXCNTG'
     c                   parm                    centerdsc
     c                   parm                    centerlen
     c                   parm                    centerfil
     c                   eval      wshdr = centerdsc
      **
     c                   exfmt     hmfphyac
      **
     c                   if        *inka = *on
     c                   exsr      chklin
     c                   endif
      **
     c                   if        *inkb = *on
     c                   movea     *in(01)       indice
     c                   call      'XFCFKEY'
     c                   parm                    dspfnm
     c                   parm                    rcdfmt
     c                   parm                    indice
     c                   parm                    xx
     c     xx            cabeq     '  '          dspadr
     c                   exsr      srfkey
     c                   endif
      **
     c                   if        *inkc = *on
     c                   exsr      unlock
     c                   goto      exit
     c                   endif
      **
     c                   if        *inkl = *on
     c                   exsr      inzscn
     c                   exsr      phload
     c                   exsr      clrerr
     c                   move      'Y'           errid
     c                   eval      dspmsg = *blanks
     c****               goto      displ1
     c                   goto      dspl01
     c                   endif
      **
     c                   if        *in98 = *on
     c                   call      'HXWHLP '
     c                   parm                    dspfnm
     c                   parm                    rcdfmt
     c                   goto      dspadr
     c                   endif
      **
     c     rc            cabeq     3             lic#
      **
     c                   eval      inc = 1
1    c                   dou       *in89 = *on
     c     inc           chain     hmfphyas                           89
2    c                   if        *in89 = *off
     c                   eval      inc = inc + 1
     c                   move      *off          *in65
     c                   movea     '0000000'     *in(01)
      **
3    c                   if        hmaoad = *blanks
4    c                   if        hmaoa2 <> *blanks
     c                             or hmaoct <> *blanks
     c                             or hmaost <> *blanks
     c                             or hmaoz1 <> *blanks
     c                             or hmaoz2 <> *blanks
      ************                 or hmaotl <> 0
     c                             or wswphn <> 0
      ************                 or hmaofx <> 0
     c                             or wswfax <> 0
     c                   move      *on           *in01
E4   c                   endif
E3   c                   endif
      **
     c                   if        hmaoz1 <> *blanks
     c                   if        hmaoct = *blanks
     c                             or hmaost = *blanks
     c     hmaoz1        chain     hxfctzp                            79
     c                   if        *in79 = *off
     c                   if        hmaoct = *blanks
     c                   movel     hxcity        hmaoct
     c                   endif
     c                   if        hmaost = *blanks
     c                   movel     hxstat        hmaost
     c                   endif
     c                   endif
     c                   endif
     c                   endif
      **
3    c                   if        hmaost = *blanks
4    c                   if        hmaoad <> *blanks
     c                   move      *on           *in04
E4   c                   endif
X3   c                   else
     c                   exsr      clrtbl
     c                   movel     'XSTA'        tcode
     c                   movel     hmaost        ecode
     c                   exsr      srtabl
4    c                   if        tind = 'E'
     c                   move      *on           *in04
X3   c                   else
     c*                  movel     hmaost        ckzip
     c*                  movel     hmaoz1        tmp3
     c*                  move      tmp3          ckzip
     c*                  exsr      clrtbl
     c*                  movel     'XZIP'        tcode
     c*                  movel     ckzip         ecode
     c*                  exsr      srtabl
4    c*                  if        tind = 'E'

        xfxstzp( hmaost : hmaoz1 : hmaoz2 : *blanks
               : errStZp : city : county : country : state );

     c                   if        errStZp = 2 or errStZp = 3
     c                   move      *on           *in05
E4   c                   endif
E3   c                   endif
E3   c                   endif
      **
3    c                   if        hmaoct = *blanks
4    c                   if        hmaoad <> *blanks
     c                   move      *on           *in03
E4   c                   endif
E3   c                   endif
      **
3     *****              if        hmaotl = 0
3    c                   if        wswphn = 0
4    c                   if        hmaoad <> *blanks
     c                   move      *on           *in06
E4   c                   endif
E3   c                   endif
      **
     c                   movea     *in(01)       tst7
3    c                   if        tst7 <> *all'0'
     c                   move      'Y'           errexs
     c                   move      *on           *in65
     c                   move      'N'           verify
     c                   endif
     c                   update    hmfphyas
     c                   endif
     c                   enddo
      **
     c                   if        errexs = 'Y'
     c                   move      'N'           verify
     c                   move      *on           *in83
     c                   movel     updt(1)       dspms2
     c                   goto      dspadr
     c                   else
     c                   if        verify = 'N'
     c                   move      'Y'           verify
     c                   move      *on           *in83
     c                   movel     updt(2)       dspms2
     c                   goto      dspadr
     c                   endif
     c                   endif
      **
     c     1             do        20            n
     c     n             chain     hmfphyas                           79
     c                   if        *in79 = *off
     c                   movel     hmaoad        svaoad
     c                   movel     hmaoa2        svaoa2
     c                   movel     hmaoct        svaoct
     c                   movel     hmaost        svaost
     c                   move      hmaoz1        svaoz1
     c                   move      hmaoz2        svaoz2
      ****               eval      svaotl = hmaotl
3    c                   eval      svaotl = wswphn
3    c                   eval      svextn = wsextn
      ****               eval      svaofx = wswfax
3    c                   eval      svaofx = wswfax
     c                   eval      adrseq = hmaosq
     c     adrkey        chain     hmfphyad                           79
     c                   if        *in79 = *off
     c                   movel     svaoad        hmaoad
     c                   movel     svaoa2        hmaoa2
     c                   movel     svaoct        hmaoct
     c                   movel     svaost        hmaost
     c                   move      svaoz1        hmaoz1
     c                   move      svaoz2        hmaoz2
      *****              eval      hmaotl = svaotl
      *****              eval      hmaofx = hmaofx
     c                   update    hmfphyad
     c                   exsr      oophup
      ************ new phone routine
     c                   else
     c                   if        hmaoad <> *blanks
     c                   write     hmfphyad
     c                   exsr      oophup
     c                   endif
     c                   endif
     c                   endif
     c                   enddo
      *****************************************************************
      **      PHYSICIAN STATE LICENSE NUMBERS
      *****************************************************************
     c     lic#          tag
      **
     c                   move      *on           *in96
     c                   movea     '00'          *in(82)
      **
     c                   select
     c                   when      rc = 1
     c                   move      'A'           ropt
     c                   when      rc = 2
     c                   move      'C'           ropt
     c                   when      rc = 3
     c                   move      'I'           ropt
     c                   other
     c                   move      'I'           ropt
     c                   endsl
      **
     c                   call      'HMWLICE'
     c                   parm                    ropt
     c                   parm      hmddr#        rphy
     c                   parm                    hmdtyp
     c                   parm                    hmdnam
      **
     c                   call      'HMWLVASC'
     c                   parm      hmddr#        drnum
     c                   parm                    hmdtyp
     c                   parm                    ropt
      **
     c                   eval      string = *blanks
     c                   eval      dspms2 = *blanks
     c                   movea     zrofld        *in(01)
     c     enddru        endsr
      *****************************************************************
      /EJECT
     c     chck01        begsr
      **
     c                   eval      hmdcgp = cgp1
     c                   if        cgp1 <> 0
     c                   exsr      clrtbl
     c***                   movel     'MGRP'        tcode
     c                   movel     cgp1          ecode
     c                   exsr      srphysgrp
     c                   if        tind = 'E'
     c                   move      *on           *in27
     c                   endif
     c                   endif
      **
     c                   if        hmdlg1 <> *blanks
     c                   exsr      clrtbl
     c                   movel     'MLNG'        tcode
     c                   movel     hmdlg1        ecode
     c                   exsr      srtabl
     c                   if        tind <> 'E'
     c                   movel     ldesc         wlgs1
     c                   else
     c                   move      *on           *in18
     c                   endif
     c                   endif
      **
     c                   if        hmdlg2 <> *blanks
     c                   exsr      clrtbl
     c                   movel     'MLNG'        tcode
     c                   movel     hmdlg2        ecode
     c                   exsr      srtabl
     c                   if        tind <> 'E'
     c                   movel     ldesc         wlgs2
     c                   else
     c                   move      *on           *in19
     c                   endif
     c                   endif
      **
     c                   if        hmdlg3 <> *blanks
     c                   exsr      clrtbl
     c                   movel     'MLNG'        tcode
     c                   movel     hmdlg3        ecode
     c                   exsr      srtabl
     c                   if        tind <> 'E'
     c                   movel     ldesc         wlgs3
     c                   else
     c                   move      *on           *in20
     c                   endif
     c                   endif
      **
     c                   exsr      clrtbl
     c                   movel     'XYON'        tcode
     c                   movel     hmdapr        ecode
     c                   exsr      srtabl
B4   c                   if        tind = 'E'
     c*                  move      *on           *in17
     c                   endif
      **
     c                   exsr      clrtbl
     c                   movel     'XYON'        tcode
     c                   movel     hmdphr        ecode
     c                   exsr      srtabl
B4   c                   if        tind = 'E'
     c*                  move      *on           *in02
     c                   endif
      **
     c                   if        wsdbqy <> 0
     c                   eval      mdate = wsdbqy
     c                   exsr      srcmdy
     c                   if        ydate = 0
     c                   move      *on           *in06
     c                   else
     c                   eval      hmdbqy = ydate
     c                   endif
     c                   else
     c                   eval      hmdbqy = 0
     c                   endif
      **
     c                   if        wsmdoh <> 0
     c                   eval      mdate = wsmdoh
     c                   exsr      srcmdy
     c                   if        ydate = 0
     c                   move      *on           *in01
     c                   else
     c                   eval      hmddoh = ydate
     c                   endif
     c                   else
     c                   eval      hmddoh = 0
     c                   endif
      **
     c                   if        wsdbid <> 0
     c                   eval      mdate = wsdbid
     c                   exsr      srcmdy
     c                   if        ydate = 0
     c                   move      *on           *in07
     c                   else
     c                   eval      hmdbid = ydate
     c                   if        hmdbqy >= hmdbid
     c                   move      *on           *in07
     c                   move      *on           *in06
     c                   endif
     c                   endif
     c                   else
     c                   eval      hmdbid = 0
     c                   endif
      **
     c                   if        hmdbnk <> ' '
     c                   exsr      clrtbl
     c                   movel     'XYON'        tcode
     c                   movel     hmdbnk        ecode
     c                   exsr      srtabl
B4   c                   if        tind = 'E'
     c                   move      *on           *in05
     c                   endif
     c                   endif
      **
     c***                   exsr      clrtbl
     c***                   movel     'MADP'        tcode
     c***                   movel     hmdadp        ecode
     c***                   exsr      srtabl
B4   c***                   if        tind = 'E'
     c***                   move      *on           *in08
     c***                   endif
      **
     c                   exsr      clrtbl
     c                   movel     'XYON'        tcode
     c                   movel     hmdbil        ecode
     c                   exsr      srtabl
B4   c                   if        tind = 'E'
     c*                  move      *on           *in09
     c                   endif
      **
     c                   if        hmdegc <> 'N'
     c                   if        hmdegc <> 'Y'
     c*                  move      *on           *in03
     c                   else
     c                   eval      mdate = wscex
     c                   exsr      srcmdy
     c                   if        ydate = 0
     c                   move      *on           *in04
     c                   else
     c                   eval      hmdcex = ydate
     c                   endif
     c                   endif
     c                   endif
      **
     c                   if        wslid <> 0
     c                   eval      mdate = wslid
     c                   exsr      srcmdy
     c                   if        ydate = 0
     c                   move      *on           *in24
     c                   else
     c                   eval      hmdlid = ydate
     c                   endif
     c                   endif
      **
     c                   movel     wslpm1        hmdlp1
     c                   movel     wslpm2        hmdlp2
     c                   movel     wslpm3        hmdlp3
      **
     c                   if        hmdlty <> ' '
     c                   exsr      clrtbl
     c                   movel     'XLTY'        tcode
     c                   movel     hmdlty        ecode
     c                   exsr      srtabl
B4   c                   if        tind = 'E'
     c                   move      *on           *in25
     c                   endif
     c                   endif
      **
     c****               exsr      clrtbl
     c****               movel     'MADP'        tcode
     c****               movel     hmdadp        ecode
     c****               exsr      srtabl
B4   c****               if        tind = 'E'
     c****               move      *on           *in08
     c****               endif
      **
     c                   if        wsled <> 0
     c                   eval      mdate = wsled
     c                   exsr      srcmdy
     c                   if        ydate = 0
     c                   move      *on           *in26
     c                   else
     c                   eval      hmdled = ydate
     c                   endif
     c                   endif
      **
     c                   if        hmdcp1 <> 0
     c     hmdcp1        chain     hmfmamsz                           10
     c                   endif
     c                   if        hmdcp2 <> 0
     c     hmdcp2        chain     hmfmamsz                           11
     c                   endif
     c                   if        hmdcp3 <> 0
     c     hmdcp3        chain     hmfmamsz                           12
     c                   endif
     c                   if        hmdcp4 <> 0
     c     hmdcp4        chain     hmfmamsz                           13
     c                   endif
     c***                   if        hmdcp5 <> 0
     c***     hmdcp5        chain     hmfmamsz                           14
     c***                   endif
      **
     c                   exsr      clrtbl
     c                   movel     'XYON'        tcode
     c                   movel     hmdmdp        ecode
     c                   exsr      srtabl
B4   c                   if        tind = 'E'
     c                   move      *on           *in16
     c                   endif
      **
     c                   eval      screen = 3
     c                   exsr      error
      **
     c                   endsr
      *****************************************************************
      /EJECT
     c     check1        begsr
      **
     c                   if        hmdnam = *blanks
     c                   move      *on           *in01
     c                   else
                         hmdnam = ReplaceNonBreakCharacters(hmdnam); // Replace with regualr space
     c                   movea     hmdnam        cknm(1)
     c                   exsr      chknam
     c                   move      *in79         *in01
     c                   endif

     c****               if        hmdorp = *blanks or (hmdorp <> *blanks
     c****                           and hmdorp <> 'Y' and hmdorp <> 'N')
     c****               move      *on           *in37
     c****               else
     c****               move      *off          *in37
     c****               endif
      **
     c                   if        *in92 = *off
     c                   if        hmdmsc = 0
     c                   move      *on           *in29
     c                   else
     c                   exsr      clrtbl
     c                   movel     'MSVC'        tcode
     c                   movel     hmdmsc        ecode
     c                   exsr      srtabl
     c                   if        tind = 'E'
     c                   move      *on           *in29
     c                   endif
     c                   movel     ldesc         wsidsc
     c                   endif
      **
     c                   if        hmddyr = 0
     c                   endif
      **
     c                   eval      svjdte = 0
     c                   if        w5dpfd <> 0
     c                   eval      mdate = w5dpfd
     c                   exsr      srcmdy
     c                   if        ydate = 0
     c                   move      *on           *in27
     c                   else
     c                   eval      svjdte = ydate
     c                   eval      hmdpfd = ydate
     c                   endif
     c                   else
     c                   eval      hmdpfd = 0
     c                   endif
      **
     c                   if        w5dptd <> 0
     c                   eval      mdate = w5dptd
     c                   exsr      srcmdy
     c                   if        ydate = 0
     c                   move      *on           *in28
     c                   else
     c                   if        ydate < svjdte
     c                             or svjdte = 0
     c                   move      *on           *in28
     c                   else
     c                   eval      hmdptd = ydate
     c                   endif
     c                   endif
     c                   else
     c                   eval      hmdpfd = 0
     c                   endif
     c                   endif
      **
     c                   if        hmdusr <> *blanks
     c     hmdusr        chain     hxfuser                            79
     c                   if        *in79 = *on
     c                   move      *on           *in22
     c                   else
     c     hmdusr        setll     hmfmamsi                           79
     c                   dou       *in79 = *on
     c     hmdusr        reade     hmfmamsi                               79
     c                   if        *in79 = *off
     c                             and hmddr# <> imddr#
     c                   move      *on           *in22
     c                   endif
     c                   enddo
     c                   endif
     c                   endif
      **
     c****               if        *in92 = *off
     c****               exsr      clrtbl
     c****               movel     'MADP'        tcode
     c****               movel     hmdadp        ecode
     c****               exsr      srtabl
B4   c****               if        tind = 'E'
     c****               move      *on           *in31
     c****               endif
     c****               endif
      **
     c*                  movel     *blanks       wsnpid
     c*                  movel     *blanks       wsnpid3
     c*                  if        wsnpic <> *blanks
     c*    wsnpic        chain     hmpnpi                             79
     c*                  if        *in79
     c*                  eval      *in32 = *on
     c*                  else
     c*                  movel     npname        wsnpid
     c*                  movel     npname        wsnpid3

     c*                  if        hmaoad = *blanks and hmaoa2 = *blanks
     c*                            and hmaoct = *blanks and hmaost = *blanks
     c*                            and hmaoz1 = *blanks and hmaoz2 = *blanks
     c*                            and wswphn = 0 and wswfax = 0
     c*                  movel     nplad1        hmaoad
     c*                  movel     nplad2        hmaoa2
     c*                  movel     nplcty        hmaoct
     c*                  movel     nplst         hmaost
     c*                  movel     npzip1        hmaoz1
     c*                  movel     npzip2        hmaoz2
      /free
      // RemoveNonNumericCharacters(
      //     nplphn :
      //     numericPhoneNumber :
      //     extensionNumber);
      // wswphn = numericPhoneNumber;
      // wsextn = extensionNumber;
      /end-free
     c*                  movel     npifax        wswfax
     c*                  endif

     c*                  endif
     c*                  endif
       //NPI info
       wsnpid = *blanks;
       wsnpid3 = *blanks;
       if wsnpic <> *blanks;
         exsr NPIsr;
         if *in32 = *off;
           wsnpid = npiDS.name;
           wsnpid3 = npiDS.name;
           if hmaoad = *blanks and hmaoa2 = *blanks
              and hmaoct = *blanks and hmaost = *blanks
              and hmaoz1 = *blanks and hmaoz2 = *blanks
              and wswphn = 0 and wswfax = 0;
             hmaoad = npiDS.address;
             hmaoa2 = npiDS.address2;
             hmaoct = npiDS.city;
             hmaost = npiDS.state;
             hmaoz1 = zip1;
             hmaoz2 = zip2;
             RemoveNonNumericCharacters(npiDS.phone :
                                        numericPhoneNumber :
                                        extensionNumber);
             wswphn = numericPhoneNumber;
             wsextn = extensionNumber;
             nplfax = npiDS.fax;
             monitor;
               wswfax = %dec(npifax:10:0);
             on-error;
               wswfax = 0;
             endmon;
           endif;
         endif;
       else;
         exsr checkNpiRequired;
       endif;
      **
     c                   movel     *blanks       lvl6ds
     c                   movel     *blanks       lvl6ds2
     c                   if        lvlnum <> 0
     c     lvlnum        chain     hxplvl6                            79
     c                   if        *in79
     c                   eval      *in33 = *on
     c                   else
     c                   movel     hx6nam        lvl6ds
     c                   movel     hx6nam        lvl6ds2
     c                   endif
     c                   endif
      **
     c                   eval      *in81 = *off
     c***                   if        hmdgp# <> 0
     c                   if        wsdgp# <> 0
     c                   exsr      clrtbl
     c***                   movel     'MGRP'        tcode
     c***                   movel     hmdgp#        ecode
     c                   movel     wsdgp#        ecode
     c                   exsr      srphysgrp
     c                   if        tind = 'E'
     c                   move      *on           *in03
     c                   else
     c                   eval      hmdgpn = ''
     c                   movel     sdesc         hmdgpn
     c                   eval      *in81 = *on                                  =protect field
     c                   endif
     c                   endif
      **
     c                   exsr      clrtbl
     c                   eval      tcode ='XYON'
     c                   eval      ecode = wscosn
     c                   exsr      srtabl
     c                   if        tind = 'E'
     c                   move      *on           *in35
     c                   endif
     c                   if        wscosn = 'Y'                                 = when 'Y' group nee
     c                             and hmdgpn = *blanks                           ds to be entered
     c                   eval      *in15 = *on
     c                   move      *on           *in36
     c                   endif
      **
     c                   if        wsddob <> 0
     c                   eval      mdate = wsddob
     c                   exsr      srcmdy
     c                   if        ydate = 0
     c                   move      *on           *in05
     c                   else
     c                   if        ydate >= today
     c                   move      *on           *in05
     c                   endif
     c                   endif
     c                   if        *in05 = *off
     c                   eval      hmddob = ydate
     c                   endif
     c                   endif
      **
     c                   if        hmdsex <> *blanks
     c                   if        hmdsex <> 'M'
     c                             and hmdsex <> 'F'
     c                   move      *on           *in06
     c                   endif
     c                   endif
      **
     c                   if        *in92 = *on
     c                   eval      wsdbth = *blanks
     c                   if        hmdbth <> *blanks
     c                   exsr      clrtbl
     c                   movel     'BCOU'        tcode
     c                   movel     hmdbth        ecode
     c                   exsr      srtabl
     c                   if        tind = 'E'
     c                   move      *on           *in07
     c                   else
     c                   movel     ldesc         wsdbth
     c                   endif
     c                   endif
     c                   endif
      **
     c                   eval      wsdsv1 = *blanks
     c                   if        hmdsv1 = *blanks
     c                   move      *on           *in08
     c                   else
     c                   exsr      clrtbl
     c                   movel     'MSPC'        tcode
     c                   movel     hmdsv1        ecode
     c                   exsr      srtabl
     c                   if        tind = 'E'
     c                   move      *on           *in08
     c                   else
     c                   movel     ldesc         wsdsv1
     c                   endif
     c                   endif
      **
     c                   eval      wsdsv2 = *blanks
     c                   if        hmdsv2 <> *blanks
     c                   exsr      clrtbl
     c                   movel     'MSPC'        tcode
     c                   movel     hmdsv2        ecode
     c                   exsr      srtabl
     c                   if        tind = 'E'
     c                   move      *on           *in09
     c                   else
     c                   movel     ldesc         wsdsv2
     c                   endif
     c                   endif
      **
     c                   if        *in92 = *on
     c****               if        hmddg1 = *blanks
     c****               move      *on           *in10
     c****               else
     c                   if        hmddg1 <> *blanks
     c                   exsr      clrtbl
     c                   movel     'MDEG'        tcode
     c                   movel     hmddg1        ecode
     c                   exsr      srtabl
     c                   if        tind = 'E'
     c                   move      *on           *in10
     c                   endif
     c                   endif
      **
     c                   else
     c****               if        hmddg1 = *blanks
     c****               move      *on           *in25
     c****               else
     c                   if        hmddg1 <> *blanks
     c                   exsr      clrtbl
     c                   movel     'MDEG'        tcode
     c                   movel     hmddg1        ecode
     c                   exsr      srtabl
     c                   if        tind = 'E'
     c                   move      *on           *in25
     c                   endif
     c                   endif
     c                   if        hmddg2 <> *blanks
     c                   exsr      clrtbl
     c                   movel     'MDEG'        tcode
     c                   movel     hmddg2        ecode
     c                   exsr      srtabl
     c                   if        tind = 'E'
     c                   move      *on           *in26
     c                   endif
     c                   endif
     c                   endif

       if (hmddg1 = *blanks and hmddg2 = *blanks);
         HXXAPPPRF( 'HIM' : 'reqPhysicianDegree' : prefdesc );
         reqPhysicianDegree = (prefdesc = 'Y');
         if (reqPhysicianDegree);
           reqPhysicianDegree = *off;
           if (*in92);
             *in10 = *on;
           else;
             *in25 = *on;
           endif;
         endif;
       endif;
      **
     c                   if        hmaoad = *blanks
     c                             and hmdtyp = 'PH'
     c                   move      *on           *in16
     c                   endif
      **
     c                   if        hmaoad = *blanks
     c                   if        hmaoz1 <> *blanks
     c                             and hmaost <> *blanks
     c                   move      *on           *in16
     c                   endif
     c                   endif
      **
        //Check zip code if available, make it required for type 'PH'
        if hmaoz1 = *blanks and hmdtyp = 'PH';
          *in19 = *on;
        elseif hmaoz1 <> *blanks;
          xfxstzp( hmaost : hmaoz1 : hmaoz2 : *blanks
                 : errStZp : city : county : country : state );
          if errStZp = 2 or errStZp = 3;
            *in19 = *on;
          else;
            if hmaoct = *blanks and city <> *blanks;
              hmaoct = city;
            endif;
            if hmaost = *blanks and state <> *blanks;
              hmaost = state;
            endif;
          endif;
        endif;
     c*                  if        hmaoz1 = *blanks
     c*                            and hmaoz2 = *blanks
     c*                            and hmdtyp = 'PH'
     c*                  move      *on           *in19
     c*                  else
     c*                  if        hmaost <> *blanks
     c*                  movel     hmaost        ckzip
     c*                  movel     hmaoz1        tmp3
     c*                  move      tmp3          ckzip
     c*                  exsr      clrtbl
     c*                  movel     'XZIP'        tcode
     c*                  movel     ckzip         ecode
     c*                  exsr      srtabl
     c*                  if        tind = 'E'
     c*                  move      *on           *in19
     c*                  else
     c*                  if        hmaoct = *blanks
     c*                            or hmaost = *blanks
     c*    hmaoz1        chain     hxfctzp                            79
     c*                  if        *in79 = *off
     c*                  if        hmaoct = *blanks
     c*                  movel     hxcity        hmaoct
     c*                  endif
     c*                  if        hmaost = *blanks
     c*                  movel     hxstat        hmaost
     c*                  endif
     c*                  endif
     c*                  endif
     c*                  endif
     c*                  else
     c*    hmaoz1        chain     hxfctzp                            79
     c*                  if        *in79 = *off
     c*                  if        hmaoct = *blanks
     c*                  movel     hxcity        hmaoct
     c*                  endif
     c*                  if        hmaost = *blanks
     c*                  movel     hxstat        hmaost
     c*                  endif
     c*                  endif
     c*                  endif
     c*                  endif
      **
     c                   if        hmaost <> *blanks
     c                   exsr      clrtbl
     c                   movel     'XSTA'        tcode
     c                   movel     hmaost        ecode
     c                   exsr      srtabl
     c                   if        tind = 'E'
     c                   move      *on           *in18
     c                   endif
     c                   endif
      **
     c                   if        hmaoct = *blanks
     c                             and hmdtyp = 'PH'
     c                   move      *on           *in17
     c                   endif
      **
      ******             if        hmaotl = 0
     c                   if        wswphn = 0
     c                   if        hmaost <> *blanks
     c                   move      *on           *in20
     c                   endif
     c                   endif
      **
     c***                   exsr      clrtbl
     c***                   movel     'XYON'        tcode
     c***                   movel     addoad        ecode
     c***                   exsr      srtabl
B4   c***                   if        tind = 'E'
     c***                   move      *on           *in21
     c***                   endif
     c                   if        shw2nd <> *blanks
     c                             and shw2nd <> 'X'
     c                   move      *on           *in21
     c                   endif
      **
     c                   if        hmdhs1 <> *blanks
     c                   exsr      clrtbl
     c                   movel     'XSTA'        tcode
     c                   movel     hmdhs1        ecode
     c                   exsr      srtabl
     c                   if        tind = 'E'
     c                   move      *on           *in48
     c                   endif
     c                   endif
      **
     c                   if        not *in92
     c                   eval      lv5nam = *blanks
     c     hmdlv5        chain     hxflvl5                            89
B2   c                   if        *in89 = *on and hmdlv5 <> 0
     c                   move      *on           *in11
X2   c                   else
     c                   if        *in89 = *off
     c                   movel     hx5nam        lv5nam
E2   c                   endif
E2   c                   endif
E2   c                   endif
      **
      /free
       // Supervising Clinician - cannot be self, cannot be another NP, cannot be deleted //
       *in40 = *off;
       if hmdcp5 <> 0;
          *in40 = (hmdcp5 = hmddr#);
          if not *in40;
             goodPhys = *off;
             exec sql select '1' into :goodPhys from HMPMAMS
                       where hmddr# = :hmdcp5 and hmdcp5= 0 and
                       HMDDLD = 0;
             *in40 = (not goodPhys);
          endif;
       endif;
      /end-free

       //check zip code if available
       if hmdhz1 <> *blanks;
         xfxstzp( hmdhs1 : hmdhz1 : hmdhe2 : *blanks
                : errStZp : city : county : country : state );
         if errStZp = 2 or errStZp = 3;
           *in49 = *on;
         else;
           if hmdhc1 = *blanks and city <> *blanks;
             hmdhc1 = city;
           endif;
           if state <> *blanks;
             hmdhs1 = state;
           endif;
         endif;
       endif;

     c*                  if        hmdhz1 <> *blanks
     c*                  if        hmdhs1 <> *blanks
     c*                  movel     hmdhs1        ckzip
     c*                  movel     hmdhz1        tmp3
     c*                  move      tmp3          ckzip
     c*                  exsr      clrtbl
     c*                  movel     'XZIP'        tcode
     c*                  movel     ckzip         ecode
     c*                  exsr      srtabl
     c*                  if        tind = 'E'
     c*                  move      *on           *in49
     c*                  else
     c*                  if        hmdhc1 = *blanks
     c*    hmdhz1        chain     hxfctzp                            79
     c*                  if        *in79 = *off
     c*                  if        hxcity <> *blanks
     c*                  movel     hxcity        hmdhc1
     c*                  endif
     c*                  if        hxstat <> *blanks
     c*                  movel     hxstat        hmdhs1
     c*                  endif
     c*                  endif
     c*                  endif
     c*                  endif
     c*                  else
     c*    hmdhz1        chain     hxfctzp                            79
     c*                  if        *in79 = *off
     c*                  if        hxcity <> *blanks
     c*                  movel     hxcity        hmdhc1
     c*                  endif
     c*                  if        hxstat <> *blanks
     c*                  movel     hxstat        hmdhs1
     c*                  endif
     c*                  endif
     c*                  endif
     c*                  endif
      **
     c                   eval      hmdhob = *blanks
     c                   movel     wsdhob        hmdhob
      **
     c                   movel(p)  wsdcm1        hmdcm1
      **
     c                   movel(p)  wsdcm2        hmdcm2
      **
     c                   eval      screen = 1
     c                   exsr      error
      **
     c                   endsr
      *****************************************************************
      /EJECT
     c     inzscn        begsr
      **
     c                   move      1             adrseq
     c                   move      *on           *in84
      **
     c                   eval      hmdhe2 = *blanks

     c                   select
     c                   when      rc = 1
     c                   clear                   hmfmams
     c                   clear                   hmfphyad
     c                   movea     '00'          *in(85)
     c                   move      *off          *in78
     c                   eval      hmddr# = 0
     c                   movel     hldtyp        hmdtyp
     c****               if        hmdtyp = 'PH'
     c****               eval      hmdorp = 'Y'
     c****               else
     c****               eval      hmdorp = 'N'
     c****               endif
      **
     c                   when      rc = 2
     c                   movea     '10'          *in(85)
     c     hmddr#        chain     hmfmams                            7978
     c     adrkey        chain     hmfphyad                           87
      **
     c                   when      rc = 3
     c                   movea     '11'          *in(85)
     c     hmddr#        chain(n)  hmfmams                            7978
     c     adrkey        chain(n)  hmfphyad                           87
      **
     c                   other
     c                   movea     '11'          *in(85)
     c     hmddr#        chain     hmfmams                            7978
     c     adrkey        chain     hmfphyad                           87
     c                   endsl
      **
     c                   eval      hmdhe2 = hmdhe1

     c                   if        *in78 = *on
     c                   exsr      lckrec
     c                   endif
      **
     c                   if        rc = 1
     c                   if        *in92 = *off
     c                   endif
     c                   endif
      **
     c                   eval      ydate = hmddob
     c                   exsr      srcymd
     c                   eval      wsddob = mdate
      **
     c                   exsr      clrtbl
     c                   movel     'MSVC'        tcode
     c                   movel     hmdmsc        ecode
     c                   exsr      srtabl
     c                   movel     ldesc         wsidsc
      **
     c                   eval      *in81 = *off
     c***                   if        hmdgp# <> 0
     c                   if        wsdgp# <> 0
     c                   exsr      clrtbl
     c***                   movel     'MGRP'        tcode
     c***                   movel     hmdgp#        ecode
     c                   movel     wsdgp#        ecode
     c                   exsr      srphysgrp
     c                   eval      hmdgpn = ''
     c                   movel     sdesc         hmdgpn
     c                   eval      *in81 = *on                                  =protect field
     c                   endif
      **
     c                   if        reqopt = 'IN' or reqopt = 'CH'
     c                   eval      wscosn = %subst(hmdfut:6:1)
     c                   else
     c                   eval      wscosn = 'N'
     c                   endif
      **
     c                   eval      wlgs1 = *blanks
     c                   exsr      clrtbl
     c                   movel     'MLNG'        tcode
     c                   movel     hmdlg1        ecode
     c                   exsr      srtabl
     c                   if        tind <> 'E'
     c                   movel     ldesc         wlgs1
     c                   endif
      **
     c                   eval      wlgs2 = *blanks
     c                   exsr      clrtbl
     c                   movel     'MLNG'        tcode
     c                   movel     hmdlg2        ecode
     c                   exsr      srtabl
     c                   if        tind <> 'E'
     c                   movel     ldesc         wlgs2
     c                   endif
      **
     c                   eval      wlgs3 = *blanks
     c                   exsr      clrtbl
     c                   movel     'MLNG'        tcode
     c                   movel     hmdlg3        ecode
     c                   exsr      srtabl
     c                   if        tind <> 'E'
     c                   movel     ldesc         wlgs3
     c                   endif
      **
     c                   eval      ydate = hmddoh
     c                   exsr      srcymd
     c                   eval      wsmdoh = mdate
      **
     c                   eval      ydate = hmdcex
     c                   exsr      srcymd
     c                   eval      wscex = mdate
      **
     c                   eval      cgp1 = hmdcgp
      **
     c                   eval      ydate = hmdbqy
     c                   exsr      srcymd
     c                   eval      wsdbqy = mdate
      **
     c                   eval      ydate = hmdbid
     c                   exsr      srcymd
     c                   eval      wsdbid = mdate
      **
     c                   eval      ydate = hmdlid
     c                   exsr      srcymd
     c                   eval      wslid = mdate
      **
     c                   eval      ydate = hmdled
     c                   exsr      srcymd
     c                   eval      wsled = mdate
      **
     c                   eval      ydate = hmdpfd
     c                   exsr      srcymd
     c                   eval      w5dpfd = mdate
      **
     c                   eval      ydate = hmdptd
     c                   exsr      srcymd
     c                   eval      w5dptd = mdate
      **
     c                   movel     hmdlp1        wslpm1
     c                   movel     hmdlp2        wslpm2
     c                   movel     hmdlp3        wslpm3
      **
     c                   if        rc <> 1
      **
     c                   eval      wsdbth = *blanks
     c                   exsr      clrtbl
     c                   movel     'BCOU'        tcode
     c                   movel     hmdbth        ecode
     c                   exsr      srtabl
     c                   if        tind <> 'E'
     c                   movel     ldesc         wsdbth
     c                   endif
     c                   endif
      **
     c                   eval      wsdsv1 = *blanks
     c                   exsr      clrtbl
     c                   movel     'MSPC'        tcode
     c                   movel     hmdsv1        ecode
     c                   exsr      srtabl
     c                   movel     ldesc         wsdsv1
      **
     c                   eval      wsdsv2 = *blanks
     c                   exsr      clrtbl
     c                   movel     'MSPC'        tcode
     c                   movel     hmdsv2        ecode
     c                   exsr      srtabl
     c                   movel     ldesc         wsdsv2
      **
     c                   movel     hmdhob        wsdhob
     c                   movel     hmdcm1        wsdcm1
     c                   movel     hmdcm2        wsdcm2
      **
      *****  REFERRAL STATS
      **
     c                   if        *in92 = *on
      **
     c                   eval      year = tyyyy
     c                   eval      pryear = year - 1
     c     rpkey         chain     hmfphyrf                           89
1    c                   if        *in89 = *on
     c                   eval      mtdref = 0
     c                   eval      hmrydr = 0
     c                   eval      hmrlrr = 0
     c                   eval      wsrlrd = 0
     c                   eval      year = 9999
     c     rpkey         setgt     hmfphyrf
     c     hmddr#        readpe    hmfphyrf                               89
2    c                   if        *in89 = *off
     c                   eval      ydate = hmrlrd
     c                   exsr      srcymd
     c                   eval      wsrlrd = mdate
3    c                   if        hmryer = pryear
     c                   xfoot     mdr           hmrlrr
E3   c                   endif
E2   c                   endif
X1   c                   else
     c                   eval      ydate = hmrlrd
     c                   exsr      srcymd
     c                   eval      wsrlrd = mdate
      **
     c                   xfoot     mdr           hmrydr
      **
     c                   eval      n = tmm
     c                   eval      mtdref = mdr(n)
      **
     c                   eval      year = pryear
     c     rpkey         chain     hmfphyrf                           89
2    c                   if        *in89 = *off
     c                   xfoot     mdr           hmrlrr
X2   c                   else
     c                   eval      hmrlrr = 0
E2   c                   endif
E1   c                   endif
     c                   endif
      **
      *****  OFFICE ADDRESSES
      **
     c                   movea     '10'          *in(95)
     c                   write     hmfphyac
     c                   movea     '00'          *in(95)
     c                   eval      recno6 = 1
     c                   eval      hmaosq = 1
     c                   move      *on           *in65
      **
     c****                   move      'N'           addoad
     c                   move      ' '           shw2nd
     c                   eval      adrseq = 2
     c     adrkey        setll     hmfphyad
     c     hmddr#        reade     hmfphyad                               79
     c                   dow       *in79 <> *on
     c                   eval      wswphn = 0
     c                   eval      wsextn = *blanks
     c                   eval      wswfax = 0
     c                   exsr      oophld
     c                   write     hmfphyas
     c                   eval      recno6 = recno6 + 1
     c                   move      '1'           on2
     c****                   move      'Y'           addoad
     c                   move      'X'           shw2nd
      **
     c     hmddr#        reade     hmfphyad                               79
     c                   if        recno6 >= 21
     c                   move      *on           *in79
     c                   endif
     c                   enddo
      **
     c                   if        recno6 < 20
     c                   eval      nxtseq = hmaosq + 1
     c                   eval      begin = recno6
     c     begin         do        20            recno6
     c                   eval      hmaoad = *blanks
     c                   eval      hmaoa2 = *blanks
     c                   eval      hmaoct = *blanks
     c                   eval      hmaost = *blanks
     c                   eval      hmaoz1 = *blanks
     c                   eval      hmaoz2 = *blanks
      ****               eval      hmaotl = 0
      ****               eval      hmaofx = 0
     c                   eval      wswphn = 0
     c                   eval      wsextn = *blanks
     c                   eval      wswfax = 0
     c                   eval      hmaosq = nxtseq
     c                   eval      nxtseq = nxtseq + 1
     c                   write     hmfphyas
     c                   enddo
     c                   endif
      **
     c                   eval      adrseq = 1
     c                   move      'N'           adrchn
     c     adrkey        chain     hmfphyad                           79
     c                   if        *in79 = *off
     c                   move      'Y'           adrchn
     c                   else
     c                   eval      hmaoz1 = *blanks
     c                   eval      hmaoz2 = *blanks
     c                   eval      hmaotl = 0
     c                   eval      hmaofx = 0
     c                   endif
      **
     c                   eval      lv5nam = *blanks
     c     hmdlv5        chain     hxflvl5                            79
     c                   if        *in79 = *off
     c                   movel     hx5nam        lv5nam
     c                   endif
      **
     c                   movel     level5        string
     c     high:lo       xlate     string        result
     c                   movel     lev6(1)       result
      **
     c***                   eval      homel5 = *blanks
     c***                   movel     result        homel5
     c***                   cat       ':':0         homel5
      **
     c                   eval      wsnpic = *blanks
     c                   eval      wsnpid  = *blanks
     c                   eval      wsnpid3 = *blanks
     C                   eval      npicod = 'NPI'
     C     npikey        chain(n)  hmflice                            79
     C                   if        *in79 = *off
     C                   movel     hlcnum        wsnpic
     C*    wsnpic        chain     hmfnpi
     C*                  movel     npname        wsnpid
     C*                  movel     npname        wsnpid3
     c                   exsr      NPIsr
     c                   eval      wsnpid = npiDS.name
     c                   eval      wsnpid3 = npiDS.name
     C                   endif
      **
     c                   eval      lvlnum = 0
     c                   eval      lvl6ds  = *blanks
     c                   eval      lvl6ds2 = *blanks
     c     hmddr#        setll     hmfthrcp
     c     hmddr#        reade(n)  hmfthrcp                               79
     c                   if        *in79 = *off
     c                             and hmtlv6 <> 0
     c                   eval      lvlnum = hmtlv6
     c     lvlnum        chain     hxplvl6                            79
     c                   movel     hx6nam        lvl6ds
     c                   movel     hx6nam        lvl6ds2
     c                   endif
     c
     c                   endsr
      *****************************************************************
      /EJECT
     c     drpln         begsr
      **         =====     =====
     c                   move      'N'           dblchk
     c                   eval      pln = 0
     c                   eval      pyr = 0
     c                   eval      lcn = 0
     c                   eval      tmpr = 0
     c                   eval      tmpn = 0
     c                   eval      tmpl = 0
     c                   eval      k = 0
      **
     c                   if        *in72 = *off
     c                   movel     nao(1)        dspmsg
     c                   move      *on           *in04
     c                   goto      dsplst
     c                   endif
      **
     c                   if        reqlno = 0
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c     reqlno        chain     hmfdrupr                           89
     c                   if        *in89 = *on
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c                   if        fstlet = '*'
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   endif
     c                   endif
     c                   endif
      **
     c                   move      *off          *in83
     c                   exsr      clrthr
     c                   exsr      bldthr
     c                   move      'N'           chg
     c                   eval      reqopt = *blanks
      **
     c     dsppv#        tag
      **         ======    ===
     c                   eval      relrcd = 0
     c                   eval      csrrc# = 0
     c**                 write     hxffkey
     c**
     c                   eval      centerdsc = 'STAFF MAINTENANCE'
     c                   call      'XFXCNTG'
     c                   parm                    centerdsc
     c                   parm                    centerlen
     c                   parm                    centerfil
     c                   eval      wshdr = centerdsc
      **
     c                   exfmt     hmfthrpc
      **
     c                   eval      reqopt = *blanks
      **
     c                   move      ' '           once2
     c                   if        once2 = ' '
     c                   move      'Y'           once2
     c                   move      'Y'           errid
     c                   endif
      **
     c                   if        *inka = *on
     c                   if        csrfld = 'HMDTYP    '
     c                   if        *in85 = *on or *in86 = *on
     c                             or *in64 = *on
     c                   goto      dsppv#
     c                   endif
     c                   endif
     c                   exsr      chklin
     c                   goto      dsppv#
     c                   endif
      **
     c                   if        *inkb = *on
     c                   movea     *in(01)       indice
     c                   call      'XFCFKEY'
     c                   parm                    dspfnm
     c                   parm                    rcdfmt
     c                   parm                    indice
     c                   parm                    xx
     c     xx            cabeq     '  '          dsppv#
     c                   exsr      srfkey
     c                   endif
      **
     c                   if        *inkc = *on
     c                   movea     '00'          *in(01)
     c                   goto      dsplst
     c                   endif
      **
     c                   if        *in98 = *on
     c                   call      'HXWHLP '
     c                   parm                    dspfnm
     c                   parm                    rcdfmt
     c                   goto      dsppv#
     c                   endif
      **
     c                   if        *inkl = *on
     c                   movea     '00'          *in(01)
     c                   goto      dsplst
     c                   endif
      **
     c                   if        *in90 = *on
     c                   move      'Y'           chg
     c                   endif
      **
     c                   exsr      chkpv#
     c     dblchk        cabeq     'Y'           moveon
      **                                                    + VERIFIED
     c                   if        errid = 'N'
     c                   move      'Y'           dblchk
     c                   movel     nef(2)        dspms2
     c                   eval      sfkey = relrcd
      **
     c     sfkey         chain     hmfthrps                           59
     c                   if        *in59 = *off
     c                   if        wsdlcn <> 0
     c     wsdlcn        chain     hxflvl6                            79
     c                   if        *in79 = *off
     c                   movel     hx6nam        wsddsc
     c                   endif
     c                   update    hmfthrps
     c                   move      'Y'           chg
     c                   eval      tmprc# = tmprc# + 1
     c                   else
     c     sfkey         chain     hmfthrps                           59
     c                   if        *in59 = *off
     c                   eval      wsddsc = *blanks
     c                   move      'Y'           chg
     c                   eval      tmprc# = tmprc# + 1
     c                   endif
     c                   endif
     c                   endif
     c                   move      *on           *in83
     c                   endif
      **
     c                   goto      dsppv#
      **
     c     moveon        tag
      **         ======    ===
     c                   if        chg = 'Y'
      **
     c                   eval      hmtth# = hmddr#
     c                   eval      wsdlcn = tmplcn
     c     thrkey        setll     hmfplnpr
     c     thrkey        reade     hmfplnpr                               89
     c                   dow       *in89 <> *on
     c                   delete    hmfplnpr
     c     thrkey        reade     hmfplnpr                               89
     c                   enddo
      **
     c     1             do        k             c
     c                   eval      oldpyr = pyr(c)
     c                   eval      oldpln = pln(c)
     c                   eval      oldlcn = lcn(c)
     c     allkey        setll     hmfplnpa
     c                   move      *off          *in51
     c                   dou       *in51 = *on
     c     oldkey        reade     hmfplnpa                               51
     c                   if        *in51 = *off
     c                   delete    hmfplnpa
     c                   endif
     c                   enddo
     c                   enddo
      **
     c     1             do        tmprc#        recno2
     c     recno2        chain     hmfthrps                           89
     c                   if        *in89 = *off
     c                             and hmtpyr > 0
     c                   if        wsdlcn = 0
     c     ppkey         chain     hmfplnpp                           44
     c                   eval      hmtlcn = wsdlcn
     c                   if        *in44 = *off
     c                   update    hmfplnpp
     c                   else
     c                   write     hmfplnpr
     c                   endif
     c                   else
     c                   eval      hmtlcn = wsdlcn
     c     thrkey        chain     hmfplnpr                           71
     c                   if        *in71 = *off
     c                   update    hmfplnpr
     c                   else
     c                   write     hmfplnpr
     c                   endif
     c                   endif
     c                   endif
     c                   enddo
      **
     c                   endif
      **
     c                   endsr
      *****************************************************************
      /EJECT
     c     chkpv#        begsr
      **
     c                   movea     '000'         *in(01)
     c                   move      *off          *in14
     c                   move      *off          *in83
     c                   move      'N'           errid
      **
     c                   move      *off          *in79
1D   c                   dou       *in79 = *on
     c                   readc     hmfthrps                               79
3    c                   if        *in79 = *off
     c                   move      'N'           dblchk
     c                   movea     '000'         *in(01)
     c                   movea     '00'          *in(62)
     c                   move      *off          *in14
4    c                   if        hmtpyr <> 0
     c     bnkey         chain     xffbnfit                           69
5    c                   if        *in69 = *on
     c                   movea     '11'          *in(01)
     c                   move      'Y'           errid
     c                   movel     ipp(1)        dspms2
     c                   move      *on           *in83
     c                   move      *on           *in63
X5   c                   else
8    c                   if        hmtpr# = *blanks
     c                   move      *on           *in03
     c                   move      'Y'           errid
     c                   movel     mpr(1)        dspms2
     c                   move      *on           *in83
     c                   move      *on           *in63
E8   c                   endif
9    c                   if        wsdlcn <> 0
     c     wsdlcn        chain     hxflvl6                            74
10   c                   if        *in74 = *on
     c                   move      *on           *in14
     c                   move      'Y'           errid
     c                   movel     ilc(1)        dspms2
     c                   move      *on           *in83
     c                   move      *on           *in63
X10  c                   else
     c                   movel     hx6nam        wsddsc
     c                   eval      tmplcn = wsdlcn
E10  c                   endif
E9   c                   endif
E5   c                   endif
E4   c                   endif
     c                   update    hmfthrps
      *** BLOCK OF CODE MOVED OUTSIDE OF MAIN DO LOOP 6/19/01
     c                   move      'Y'           chg
     c                   if        recno2 > tmprc#
     c                   eval      tmprc# = recno2
     c                   endif
E3   c                   endif
ED1  c                   enddo
      **
     c                   eval      j = 1
12D  c     1             do        t             w
13   c                   if        j <= t
     c                             or flg <> 'Y'
     c     j             chain     hmfthrps                           71
14   c                   if        *in71 = *off
15   c                   if        tmpr(w) = hmtpyr
     c                             and tmpn(w) = hmtpln
     c                             and tmpl(w) = wsdlcn
     c                   move      'N'           flg
X15  c                   else
     c                   move      'Y'           flg
     c                   eval      j = t + 1
E15  c                   endif
E14  c                   endif
E13  c                   endif
16   c                   if        flg = 'Y'
     c                   eval      k = k + 1
     c                   eval      pyr(k) = tmpr(w)
     c                   eval      pln(k) = tmpn(w)
     c                   eval      lcn(k) = tmpl(w)
E16  c                   endif
     c                   eval      j = j + 1
ED12 c                   enddo
      **
     c                   endsr
      *****************************************************************
      /SPACE 3
     c     clrerr        begsr
     c                   movea     '10'          *in(95)
     c                   write     hmfdruec
     c                   movea     '00'          *in(95)
     c                   movea     zrofld        *in(01)
     c                   move      'N'           errid
     c                   endsr
      *****************************************************************
      /EJECT
     c     clrthr        begsr
     c                   movea     '10'          *in(95)
     c                   write     hmfthrpc
     c                   movea     '00'          *in(95)
     c                   movea     zrofld        *in(01)
     c                   eval      recno2 = 0
     c                   eval      srecno = 1
     c                   endsr
      *****************************************************************
      /EJECT
     c     clraddg       begsr
     c                   eval      recno7 = 0
     c                   movea     '10'          *in(95)
     c                   write     hmfaddgc
     c                   movea     '000'         *in(94)
     c                   movea     zrofld        *in(01)
     c                   eval      *in12 = *off
     c                   eval      *in23 = *off
     c                   eval      *in37 = *off
     c                   eval      savgp# = 0
     c                   eval      dspmg2 = *blanks
     c                   eval      reqgrp = 0
     c                   endsr
      *****************************************************************
      /SPACE 3
     c**   clrdr         begsr
     c**                 movea     '10'          *in(95)
     c**                 write     hmfphypc
     c**                 movea     '00'          *in(95)
     c**                 movea     zrofld        *in(01)
     c**                 eval      recno3 = 0
     c**                 eval      srecno = 1
     c**                 endsr
      *****************************************************************
      /EJECT
     c     bldthr        begsr
      **         ======    =====
     c                   eval      t = 0
     c     hmddr#        setll     hmfplnpr
     c                   dou       *in89 = *on
     c     hmddr#        reade     hmfplnpr                               89
     c                   if        *in89 = *off
     c                   move      *on           *in96
     c                   eval      recno2 = recno2 + 1
     c                   eval      t = t + 1
     c                   eval      wsdlcn = hmtlcn
     c     wsdlcn        chain     hxflvl6                            79
     c                   if        *in79 = *off
     c                   movel     hx6nam        wsddsc
     c                   else
     c                   eval      wsddsc = *blanks
     c                   endif
     c                   eval      tmpr(t) = hmtpyr
     c                   eval      tmpn(t) = hmtpln
     c                   eval      tmpl(t) = hmtlcn
     c                   write     hmfthrps
     c                   endif
     c                   enddo
      **
     c                   if        *in96 = *off
     c                   if        dspms2 = *blanks
     c                   movel     ntf(1)        dspms2
     c                   move      *on           *in83
     c                   endif
     c                   endif
      **
     c                   movea     '11'          *in(62)
     c                   eval      tmprc# = recno2
     c                   eval      strnum = recno2 + 1
     c     strnum        do        50            recno2
     c                   eval      hmtpyr = 0
     c                   eval      wsdlcn = 0
     c                   eval      hmtpln = 0
     c                   eval      hmtpr# = *blanks
     c**                 eval      hmtdes = *blanks
     c                   eval      wsddsc = *blanks
     c                   move      *on           *in96
     c                   write     hmfthrps
     c                   enddo
      **
     c                   move      *off          *in94
      **
     c                   endsr
      *****************************************************************
      /EJECT
     c**   blddr         begsr
      **         =====     =====
     c**   hmddr#        setll     hmfplnpr
     c**   hmddr#        setll     hmfplnpr
     c**                 dou       *in89 = *on
     c**   hmddr#        reade     hmfplnpr                               89
     c**                 if        *in89 = *off
     c**   bnkey         chain     xffbnfit                           79
     c**                 if        *in79 = *off
     c**                 movel     xfbnam        hmtdes
     c**                 else
     c**                 eval      hmtdes = *blanks
     c**                 endif
     c**                 move      *on           *in96
     c**                 eval      recno3 = recno3 + 1
     c**                 write     hmfphyps
     c**                 endif
     c**                 enddo
      **
     c**                 if        *in96 = *off
     c**                 if        dspms2 = *blanks
     c**                 if        hmdtyp = 'RF'
      ***        HMDTYP    OREQ 'AT'
     c**                           or hmdtyp = 'PH'
     c**                 movel     nnf(1)        dspms2
     c**                 else
     c**                 movel     ntf(1)        dspms2
     c**                 endif
     c**                 move      *on           *in83
     c**                 endif
     c**                 endif
      **
     c**                 movea     '11'          *in(62)
     c**                 eval      strnum = recno3 + 1
     c**   strnum        do        50            recno3
     c**                 eval      hmtpyr = 0
     c**                 eval      hmtpln = 0
     c**                 eval      hmtpr# = *blanks
     c**                 eval      hmtdes = *blanks
     c**                 move      *on           *in96
     c**                 write     hmfphyps
     c**                 enddo
      **
     c**                 move      *off          *in94
      **
     c**                 endsr
      *****************************************************************
      /EJECT
     c     error         begsr
     c                   eval      errnum = 0
      **
     c                   eval      ems = *blanks
     c                   if        screen = 1
     c                   movea     ems1          ems
     c                   else
     c                   if        screen = 2
     c                   movea     ems2          ems
     c                   else
     c                   if        screen = 3
     c                   movea     ems3          ems
     c                   endif
     c                   endif
     c                   endif
      **
     c     1             do        59            x
     c                   if        *in(x) = *on
     c                   exsr      setmsg
     c                   eval      errnum = errnum + 1
     c                   move      *on           *in96
     c                   write     hmfdruer
     c                   endif
     c                   enddo
      **
     c                   if        *in96 = *on
     c                   movel     sms(1)        dspmsg
     c                   move      'Y'           errid
     c                   else
     c                   movel     sms(2)        dspmsg
     c                   move      ' '           errid
     c                   endif
      **
     c                   endsr
      *****************************************************************
      /EJECT
     c     clrsfl        begsr
     c                   movea     '10'          *in(95)
     c                   write     hmfdrupc
     c                   movea     '000'         *in(94)
     c                   eval      srecno = 1
     c                   eval      reqlno = 0
     c                   eval      d = 14
     c                   eval      g = 1
     c                   endsr
      *****************************************************************
      /SPACE 3
     c     bldsfl        begsr
      **
     c                   eval      wsdsvs = *blanks
     c                   movel     hmdsv1        wsdsvs
     c                   eval      wsdnam = *blanks
     c                   movel     hmdnam        wsdnam
     c                   eval      wsprl6 = 0
     c                   eval      wsidno = 0
      **
     c                   if        hmdtyp = 'RF'
     c                             or hmdtyp = 'PH'
     c                   eval      adrseq = 1
     c                   eval      wsaddr = *blanks
     c                   eval      wscity = *blanks
     c     adrkey        chain(n)  hmfphyad                           79
     c                   if        *in79 = *off
     c                   movel(p)  hmaoad        wsaddr
     c                   movel(p)  hmaoct        wscity
     c                   eval      wsidno = hmadr#
     c                   else
     c                   movel(p)  hmdha1        wsaddr
     c                   eval      wsidno = hmddr#
     c                   movel(p)  hmdhc1        wscity
     c                   endif
      **
     c                   else
     c                   movel(p)  hmdha1        wsaddr
     c                   eval      wsidno = hmddr#
     c                   movel(p)  hmdhc1        wscity
     c                   endif
     c     hmddr#        setll     hmfthrcp
     c     hmddr#        reade(n)  hmfthrcp                               79
     c                   if        *in79 = *off
     c                   eval      wsprl6 = hmtlv6
     c                   endif
      **
     c                   move      *on           *in96
     c                   write     hmfdrupr
      **
     c                   endsr
      *****************************************************************
      /EJECT
     c     chknam        begsr
     c     cknm(1)       cablt     'A'           namerr
     c     cknm(1)       cabgt     'Z'           namerr
     c                   move      cknm(1)       lstchr
     c                   eval      x = 2
     c                   dow       cknm(x) <> ','
     c                   if        cknm(x) = ' '
     c     lstchr        cabeq     ' '           namerr
     c                   endif
     c                   move      cknm(x)       lstchr
     c                   eval      x = x + 1
     c     x             cabgt     24            namerr
     c                   enddo
     c     lstchr        cabeq     ' '           namerr
     c                   eval      x = x + 1
     c     cknm(x)       cabne     ' '           namerr
     c                   eval      x = x + 1
     c     cknm(x)       cabeq     ' '           namerr
     c     x             do        26            x
     c     cknm(x)       cabeq     ','           namerr
     c                   enddo
     c                   move      *off          *in79
     c                   goto      endchk
     c     namerr        tag
     c                   move      *on           *in79
     c     endchk        tag
     c                   endsr
      *****************************************************************
      /SPACE 3
     c     unlock        begsr
     c                   eval      hmddr# = 0
     c     hmddr#        chain     hmfmams                            89
     c                   if        *in89 = *off
     c                   except    unlkms
     c                   endif
     c                   endsr
      *****************************************************************
      /EJECT
     c     srauth        begsr
      **
     c                   move      *all'1'       inopt
     c                   movea     inopt         *in(66)
     c                   movea     '11'          *in(60)
     c                   move      '1'           inps
     c                   move      '1'           inpu
     c                   move      '1'           inpp
     c                   move      '1'           inpa
     c                   move      '1'           inta
     c                   move      '1'           insp
     c                   move      '1'           indd
     c                   move      '1'           inbc
     c                   move      '1'           inir
     c                   move      '1'           inli
     c                   move      '1'           inzi
     c                   move      '1'           inzs
     c                   move      '1'           inzl
      **
     c                   move      'M'           system
     c                   eval      incercd = *on
     c                   call      'XFCNHKAT'
     c                   parm      3593          authnum
     c                   parm                    rtrncd
     c                   if        rtrncd = 'E'
     c                   eval      incercd = *off
     c                   endif

     c                   move      'ZA'          rqstop
     c                   call      'XFCCHKAT'
     c                   parm                    system
     c                   parm                    rqstop
     c                   parm                    rtrncd
     c                   if        rtrncd = 'E'
     c                   move      *off          *in67
     c                   endif
      **
     c                   move      'ZD'          rqstop
     c                   call      'XFCCHKAT'
     c                   parm                    system
     c                   parm                    rqstop
     c                   parm                    rtrncd
     c                   if        rtrncd = 'E'
     c                   move      *off          *in68
     c                   endif
      **
     c                   move      'ZI'          rqstop
     c                   call      'XFCCHKAT'
     c                   parm                    system
     c                   parm                    rqstop
     c                   parm                    rtrncd
     c                   if        rtrncd = 'E'
     c                   move      '0'           inzi
     c                   endif
      **
     c                   move      'ZC'          rqstop
     c                   call      'XFCCHKAT'
     c                   parm                    system
     c                   parm                    rqstop
     c                   parm                    rtrncd
     c                   if        rtrncd = 'E'
     c                   move      *off          *in70
     c                   endif
      **
     c                   move      'ZS'          rqstop
     c                   call      'XFCCHKAT'
     c                   parm                    system
     c                   parm                    rqstop
     c                   parm                    rtrncd
     c                   if        rtrncd = 'E'
     c                   move      '0'           inzs
     c                   endif
      **
     c                   move      'ZP'          rqstop
     c                   call      'XFCCHKAT'
     c                   parm                    system
     c                   parm                    rqstop
     c                   parm                    rtrncd
     c                   if        rtrncd = 'E'
     c                   move      *off          *in72
     c                   endif
      **
     c                   move      'ZH'          rqstop
     c                   call      'XFCCHKAT'
     c                   parm                    system
     c                   parm                    rqstop
     c                   parm                    rtrncd
     c                   if        rtrncd = 'E'
     c                   move      *off          *in60
     c                   endif
      **
     c                   move      'ZR'          rqstop
     c                   call      'XFCCHKAT'
     c                   parm                    system
     c                   parm                    rqstop
     c                   parm                    rtrncd
     c                   if        rtrncd = 'E'
     c                   move      *off          *in73
     c                   endif
      **
     c                   move      'ZL'          rqstop
     c                   call      'XFCCHKAT'
     c                   parm                    system
     c                   parm                    rqstop
     c                   parm                    rtrncd
     c                   if        rtrncd = 'E'
     c                   move      '0'           inzl
     c                   endif
      **
     c                   move      'ZO'          rqstop
     c                   call      'XFCCHKAT'
     c                   parm                    system
     c                   parm                    rqstop
     c                   parm                    rtrncd
     c                   if        rtrncd = 'E'
     c                   move      *off          *in75
     c                   endif
      **
     c                   move      'ZE'          rqstop
     c                   call      'XFCCHKAT'
     c                   parm                    system
     c                   parm                    rqstop
     c                   parm                    rtrncd
     c                   if        rtrncd = 'E'
     c                   move      *off          *in76
     c                   endif
      **
     c                   move      'ZZ'          rqstop
     c                   call      'XFCCHKAT'
     c                   parm                    system
     c                   parm                    rqstop
     c                   parm                    rtrncd
     c                   if        rtrncd = 'E'
     c                   move      *off          *in61
     c                   move      *off          *in66
     c                   endif
      **
     c                   move      'ZB'          rqstop
     c                   call      'XFCCHKAT'
     c                   parm                    system
     c                   parm                    rqstop
     c                   parm                    rtrncd
     c                   if        rtrncd = 'E'
     c                   move      '0'           inbc
     c                   endif
      **
     c                   move      'ZG'          rqstop
     c                   call      'XFCCHKAT'
     c                   parm                    system
     c                   parm                    rqstop
     c                   parm                    rtrncd
     c                   if        rtrncd = 'E'
     c                   move      '0'           insp
     c                   endif
      **
     c                   move      'ZJ'          rqstop
     c                   call      'XFCCHKAT'
     c                   parm                    system
     c                   parm                    rqstop
     c                   parm                    rtrncd
     c                   if        rtrncd = 'E'
     c                   move      '0'           inpu
     c                   endif
      **
     c                   move      'ZK'          rqstop
     c                   call      'XFCCHKAT'
     c                   parm                    system
     c                   parm                    rqstop
     c                   parm                    rtrncd
     c                   if        rtrncd = 'E'
     c                   move      '0'           inps
     c                   endif
      **
     c                   move      'ZM'          rqstop
     c                   call      'XFCCHKAT'
     c                   parm                    system
     c                   parm                    rqstop
     c                   parm                    rtrncd
     c                   if        rtrncd = 'E'
     c                   move      '0'           inpp
     c                   endif
      **
     c                   move      'ZQ'          rqstop
     c                   call      'XFCCHKAT'
     c                   parm                    system
     c                   parm                    rqstop
     c                   parm                    rtrncd
     c                   if        rtrncd = 'E'
     c                   move      '0'           inpa
     c                   endif
      **
     c                   move      'ZT'          rqstop
     c                   call      'XFCCHKAT'
     c                   parm                    system
     c                   parm                    rqstop
     c                   parm                    rtrncd
     c                   if        rtrncd = 'E'
     c                   move      '0'           inta
     c                   endif
      **
     c                   move      'ZU'          rqstop
     c                   call      'XFCCHKAT'
     c                   parm                    system
     c                   parm                    rqstop
     c                   parm                    rtrncd
     c                   if        rtrncd = 'E'
     c                   move      '0'           indd
     c                   endif
      **
     c                   move      'ZV'          rqstop
     c                   call      'XFCCHKAT'
     c                   parm                    system
     c                   parm                    rqstop
     c                   parm                    rtrncd
     c                   if        rtrncd = 'E'
     c                   move      '0'           inir
     c                   endif
      **
     c                   move      'ZW'          rqstop
     c                   call      'XFCCHKAT'
     c                   parm                    system
     c                   parm                    rqstop
     c                   parm                    rtrncd
     c                   if        rtrncd = 'E'
     c                   move      '0'           inli
     c                   endif
      **
     c                   eval      tchgauth = *on
     c                   call      'XFCNHKAT'
     c                   parm      3980          authnum
     c                   parm                    rtrncd
     c                   if        rtrncd = 'E'
     c                   eval      tchgauth = *off
     c                   endif
      **
     c                   endsr
      *****************************************************************
      /EJECT
     c     setmsg        begsr
      **                                                   =MESSAGES
     c                   eval      errmsg = *blanks
      **
     c                   if        screen = 2
     c                   if        x = 11
     c                   movel     level5        string
     c     high:lo       xlate     string        result
     c                   movel     lev6(1)       result
     c                   movel     result        errmsg
     c                   cat       ems(x):1      errmsg
     c                   elseif    x = 33
     c                   movel     level6        string
     c     high:lo       xlate     string        result
     c                   movel     lev6(1)       result
     c                   movel     result        errmsg
     c                   cat       ems(x):1      errmsg
     c                   else
     c                   if        x = 30
     c                   movel     ctgry2        string
     c     high:lo       xlate     string        result
     c                   movel     lev6(1)       result
     c                   movel     result        errmsg
     c                   cat       ems(x):1      errmsg
     c                   else
     c                   movel     ems(x)        errmsg
     c                   endif
     c                   endif
     c                   else
     c                   if        screen = 1
     c                   if        x = 11 and not *in92
     c                   movel     level5        string
     c     high:lo       xlate     string        result
     c                   movel     lev6(1)       result
     c                   movel     result        errmsg
     c                   cat       ems(x):1      errmsg
     c                   elseif    x = 32 and npiDuplicated
     c                   eval      errmsg = %trim(npiDuplMessage)
     c                   elseif    x = 33
     c                   movel     level6        string
     c     high:lo       xlate     string        result
     c                   movel     lev6(1)       result
     c                   movel     result        errmsg
     c                   cat       ems(x):1      errmsg
     c                   else
     c                   movel     ems(x)        errmsg
     c                   endif
     c                   else
     c                   if        screen = 3
     c                   movel     ems(x)        errmsg
     c                   endif
     c                   endif
      **
     c                   endif
     c                   endsr
      *****************************************************************
      /SPACE 3
     c     lckrec        begsr
     c                   movel     fiu(1)        dspmsg
     c                   eval      reqnam = *blanks
     c                   eval      reqgp# = 0
     c                   goto      start
     c                   endsr
      *****************************************************************
      /EJECT
     c     srfkey        begsr
      **                                                   =WINDOWS
     c                   movel     xx            one
     c                   if        one <> 'K'
     c                   move      xx            id
     c                   move      *on           *in(id)
     c                   else
     c                   select
     c                   when      xx = 'KA'
     c                   move      *on           *inka
     c                   when      xx = 'KB'
     c                   move      *on           *inkb
     c                   when      xx = 'KC'
     c                   move      *on           *inkc
     c                   when      xx = 'KD'
     c                   move      *on           *inkd
     c                   when      xx = 'KE'
     c                   move      *on           *inke
     c                   when      xx = 'KF'
     c                   move      *on           *inkf
     c                   when      xx = 'KG'
     c                   move      *on           *inkg
     c                   when      xx = 'KH'
     c                   move      *on           *inkh
     c                   when      xx = 'KI'
     c                   move      *on           *inki
     c                   when      xx = 'KJ'
     c                   move      *on           *inkj
     c                   when      xx = 'KK'
     c                   move      *on           *inkk
     c                   when      xx = 'KL'
     c                   move      *on           *inkl
     c                   when      xx = 'KM'
     c                   move      *on           *inkm
     c                   when      xx = 'KN'
     c                   move      *on           *inkn
     c                   when      xx = 'KP'
     c                   move      *on           *inkp
     c                   when      xx = 'KQ'
     c                   move      *on           *inkq
     c                   when      xx = 'KR'
     c                   move      *on           *inkr
     c                   when      xx = 'KS'
     c                   move      *on           *inks
     c                   when      xx = 'KT'
     c                   move      *on           *inkt
     c                   when      xx = 'KU'
     c                   move      *on           *inku
     c                   when      xx = 'KV'
     c                   move      *on           *inkv
     c                   when      xx = 'KW'
     c                   move      *on           *inkw
     c                   when      xx = 'KX'
     c                   move      *on           *inkx
     c                   when      xx = 'KY'
     c                   move      *on           *inky
     c                   endsl
     c                   endif
      **
     c                   endsr
      **************************************************************************
      /EJECT
     c     *inzsr        begsr
      **
     c     *entry        plist
     c                   parm                    rqcode
     c                   parm                    rqlev6
      **
     c     *dtaara       define    *lda          ldads
     c     *dtaara       define    hxalevel      level
     c     *dtaara       define    hmaphynum     nxtnum            9 0
     c                   in        level
     c                   in        ldads
      **
     c                   call      'XFXLDSCI'
     c                   parm                    ldatyp
     c                   parm      ldamap        reqmap
     c                   parm                    lvldsc
     c**
     c                   call      'XFXCNTG'
     c                   parm      lvldsc        centerdsc
     c                   parm                    centerlen
     c                   parm                    centerfil
     c                   eval      lvldsc = centerdsc
      **
     c**                 movel     ldaabr        crpabr
      **
     c***  empky         klist
     c***                kfld                    ldacrp
     c***                kfld                    prmlnm
      **
     c     em#ky         klist
     c                   kfld                    ldacrp
     c                   kfld                    rqemp#
      **
     c     rpkey         klist
     c                   kfld                    hmddr#
     c                   kfld                    year
      **
     c     clkey         klist
     c                   kfld                    hmddr#
     c                   kfld                    reqlv6
      **
     c     thrkey        klist
     c                   kfld                    hmtth#
     c                   kfld                    wsdlcn
      **
     c     allkey        klist
     c                   kfld                    hmtth#
     c                   kfld                    oldlcn
     c                   kfld                    oldpyr
     c                   kfld                    oldpln
      **
     c     oldkey        klist
     c                   kfld                    hmtth#
     c                   kfld                    oldlcn
      **
     c     bnkey         klist
     c                   kfld                    hmtpyr
     c                   kfld                    hmtpln
      **
     c     ppkey         klist
     c                   kfld                    hmtth#
     c                   kfld                    hmtpyr
     c                   kfld                    hmtpln
      **
     c     npikey        klist
     c                   kfld                    hmddr#
     c                   kfld                    npicod
      **
     c     prvkey        klist
     c                   kfld                    hmtth#
     c                   kfld                    oldpyr
     c                   kfld                    oldpln
      **
     c     adrkey        klist
     c                   kfld                    hmddr#
     c                   kfld                    adrseq
      **
     c     hlpkey        klist
     c                   kfld                    filenm
     c                   kfld                    rcdfmt
     c                   kfld                    l
     c                   kfld                    p
      **
     c     clky2         klist
     c                   kfld                    hmddr#
     c                   kfld                    lvlnum
      **
     c                   eval      hmadr# = 0
     c                   eval      hmaosq = 0
      ****               eval      hmaotl = 0
      ****               eval      hmaofx = 0
     c                   eval      wswphn = 0
     c                   eval      wsextn = *blanks
     c                   eval      wswfax = 0
     c                   move      *off          *in92
     c                   move      *off          *in30
      **
     c                   eval      zrofld = *zeros
     c                   eval      blkfld = *blanks
      **
     c                   eval      dspmsg = *blanks
     c                   time                    curtim
     c                   eval      today = tmmdd
     c                   movel     tyyyy         today
      **
     c                   exsr      srauth
      **
     c                   movel(p)  lv5abl        strnga
     c     high:lo       xlate     strnga        tempct
     c                   movel(p)  lv5abc        lvl5nm
     c                   cat       tempct:0      lvl5nm
     c                   cat       ':':0         lvl5nm
     c                   movel(p)  lv6abl        strnga
     c     high:lo       xlate     strnga        tempct
     c                   movel(p)  lv6abc        lvl6nm
     c                   cat       tempct:0      lvl6nm
     c                   cat       ':':0         lvl6nm
     c                   endsr
      *****************************************************************
      /EJECT
     c     srcmdy        begsr
     c                   call      'XFXCMDY'
     c                   parm                    mdate
     c                   parm                    ydate
     c                   endsr
      *****************************************************************
      /SPACE 3
     c     srcymd        begsr
     c                   call      'XFXCYMD'
     c                   parm                    ydate
     c                   parm                    mdate
     c                   endsr
      *****************************************************************
      /SPACE 3
     c     srumap        begsr
     c                   call      'XFXUUMAP'
     c                   parm                    cktype
     c                   parm                    cknumb
     c                   parm                    rtnlv1
     c                   parm                    rtnlv2
     c                   parm                    rtnlv3
     c                   parm                    rtnlv4
     c                   parm                    rtnlv5
     c                   endsr
      *****************************************************************
      /EJECT
     c     clrtbl        begsr
      **
     c                   eval      ecode = *blanks
     c                   eval      hmap = *blanks
     c                   eval      edate = 0
     c                   eval      sdesc = *blanks
     c                   eval      ldesc = *blanks
     c                   eval      tind = *blanks
      **
     c                   endsr
      *****************************************************************
      /SPACE 3
     c     srtabl        begsr
      **
     c                   call      'XFXTABL'
     c                   parm                    tcode
     c                   parm                    ecode
     c                   parm                    hmap
     c                   parm                    edate
     c                   parm                    sdesc
     c                   parm                    ldesc
     c                   parm                    tind
      **
     c                   endsr
      *****************************************************************
      /EJECT
     c     spcprv        begsr
      **
     c                   if        insp = '0'
     c                   movel     nao(1)        dspmsg
     c                   move      *on           *in04
     c                   goto      dsplst
     c                   endif
      **
     c                   if        reqlno = 0
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c     reqlno        chain     hmfdrupr                           89
     c                   if        *in89 = *on
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c                   if        fstlet = '*'
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   endif
     c                   endif
     c                   endif
     c                   call      'HMWMASC'
     c                   parm                    hmddr#
     c                   parm                    reqpgm
     c                   endsr
      *****************************************************************
      /EJECT
     c     brdcer        begsr
      **
     c                   if        inbc = '0'
     c                   movel     nao(1)        dspmsg
     c                   move      *on           *in04
     c                   goto      dsplst
     c                   endif
      **
     c                   if        reqlno = 0
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c     reqlno        chain     hmfdrupr                           89
     c                   if        *in89 = *on
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c                   if        fstlet = '*'
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   endif
     c                   endif
     c                   endif
     c                   call      'HMWMABF'
     c                   parm                    hmddr#
     c                   parm                    reqpgm
     c                   endsr
      *****************************************************************
      /EJECT
     c     spcpro        begsr
      **
     c                   if        inps = '0'
     c                   movel     nao(1)        dspmsg
     c                   move      *on           *in04
     c                   goto      dsplst
     c                   endif
      **
     c                   if        reqlno = 0
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c     reqlno        chain     hmfdrupr                           89
     c                   if        *in89 = *on
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c                   if        fstlet = '*'
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   endif
     c                   endif
     c                   endif
     c                   call      'HMWMAPS'
     c                   parm                    hmddr#
     c                   parm                    reqpgm
     c                   endsr
      *****************************************************************
      /EJECT
     c     spcpun        begsr
      **
     c                   if        inpu = '0'
     c                   movel     nao(1)        dspmsg
     c                   move      *on           *in04
     c                   goto      dsplst
     c                   endif
      **
     c                   if        reqlno = 0
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c     reqlno        chain     hmfdrupr                           89
     c                   if        *in89 = *on
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c                   if        fstlet = '*'
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   endif
     c                   endif
     c                   endif
     c                   call      'HMWMAPU'
     c                   parm                    hmddr#
     c                   parm                    reqpgm
     c                   endsr
      *****************************************************************
      /EJECT
     c     spcpah        begsr
      **
     c                   if        inpa = '0'
     c                   movel     nao(1)        dspmsg
     c                   move      *on           *in04
     c                   goto      dsplst
     c                   endif
      **
     c                   if        reqlno = 0
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c     reqlno        chain     hmfdrupr                           89
     c                   if        *in89 = *on
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c                   if        fstlet = '*'
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   endif
     c                   endif
     c                   endif
     c                   call      'HMWMAPA'
     c                   parm                    hmddr#
     c                   parm                    reqpgm
     c                   endsr
      *****************************************************************
      /EJECT
     c     spctap        begsr
      **
     c                   if        inta = '0'
     c                   movel     nao(1)        dspmsg
     c                   move      *on           *in04
     c                   goto      dsplst
     c                   endif
      **
     c                   if        reqlno = 0
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c     reqlno        chain     hmfdrupr                           89
     c                   if        *in89 = *on
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c                   if        fstlet = '*'
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   endif
     c                   endif
     c                   endif
     c                   call      'HMWMATA'
     c                   parm                    hmddr#
     c                   parm                    reqpgm
     c                   endsr
      *****************************************************************
      /EJECT
     c     spcppr        begsr
      **
     c                   if        inpp = '0'
     c                   movel     nao(1)        dspmsg
     c                   move      *on           *in04
     c                   goto      dsplst
     c                   endif
      **
     c                   if        reqlno = 0
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c     reqlno        chain     hmfdrupr                           89
     c                   if        *in89 = *on
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c                   if        fstlet = '*'
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   endif
     c                   endif
     c                   endif
     c                   call      'HMWMAPP'
     c                   parm                    hmddr#
     c                   parm                    reqpgm
     c                   endsr
      *****************************************************************
      /EJECT
     c     spclic        begsr
      **
     c                   if        inli = '0'
     c                   movel     nao(1)        dspmsg
     c                   move      *on           *in04
     c                   goto      dsplst
     c                   endif
      **
     c                   if        reqlno = 0
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c     reqlno        chain     hmfdrupr                           89
     c                   if        *in89 = *on
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c                   if        fstlet = '*'
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c                   movel     wsdnam        hmdnam
     c                   endif
     c                   endif
     c                   endif
     c                   move      'A'           ropt
      **
     c                   call      'HMWLICE'
     c                   parm                    ropt
     c                   parm      hmddr#        rphy
     c                   parm                    hmdtyp
     c                   parm                    hmdnam
      **
     c                   endsr
      *****************************************************************
      /EJECT
     c     spcddd        begsr
      **
     c                   if        indd = '0'
     c                   movel     nao(1)        dspmsg
     c                   move      *on           *in04
     c                   goto      dsplst
     c                   endif
      **
     c                   if        reqlno = 0
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c     reqlno        chain     hmfdrupr                           89
     c                   if        *in89 = *on
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c                   if        fstlet = '*'
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   endif
     c                   endif
     c                   endif
     c                   call      'HMWMADD'
     c                   parm                    hmddr#
     c                   parm                    reqpgm
     c                   endsr
      *****************************************************************
      /EJECT
     c     spcirf        begsr
      **
     c                   if        inir = '0'
     c                   movel     nao(1)        dspmsg
     c                   move      *on           *in04
     c                   goto      dsplst
     c                   endif
      **
     c                   if        reqlno = 0
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c     reqlno        chain     hmfdrupr                           89
     c                   if        *in89 = *on
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c                   if        fstlet = '*'
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   endif
     c                   endif
     c                   endif
     c                   call      'HMWMAIR'
     c                   parm                    hmddr#
     c                   parm                    reqpgm
     c                   endsr
      *****************************************************************
     c**** empsr2        begsr
      **
     c****               eval      rqemp# = hmddr#
      **
     c***  em#ky         chain     hpfmast                            79
     c***                if        *in79 = *off
     c***                movel     prmlnm        lastnm
     c***                movel     prmfnm        frstnm
     c***                call      'XFXCNAM'
     c***                parm                    namein
     c***                parm                    nameou
     c***                movel     nameou        wsenam
     c***                exsr      filfld
     c***                endif
      **
     c****               endsr
      *****************************************************************
     c**** empsrh        begsr
      **
     c****               if        rc = 2
     c****               exsr      empsr2
     c****               goto      endsrh
     c****               endif
      ***
     c****               exsr      clrfld
     c****               eval      reqlin = 0
     c****               eval      rqemp# = 0
     c****               eval      reqemp = *blanks
     c****               eval      dspmg2 = *blanks
     c****               move      '00000'       *in(01)
     c****               exsr      clremp
      **
     c**** nxtsrh        tag
      **
     c****               if        reqemp <> *blanks
     c****               exsr      bldemp
     c****               movel     reqemp        savemp
     c****               endif
     c****               if        rqemp# <> 0
     c****               exsr      bldem#
     c****               eval      savem# = rqemp#
     c****               eval      rqemp# = 0
     c****               endif
      **
     c**** dspsrh        tag
     c****               eval      reqlin = 0
     c**
     c****               eval      centerdsc = 'MEDICAL ADMINISTRATION ' +
     c****                                     'EMPLOYEE SEARCH'
     c****               call      'XFXCNTG'
     c****               parm                    centerdsc
     c****               parm                    centerlen
     c****               parm                    centerfil
     c****               eval      wshdr = centerdsc
      **
     c****               exfmt     hmfemppc
     c****               eval      dspmg2 = *blanks
     c****               movea     '00'          *in(01)
     c****               move      *off          *in05
      **
     c****               if        csrrrn <> 0
     c****               eval      reqlin = csrrrn
     c****               endif
      **
     c****               if        *inkb = *on
     c****               movea     *in(01)       indice
     c****               call      'XFCFKEY'
     c****               parm                    dspfnm
     c****               parm                    rcdfmt
     c****               parm                    indice
     c****               parm                    xx
     c**** xx            cabeq     '  '          dspsrh
     c****               exsr      srfkey
     c****               endif
      **
     c****               if        *inkc = *on
     c****               goto      endsrh
     c****               endif
      **
     c****               if        *in98 = *on
     c****               call      'HXWHLP '
     c****               parm                    dspfnm
     c****               parm                    rcdfmt
     c****               goto      dspsrh
     c****               endif
      **
     c**** *inkl         cabeq     *on           endsrh
      **
     c****               if        *in97 = *on
     c****               movel     savemp        reqemp
     c****               eval      rqemp# = savem#
     c****               goto      nxtsrh
     c****               endif
      **
     c**** reqlin        cabne     0             chkreq
      **                                                      IF FILLED
     c****               if        rqemp# = 0
     c****                         and reqemp = *blanks
     c****               movel     rqe(2)        dspmg2
     c****               movea     '11'          *in(01)
     c****               goto      dspsrh
     c****               else
     c****               goto      nxtsrh
     c****               endif
      **
     c**** chkreq        tag
     c****               if        reqlin = 0
     c****               movel     iln(1)        dspmg2
     c****               move      *on           *in05
     c****               goto      dspsrh
     c****               else
     c****               move      *off          *in89
     c**** reqlin        chain     hmfemppr                           89
     c****               if        *in89 = *on
     c****               movel     iln(1)        dspmg2
     c****               move      *on           *in05
     c****               goto      dspsrh
     c****               else
     c****               if        fstlt2 = '*'
     c****               movel     iln(1)        dspmg2
     c****               move      *on           *in05
     c****               goto      dspsrh
     c****               else
     c****               exsr      filfld
     c****               goto      chkdoc
     c****               endif
     c****               endif
     c****               endif
      **
     c**** *in90         cabeq     *on           nxtsrh
      **
     c**** chkdoc        tag
     c****               eval      dspmsg = *blanks
     c**** hmddr#        chain(n)  hmfmams                            79
     c****               if        *in79 = *off
     c****               exsr      clrfld
     c****               movel     dae(2)        dspmsg
     c****               endif
      **
     c**** endsrh        endsr
      *****************************************************************
     c**** bldemp        begsr
      **
     c***                if        *in97 = *off
     c***                exsr      clremp
     c***                eval      recno7 = 1
     c***                move      *off          *in94
     c***                eval      wsenam = *blanks
     c***                eval      prmemp = 0
     c***                eval      wnm2 = *blanks
     c***                eval      wnam = *blanks
      **
     c***                movea(p)  reqemp        nme(1)
     c***  1             do        16            b
     c***                if        nme(b) = ','
     c***                eval      b = b + 1
     c***                if        nme(b) <> ' '
     c***                move      *on           *in01
     c***                movel     nke(1)        dspmg2
     c***                goto      endemp
     c***                endif
     c***                leave
     c***                else
     c***                if        nme(b) <> ' '
     c***                move      nme(b)        wnam(b)
     c***                endif
     c***                endif
     c***                enddo
      **
     c***                eval      n = 16
     c***                movea     reqemp        wnm3(1)
     c***                dou       wnm3(n) <> ' '
     c***                eval      n = n - 1
     c***                enddo
     c***                eval      n = n + 1
      **
     c***                movea(p)  wnam          prmlnm
     c***                if        reqemp = '*ALL'
     c***  *loval        setll     hpfmasta
     c***                else
     c***  empky         setll     hpfmasta
     c***                endif
     c***                else
     c***                eval      srecno = srecno + 15
     c***                endif
      **
     c***                eval      maxrec = srecno + 14
      **
     c***  srecno        do        maxrec        recno7
     c***  nxtemp        tag
     c***                read      hpfmasta                               89
     c***  *in89         cabeq     *on           outemp
      **
     c***  prmemp        cabeq     0             nxtemp
     c***                if        rqemp# <> 0
     c***  prmemp        cabne     rqemp#        nxtemp
     c***                endif
      **
     c***                if        reqemp <> '*ALL'
     c***                call      'XFXHNAM'
     c***                parm                    prmlnm
     c***                parm                    prmfnm
     c***                parm                    prmint
     c***                parm                    rtnnam
     c***                movea     rtnnam        wnm2(1)
     c***                movea     blkfld        wnm2(n)
     c***                movea(p)  wnm2(1)       rtnnam
      **
     c***  rtnnam        cabne     reqemp        nxtemp
     c***                endif
      **
     c***                exsr      bldsf2
      **
     c***                if        recno7 >= 990
     c***                movel     eos(1)        dspmg2
     c***                goto      endemp
     c***                else
     c***                move      *on           *in94
     c***                endif
     c***                eval      savrc7 = recno7
     c***                enddo
      **
     c***                goto      endemp
      **
     c***  outemp        tag
     c***                if        *in96 = *on
     c***                eval      savrc7 = recno7
     c***                movel     eod(1)        wsenam
     c***                eval      prmemp = 0
     c***                move      *on           *in96
     c***                write     hmfemppr
     c***                move      *off          *in94
     c***                else
     c***                if        dspmg2 = *blanks
     c***                exsr      clremp
     c***                eval      wsenam = *blanks
     c***                eval      prmemp = 0
     c***                movel     npf(1)        dspmg2
     c***                endif
     c***                endif
      **
     c**** endemp        endsr
      *****************************************************************
     c**** bldem#        begsr
      **
     c***                exsr      clremp
     c***                eval      recno7 = 1
     c***                move      *off          *in94
     c***                eval      wsenam = *blanks
     c***                eval      prmemp = 0
      **
     c***                if        *in97 = *off
     c***  em#ky         setll     hpfmast
     c***                else
     c***                eval      srecno = srecno + 15
     c***                endif
      **
     c***                eval      maxrec = srecno + 14
      **
     c***  srecno        do        maxrec        recno7
     c***  nxtem#        tag
     c***  em#ky         reade     hpfmast                                89
     c***  *in89         cabeq     *on           outem#
      **
     c***                exsr      bldsf2
      **
     c***                if        recno7 >= 990
     c***                movel     eos(1)        dspmg2
     c***                goto      endem#
     c***                else
     c***                move      *on           *in94
     c***                endif
     c***                eval      savrc7 = recno7
     c***                enddo
      **
     c***  outem#        tag
     c***                if        *in96 = *on
     c***                eval      savrc7 = recno7
     c***                movel     eod(1)        wsenam
     c***                eval      prmemp = 0
     c***                move      *on           *in96
     c***                write     hmfemppr
     c***                else
     c***                if        dspmg2 = *blanks
     c***                exsr      clremp
     c***                eval      wsenam = *blanks
     c***                eval      prmemp = 0
     c***                movel     npf(1)        dspmg2
     c***                endif
     c***                endif
      **
     c**** endem#        endsr
      *****************************************************************
     c***  bldsf2        begsr
      **
      **
     c***                movel     prmlnm        lastnm
     c***                movel     prmfnm        frstnm
     c***                call      'XFXCNAM'
     c***                parm                    namein
     c***                parm                    nameou
     c***                movel     nameou        wsenam
      **
     c***                move      *on           *in96
     c***                write     hmfemppr
      **
     c***                endsr
      *****************************************************************
     c**** filfld        begsr
      **         ******    *****
      **
     c***                if        prmemp <> *zeros
     c***                eval      hmddr# = prmemp
     c***                endif
     c***                if        wsenam <> *blanks
     c***                movel     wsenam        hmdnam
     c***                endif
     c***                if        prmssn <> *zeros
     c***                eval      hmdss# = prmssn
     c***                endif
     c***                if        prmad1 <> *blanks
     c***                movel     prmad1        hmdha1
     c***                endif
     c***                if        prmcty <> *blanks
     c***                movel     prmcty        hmdhc1
     c***                endif
     c***                if        prmsta <> *blanks
     c***                movel     prmsta        hmdhs1
     c***                endif
     c***                if        wszip1 <> *blanks
     c***                movel     wszip1        hmdhz1
     c***                endif
     c***                if        wszip2 <> *blanks
     c***                movel     wszip2        hmdhe1
     c***                endif
     c***                if        prmtel <> *zeros
      ****               eval      hmdht1 = prmtel
     c***                eval      wshphn = prmtel
     c***                endif
     c***                if        prmedl <> *blanks
     c***                movel     prmedl        hmddg1
     c***                endif
     c***                if        prmbdt <> *zeros
     c***                eval      ydate = prmbdt
     c***                exsr      srcymd
     c***                eval      wsddob = mdate
     c***                endif
     c***                if        prmsex <> *blanks
     c***                movel     prmsex        hmdsex
     c***                endif
     c***                if        prmedn <> *blanks
     c***                movel     prmedn        hmdpsn
     c***                endif
     c***                if        prorgd <> *zeros
     c***                eval      ydate = prorgd
     c***                exsr      srcymd
     c***                eval      wsmdoh = mdate
     c***                endif
     c***                if        prmlg1 <> *blanks
     c***                movel     prmlg1        hmdlg1
     c***                endif
     c***                if        prmlg2 <> *blanks
     c***                movel     prmlg2        hmdlg2
     c***                endif
      **
     c****               endsr
      *****************************************************************
     c     clrfld        begsr
      **         ******    *****
      **
     c                   eval      hmddr# = 0
     c                   eval      hmdnam = *blanks
     c                   eval      hmdss# = 0
     c                   eval      hmdha1 = *blanks
     c                   eval      hmdhc1 = *blanks
     c                   eval      hmdhs1 = *blanks
     c                   eval      hmdhz1 = *blanks
     c                   eval      hmdhe1 = *blanks
      ****               eval      hmdht1 = 0
     c                   eval      wshphn = 0
     c                   eval      hmdsex = *blanks
     c                   eval      hmddg1 = *blanks
     c                   eval      hmdpsn = *blanks
     c                   eval      wsddob = 0
     c                   eval      wsmdoh = 0
     c                   eval      hmdlg1 = *blanks
     c                   eval      hmdlg2 = *blanks
     c                   eval      hmdtyp = *blanks
     c***                   eval      hmdgp# = 0
     c                   eval      wsdgp# = 0
     c                   eval      hmdbth = *blanks
     c                   eval      hmdsv2 = *blanks
     c                   eval      hmdsv1 = *blanks
     c                   eval      hmddg1 = *blanks
     c                   eval      hmdint = *blanks
     c                   eval      hmdusr = *blanks
     c                   eval      hmdimp = *blanks
     c                   eval      hmdrec = *blanks
     c                   eval      hmdomg = *blanks
     c                   eval      hmdgpn = *blanks
     c                   eval      hmdlv5 = 0
     c                   eval      hmdmsc = 0
     c                   eval      hmddyr = 0
     c                   eval      hmdcm3 = *blanks
     c                   eval      hmdecn = *blanks
      ****               eval      hmdect = 0
     c                   eval      wsephn = 0
      **
     c                   endsr
      *****************************************************************
     c**** clremp        begsr
     c****               movea     '10'          *in(95)
     c****               write     hmfemppc
     c****               movea     '000'         *in(94)
     c****               eval      srecno = 1
     c****               eval      reqlin = 0
     c****               eval      d = 14
     c****               eval      g = 1
     c****               endsr
      *****************************************************************
     c     sphone        begsr
      **
     c                   if        reqlno = 0
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c     reqlno        chain     hmfdrupr                           89
     c                   if        *in89 = *on
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c                   if        fstlet = '*'
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c                   movel     wsdnam        hmdnam
     c                   endif
     c                   endif
     c                   endif
      **
     c                   call      'HXWPHON'
     c                   parm      0             reqlv6
     c                   parm      hmddr#        reqnum
     c                   parm      0             reqnm2
     c                   parm      'MM'          reqsrc
     c                   parm      hmdnam        dspnam
     c                   parm      'U'           inqopt
      **
     c                   endsr
      *****************************************************************
     c     srphnu        begsr
      **
     c                   call      'HXXPHNU'
     c                   parm      hmdlv6        drlvl6
     c                   parm      hmddr#        reqnum
     c                   parm      mmseq         reqnm2
     c                   parm      'MM'          reqsrc
     c                   parm                    rqstyp
     c                   parm                    reqphn
     c                   parm                    reqext
     c                   parm      *blanks       reqcmt
     c                   parm      -1            reqSeq                         =should not be found
      **
     c                   endsr
      *****************************************************************
     c     srphnl        begsr
      **
5    c                   call      'HXXPHNL'
     c                   parm      hmdlv6        drlvl6
     c                   parm      hmddr#        reqnum
     c                   parm      mmseq         reqnm2
     c                   parm      'MM'          reqsrc
     c                   parm                    rqstyp
     c                   parm      0             rtnphn
     c                   parm      *blanks       rtnext
      **
     c                   endsr
      *****************************************************************
     c     phload        begsr
      **
     c                   eval      mmseq = 0
     c                   eval      rqstyp = 'B '
     c                   exsr      srphnl
     c                   eval      wspagr = rtnphn
      **
     c                   eval      rqstyp = 'C '
     c                   exsr      srphnl
     c                   eval      wscphn = rtnphn
      **
     c                   eval      rqstyp = 'EC'
     c                   exsr      srphnl
     c                   eval      wsephn = rtnphn
      **
     c                   eval      rqstyp = 'F '
     c                   exsr      srphnl
     c                   eval      wswfax = rtnphn
      **
     c                   eval      rqstyp = 'H '
     c                   exsr      srphnl
     c                   eval      wshphn = rtnphn
      **
     c                   eval      rqstyp = 'W '
     c                   exsr      srphnl
     c                   eval      wswphn = rtnphn
     c                   eval      wsextn = rtnext
     c                   eval      rqstyp = '  '
      **
     c                   exsr      sremlk
     c                   eval      wsemal = rtneml
      **
     c                   endsr
      *****************************************************************
     c     phupdt        begsr
      **
     c                   eval      mmseq = 0
     c                   eval      rqstyp = 'B '
     c                   eval      reqphn = wspagr
     c                   eval      reqext = *blanks
     c                   exsr      srphnu
      **
     c                   eval      rqstyp = 'C '                                = update cell phone
           exsr checkUserCell;
     c                   eval      reqphn = wscphn
     c                   eval      reqext = *blanks
     c                   exsr      srphnu
      **
     c                   eval      rqstyp = 'EC'
     c                   eval      reqphn = wsephn
     c                   eval      reqext = *blanks
     c                   exsr      srphnu
      **
     c                   eval      rqstyp = 'F '
     c                   eval      reqphn = wswfax
     c                   eval      reqext = *blanks
     c                   exsr      srphnu
      **
     c                   eval      rqstyp = 'H '
     c                   eval      reqphn = wshphn
     c                   eval      reqext = *blanks
     c                   exsr      srphnu
      **
     c                   eval      rqstyp = 'W '
     c                   eval      reqphn = wswphn
     c                   eval      reqext = wsextn
     c                   exsr      srphnu
      **
     c                   eval      rqstyp = '  '
     c                   eval      reqeml = wsemal
     c                   exsr      sremup
      **
     c                   endsr
      *****************************************************************
     c     phnclr        begsr
      **
     c                   eval      wspagr = 0
     c                   eval      wscphn = 0
     c                   eval      wsephn = 0
     c                   eval      wswfax = 0
     c                   eval      wshphn = 0
     c                   eval      wswphn = 0
     c                   eval      wsextn = *blanks
     c                   eval      wsemal = *blanks
      **
     c                   endsr
      *****************************************************************
     c     sremup        begsr
      **
     c                   call      'HXXEMUP'
     c                   parm      hmdlv6        drlvl6
     c                   parm      hmddr#        reqnum
     c                   parm      0             reqnm2
     c                   parm      'MM'          reqsrc
     c                   parm                    reqeml
      **
     c                   endsr
      *****************************************************************
     c     sremlk        begsr
      **
     c                   call      'HXXEMLK'
     c                   parm      hmdlv6        drlvl6
     c                   parm      hmddr#        reqnum
     c                   parm      0             reqnm2
     c                   parm      'MM'          reqsrc
     c                   parm                    rtneml
      **
     c                   endsr
      *****************************************************************
     c     oophup        begsr
      **
     c                   eval      rqstyp = 'W '
     c                   eval      reqphn = wswphn
     c                   eval      reqext = wsextn
     c                   z-add     hmaosq        mmseq
     c                   exsr      srphnu
      **
     c                   eval      rqstyp = 'F '
     c                   eval      reqphn = wswfax
     c                   eval      reqext = *blanks
     c                   z-add     hmaosq        mmseq
     c                   exsr      srphnu
      **
     c                   endsr
      *****************************************************************
     c     oophld        begsr
      **
     c                   eval      rqstyp = 'W '
     c                   z-add     hmaosq        mmseq
     c                   exsr      srphnl
     c                   eval      wswphn = rtnphn
     c                   eval      wsextn = rtnext
      **
     c                   eval      rqstyp = 'F '
     c                   z-add     hmaosq        mmseq
     c                   exsr      srphnl
     c                   eval      wswfax = rtnphn
      **
     c                   endsr
      *****************************************************************
      /EJECT
     c     srcercd       begsr
      **
     c                   if        incercd = *off
     c                   movel     nao(1)        dspmsg
     c                   move      *on           *in04
     c                   goto      dsplst
     c                   endif
      **
     c                   if        reqlno = 0
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c     reqlno        chain     hmfdrupr                           05
     c                   if        *in05 = *on
     c                             or fstlet = '*'
     c                   movel     iln(1)        dspmsg
     c                   goto      dsplst
     c                   endif
     c                   endif
      **
     c                   call      'HMWCERCD'
     c                   parm      hmddr#        drnum
      **
     c                   endsr
      *****************************************************************
     c     srtypchg      begsr

      **
     c                   eval      phupdate = *off
      **
     c                   if        tchgauth = *off
     c                   movel     nao(1)        dspmsg
     c                   move      *on           *in04
     c                   goto      dsplst
     c                   endif

      **
     c                   if        reqlno = 0
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c     reqlno        chain     hmfdrupr                           05
     c                   if        *in05 = *on
     c                             or fstlet = '*'
     c                   movel     iln(1)        dspmsg
     c                   goto      dsplst
     c                   endif
     c                   endif

     c                   if        hmdtyp = 'PH'
     c                   movel     tcm(5)        dspmsg
     c                   goto      dsplst
     c                   endif

     c                   eval      wspnam = wsdnam
     c                   eval      wsdoc# = wsidno
     c                   eval      wsctyp = hmdtyp
     c                   eval      wscdsc = *blanks
     c                   exsr      clrtbl
     c                   movel     'MMTP'        tcode
     c                   movel     wsctyp        ecode
     c                   exsr      srtabl
B4   c                   if        tind <> 'E'
     c                   eval      wscdsc = ldesc
E4   c                   endif
     c                   eval      wsntyp = *blanks
     c                   eval      wsndsc = *blanks

     c                   eval      errid =  'Y'
     c     dsptypc       tag
     c                   exfmt     hmfstchg

      **
     c                   eval      *in01 = *off
     c                   eval      dspmsg = *blanks
      **
     c                   if        *inkb = *on
     c                   movea     *in(01)       indice
     c                   call      'XFCFKEY'
     c                   parm                    dspfnm
     c                   parm                    rcdfmt
     c                   parm                    indice
     c                   parm                    xx
     c                   eval      errid = 'Y'
     c     xx            cabeq     '  '          dsptypc
     c                   exsr      srfkey
     c                   endif
      **
     c                   if        *inkc = *on
     c                   movel(p)  tnc(1)        dspmsg
     c                   goto      endstc
     c                   endif
      **
     c                   if        *inkl = *on
     c                   movel(p)  tnc(1)        dspmsg
     c                   goto      endstc
     c                   endif
      **
     c                   if        *in98 = *on
     c                   call      'HXWHLP '
     c                   parm                    dspfnm
     c                   parm                    rcdfmt
     c                   eval      errid = 'Y'
     c                   goto      dsptypc
     c                   endif

      **
     c                   if        *inka = *on
     c                   exsr      chklin
     c                   eval      errid = 'Y'
     c                   endif
      **
     c                   if        wsntyp = *blanks
     c                   eval      *in01 = *on
     c                   eval      dspmsg = tcm(1)
     c                   eval      errid =  'Y'
     c                   eval      wsndsc = *blanks
     c                   goto      dsptypc
     c                   else
     c                   exsr      clrtbl
     c                   movel     'MMTP'        tcode
     c                   movel     wsntyp        ecode
     c                   exsr      srtabl
B4   c                   if        tind <> 'E'
     c                   eval      wsndsc = ldesc
     c                   else
     c                   eval      wsndsc = *blanks
     c                   eval      *in01 = *on
     c                   eval      dspmsg = tcm(2)
     c                   eval      errid =  'Y'
     c                   goto      dsptypc
E4   c                   endif
     c                   endif

     c                   if        wsctyp = wsntyp
     c                   eval      *in01 = *on
     c                   eval      dspmsg = tcm(3)
     c                   eval      errid =  'Y'
     c                   goto      dsptypc
     c                   endif

     c                   select
     c                   when      wsctyp = 'RF'
     c                   if        wsntyp <> 'PH'
     c                   eval      *in01 = *on
     c                   eval      dspmsg = tcm(4)
     c                   eval      errid =  'Y'
     c                   goto      dsptypc
     c                   endif
     c                   other
     c                   if        wsntyp = 'PH'
     c                             or wsntyp = 'RF'
     c                   eval      *in01 = *on
     c                   eval      dspmsg = tcm(6)
     c                   eval      errid =  'Y'
     c                   goto      dsptypc
     c                   endif
     c                   endsl

      **
     c                   if        *in90 = *on
     c                             or errid = 'Y'
     c                   eval      errid = ' '
     c                   eval      dspmsg = updt(2)
     c                   goto      dsptypc
     c                   endif
      **
     c     wsdoc#        chain     hmfmams                            79
     c                   if        *in79 = *off
     c                   eval      hmdtyp = wsntyp
     c                   update    hmfmams
     c                   if        wsctyp = 'RF'
     c                             and wsntyp = 'PH'
     c                   eval      rc = 2
     c                   move      *off          *in92
     c                   exsr      drinfo
     c                   if        phupdate = *off
     c     wsdoc#        chain     hmfmams                            79
     c                   if        *in79 = *off
     c                   eval      hmdtyp = wsctyp
     c                   update    hmfmams
     c                   endif
     c                   endif
     c                   endif
     c                   endif
      **

     c     endstc        endsr
      *****************************************************************
     c     srimmu        begsr
      **
     c                   if        reqlno = 0
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c     reqlno        chain     hmfdrupr                           89
     c                   if        *in89 = *on
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c                   if        fstlet = '*'
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   endif
     c                   endif
     c                   endif
     c                   movel     wsdnam        pname
     c                   call      'XFCNHKAT'
     c                   parm      4500          authnum
     c                   parm                    rtrncd
     c                   if        rtrncd = 'E'
     c                   movel     nao(1)        dspmsg
     c                   goto      dsplst
     c                   endif
     c     wsidno        chain(n)  hmfmams                            89
     c                   if        *in89 = *off
     c                   call      'HMWREHT'
     c                   parm                    pname
     c                   parm                    hmddr#
     c                   endif
      **
     c                   endsr
      *****************************************************************
     c     sreduc        begsr
      **
     c                   if        reqlno = 0
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c     reqlno        chain     hmfdrupr                           89
     c                   if        *in89 = *on
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c                   if        fstlet = '*'
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   endif
     c                   endif
     c                   endif
     c                   movel     wsdnam        pname
     c                   call      'XFCNHKAT'
     c                   parm      4499          authnum
     c                   parm                    rtrncd
     c                   if        rtrncd = 'E'
     c                   movel     nao(1)        dspmsg
     c                   goto      dsplst
     c                   endif
     c     wsidno        chain(n)  hmfmams                            89
     c                   if        *in89 = *off
     c                   call      'HMWREED'
     c                   parm                    pccode
     c                   parm                    hmddr#
     c                   endif
      **
     c                   endsr
      *****************************************************************
     c     writeGroups   begsr
      **
     c     grpky3        klist
     c                   kfld                    hmddr#
     c                   kfld                    wsdgp#

     c     grpky3        chain     hmfmamg
     c                   if        not%found(hmpmamg)
     c                   eval      hgddr# = hmddr#
     c                   eval      hgdgrp = wsdgp#
     c                   eval      hgdpri = 'X'
     c                   write     hmfmamg
     c                   endif

      **
     c                   endsr
      *****************************************************************
     c     updateGroup   begsr
      **
     c     grpky4        klist
     c                   kfld                    hmddr#
     c                   kfld                    wsdgp#
     c                   kfld                    primary

     c     hmddr#        setll     hmfmamg
     c                   dou       %eof(hmpmamg)
     c     hmddr#        reade     hmfmamg
     c                   if        not%eof(hmpmamg) and hgdpri = 'X'
     c                   delete    hmfmamg
     c                   endif
     c                   enddo

     c                   if        wsdgp# <> 0
     c     grpky3        chain     hmfmamg
     c                   if        %found(hmpmamg)
     c                   eval      hgdpri = 'X'
     c                   update    hmfmamg
     c                   else
     c                   eval      hgddr# = hmddr#
     c                   eval      hgdgrp = wsdgp#
     c                   eval      hgdpri = 'X'
     c                   write     hmfmamg
     c                   endif
     c                   endif
      **
     c                   endsr
      *****************************************************************
     c     srAddGroup    begsr

     c                   if        reqlno = 0
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c     reqlno        chain     hmfdrupr                           89
     c                   if        *in89 = *on
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c                   if        fstlet = '*'
     c                   movel     iln(1)        dspmsg
     c                   move      *on           *in05
     c                   goto      dsplst
     c                   else
     c                   movel     wsdnam        hmdnam
     c                   endif
     c                   endif
     c                   endif

     c                   eval      centerdsc = 'ADDITIONAL GROUP MAINTENANCE'
     c                   call      'XFXCNTG'
     c                   parm                    centerdsc
     c                   parm                    centerlen
     c                   parm                    centerfil

     c                   eval      wshdr = centerdsc
     c     addRedo       tag
     c                   exsr      clraddg
     c                   eval      addopt = *blanks
     c                   eval      *inkd = *off
     c                   eval      reqlin = 0

     c     wsidno        chain     hmfmams
     c                   if        %found(hmpmams)
     c                   eval      wsdnm2 = hmdnam
     c                   endif

     c     wsidno        setll     hmfmamg
     c                   dou       %eof(hmpmamg)
     c     wsidno        reade     hmfmamg
     c                   if        not%eof(hmpmamg)
     c                   eval      recno7 +=1

     c                   eval      wsprim = hgdpri
     c                   eval      wsgrab = hgdgrp
     c                   eval      wsgrp# = hgdgrp
     c                   exsr      clrtbl
     c***                   movel     'MGRP'        tcode
     c                   movel     wsgrab        ecode
     c                   exsr      srphysgrp
B4   c                   if        tind <> 'E'
     c                   eval      wsgrop = ldesc
E4   c                   endif

     c                   eval      *in96 = *on
     c                   write     hmfaddgr
     c                   endif
     c                   enddo

     c                   if        recno7 <> 0
     c****               eval      recno7 += 1
     c                   eval      wsgrop = *blanks
     c                   eval      wsprim = *blanks
     c                   eval      wsgrp# = 0
     c****               write     hmfaddgr
     c                   eval      *in96 = *on
     c                   else
     c                   movel     npf(3)        dspmg2
     c                   goto      adddspl
     c                   endif


     c     adddspl       tag
     c                   exfmt     hmfaddgc

     c                   if        savOpt <> addopt
     c                   eval      dspmg2 = *blanks
     c                   eval      savgp# = 0
     c                   endif
     c                   eval      savopt = addopt

     c                   if        csrrrn <> 0
     c                   eval      reqlin = csrrrn
     c                   endif
      **

     c                   if        *inka = *on
     c                   exsr      chklin
     c                   goto      adddspl
     c                   endif
      **
     c                   if        *inkc = *on
     c                             or *inkl = *on
     c                   goto      endadg
     c                   endif

     c                   if        addopt <> 'D' and addopt <> *blanks
     c                   movel     iop(1)        dspmg2
     c                   move      *on           *in23
     c                   goto      adddspl
     c                   endif
      ***ADD GROUP***
     c                   if        *inkd = *on
     c                   eval      addopt = *blanks
     c                   eval      dspmg2 = *blanks
     c                   eval      *in12 = *off
     c                   eval      reqlin = 0
     c                   eval      reqgrp = 0
     c                   eval      wsgpds = *blanks

     c     addwin        tag
     c                   exfmt     hmfaddwn
     c                   eval      *in12 = *off
     c                   eval      dspmsg = *blanks

     c                   if        *inkc = *on
     c                             or *inkl = *on
     c                   goto      adddspl
     c                   endif

     c                   if        *inka = *on
     c                   eval      *in12 = *off
     c                   exsr      chklin
     c                   endif

     c                   if        reqgrp = 0
     c                   eval      *in12 = *on
     c                   movel     ems4(2)       dspmsg
     c                   eval      savgp# = 0
     c                   eval      wsgpds = *blanks
     c                   goto      addwin
     c                   else

     c     grpky         klist
     c                   kfld                    wsidno
     c                   kfld                    reqgrp

     c     grpky2        klist
     c                   kfld                    wsidno
     c                   kfld                    wsgrab

     c                   exsr      clrtbl
     c***                   movel     'MGRP'        tcode
     c                   movel     reqgrp        ecode
     c                   exsr      srphysgrp
     c                   if        tind = 'E'
     c                   move      *on           *in12
     c                   movel     ems1(3)       dspmsg
     c                   eval      savgp# = 0
     c                   goto      addwin
     c                   else
     c                   eval      wsgpds = ldesc
     c                   endif

     c     grpky         chain     hmfmamg
     c                   if        %found(hmpmamg)
     c                   eval      *in12 = *on
     c                   movel     ems4(1)       dspmsg
     c                   eval      savgp# = 0
     c                   goto      addwin
     c                   else
     c                   if        savgp# <> reqgrp
     c                   eval      savgp# = reqgrp
     c                   movel     nef(1)        dspmsg
     c                   goto      addwin
     c                   else
     c                   eval      hgddr# = wsidno
     c                   eval      hgdgrp = reqgrp
     c                   eval      hgdpri = *blanks
     c                   write     hmfmamg
     c                   endif

     c                   goto      addRedo
     c                   endif
     c                   endif
     c                   endif

      ***DELETE GROUP***
     c                   if        addopt = 'D'
     c                   if        reqlin = 0
     c                   movel     iln(1)        dspmg2
     c                   move      *on           *in23
     c                   goto      adddspl
     c                   else
     c     reqlin        chain     hmfaddgr                           89
     c                   if        *in89 = *on
     c                   movel     iln(1)        dspmg2
     c                   move      *on           *in23
     c                   goto      adddspl
     c                   else
     c                   if        fstlet = '*'
     c                   movel     iln(1)        dspmg2
     c                   move      *on           *in23
     c                   goto      adddspl
     c                   endif
     c                   endif
     c                   endif

     c     grpky2        chain     hmfmamg
     c                   if        %found(hmpmamg)

     c                   if        savgp# <> hgdgrp
     c                   eval      savgp# = hgdgrp
     c                   movel     nef(3)        dspmg2
     c                   goto      adddspl
     c                   else
     c                   delete    hmfmamg
     c                   goto      addRedo
     c                   endif
     c                   endif

     c                   endif

     c                   goto      adddspl
      **
     c     endadg        endsr
      *****************************************************************
     c     chklin        begsr
     c                   movel     dspfnm        filenm
     c                   movel     csrfld        fldnam
     c                   move      *blanks       data

     c     hlpky         klist
     c                   kfld                    filenm
     c                   kfld                    rcdfmt
     c                   kfld                    fldnam
      **
     c                   if        rcdfmt = 'HMFTHRPC'
     c                   eval      rcdfmt = 'HMFTHRPS'
     c                   endif
      **
     c     hlpky         chain     hxlwinh                            79
     c     *in79         cabeq     *on           nohlp
      **
     c                   eval      l = hrclin
     c                   eval      p = hrcpos
      **
     c                   if        hlufnm = 'HMPMAMS'
     c                   eval      %subst(data:9:1) = 'N'
     c                   endif

     c                   call      'HXCWINH'
     c                   parm                    htable
     c                   parm                    htdstp
     c                   parm                    hlufnm
     c                   parm                    hlufcd
     c                   parm                    l
     c                   parm                    p
     c                   parm                    data
      **
     c                   if        data <> *blanks
     c                   move      'Y'           slcmde
      **
     c                   if        rcdfmt = 'HMFDRUPC'
     c                   select
     c                   when      hfield = 'REQTYP'
     c                   movel     data          reqtyp
     c                   when      hfield = 'REQLV5'
     c                   movel     data          reqlv5
     c                   when      hfield = 'REQLV6'
     c                   movel     data          reqlv6
     c                   when      hfield = 'REQGP#'
     c                   movel     data          reqgp#
     c                   when      hfield = 'WSDDLD'
     c                   movel     data          wsddld
     c                   when      hfield = 'REQID#'
     c                   movel     data          reqid#
     c                   when      hfield = 'WSPCON'
     c                   movel     data          wspcon
     c                   endsl
     c                   endif

     c                   if        rcdfmt = 'HMFADDWN'
     c                   select
     c                   when      hfield = 'REQGRP'
     c                   movel     data          reqgrp
     c                   endsl
     c                   endif

     c                   if        rcdfmt = 'HMFPHUP2'
     c                   select
     c****               when      hfield = 'HMDORP'
     c****               movel     data          hmdorp
     c                   when      hfield = 'HMDMSC'
     c                   movel     data          hmdmsc
     c                   when      hfield = 'HMDNAM'
     c                   movel     data          hmdnam
     c                   when      hfield = 'HMDSS#'
     c                   movel     data          hmdss#
     c                   when      hfield = 'HMDTYP'
     c                   movel     data          hmdtyp
     c                   when      hfield = 'WSDDOB'
     c                   movel     data          wsddob
     c                   when      hfield = 'HMDSEX'
     c                   movel     data          hmdsex
     c                   when      hfield = 'HMDSV1'
     c                   movel     data          hmdsv1
     c                   when      hfield = 'HMDSV2'
     c                   movel     data          hmdsv2
     c***                   when      hfield = 'HMDGP#'
     c***                   movel     data          hmdgp#
     c                   when      hfield = 'WSDGP#'
     c                   movel     data          wsdgp#
     c                   when      hfield = 'HMDDG1'
     c                   movel     data          hmddg1
     c                   when      hfield = 'HMDDG2'
     c                   movel     data          hmddg2
     c                   when      hfield = 'HMDREC'
     c                   movel     data          hmdrec
     c                   when      hfield = 'HMDOMG'
     c                   movel     data          hmdomg
     c                   when      hfield = 'HMDGPN'
     c                   movel     data          hmdgpn
     c                   when      hfield = 'HMAOST'
     c                   movel     data          hmaost
     c                   when      hfield = 'HMAOZ1'
     c                   movel     data          hmaoz1
     c                   when      hfield = 'HMAOZ2'
     c                   movel     data          hmaoz2
      ****               when      hfield = 'HMAOTL'
      ****               movel     data          hmaotl
     c                   when      hfield = 'WSWPHN'
     c                   movel     data          WSWPHN
      ****               when      hfield = 'HMAOFX'
      ****               movel     data          hmaofx
     c                   when      hfield = 'WSWFAX'
     c                   movel     data          wswfax
     c***                   when      hfield = 'ADDOAD'
     c***                   movel     data          addoad
     c                   when      hfield = 'HMDHA1'
     c                   movel     data          hmdha1
     c                   when      hfield = 'HMDHC1'
     c                   movel     data          hmdhc1
     c                   when      hfield = 'HMDHS1'
     c                   movel     data          hmdhs1
     c                   when      hfield = 'HMDHZ1'
     c                   movel     data          hmdhz1
     c                   when      hfield = 'HMDHE2'
     c                   movel     data          hmdhe2
      ****               when      hfield = 'HMDHT1'
      ****               movel     data          hmdht1
     c                   when      hfield = 'WSHPHN'
     c                   movel     data          wshphn
     c                   when      hfield = 'HMDLV5'
     c                   movel     data          hmdlv5
     c                   when      hfield = 'HMDCM3'
     c                   movel     data          hmdcm3
     c****               when      hfield = 'HMDADP'
     c****               movel     data          hmdadp
     c                   when      hfield = 'LVLNUM'
     c                   movel     data          lvlnum
     c                   when      hfield = 'WSNPIC'
     c                   movel     data          wsnpic
     c                   when      hfield = 'WSCOSN'
     c                   movel     data          wscosn
     c                   when      hfield = 'HMDCP5'
     c                   movel     data          hmdcp5
     c                   endsl
     c                   endif
     c                   if        rcdfmt = 'HMFPHUP3'
     c                   select
     c****               when      hfield = 'HMDORP'
     c****               movel     data          hmdorp
     c                   when      hfield = 'HMDPHR'
     c                   movel     data          hmdphr
     c                   when      hfield = 'HMDEGC'
     c                   movel     data          hmdegc
     c                   when      hfield = 'HMDBNK'
     c                   movel     data          hmdbnk
     c                   when      hfield = 'HMDBIL'
     c                   movel     data          hmdBIL
     c                   when      hfield = 'CGP1'
     c                   movel     data          cgp1
     c                   when      hfield = 'HMDCP1'
     c                   movel     data          hmdcp1
     c                   when      hfield = 'HMDCP2'
     c                   movel     data          hmdcp2
     c                   when      hfield = 'HMDCP3'
     c                   movel     data          hmdcp3
     c                   when      hfield = 'HMDLTY'
     c                   movel     data          hmdlty
     c                   when      hfield = 'HMDCP4'
     c                   movel     data          hmdcp4
     c***                   when      hfield = 'HMDCP5'
     c***                   movel     data          hmdcp5
     c                   when      hfield = 'HMDAPR'
     c                   movel     data          hmdapr
     c                   when      hfield = 'HMDLG1'
     c                   movel     data          hmdlg1
     c                   when      hfield = 'HMDLG2'
     c                   movel     data          hmdlg2
     c                   when      hfield = 'HMDLG3'
     c                   movel     data          hmdlg3
     c                   when      hfield = 'HMDMDP'
     c                   movel     data          hmdmdp
     c                   endsl
     c                   endif
     c                   if        rcdfmt = 'HMFPHYAS'
     c     csrrc#        chain     hmfphyas                           78
     c                   if        *in78 = *off
     c                   select
     c                   when      hfield = 'HMAOST'
     c                   movel     data          hmaost
     c                   when      hfield = 'HMAOZ1'
     c                   movel     data          hmaoz1
     c                   when      hfield = 'HMAOZ2'
     c                   movel     data          hmaoz2
      ****               when      hfield = 'HMAOTL'
      ****               movel     data          hmaotl
     c                   when      hfield = 'WSWPHN'
     c                   movel     data          wswphn
      ****               when      hfield = 'HMAOFX'
      ****               movel     data          hmaofx
     c                   when      hfield = 'WSWFAX'
     c                   movel     data          wswfax
     c                   endsl
     c                   update    hmfphyas
     c                   endif
     c                   endif
     c                   if        rcdfmt = 'HMFPHYUP'
     c                   select
     c****               when      hfield = 'HMDORP'
     c****               movel     data          hmdorp
     c                   when      hfield = 'HMDNAM'
     c                   movel     data          hmdnam
     c                   when      hfield = 'HMDTYP'
     c                   movel     data          hmdtyp
     c***                   when      hfield = 'HMDGP#'
     c***                   movel     data          hmdgp#
     c                   when      hfield = 'WSDGP#'
     c                   movel     data          wsdgp#
     c                   when      hfield = 'WSDDOB'
     c                   movel     data          wsddob
     c                   when      hfield = 'HMDSEX'
     c                   movel     data          hmdsex
     c                   when      hfield = 'HMDBTH'
     c                   movel     data          hmdbth
     c                   when      hfield = 'HMDSS#'
     c                   movel     data          hmdss#
     c                   when      hfield = 'HMDSV1'
     c                   movel     data          hmdsv1
     c                   when      hfield = 'HMDSV2'
     c                   movel     data          hmdsv2
     c                   when      hfield = 'HMDDG1'
     c                   movel     data          hmddg1
     c                   when      hfield = 'HMDDG2'
     c                   movel     data          hmddg2
     c                   when      hfield = 'HMDREC'
     c                   movel     data          hmdrec
     c                   when      hfield = 'HMDOMG'
     c                   movel     data          hmdomg
     c                   when      hfield = 'HMDGPN'
     c                   movel     data          hmdgpn
     c                   when      hfield = 'HMAOST'
     c                   movel     data          hmaost
     c                   when      hfield = 'HMAOZ1'
     c                   movel     data          hmaoz1
     c                   when      hfield = 'HMAOZ2'
     c                   movel     data          hmaoz2
      ****               when      hfield = 'HMAOTL'
      ****               movel     data          hmaotl
     c                   when      hfield = 'WSWPHN'
     c                   movel     data          wswphn
      ****               when      hfield = 'HMAOFX'
      ****               movel     data          hmaofx
     c                   when      hfield = 'WSWFAX'
     c                   movel     data          wswfax
     c***                   when      hfield = 'ADDOAD'
     c***                   movel     data          addoad
     c                   when      hfield = 'HMDHA1'
     c                   movel     data          hmdha1
     c                   when      hfield = 'HMDHC1'
     c                   movel     data          hmdhc1
     c                   when      hfield = 'HMDHS1'
     c                   movel     data          hmdhs1
     c                   when      hfield = 'HMDHZ1'
     c                   movel     data          hmdhz1
     c                   when      hfield = 'HMDHE1'
     c                   movel     data          hmdhe1
      ****               when      hfield = 'HMDHT1'
      ****               movel     data          hmdht1
     c                   when      hfield = 'WSHPHN'
     c                   movel     data          wshphn
     c                   when      hfield = 'HMDECN'
     c                   movel     data          hmdecn
      ****               when      hfield = 'HMDECT'
      ****               movel     data          hmdect
     c                   when      hfield = 'WSEPHN'
     c                   movel     data          wsephn
     c                   when      hfield = 'LVLNUM'
     c                   movel     data          lvlnum
     c                   when      hfield = 'WSNPIC'
     c                   movel     data          wsnpic
     c                   when      hfield = 'WSCOSN'
     c                   movel     data          wscosn
     c                   endsl
     c                   endif
     c                   if        rcdfmt = 'HMFTHRPS'
     c     relrcd        chain     hmfthrps                           79
     c                   select
     c                   when      hfield = 'HMTPYR'
     c                   movel     data          hmtpyr
     c                   move      data          hmtpln
     c                   update    hmfthrps
     c                   when      hfield = 'HMTPLN'
     c                   movel     data          hmtpyr
     c                   move      data          hmtpln
     c                   update    hmfthrps
     c                   when      hfield = 'HMTPR#'
     c                   movel     data          hmtpr#
     c                   update    hmfthrps
     c                   when      hfield = 'WSDLCN'
     c                   movel     data          wsdlcn
     c                   if        wsdlcn <> 0
     c     wsdlcn        chain     hxflvl6                            79
     c                   if        *in79 = *off
     c                   movel     hx6nam        wsddsc
     c                   endif
     c                   update    hmfthrps
     c                   endif
     c                   endsl
     c                   endif
     c                   if        rcdfmt = 'HMFTHRUP'
     c                   select
     c****               when      hfield = 'HMDORP'
     c****               movel     data          hmdorp
     c                   when      hfield = 'HMDDR#'
     c                   movel     data          hmddr#
     c                   when      hfield = 'HMDNAM'
     c                   movel     data          hmdnam
     c                   when      hfield = 'HMDSS#'
     c                   movel     data          hmdss#
     c                   when      hfield = 'HMDTYP'
     c                   movel     data          hmdtyp
      ****               when      hfield = 'HMAOTL'
      ****               movel     data          hmaotl
     c                   when      hfield = 'WSWPHN'
     c                   movel     data          wswphn
     c                   when      hfield = 'WSDEFL'
     c                   movel     data          wsdefl
     c                   when      hfield = 'HMDHA1'
     c                   movel     data          hmdha1
     c                   when      hfield = 'HMDHC1'
     c                   movel     data          hmdhc1
     c                   when      hfield = 'HMDHS1'
     c                   movel     data          hmdhs1
     c                   when      hfield = 'HMDHZ1'
     c                   movel     data          hmdhz1
     c                   when      hfield = 'HMDHE2'
     c                   movel     data          hmdhe2
      ****               when      hfield = 'HMDHT1'
      ****               movel     data          hmdht1
     c                   when      hfield = 'WSHPHN'
     c                   movel     data          wshphn
     c                   when      hfield = 'HMDLV5'
     c                   movel     data          hmdlv5
     c                   when      hfield = 'HMDCM3'
     c                   movel     data          hmdcm3
     c                   when      hfield = 'HMDCP5'
     c                   movel     data          hmdcp5
     c                   when      hfield = 'LVLNUM'
     c                   movel     data          lvlnum
     c                   when      hfield = 'WSNPIC'
     c                   movel     data          wsnpic
     c***                   when      hfield = 'HMDGP#'
     c***                   movel     data          hmdgp#
     c                   when      hfield = 'WSDGP#'
     c                   movel     data          wsdgp#
     c                   when      hfield = 'WSCOSN'
     c                   movel     data          wscosn
     c                   endsl
     c                   endif
     c                   if        rcdfmt = 'HMFSTCHG'
     c                   select
     c                   when      hfield = 'WSNTYP'
     c                   movel     data          wsntyp
     c                   endsl
     c                   endif
     c                   endif
      **
     c     nohlp         endsr
      *****************************************************************
     c     NPI_Lv6       begsr

     c     npikey        chain     hmflice                            79
     c                   eval      hlcphy = hmddr#
     c                   eval      hlccod = 'NPI'
     c                   eval      hlcnum = wsnpic
     c                   if        wsnpic <> *blanks
     c                   if        *in79
     c                   write     hmflice
     c                   else
     c                   update    hmflice
     c                   endif
     c                   else
     c                   if        *in79 = *off
     c                   delete    hmflice
     c                   endif
     c                   endif
      **
      * //Flag all records as non-primary first //
     c     hmddr#        setll     hmfthrcp
     c                   dou       *in79 = *on
     c     hmddr#        reade     hmfthrcp                               79
     c                   if        *in79 = *off
     c                   eval      hmtprm = 'N'
     c                   update    hmfthrcp
     c                   endif
     c                   enddo

      * //Make chosen record primary //
     c     clky2         chain     hmfthrco                           79
     c                   eval      hmtprm = 'Y'
     c                   if        hmtdlt = 'D'
     c                   eval      hmtdlt = ' '
     c                   endif
     c                   if        *in79 and lvlnum <> 0
     c                   eval      hmtth# = hmddr#
     c                   eval      hmtlv6 = lvlnum
     c                   write     hmfthrco
     c                   elseif    *in79 = *off
     c                   update    hmfthrco
     c                   endif

     c                   endsr
      *****************************************************************
        begsr srphysgrp;

          // Get physgroup from TMPHYSGRP //
          exec sql
            SELECT LEFT(group_description,20), group_description
            INTO :sdesc, :ldesc
            FROM tmphysgrp
            WHERE group_code = INT(:ecode)
              AND delete_flag <> 'D';

          if ldesc = *blanks;
            tind = 'E';
          endif;

        ENDSR;
      *****************************************************************
      ** checkUserCell - sync physician cell # and physician's user cell #
      *****************************************************************
        begsr checkUserCell;

          if wscphn <> 0 and hmdusr <> *blanks;
            // check if mams cell = user cell
            exec sql
              SELECT phone_number
              INTO :userPhone
              FROM txuphone
              WHERE user_id = :hmdusr;
            if userPhone <> wscphn; // mams phone <> user phone, prompt user
              yorn = 'N';
              exfmt HMFCELLWN;
              if yorn = 'Y';
                //update user cell
                exec sql
                  UPDATE txuphone
                  SET phone_number = :wscphn
                  WHERE user_id = :hmdusr;
              ENDIF;
            ENDIF;

          ENDIF;

        ENDSR;
      *****************************************************************
        begsr NPIsr;
          clear npiDS;
          clear zipCode;
          EXEC SQL SELECT NPNAME,NPMAD1,NPMAD2,NPMCTY,NPMST,NPMZIP,NPMPHN,NPMFAX
           into :NPIds
           FROM TABLE(NPI_HMPNPI(:WSNPIC,DEFAULT,DEFAULT,
                                 DEFAULT,DEFAULT,DEFAULT));
          *in32 = *off;
          if npiDS.name = *blanks;
            npiDuplicated = *off;
            *in32 = *on;
          else;
            zipCode = npiDS.zip;
            exsr isNPIduplSR;
          endif;
        endsr;
      *****************************************************************
        begsr isNPIduplSR;
          if WSNPIC = *blanks;
            exec sql select hlcnum
            into :WSNPIC
            from hmplice
            where hlccod = 'NPI'
            and HLCPHY = :HMDDR#
            fetch first row only;
          endif;

          npiDuplicated = *off;
          EXEC SQL SELECT '1',HLCPHY
          INTO :npiDuplicated,:duplID#
          from hmplice
          inner join hmpmams on HLCPHY = HMDDR#
          where hlccod = 'NPI'
            and hmddel = ''
            and hlcnum = :WSNPIC
            and hlcphy <> :HMDDR#
          fetch first row only;

          if npiDuplicated;
            npiDuplMessage = 'NPI number already exists for ID# ' +
                              %char(duplID#);
            *in32 = *on;
          endif;
        endsr;
      *****************************************************************
       begsr checkNpiRequired;
       HXXAPPPRF( 'HIM' : 'reqPhysicianNPI' : prefdesc );
       reqPhysicianNPI = (prefDesc = 'Y');
       if (reqPhysicianNPI);
         *in32 = *on;
         reqPhysicianNPI = *off;
       endif;
       endsr;
      *****************************************************************
      /EJECT
     ohmfmams   e            unlkms

      **************************************************************************
      ** RemoveNonNumericCharacters - Remove Non Numeric Characters
      **************************************************************************
     p RemoveNonNumericCharacters...
     p                 b

     d RemoveNonNumericCharacters...
     d                 pi
     d  iPhoneNumber                 20
     d  oNumericPhoneNumber...
     d                               10  0
     d  oExtensionNumber...
     d                                6

     d i               s              5  0

     d phoneNumberDs   ds
     d  phoneNumber                  20
     d  phoneNumberCharacter...
     d                                1    overlay(phoneNumber) dim(20)

     d phoneNumberExtensionDS...
     d                 ds            20
     d  numericOnlyPhoneNumber...
     d                               10
     d  extension                     6

      /free
       oNumericPhoneNumber = 0;
       oExtensionNumber = '';

       phoneNumber = iPhoneNumber;

       for i = 1 to 20;
         if phoneNumberCharacter(i) <> ''
            and IsCharacterNumeric(phoneNumberCharacter(i));
           phoneNumberExtensionDS = %trim(phoneNumberExtensionDS) +
               phoneNumberCharacter(i);
         endif;
       endfor;

       if phoneNumberExtensionDS <> '';
         oNumericPhoneNumber = %int(numericOnlyPhoneNumber);
         oExtensionNumber = extension;
       endif;

      /end-free

     p RemoveNonNumericCharacters...
     p                 e

      **************************************************************************
      ** IsCharacterNumeric - Check if the character is numeric (0 through 9)
      **************************************************************************
     p IsCharacterNumeric...
     p                 b

     d IsCharacterNumeric...
     d                 pi              n
     d  iCharacter                    1

     d numericPosition...
     d                 s              5  0
     d numeric         s               n
     d numericValues   c                   '0123456789'

      /free
       numericPosition = %scan(iCharacter : numericValues);
       if numericPosition <> 0;
         numeric = *on;
       endif;

       return numeric;
      /end-free

     p IsCharacterNumeric...
     p                 e

      **************************************************************************

** NEF - NO ERRORS FOUND
No errors found, press ENTER to add record.
No errors found, press ENTER to update record.
No errors found, press ENTER to delete record.
** RNF - RECORD NOT FOUND
Record not found, contact data processing about this problem.
**  EOD - END OF DISPLAY
********* E-O-D **********
**  EOS - END OF SEARCH
Search terminated, enter a more specific request to continue.
**  NPF - NO PHYSICIANS/STAFF FOUND
No staff members found for current request.
No physicians found for current request.
No additional groups found for current request.
**  RQE - REQUIRED ENTRY
Request entry is required for NAME, GROUP#, OR TYPE.
Request entry is required for NAME OR EMPLOYEE #.
**  IOP - INVALID OPTION
Invalid option entered  (SEE WINDOW OPTIONS).
Option invalid for staff member (only RF, PH or AT).
Option invalid for physician (only TH or CM).
**  ILN - INVALID LINE NUMBER
Line number is missing or invalid.
**  ILC - INVALID LOCATION NUMBER
Location number is invalid.
**  DTP - DELETED PHYSICIAN
Physician/Staff member has been deleted.
**  PND - NOT DELETED
Physician/Staff member not deleted.
Delete Failed. Delete related user in User Security Maintenance.
Reinstate Failed. Reinstate related user in User Security Maintenance.
Reinstate Failed. NPI number already exists for ID#
**  RNP - REINSTATED PHYSICIAN
Physician/Staff member has been reinstated.
** SMS - STATUS MESSAGES
Errors occurred, press HELP to display.
Update pending, press ENTER to accept or F3 to cancel.
** CMS - UPDATE MESSAGES
Doctor Master Updated.
Physician information updated.
Physician Added.
** FIU - FILE IN USE
File in use.  Please retry in a few minutes.
** NTF - NO PROVIDER #S FOUND
No provider #s found for this staff member.
** NNF - NO PLANS FOUND FOR PHYSICIAN
No participating plans found for this physician.
** NAO - NOT AUTHORIZED
User not authorized to option - check with supervisor.
** IPP - INVALID PAYOR OR PLAN
Invalid payor and/or plan.
** EMS1 - ERROR MESSAGES FOR SCREEN 1
Physician name is missing or invalid.                         1
Doctor Number is missing or invalid.                          2
Group Number is invalid.                                      3
Type is missing or invalid.                                   4
Birthdate is missing or invalid.                              5
Sex is missing or invalid. (s/b  M or F).                     6
Birthplace is missing or invalid.                             7
Medical service is missing or invalid.                        8
Secondary service is missing or invalid.                      9
Degree is missing.                                            10
is invalid.                                                   11
Receptionist is missing.                                      12
Office Manager is missing.                                    13
License state is invalid.                                     14
Pract/Group name is missing.                                  15
Office address 1 is missing.                                  16
Office city 1 is missing.                                     17
Office state 1 is missing or invalid.                         18
Office zip 1 is missing or invalid.                           19
Office phone 1 is missing.                                    20
Show more information field is invalid.                       21
User ID is missing or duplicate.                              22
Graduation year is missing or incorrect.                      23
Graduation year is missing or incorrect.                      24
Degree is incorrect or missing.                               25
Degree is incorrect or missing.                               26
From date is incorrect or missing.                            27
To date is incorrect or missing.                              28
Medical service number is missing or incorrect.               29
Type is missing or invalid.                                   30
Allowed to admit patients is incorrect or missing.            31
National Provider Identifier is invalid                       32
is invalid.                                                   33
                                                              34
Co-signer requirement is incorrect or missing.(Y/N)           35
Pract/Group name needed when Co-signer required.              36
Ordering privileges is invalid (Y or N).                      37
                                                              38
                                                              39
Supervising Clinician is invalid.                             40
                                                              41
                                                              42
                                                              43
                                                              44
                                                              45
Home address is missing.                                      46
Home city is missing.                                         47
Home state is missing or invalid.                             48
Home zip is missing or invalid.                               49
Home telephone is missing.                                    50
                                                              51
                                                              52
                                                              53
Emergency contact is misssing.                                54
Emergency contact telephone is missing.                       55
UPIN# is missing.                                             56
** EMS2 - ERROR MESSAGES FOR SCREEN 2
Staff number is missing, invalid, or already exists.          1
Staff name is missing or invalid.                              2
Social Security Number is missing.                             3
Contractor flag is invalid(Y or N).                            4
Address is missing.                                            5
City is missing.                                               6
State is missing or invalid.                                   7
Zip code is missing or invalid.                                8
Work telephone number is missing.                              9
License state is missing or invalid.                          10
is invalid.                                                   11
Home telephone number is missing.                             12
User ID is missing or duplicate.                              13
Graduation year is missing or incorrect.                      14
Graduation year is missing or incorrect.                      15
Degree is incorrect or missing.                               16
Degree is incorrect or missing.                               17
                                                              18
                                                              19
                                                              20
From date is incorrect or missing.                            21
To date is incorrect or missing.                              22
Medical service number is missing or incorrect.               23
Staff type is missing or invalid.                             24
Staff number is missing or a duplicate.                       25
                                                              26
At least one registration number is required.                 27
Duplicate state/disciplines exist.                            28
Registration number is invalid.                               29
is missing or invalid.                                        30
Expiration date is invalid.                                   31
National Provider Identifier is invalid                       32
is invalid.                                                   33
Show more information field is invalid.                       34
Co-signer requirement is incorrect or missing.(Y/N)           35
Pract/Group name needed when Co-signer required.              36
Ordering privileges is invalid (Y or N).                      37
                                                              38
                                                              39
Supervising Clinician is invalid.                             40
                                                              41
Invalid therapist code.                                       42
Pract/Group name is missing.                                  43
Group Number is invalid.                                      44
** EMS3 - ERROR MESSAGES FOR SCREEN 3
Date of hire is incorrect or missing.                         1
Pre-Hire is missing or incorrect.                             2
Contract missing or incorrect.(Y/N)                           3
Expiration date is incorrect or missing.                      4
The data bank issue is incorrect or missing.                  5
Query date is incorrect or missing.                           6
Rcv date is incorrect or missing.                             7
Allowed to admit patients is incorrect or missing.            8
Billable is incorrect or missing.                             9
Doctor # is missing or incorrect.                             10
Doctor # is missing or incorrect.                             11
Doctor # is missing or incorrect.                             12
Doctor # is missing or incorrect.                             13
Doctor # is missing or incorrect.                             14
Type of practice is missing or incorrect.                     15
Medicare Participation is missing or incorrect.               16
Accept patient referrals is missing or incorrect.             17
Language code is missing or incorrect.                        18
Language code is missing or incorrect.                        19
Language code is missing or incorrect.                        20
Liability insur. is incorrect or missing.                     21
Low amount is incorrect or missing.                           22
High amount is incorrect or missing.                          23
Issue Date is incorrect or missing.                           24
Ins type is incorrect or missing.                             25
Expiration date is incorrect or missing.                      26
Covering division is missing or invalid.                      27
                                                              28
                                                              29
Type is missing or invalid.                                   30
                                                              31
                                                              32
                                                              33
                                                              34
Co-signer requirement is incorrect or missing.(Y/N)           35
Pract/Group name needed when Co-signer required.              36
Ordering privilegs is invalid (Y or N).                       37
                                                              38
                                                              39
                                                              40
                                                              41
                                                              42
                                                              43
                                                              44
                                                              45
Home address is missing.                                      46
Home city is missing.                                         47
Home state is missing or invalid.                             48
Home zip is missing or invalid.                               49
Home telephone is missing.                                    50
                                                              51
                                                              52
                                                              53
Emergency contact is misssing.                                54
Emergency contact telephone is missing.                       55
UPIN# is missing.                                             56
** EMS4 - ERROR MESSAGES
Group is already associated with this staff.                   1
Group cannot be blank when adding a new group.
**  DLT - DELETE MESSAGES
Delete effective date is missing or invalid.
Delete effective date can not be before system date.
**  PRT - PHYSICIAN PROFILE PRINTED
Physician profile has been submitted for printing.
Staff member profile has been submitted for printing.
**  TAD - THERAPIST/AIDE ADDED.
Staff member Added.
**  TUP - THERAPIST/AIDE UPDATED.
Staff member Updated.
** UPDT
The highlighted fields are in error - please correct.
No errors exist - press ENTER to update.
** MPR
Provider # is missing.
** DAE - DOCTOR ALREADY EXISTS
Doctor already exists, record not written.
Doctor already exists, please select another.
** NKE - NAME KEYING ERROR
The search name was keyed incorrectly. (LAST, FIRST)
** TCM - TYPE CHANGE ERROR
Staff Type cannot be blank.
Staff Type is invalid.
Staff Type entered is the same as the Current Type.
Staff Type 'RF' can only be changed to 'PH'
Staff Type 'PH' cannot be changed.
Current Staff Type cannot be changed to 'RF' or 'PH'.
** TNC - TYPE CHANGE ERROR
Staff Type not changed.
