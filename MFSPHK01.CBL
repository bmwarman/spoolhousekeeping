000100 IDENTIFICATION DIVISION.
000300 PROGRAM-ID. MFSPHK01.
003100***************************************************************
      *Github
004600 ENVIRONMENT DIVISION.
004700 input-output section.
       file-control.
           select infile assign to ifile organization is sequential
                  file status is status-codei.
           select outfile assign to ofile organization is sequential
                  file status is status-codeo.
       DATA DIVISION.
       file section.
       fd  infile.
       01  inrec                         pic x(80).
       fd  outfile.
       01  outrec                        pic x(120).
       WORKING-STORAGE SECTION.
       01 status-codei                   pic xx.
       01 status-codeo                   pic xx.
       01 ws-message                     pic x(120).
       01  spool-hk-settings.
           copy spoolhk.
       01  ws-mvsp0027-rc                pic xx comp-5.
       01  ws-mvsp0027-reason            pic xx.
       01  disp-num                      pic 99.
       PROCEDURE DIVISION.

      *****************************************************************
      *                                                               *
      *****************************************************************
       0000-MAINLINE-MODULE.
      *    Perform FILE-INPUT
           move 0 to ws-mvsp0027-rc 
           call 'mvsp0027' using spool-hk-settings
                                ws-mvsp0027-rc
                                ws-mvsp0027-reason
           move spaces to ws-message
           evaluate ws-mvsp0027-rc
             when 0
               string
                    'SPOOL CLEAN-UP ENDED NORMALLY' delimited by size
                    ' maximum retain period is '    delimited by size
                     ws-keep-for                    delimited by size
                     ' days. '                      delimited by size
               into ws-message
             when 4
               string
                    'SPOOL CLEAN-UP ENDED, '        delimited by size
                    ' empty CASSPOOL/MVSSPOOL  '    delimited by size
                    ' maximum retain period is '    delimited by size
                     ws-keep-for                    delimited by size
                     ' days. '                      delimited by size
               into ws-message
           when other
               move  ws-mvsp0027-rc  to disp-num
               string
                    'SPOOL clean-up failed RC = '  delimited by size
                     disp-num                      delimited by size
                    ' reason code = '              delimited by size
                     ws-mvsp0027-reason            delimited by size
               into ws-message
           end-evaluate
           display ws-message upon console
           goback.

       FILE-INPUT SECTION.
           perform open-infile
           perform read-infile
           PERFORM UNTIL status-codei NOT = '00'
              *> populate mvsp0027 parms
              perform read-infile
           end-perform
           .

       open-infile section.
           open input infile
           if status-codei = '00'
               continue
           else
               move 12 to return-code
               goback
           end-if
           .
       read-infile section.
           read infile
           evaluate status-codei
               when '00'
               when '10'
                   continue
               when other
                   move 16 to return-code
                   goback
           end-evaluate
           .
