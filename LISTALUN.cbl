      ******************************************************************
      * Author: Breno Marques
      * Date: 25/01/2024
      * Purpose: listar alunos salvos num arquivo
      * Tectonics: cobc Linguagem: COBOL
      * Complexidade: C
      *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LISTALUN.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.
            INPUT-OUTPUT SECTION.
            FILE-CONTROL.
                SELECT ALUNO ASSIGN TO
                'D:\Curso 1 COBOL\Desafio modulo 3\CFP001S1.DAT'
                ORGANISATION IS INDEXED
                ACCESS  MODE IS SEQUENTIAL
                RECORD KEY IS ID-ALUNO
                FILE STATUS IS WS-FILES.

       DATA DIVISION.
       FILE SECTION.
       FD ALUNO.
          COPY CFPK0001.

       WORKING-STORAGE SECTION.
       01 WS-ALUNO                    PIC X(32) VALUE SPACES.
       01 FILLER REDEFINES WS-ALUNO.
          03 WS-ID-ALUNO                 PIC 9(03).
          03 WS-NM-ALUNO                 PIC X(20).
          03 WS-TL-ALUNO.
                   05 WS-PREFIXO         PIC 9(05).
                   05 WS-SUFIXO          PIC 9(04).
       77 WS-FILES                       PIC 99.
          88 FILES-OK                    VALUE 0.
       77 WS-EOF                         PIC X.
          88 EOF-OK                      VALUE 'S' FALSE 'N'.
       77 WS-EXIT                        PIC X.
          88 EXIT-OK                     VALUE 'F' FALSE 'N'.
       77 WS-CONT                        PIC 9(003) VALUE ZEROS.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY '** lista de alunos'
            SET EXIT-OK              TO FALSE
            PERFORM P1-LISTAR    THRU P1-FIM UNTIL EXIT-OK
            PERFORM P0-FIM


            .
       P1-LISTAR.
            SET EOF-OK                TO FALSE
            SET FILES-OK              TO TRUE
            SET WS-CONT               TO 0.

            OPEN INPUT ALUNO
            IF FILES-OK THEN
            PERFORM UNTIL EOF-OK
                IF FILES-OK THEN
                    READ ALUNO INTO WS-ALUNO

                         AT END
                            SET EOF-OK TO TRUE
                         NOT AT END
                             ADD 1     TO WS-CONT
                             DISPLAY'REGISTRO '
                                     WS-CONT
                                     ': '
                                     WS-ID-ALUNO
                                     ' - '
                             FUNCTION TRIM(WS-NM-ALUNO)
                                     ' - Tel: '
                                     WS-PREFIXO
                                     '-'
                                     WS-SUFIXO

                    END-READ
                  END-PERFORM
            ELSE
                DISPLAY 'ERRO AO ABRIR ARQUIVO DE ALUNOS'
                DISPLAY 'FILE STATUS: ' WS-FILES
            END-IF

            CLOSE ALUNO

            DISPLAY
              'TECLE: '
              '<QUALQUER TECLA> para continuar ou <F> para finalizar.'
              ACCEPT WS-EXIT
              IF WS-EXIT = 'f'
                       MOVE 'F'       TO WS-EXIT
              END-IF


            .

       P1-FIM.


       P0-FIM.
            STOP RUN.
       END PROGRAM LISTALUN.
