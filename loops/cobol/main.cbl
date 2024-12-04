       IDENTIFICATION DIVISION.
       PROGRAM-ID. looopy.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Input number from command line
       01 INPUT-NUMBER              PIC 9(05) VALUE ZERO.
      * Random number
       01 RANDOM-NUMBER             PIC 9(06) VALUE zero.
      * Array of 10,000 elements
       01 ARRAY                     OCCURS 10000 TIMES INDEXED BY IDX.
           05 ELEMENT               PIC S9(09) COMP VALUE ZERO.
      * Loop counters
       01 I                         PIC 9(05) VALUE 1.
       01 J                         PIC 9(06) VALUE 1.
      * Temporary variables
       01 MODULO-RESULT             PIC S9(05) COMP.
       01 ELEMENT-RESULT            PIC S9(10) COMP.

       PROCEDURE DIVISION.
       MAIN-PROCESS.
           COMPUTE INPUT-NUMBER = FUNCTION RANDOM * 10000
           COMPUTE RANDOM-NUMBER = FUNCTION RANDOM * 10000

      * Outer loop: 10,000 iterations
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10000
      * Inner loop: 100,000 iterations
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 100000
                   ADD function MOD(J, INPUT-NUMBER)
                   TO ELEMENT OF ARRAY (I)
               END-PERFORM
      * Add random number to the current element
               ADD RANDOM-NUMBER TO ELEMENT OF ARRAY (I)
           END-PERFORM.

      * Retrieve and print the element at the random index
           MOVE ELEMENT OF ARRAY (RANDOM-NUMBER + 1) TO ELEMENT-RESULT
           DISPLAY ELEMENT-RESULT
           STOP RUN.

       END PROGRAM looopy.

