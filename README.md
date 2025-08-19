# StarTrek-in-COBOL
COBOL version of the classic StarTrek game

To run this in Linux be sure you have the GNU Cobol compiler.

GnuCOBOL (formerly OpenCOBOL) is a free, modern COBOL compiler. GnuCOBOL
implements a substantial part of the COBOL 85, COBOL 2002 and COBOL 2014
standards and X/Open COBOL, as well as many extensions included in other COBOL
compilers (IBM COBOL, MicroFocus COBOL, ACUCOBOL-GT and others).

GnuCOBOL translates COBOL into C and compiles the translated code using a
native C compiler.

You can compile it with the following command:

    $ cobc -x strek.cbl 
    <command-line>: warning: "_FORTIFY_SOURCE" redefined
    <command-line>: note: this is the location of the previous definition
    $

Execution is simply

    $ ./strek
