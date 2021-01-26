;-------------------------------------; ;-------------------------------------; ;-------------------------------------;
;                                     ; ;                                     ; ;                                     ;
;   For Genexi2 :)                    ; ;   It is not required to read or     ; ;   This document is loveware.        ;
;                                     ; ;   study the Global variables,       ; ;                                     ;
;   This bundle includes:             ; ;   Types or functions.               ; ;   You may take advantage of all     ;
;                                     ; ;                                     ; ;   contained material in any way     ;
;   * Custom Print Library            ; ;   The documentation is provided     ; ;   you want, as long as it will      ;
;   * Custom Print Documentation      ; ;   to explain all that the user      ; ;   help you on your way, as well     ;
;   * Binary Data Tutorial            ; ;   needs to know.                    ; ;   as others. The type and amount    ;
;   * Binary Data Library             ; ;                                     ; ;   of credit you will give me,       ;
;   * Binary Data Documentation       ; ;   Have fun =)                       ; ;   I'll leave up to you ;)           ;
;                                     ; ;                                     ; ;                                     ;
;-------------------------------------; ;-------------------------------------; ;-------------------------------------;

;----------------------------------------------------------------------------------------------------------------------;
; -------------------------------------------------------------------------------------------------------------------- ;
; *                                                                                                                  * ;
; *                                                                                                                  * ;
; *   Custom Print Library                                                                                           * ;
; *                                                                                                                  * ;
; *                                                                                                                  * ;
; -------------------------------------------------------------------------------------------------------------------- ;
;----------------------------------------------------------------------------------------------------------------------;
; Created by TheChange on 16 Aug 2002 at 0000 <TheChange@yahoo.com>
;----------------------------------------------------------------------------------------------------------------------;

;----------------------------------------------------------------------------------------------------------------------;
; Definition
;----------------------------------------------------------------------------------------------------------------------;

  Type PrintType
    Field X
    Field Y
    Field SizeX
    Field SizeY
    Field Red
    Field Green
    Field Blue
    Field ScreenSizeX
    Field ScreenSizeY
    Field BehindCursor
  End Type

;----------------------------------------------------------------------------------------------------------------------;
; Initialization
;----------------------------------------------------------------------------------------------------------------------;

  Global PrintObject.PrintType

  PrintReset

;----------------------------------------------------------------------------------------------------------------------;
; User functions
;----------------------------------------------------------------------------------------------------------------------;

  Function PrintReset ()
    If PrintObject = Null
      PrintObject = New PrintType
      PrintObject\Red   = 32
      PrintObject\Green = 192
      PrintObject\Blue  = 32
    EndIf
    PrintObject\SizeX = FontWidth ()
    PrintObject\SizeY = FontHeight ()
    PrintObject\ScreenSizeX = GraphicsWidth ()
    PrintObject\ScreenSizeY = GraphicsHeight ()
    FreeImage PrintObject\BehindCursor
    PrintObject\BehindCursor = CreateImage ( PrintObject\SizeX , PrintObject\SizeY )
  End Function

  Function PrintHome ()
    PrintObject\X = 0
    PrintObject\Y = 0
  End Function

  Function PrintUp ()
    PrintObject\Y = PrintObject\Y - PrintObject\SizeY
    If PrintObject\Y < 0
      PrintObject\Y = 0
    EndIf
  End Function

  Function PrintDown ()
    PrintObject\Y = PrintObject\Y + PrintObject\SizeY
    If PrintObject\Y >= PrintObject\ScreenSizeY - PrintObject\SizeY
      PrintObject\Y = PrintObject\Y - PrintObject\SizeY
      PrintScroll
    EndIf
  End Function

  Function PrintLeft ()
    PrintObject\X = PrintObject\X - PrintObject\SizeX
    If PrintObject\X < 0
      PrintObject\X = 0
    EndIf
  End Function

  Function PrintRight ()
    PrintObject\X = PrintObject\X + PrintObject\SizeX
    If PrintObject\X >= PrintObject\ScreenSizeX
      PrintObject\X = 0
      PrintDown
    EndIf
  End Function

  Function PrintText ( Text$ = "" )
    Local PrintText$
    Local MovePos
    Local LineFeed

    If Right ( Text , 1 ) = "\"
      MovePos = True
      LineFeed = False
      PrintText = Left ( Text , Len ( Text ) - 1 )
    ElseIf Right ( Text , 1 ) = "/"
      MovePos = False
      LineFeed = False
      PrintText = Left ( Text , Len ( Text ) - 1 )
    Else
      MovePos = True
      LineFeed = True
      If Right ( Text , 1 ) = "|"
        PrintText = Left ( Text , Len ( Text ) - 1 )
      Else
        PrintText = Text
      EndIf
    EndIf

    Color PrintObject\Red , PrintObject\Green , PrintObject\Blue
    Text PrintObject\X , PrintObject\Y , PrintText

    If MovePos
      PrintObject\X = PrintObject\X + StringWidth ( PrintText )
    EndIf

    If PrintObject\X >= PrintObject\ScreenSizeX
      PrintObject\X = 0
    EndIf
    If LineFeed
      PrintObject\X = 0
      PrintDown
    EndIf
  End Function

  Function PrintScroll ()
    CopyRect 0 , PrintObject\SizeY , PrintObject\ScreenSizeX , PrintObject\ScreenSizeY - PrintObject\SizeY , 0 , 0
    Color 0 , 0 , 0
    Rect 0 , PrintObject\ScreenSizeY - PrintObject\SizeY , PrintObject\ScreenSizeX , PrintObject\SizeY , True
  End Function

  Function PrintLineFeed ()
    PrintDown
    PrintObject\X = 0
  End Function

  Function PrintBackSpace ()
    PrintLeft
    Color 0 , 0 , 0
    Rect PrintObject\X , PrintObject\Y , PrintObject\SizeX , PrintObject\SizeY , True
  End Function

  Function PrintClear ( Chars% = 1 )
    Color 0 , 0 , 0
    Rect PrintObject\X , PrintObject\Y , PrintObject\SizeX * Chars , PrintObject\SizeY , True
  End Function

  Function PrintX% ()
    Return PrintObject\X
  End Function

  Function PrintY% ()
    Return PrintObject\Y
  End Function

  Function PrintShowCursor ( Invert% = False )
    Local SourceBuffer
    Local DestBuffer

    If Not Invert
      SourceBuffer = 0
      DestBuffer = ImageBuffer ( PrintObject\BehindCursor )
      CopyRect PrintObject\X , PrintObject\Y , PrintObject\SizeX , PrintObject\SizeY , 0 , 0 , SourceBuffer , DestBuffer
      Color PrintObject\Red , PrintObject\Green , PrintObject\Blue
      Rect PrintObject\X , PrintObject\Y , PrintObject\SizeX , PrintObject\SizeY , True
    Else
      PrintInvertRect PrintObject\X , PrintObject\Y , PrintObject\SizeX , PrintObject\SizeY
    EndIf
  End Function

  Function PrintHideCursor ( Invert% = False )
    If Not Invert
      DrawBlock PrintObject\BehindCursor , PrintObject\X , PrintObject\Y
    Else
      PrintInvertRect PrintObject\X , PrintObject\Y , PrintObject\SizeX , PrintObject\SizeY
    EndIf
  End Function

  Function PrintInvertRect ( PosX% , PosY% , SizeX% , SizeY% )
    Local DestX
    Local DestY
    Local RGB
    Local R
    Local G
    Local B
    Local X
    Local Y

    DestX = PosX + SizeX
    DestY = PosY + SizeY
    LockBuffer
    For X = PosX To DestX
      For Y = PosY To DestY
        RGB = ReadPixelFast ( X , Y ) And $FFFFFF
        R = 255 - RGB Shr 16 And 255
        G = 255 - RGB Shr 8 And 255
        B = 255 - RGB And 255
        WritePixelFast X , Y , R Shl 16 + G Shl 8 + B
      Next
    Next
    UnlockBuffer
  End Function

  Function PrintColor ( Red% , Green% , Blue% )
    PrintObject\Red = Red
    PrintObject\Green = Green
    PrintObject\Blue = Blue
  End Function

;----------------------------------------------------------------------------------------------------------------------;
; Features
;----------------------------------------------------------------------------------------------------------------------;
;
;   Virtual cursor support
;   Full cursor positioning control
;   Screen scrolling
;   Customizable restrictions
;   Color support
;
;----------------------------------------------------------------------------------------------------------------------;
; Documentation
;----------------------------------------------------------------------------------------------------------------------;
;
;   Function PrintReset ()
;
;     Function:
;
;       Resets and initializes the Custom Print system.
;       Call this command every time you change any fundamental visual components, such as:
;        - Graphics resolution
;        - Font typeface
;
;     Parameters:  None.
;
;     Returns:  Nothing.
;
;     Example:  PrintReset
;
;----------------------------------------------------------------------------------------------------------------------;
;
;   Function PrintHome ()
;
;     Function:  Moves the Print Cursor to the upperleft corner of the screen.
;
;     Parameters:  None.
;
;     Returns:  Nothing.
;
;     Example:  PrintHome
;
;----------------------------------------------------------------------------------------------------------------------;
;
;   Function PrintUp ()
;
;     Function:  Moves the Print Cursor one character up.
;
;     Parameters:  None.
;
;     Returns:  Nothing.
;
;     Example:  PrintUp
;
;----------------------------------------------------------------------------------------------------------------------;
;
;   Function PrintDown ()
;
;     Function:  Moves the Print Cursor one character down.
;
;     Parameters:  None.
;
;     Returns:  Nothing.
;
;     Example:  PrintDown
;
;----------------------------------------------------------------------------------------------------------------------;
;
;   Function PrintLeft ()
;
;     Function:  Moves the Print Cursor one character left.
;
;     Parameters:  None.
;
;     Returns:  Nothing.
;
;     Example:  PrintLeft
;
;----------------------------------------------------------------------------------------------------------------------;
;
;   Function PrintRight ()
;
;     Function:  Moves the Print Cursor one character right.
;
;     Parameters:  None.
;
;     Returns:  Nothing.
;
;     Example:  PrintRight
;
;----------------------------------------------------------------------------------------------------------------------;
;
;   Function PrintText ( Text$ = "" )
;
;     Function:  Prints any line of text at the current cursor position.
;
;     Parameters:
;
;       Text string (optional).
;
;         The line of text to use for display. Normally the text will be printed followed by a linefeed.
;         However there are 3 unique trailing characters serving special functions:
;          - Backslash     "\": Omit the linefeed. All text will be appended.
;          - Forward slash "/": Prevent the cursor from moving. Whatever you print, the cursor will not move.
;          - Pipe          "|": Normal printing. Use this if you want to print any of these special characters on
;                               screen last, as these special characters will be stripped from the output when last.
;         Omitting the text string will simply print a linefeed.
;         Note: The text being printed always has a transparent background.
;
;     Returns:  Nothing.
;
;     Examples:
;
;       PrintText       ; Print/append a linefeed.
;       PrintText "\|"  ; Print "\" with a linefeed.
;       PrintText "/|"  ; Print "/" with a linefeed.
;       PrintText "||"  ; Print "|" with a linefeed.
;       PrintText "x\"  ; Print "x" without a linefeed.
;       PrintText "x/"  ; Print "x" without moving the cursor.
;       PrintText "x|"  ; Same as PrintText "x".
;
;----------------------------------------------------------------------------------------------------------------------;
;
;   Function PrintScroll ()
;
;     Function:  Scrolls the screen up one line of characters.
;
;     Parameters:  None.
;
;     Returns:  Nothing.
;
;     Example:  PrintScroll
;
;----------------------------------------------------------------------------------------------------------------------;
;
;   Function PrintLineFeed ()
;
;     Function:  Prints/appends a linefeed. The same as PrintText without parameters.
;
;     Parameters:  None.
;
;     Returns:  Nothing.
;
;     Example:  PrintLineFeed
;
;----------------------------------------------------------------------------------------------------------------------;
;
;   Function PrintBackSpace ()
;
;     Function:  Removes the character space in front of the cursor and moves the cursor back one place.
;
;     Parameters:  None.
;
;     Returns:  Nothing.
;
;     Example:  PrintBackSpace
;
;----------------------------------------------------------------------------------------------------------------------;
;
;   Function PrintClear ( Chars% = 1 )
;
;     Function:  Prints a specified number of (overwriting) spaces at the cursor position without moving the cursor.
;
;     Parameters:
;
;       Number of characters.
;
;         This is the number of overwriting spaces. The default number of characters is 1, when omitting this parameter.
;
;     Returns:  Nothing.
;
;     Examples:
;
;       PrintClear               ; Clears one character space at the cursor.
;       PrintClear 10            ; Clears ten character spaces at the cursor.
;       PrintClear Len ( Text )  ; Clears the number of characters in the specified text string.
;
;----------------------------------------------------------------------------------------------------------------------;
;
;   Function PrintX% ()
;
;     Function:  Returns the current horizontal cursor position.
;
;     Parameters:  None.
;
;     Returns:  The current horizontal (X) cursor position.
;
;     Example:  RememberX = PrintX ()
;
;----------------------------------------------------------------------------------------------------------------------;
;
;   Function PrintY% ()
;
;     Function:  Returns the current vertical cursor position.
;
;     Parameters:  None.
;
;     Returns:  The current vertical (Y) cursor position.
;
;     Example:  RememberY = PrintY ()
;
;----------------------------------------------------------------------------------------------------------------------;
;
;   Function PrintShowCursor ( Invert% = False )
;
;     Function:
;
;       Displays an overlapping cursor at the current cursor position. The permanently drawn cursor will stay at the
;       drawn location until you call PrintHideCursor at the same cursor position. See the PrintHideCursor command for
;       more information and an example displaying these commands in action. Do not let the cursor go beyond the
;       dimensions of the screen when using this command, which normally cannot happen anyway.
;
;     Parameters:
;
;       Invert or overwrite cursor.
;
;         Specifying a True value as parameter will invert the color of the background to display the cursor.
;         Specifying a False value or omitting the parameter will result in an overlapping cursor in the current Print
;         Color.
;
;     Returns:  Nothing.
;
;     Example:  See the PrintHideCursor example.
;
;----------------------------------------------------------------------------------------------------------------------;
;
;   Function PrintHideCursor ( Invert% = False )
;
;     Function:
;
;       Removes the overlapping cursor at the current cursor position. Performing this command on a location where no
;       visible cursor resides can lead to the display of multiple simultaneous visible cursors. Also do not let the
;       cursor go beyond the dimensions of the screen when using this command, which normally cannot happen anyway.
;
;     Parameters:
;
;       Invert or overwrite cursor.
;
;         Specifying a True value as parameter will invert the color of background back to the original color.
;         Specifying a False value or omitting the parameter will remove the overlapping cursor at the current cursor
;         position. A temporary image is used to remember what was under the cursor.
;
;     Returns:  Nothing.
;
;     Example:
;
;       ;:. Tiny text editor .:;
;       Graphics 640 , 480
;       PrintReset
;       PrintShowCursor True             ; Draw a visible cursor before editing.
;       Repeat
;         Key = GetKey ()
;         If Key
;           PrintHideCursor True         ; Hide the visible cursor before moving the print cursor.
;           PrintText Chr ( Key ) + "/"
;           PrintRight
;           PrintShowCursor True         ; Show the visible cursor after moving the print cursor.
;         End If
;       Until Key = 27
;       End
;
;----------------------------------------------------------------------------------------------------------------------;
;
;   Function PrintInvertRect ( PosX% , PosY% , SizeX% , SizeY% )
;
;     Function:
;
;       This is the function used to display the inverted cursor. You can use this function yourself to invert a
;       specific part of the current drawing buffer. As this function is very slow, like most pixel operations, do not
;       apply it to large portions of the screen. Be careful with this function, as it can crash your program when
;       drawing beyond the dimensions of the screen.
;
;     Parameters:
;
;       PosX - The upperleft horizontal (X) coordinate of the rectangular area to invert.
;       PosY - The upperleft vertical (Y) coordinate of the rectangular area to invert.
;       SizeX - The horizontal (X) span or size of the regular area to invert.
;       SizeY - The vertical (Y) span or size of the regular area to invert.
;
;     Returns:  Nothing
;
;     Example:
;
;       PrintInvertRect 0 , 0 , GraphicsWidth () , GraphicsHeight ()          ; Inverts the whole screen.
;       PrintInvertRect PrintX () , PrintY () , FontWidth () , FontHeight ()  ; Manually inverting a cursor.
;
;----------------------------------------------------------------------------------------------------------------------;
;
;   Function PrintColor ( Red% , Green% , Blue% )
;
;     Function:  Sets the default Print Color, which is also the color used for the visible (non-inverted) cursor.
;
;     Parameters:
;
;       The parameters used are exactly the same as those for the Blitz native Color command:
;       Red   - The Red color component.
;       Green - The Green color component.
;       Blue  - The Blue color component.
;       Together these color components make a combined RGB color.
;
;     Returns:  Nothing.
;
;     Example:
;
;       PrintColor 255 ,   0 ,   0  ; Red.
;       PrintColor   0 , 255 ,   0  ; Green.
;       PrintColor   0 ,   0 , 255  ; Blue.
;       PrintColor 255 , 255 , 255  ; White.
;       PrintColor 127 , 127 , 127  ; Grey.
;       PrintColor 255 , 255 ,   0  ; Yellow.
;       PrintColor   0 , 255 , 255  ; Cyan.
;       PrintColor 255 ,   0 , 255  ; Purple.
;       PrintColor 255 , 127 ,   0  ; Orange.
;       PrintColor   0 , 223 , 159  ; Creme green.
;       PrintColor   0 , 191 , 255  ; Creme blue.
;
;----------------------------------------------------------------------------------------------------------------------;
; Example of a small text editor.
;----------------------------------------------------------------------------------------------------------------------;
;
;   Graphics 800 , 600 , 0 , 2
;   PrintReset
;   PrintColor 32 , 224 , 32
;   PrintShowCursor
;   Repeat
;     Key = GetKey ()
;     If Key
;       PrintHideCursor
;       Select Key
;       Case 8  : PrintBackSpace
;       Case 13 : PrintLineFeed
;       Case 27 : Exit
;       Case 28 : PrintUp
;       Case 29 : PrintDown
;       Case 31 : PrintLeft
;       Case 30 : PrintRight
;       Default
;         PrintClear
;         PrintText Chr ( Key ) + "/"
;         PrintRight
;       End Select
;       PrintShowCursor
;     EndIf
;   Forever
;   End
;
;----------------------------------------------------------------------------------------------------------------------;

;----------------------------------------------------------------------------------------------------------------------;
; -------------------------------------------------------------------------------------------------------------------- ;
; *                                                                                                                  * ;
; *                                                                                                                  * ;
; *   Binary Data Library                                                                                            * ;
; *                                                                                                                  * ;
; *                                                                                                                  * ;
; -------------------------------------------------------------------------------------------------------------------- ;
;----------------------------------------------------------------------------------------------------------------------;
; Created by TheChange on 20 Aug 2002 at 0000 <TheChange@yahoo.com>
;----------------------------------------------------------------------------------------------------------------------;

;----------------------------------------------------------------------------------------------------------------------;
; Tutorial
;----------------------------------------------------------------------------------------------------------------------;
;
;   The method used will be explained briefly.
;
;   The original binary files, such as sound and graphics files, will be converted to Data statements. These lines of
;   data will be placed inside the BlitzBasic file, also including the rest of the game. When this game is run, a
;   special function will temporarily extract the Data to external files, in fact, the same as the original binary
;   files. Whether you keep the files for later access, or delete them right away, is up to you.
;
;   There are 3 steps required before being able to use a binary file, such as sound or graphics, from inside a
;   BlitzBasic source file.
;
;     Step 1: Converting the binary file(s) to Data statements.
;     Step 2: Adding the Data statements and the functions to unpack them to the BlitzBasic file.
;     Step 3: Creating the functionality to unpack the binary files at run-time.
;
;  ------------------------------------------------------------------------------------------------------------------
;   Step 1
;
;     Suppose we want to convert Image.BMP and Sound.WAV to Data statements. We'd first need the functions of the
;     file you're currently reading. The easiest way is to save the file as a BlitzBasic file, such as BinaryData.BB
;
;     Then, creating a new BlitzBasic file called something like Convert.BB which would use the BinaryData file.
;     To do this, the code you'd put in the Convert.BB file would look like this:
;
;       Graphics 640 , 480 , 0 , 2
;       Include "BinaryData.BB"
;
;     If you have the BinaryData file located somewhere else, you can specify a directory and/or drive.
;     Note the Graphics command, where I stated to use windowed mode, for simple convenience.
;
;     The only function you need to convert the files to Data statements is the ConvertBinaryToData function.
;     We can specify what the function uses as output, for example a temporary text file called DataFile.TXT
;     The function takes two parameters, the binary file and the output file. You could also specify the BlitzBasic
;     file where you want the Data to be, but if you're already editing that file in BlitzBasic, it will not work.
;
;     The code, when converting the Image.BMP and Sound.WAV files to Data, when doing it the safe way, using a
;     temporary file, is displayed here:
;
;       Graphics 640 , 480 , 0 , 2
;       Include "BinaryData.BB"
;
;       ConvertBinaryToData "Image.BMP" , "DataFile.TXT"
;       ConvertBinaryToData "Sound.WAV" , "DataFile.TXT"
;
;     Logically, running this program will create the DataFile.TXT file containing the Data statements. If you have
;     debug mode enabled, the process will be significantly slower, but nothing can go wrong either :)
;
;     The ConvertBinaryToData function also displays a progress indicator, so if it's very slow at least you're
;     seeing what's going on.
;
;     Note that if the file DataFile.TXT already exists, it won't be overwritten. But instead the Data statements
;     will be appended.
;
;     Also note that each file in Data statements will be accessible by a label, taken from the original filename of
;     the binary file. So for example the DataFile.TXT file will look like something like this:
;
;       .Image
;       Data"424D0632010000000000360000002800000070000000E90000... etc
;       Data"00... etc
;
;       .Sound
;       Data"424D0632010000000000360000002800000070000000E90000... etc
;       Data"00... etc
;
;     You can change the name of the label at any time ofcourse. In the final step you'd be able to use the Restore
;     command to access a specific file :)
;
;     As a final note, the DataFile.TXT will be over 2.1 times the size of the original file(s). And as BlitzBasic
;     has problems editing files over 1 Megabyte it opposes a problem when the total size of the binary files is
;     over around 490 Kilobytes.
;
;  ------------------------------------------------------------------------------------------------------------------
;   Step 2
;
;     First, the Data statements have to be put in the final BlitzBasic file, where also the rest of the game resides.
;     You could open the DataFile.TXT in WordPad or even BlitzBasic itself to copy and paste the Data statements.
;
;     When the Data statements are in place, there are 2 functions we'll need to be able to unpack the Data statements
;     when the game is running: ExportDataToBinary and ConvertHexToInt. You can simply copy and paste these functions
;     from the this file - they are located right beneath the tutorial.
;
;  ------------------------------------------------------------------------------------------------------------------
;   Step 3
;
;     There are a number of types of functionality you can use in your game. I'll display the two most typical.
;
;     The first, and easiest way is unpacking the Data files before the game starts, and deleting them when the game
;     ends.
;
;     The function ExportDataToBinary has only one parameter, which is the filename of the binary file to create and
;     to unpack the Data to. Before calling the function you have to set the Data reading cursor by using the Restore
;     command.
;
;     Here's an example of code, which would be the actual game including the Data statements and the functions needed
;     to unpack the Data:
;
;       Graphics 640 , 480
;
;       ;-------------------------------
;       ; Unpacking the files
;
;       Restore Image
;       ExportDataToBinary "Image.BMP"
;
;       Restore Sound
;       ExportDataToBinary "Sound.WAV"
;
;       ;-------------------------------
;       ; The game
;
;       Image = LoadImage ( "Image.BMP" )
;       Sound = LoadSound ( "Sound.WAV" )
;
;       Repeat
;
;         ;<-- Game goes here :)
;
;       Until KeyHit ( 1 )
;
;       ;-------------------------------
;       ; Cleaning up
;
;       DeleteFile "Image.BMP"
;       DeleteFile "Sound.WAV"
;
;     If you'd want to use some features of the Custom Print functions, you could also include this entire file :)
;
;     The other way of unpacking the Data files is before the game starts, and removing them right after you loaded
;     them into memory. Here's some example code, also displaying the PrintText function.
;
;       Graphics 640 , 480
;
;       ;-------------------------------
;       ; Unpacking the files
;
;       PrintText "Unpacking"
;
;       Restore Image
;       ExportDataToBinary "Image.BMP"
;
;       Restore Sound
;       ExportDataToBinary "Sound.WAV"
;
;       ;-------------------------------
;       ; Loading the game
;
;       PrintText "Loading"
;
;       Image = LoadImage ( "Image.BMP" )
;       Sound = LoadSound ( "Sound.WAV" )
;
;       ;-------------------------------
;       ; Cleaning up
;
;       DeleteFile "Image.BMP"
;       DeleteFile "Sound.WAV"
;
;       ;-------------------------------
;       ; Playing the game
;
;       Repeat
;
;         ;<-- Game goes here :)
;
;       Until KeyHit ( 1 )
;
;     That's all there is to it! Enjoy! :)
;
;  ------------------------------------------------------------------------------------------------------------------
;   Tricks
;
;     Suppose you have a directory with BMP and WAV files you wish to convert to Data statements.
;     The program below converts all of these files into a single file with Data statements.
;
;       Graphics 640 , 480 , 0 , 2
;       Include "BinaryData.BB"
;
;       Dir$ = "D:\Blitz\Media"  ; Where your files are
;
;       DirHandle = ReadDir ( Dir )
;       Repeat
;         File$ = NextFile ( DirHandle )
;         Check$ = Upper ( File )
;         If Right ( Check , 4 ) = ".BMP" Or Right ( Check , 4 ) = ".WAV"
;           ConvertBinaryToData Dir + "\" + File , Dir + "\DataFile.TXT"
;         End If
;       Until File = ""
;
;       PrintText "Done"
;       WaitKey
;       End
;
;----------------------------------------------------------------------------------------------------------------------;
; User functions
;----------------------------------------------------------------------------------------------------------------------;

  Function ExportDataToBinary ( BinaryFileName$ )
    Local BinaryFile
    Local DataLine$
    Local DataSize
    Local HexValue$
    Local ByteValue
    Local Counter

    BinaryFile = WriteFile ( BinaryFileName$ )
    Repeat
      Read DataLine
      DataSize = Len ( DataLine )
      For Counter = 1 To DataSize Step 2
        HexValue = Mid ( DataLine , Counter , 2 )
        ByteValue = ConvertHexToInt ( HexValue )
        WriteByte BinaryFile , ByteValue
      Next
    Until Len ( DataLine ) < 114
    CloseFile BinaryFile
  End Function

  Function ConvertHexToInt% ( HexValue$ )
    Local Counter
    Local Char$
    Local Number
    Local Result

    For Counter = 1 To Len ( HexValue )
      Char = Upper ( Mid ( HexValue , Counter , 1 ) )
      Number = Char
      If Number = 0 And Char <> "0" Then Number = Asc ( Char ) - 55
      Result = Result * 16 + Number
    Next

    Return Result
  End Function

  ;------------------------------------------------------------------------------------------------------------------;

  Function ConvertBinaryToData% ( BinaryFileName$ , DataFileName$ )
    Local BinaryFile
    Local BinarySize
    Local DataFile
    Local DataSize
    Local Status$
    Local ByteValue
    Local Counter
    Local FinalFeed

    PrintReset

    If Not FileType ( BinaryFileName )
      PrintText "File not found - " + BinaryFileName
      Return False
    EndIf

    BinarySize = FileSize ( BinaryFileName )
    DataSize   = FileSize (   DataFileName )
    BinaryFile = ReadFile ( BinaryFileName )
    If FileSize ( DataFileName )
      PrintColor 255 , 127 , 0
      PrintText "Appending to " + DataFileName + " at " + DataSize + " \"
      DataFile = OpenFile ( DataFileName )
      SeekFile DataFile , DataSize
    Else
      PrintColor 255 , 0 , 0
      PrintText "Creating " + DataFileName + " \"
      DataFile = WriteFile ( DataFileName )
    EndIf

    WriteShort DataFile , 2573                                            ; = <LineFeed>
    WriteByte  DataFile , 46                                              ; = .
    WriteText  DataFile , StripSpaces ( GetFileName ( BinaryFileName ) )  ; = <LabelName>
    WriteShort DataFile , 2573                                            ; = <LineFeed>

    WriteInt  DataFile , 1635017028  ; = Data
    WriteByte DataFile , 34          ; = "

    Repeat

      Status = "(" + Int ( FilePos ( BinaryFile ) * 100 / BinarySize ) + "%)/"
      PrintClear Len ( Status )
      PrintText Status

      ByteValue = ReadByte ( BinaryFile )
      WriteByte DataFile , Asc ( Mid ( Right ( Hex ( ByteValue ) , 2 ) , 1 , 1 ) )
      WriteByte DataFile , Asc ( Mid ( Right ( Hex ( ByteValue ) , 2 ) , 2 , 1 ) )

      Counter = Counter + 1
      If Counter = 57
        Counter = 0
        WriteByte  DataFile , 34          ; = "
        WriteShort DataFile , 2573        ; = <LineFeed>
        WriteInt   DataFile , 1635017028  ; = Data
        WriteByte  DataFile , 34          ; = "
      EndIf

    Until Eof ( BinaryFile )

    WriteByte  DataFile , 34    ; = "
    WriteShort DataFile , 2573  ; = <LineFeed>

    CloseFile DataFile
    CloseFile BinaryFile

    PrintText

    Return True
  End Function

  Function WriteText ( FileHandle% , Text$ )
    Local Counter
    Local Length

    Length = Len ( Text )
    For Counter = 1 To Length
      WriteByte FileHandle , Asc ( Mid ( Text , Counter , 1 ) )
    Next
  End Function

  Function GetFileName$ ( FullPath$ )
    Local Strip$
    Local Length
    Local Counter
    Local Position
    Local Found

    Strip = Trim ( FullPath )
    Length = Len ( Strip )

    If Instr ( Strip , "." , 1 )
      For Counter = 0 To Length-1
        Position = Length - Counter
        If Mid ( Strip , Position , 1 ) = "."
          Found = Position
          Exit
        Else
        End If
      Next

      If Found
        Strip = Left ( Strip , Found-1 )
        Length = Len ( Strip )
        Found = False
      End If
    End If

    If Instr ( Strip , "\" , 1 )
      For Counter = 0 To Length-1
        Position = Length - Counter
        If Mid ( Strip , Position , 1 ) = "\"
          Found = Position
          Exit
        End If
      Next

      If Found
        Strip = Right ( Strip , Length - Found )
      End If
    End If

    Return Strip
  End Function

  Function StripSpaces$ ( Text$ )
    Return Replace ( Text , " " , "" )
  End Function

;----------------------------------------------------------------------------------------------------------------------;
; Documentation
;----------------------------------------------------------------------------------------------------------------------;
;
;   Function ExportDataToBinary ( BinaryFileName$ )
;
;     Function:
;
;       The purpose of this function is to be used at run time, after all Data is already in the same or a related BB
;       file. This function unpacks the binary data from the Data statements to the specified binary file. You will
;       need to set the data reading cursor to the correct label before using this function using the Restore command.
;       The function will read data until it encounters a line of data smaller than 114 characters. Also be aware of
;       the fact that this function doesn't use much checking, so be careful what you're attempting to unpack.
;
;       It is not recommended to use this command in debug mode, as it will slow the function down significantly.
;       See the ConvertBinaryToData function on how to prepare the binary data as Data statements at design time.
;
;     Parameters:
;
;       Filename of binary.
;
;         The destination file of the unpacking process which will be overwritten if existing, but not deleted. If you
;         do not include a drive or path, the current directory will be used.
;
;     Returns:  Nothing
;
;     Example:
;
;       Restore PlayerShip1
;       ExportDataToBinary "PlayerShip.BMP"
;
;       PlayerImage = LoadImage ( "PlayerShip.BMP" )
;       DeleteFile "PlayerShip.BMP"
;
;       DrawImage PlayerImage , 0 , 0
;
;       .PlayerShip1
;       Data"424D0632010000000000360000002800000070000000E90000... etc
;
;----------------------------------------------------------------------------------------------------------------------;
;
;   Function ConvertHexToInt% ( HexValue$ )
;
;     Function:
;
;       Performs the reversed of the Blitz Hex function, converting a hexadecimal value in a string, to a decimal value
;       in an integer. This function is used by the ExportDataToBinary function.
;
;     Parameters:  Hexadecimal value as a string.
;
;     Returns:  Decimal value as an integer.
;
;     Example:
;
;       Print ConvertHexToInt ( "100" )         ; 256
;       Print Hex ( ConvertHexToInt ( "FF" ) )  ; "000000FF"
;       Print Hex ( 255 )                       ; "000000FF"
;
;----------------------------------------------------------------------------------------------------------------------;
;
;   Function ConvertBinaryToData% ( BinaryFileName$ , DataFileName$ )
;
;     Function:
;
;       This is a function to be used at design time. If you have the full version of Blitz you could create a small
;       user interface around it and make an executable out of it.
;
;       The specified binary file will be used as source and the specified (text) file will be used as destination.
;       This function will convert the source file, containing any data, to Data statements inside the destination
;       file. Make sure that the file you're trying to convert is not too big as it will not only take a lot of time
;       but also will Blitz have problems editing the BB file containing the bulk amount of Data statements.
;
;       The conversion ratio is around 2.15 of the size of the source. So if you have over 500Kb of datafiles, you will
;       most likely exceed the BlitzBasic BB filesize limit, which is 1Mb, 1024Kb or 1048576 bytes.
;
;       It is not recommended to convert directly to the BB file as it can already be in use by BlitzBasic, which can
;       prevent this function from writing at all.
;
;       Some notes:
;        - This function also displays a progress indicator.
;        - You can use the function in debug mode but there will be a significant speed difference.
;        - The function will return False if the source file doesn't exist, otherwise True.
;        - A label, given the name of the source file, spaces removed, will preceed the actual data.
;
;     Parameters:
;
;       Filename of binary source for conversion.
;
;         If no drive/path is specified, the current directory will be used.
;
;       Filename of destination data file for conversion.
;
;         If this file doesn't exist, a new one will be created. If it already exists, the Data statements will be
;         appended to the file.
;
;     Returns:  True if the source file exists and False if it doesn't.
;
;     Example:
;
;       ; Converts all WAV and BMP files found to Data statements.
;       Dir$ = "C:\Desktop\Clyde Radcliffe\gfx"
;       DirHandle = ReadDir ( Dir )
;       Repeat
;         Item$ = NextFile ( DirHandle )
;         Check$ = Upper ( Item )
;         If Right ( Check , 4 ) = ".WAV" Or Right ( Check , 4 ) = ".BMP"
;           ConvertBinaryToData Dir + "\" + Item , "C:\Desktop\Output.TXT"
;         End If
;       Until Item = ""
;       WaitKey
;       End
;
;----------------------------------------------------------------------------------------------------------------------;
;
;   Function WriteText ( FileHandle% , Text$ )
;
;     Function:
;
;       A file function, same as the WriteString function, except from the part that it doesn't write a preceding
;       integer value indicating the length of the string, before writing the actual string.
;
;     Parameters:
;
;       File handle.
;
;         The file handle of a file opened using either the OpenFile or WriteFile function.
;
;       Text string.
;
;         The actual string to write at the current file position.
;
;     Returns:  Nothing
;
;     Example:
;
;       FileHandle = WriteFile ( "D:\Blitz\WriteText.Txt" )
;       WriteText FileHandle , "This text was added using the WriteText function."
;       WriteText FileHandle , Chr ( 13 ) + Chr ( 10 )
;       WriteText FileHandle , "Refrain from complexity, simplicity is brilliance."
;       CloseFile FileHandle
;
;----------------------------------------------------------------------------------------------------------------------;
;
;   Function GetFileName$ ( FullPath$ )
;
;     Function:  A filename parser that strips the path and extension from the specified file specification.
;
;     Parameters:  File specification.
;
;     Returns:  Filename only.
;
;     Example:  Print GetFileName ( "D:\Path.Dir\File.Name.Ext" )  ; "File.Name"
;
;----------------------------------------------------------------------------------------------------------------------;
;
;   Function StripSpaces$ ( Text$ )
;
;     Function:  Returns the specified text string with all space removed.
;
;     Parameters:  Text string.
;
;     Returns:  Text string without spaces.
;
;     Example:
;
;       Text$ = "  R e a d y  "
;       Print StripSpaces ( Text )         ; "Ready"
;       Print Replace ( Text , " " , "" )  ; "Ready"
;
;----------------------------------------------------------------------------------------------------------------------;