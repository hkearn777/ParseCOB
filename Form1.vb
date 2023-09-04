Imports System.IO
Imports System.Text.RegularExpressions



Public Class Form1
  ' CobParse - Reads a COBOL formatted text file and breaks out details such as
  '  file names and sizes, number of verbs, called routines, etc.
  '
  ' Written by Howard Kearney for IBM.
  ' This program is based on COBOL for Windows Version 7.5 Language Reference
  '
  ' Inputs:
  '  - Cobol program source text (*.cbl, *.cob, *.*)
  '  - Cobol copybook members (Includes) (*.cbl, *.cob, *.*)
  '    - ++include is treated like copy (support for Panvalet)
  ' Outputs:
  '  Character separated Values; to feed into either Excel or a RDBMS
  '  - *_pgm.csv - Information about the program
  '  - *_data.csv - Information about the data the program references
  '  - *_proc.csv - Information about the Procedure paragraphs
  '  - *_calls.csv- Information about program calls
  '  - *.puml - Plantuml file
  '
  ' COBOL source text must be written in COBOL reference format. Reference format
  '  consists of the following areas in a 72-character line. Although convention
  '  has lines in 80-character line.
  '  Sequence Number Area - cols. 1-6, len=6
  '  Indicator Area - col. 7, len=1
  '  Area A, cols. 8-11, len=4
  '  Area B, cols. 12-72, len=61
  '  Comment area, cols. 73-80, len=8
  '
  ' 
  Dim Delimiter As String = ""
  Dim DirectoryName As String = ""
  Dim FileNameOnly As String = ""
  Dim pgmName As String = ""
  Dim pgmSeq As Integer = 0
  Dim cWord As New List(Of String)
  Dim stmt As New List(Of String)
  Dim tempFileName As String = ""
  Dim CalledProgramsNames As New List(Of String)       'array to hold all called programs
  Dim CalledProgramsCounts As New List(Of Integer)      'array to hold a count of the called programs
  Dim ListOfParagraphs As New List(Of String)           'array to hold paragraph names


  Dim swPgmFile As StreamWriter = Nothing         'File holding all program names
  Dim swDataFile As StreamWriter = Nothing        'File holding all data file names
  Dim swParaFile As StreamWriter = Nothing        'File holding all Procedure names
  Dim swCallFile As StreamWriter = Nothing        'File holding all CALL
  Dim logFile As StreamWriter = Nothing
  Dim pumlFile As StreamWriter = Nothing          'File holding the Plantuml commands


  Dim VerbNames As New List(Of String)
  Dim VerbCount As New List(Of Integer)

  Dim IndentLevel As Integer = -1                  'how deep the if has gone
  Dim FirstWhenStatement As Boolean = False
  Dim WithinReadStatement As Boolean = False
  Dim WithinReadConditionStatement As Boolean = False
  Dim IFLevelIndex As New List(Of Integer)     'where in cWord the 'IF' is located


  Public Structure ProgramInfo
    Public Name As String
    Public Count As Integer
    Public CalledFrom As String
    Public Sub New(ByVal _name As String,
                   ByVal _count As Short,
                   ByVal _calledFrom As String)
      Name = _name
      Count = _count
      CalledFrom = _calledFrom
    End Sub
  End Structure
  Dim list_CalledPrograms As New List(Of ProgramInfo)

  Private Sub btnParseCob_Click(sender As Object, e As EventArgs) Handles btnParseCob.Click

    '
    ' Main processing logic
    '
    txtRecordsRead.Text = ""
    txtStatementCount.Text = ""
    Delimiter = txtDelimiter.Text     'set this constant


    ' Validations of file names
    If Not FileNamesAreValid() Then
      Exit Sub
    End If

    'Load the infile to the stmt List
    Dim cobRecordsCount As Integer = LoadCobStatementsToArray()


    ' log file
    'Dim logCount As Integer = -1
    'logFile = My.Computer.FileSystem.OpenTextFileWriter("C:\Users\906074897\Documents\All Projects\State of Illinois\VS Projects\log.txt", False)
    'For Each statement In stmt
    '  logCount += 1
    '  logFile.WriteLine(LTrim(Str(logCount)) & ":" & statement)
    'Next
    'logFile.Close()


    If cobRecordsCount = -1 Then
      Exit Sub
    End If
    If cobRecordsCount = 0 Then
      MessageBox.Show("No records found in inFile")
      Exit Sub
    End If
    If stmt.Count = 0 Then
      MessageBox.Show("No COB statements found on inFile")
    End If

    txtRecordsRead.Text = LTrim(Str(cobRecordsCount))
    txtStatementCount.Text = LTrim(Str(stmt.Count))

    'write the three output files (Program, Data, Proc)
    If WriteOutput() = -1 Then
      MessageBox.Show("Error while building output. See log file")
    End If

  End Sub

  Function FileNamesAreValid() As Boolean
    FileNamesAreValid = False
    Select Case True
      Case txtInFile.TextLength = 0
        MessageBox.Show("Infile name required")
      Case Not IsValidFileNameOrPath(txtInFile.Text)
        MessageBox.Show("Infile name has invalid characters")
      Case Not My.Computer.FileSystem.FileExists(txtInFile.Text)
        MessageBox.Show("Infile not found.")

      Case txtOutFolder.TextLength = 0
        MessageBox.Show("OutFolder name required")
      Case Not IsValidFileNameOrPath(txtOutFolder.Text & "/" & txtDataFileName.Text)
        MessageBox.Show("OutFolder + DataFile name has invalid characters")
      Case Not IsValidFileNameOrPath(txtOutFolder.Text & "/" & txtParaFileName.Text)
        MessageBox.Show("OutFolder + ProcFile name has invalid characters")

      Case Else
        FileNamesAreValid = True
    End Select
  End Function
  Function IsValidFileNameOrPath(ByVal name As String) As Boolean
    If name Is Nothing Then
      Return False
    End If

    For Each badChar As Char In System.IO.Path.GetInvalidPathChars
      If InStr(name, badChar) > 0 Then
        Return False
      End If
    Next

    Return True
  End Function

  Function LoadCobStatementsToArray() As Integer
    '*---------------------------------------------------------
    ' Load COBOL lines to a Cobol statements array. 
    '*---------------------------------------------------------

    ' Remove the temporary work file
    Try
      If My.Computer.FileSystem.FileExists(tempFileName) Then
        My.Computer.FileSystem.DeleteFile(tempFileName)
      End If
    Catch ex As Exception
      MessageBox.Show("Removal of Temp hlk error:" & ex.Message)
      LoadCobStatementsToArray = -1
      Exit Function
    End Try

    ' Include all the COPY members to the temporary file
    ' Drop Empty lines
    ' Only keeping: Indicator area, Area A, and Area B (cols 7-72)

    Dim swTemp As StreamWriter = Nothing
    Dim CopybookName As String = ""
    Dim CompilerDirective As String = ""
    Dim SQLDirective As String = ""
    Dim NumberOfCopysFound As Integer = 0
    Dim NumberOfScans As Integer = 0
    Dim SequenceNumberArea As String = ""
    Dim IndicatorArea As String = ""
    Dim AreaA As String = ""
    Dim AreaB As String = ""
    Dim AreaAandB As String = ""
    Dim CommentArea As String = ""
    Dim execSequenceNumberArea As String = ""
    Dim execIndicatorArea As String = ""
    Dim execAreaA As String = ""
    Dim execAreaB As String = ""
    Dim execAreaAandB As String = ""
    Dim execCommentArea As String = ""
    Dim combinedEXEC As String = ""
    Dim PeriodIsPresent As Boolean = False
    Dim cIndex As Integer = -1
    Dim debugCnt As Integer = -1
    Dim startIndex As Integer = -1
    Dim endIndex As Integer = -1


    Dim CobolLines As String() = File.ReadAllLines(txtInFile.Text)

    '
    ' Expand all copy/include members into a single file, we also drop empty lines
    '
    Do
      NumberOfScans += 1
      NumberOfCopysFound = 0
      debugCnt = 0
      swTemp = New StreamWriter(tempFileName, False)
      For index As Integer = 0 To CobolLines.Count - 1
        debugCnt += 1
        ' drop completely empty line
        If Len(Trim(CobolLines(index))) = 0 Then
          Continue For
        End If

        Call FillInAreas(CobolLines(index),
                         SequenceNumberArea, IndicatorArea, AreaA, AreaB, CommentArea)
        ' write the comment line back out
        If IndicatorArea = "*" Then
          swTemp.WriteLine(CobolLines(index))
          Continue For
        End If

        ' get the Compiler directive, if any
        AreaAandB = AreaA & AreaB

        CompilerDirective = AreaAandB.ToUpper
        Dim tDirective As String() = CompilerDirective.Trim.Split(New Char() {" "c})
        Select Case True
          Case tDirective(0) = "COPY"
          Case tDirective(0) = "++INCLUDE"
            CopybookName = Trim(tDirective(1).Replace(".", " "))
          Case tDirective(0) = "EXEC"
            ' need to "string together" this till END-EXEC 
            combinedEXEC = ""
            startIndex = index
            endIndex = -1
            For execIndex As Integer = index To CobolLines.Count - 1
              Call FillInAreas(CobolLines(execIndex),
                execSequenceNumberArea, execIndicatorArea, execAreaA, execAreaB, execCommentArea)
              If execIndicatorArea = "*" Then
                Continue For
              End If
              execAreaAandB = execAreaA & execAreaB
              combinedEXEC &= execAreaAandB.ToUpper
              If combinedEXEC.IndexOf("END-EXEC") > -1 Then
                combinedEXEC = DropDuplicateSpaces(combinedEXEC)
                endIndex = execIndex
                Exit For
              End If
            Next
            ' safety check
            If endIndex = -1 Then
              MessageBox.Show("Malformed SQL statement; missing END-EXEC:" &
                              CobolLines(index) & " line#:" & index + 1)
              LoadCobStatementsToArray = -1
              Exit Function
            End If
            ' check to see if this an SQL INCLUDE or some other SQL command
            Dim execDirective As String() = combinedEXEC.Trim.Split(New Char() {" "c})
            If execDirective(1) = "SQL" And execDirective(2) = "INCLUDE" Then
              CopybookName = execDirective(3)
              ' comment out these SQL INCLUDE statement(s)
              For execIndex As Integer = startIndex To endIndex
                Mid(CobolLines(execIndex), 7, 1) = "*"
                swTemp.WriteLine(CobolLines(execIndex))
              Next
            Else
              '  ' write out these non-INCLUDE SQL statements
              '  For execindex As Integer = startIndex To endIndex
              '    swTemp.WriteLine(CobolLines(execindex))
              '  Next
              swTemp.WriteLine(CobolLines(index))
              Continue For
            End If
            '            index = endIndex            'bypass already processed cobolLines
          Case Else
            swTemp.WriteLine(CobolLines(index))
            Continue For
        End Select

        ' Expand copybooks/includes into the source
        NumberOfCopysFound += 1

        If Len(CopybookName) > 8 Then
          MessageBox.Show("somehow Member name > 8:" & CopybookName)
          LoadCobStatementsToArray = -1
          Exit Function
        End If
        Dim CopybookFileName As String = txtIncludeFolderName.Text &
                                         "\" & CopybookName & txtExtension.Text
        swTemp.WriteLine(SequenceNumberArea & "*Begin:" & AreaAandB)
        Call IncludeCopyMember(CopybookFileName, CobolLines, swTemp)
        swTemp.WriteLine(SequenceNumberArea & "*End:  " & AreaAandB)
      Next
      swTemp.Close()

      ' check we expanded any copybooks, if so we scan again for any copy/includes
      If NumberOfCopysFound > 0 Then                      'we found at least 1 COPY stmt
        CobolLines = File.ReadAllLines(tempFileName)   ' so load what we got so far
      End If

    Loop Until NumberOfCopysFound = 0

    '
    ' We should now deal with the compiler directive: REPLACE if there are any.
    ' Directives are before the PROGRAM-ID.
    '
    Dim cStatement As String = ""
    Dim statement As String = ""
    Dim procIndex As Integer = 0
    Dim continuation As Boolean = True
    LoadCobStatementsToArray = 0

    ' Load the temp file to the array
    CobolLines = File.ReadAllLines(tempFileName)

    ' scan for REPLACE directive and then do a Global Search and Replace
    cIndex = -1
    For Each text1 As String In CobolLines
      cIndex += 1
      If Trim(text1).Length = 0 Then                        'ignore empty lines
        Continue For
      End If
      Call FillInAreas(text1, SequenceNumberArea, IndicatorArea, AreaA, AreaB, CommentArea)
      If IndicatorArea = "*" Then
        Continue For
      End If
      AreaAandB = AreaA & AreaB
      CompilerDirective = AreaAandB.ToUpper
      Dim tDirective As String() = CompilerDirective.Trim.Split(New Char() {" "c})
      If tDirective(0) = "REPLACE" Then
        Call ReplaceAll(AreaAandB, CobolLines, cIndex)
      End If
    Next
    '
    ' Process the WHOLE/ALL the cobol lines now that copybooks are now embedded
    ' and replace is done.
    ' This is also where we concatenate the lines, as needed, into a single statement.
    '
    Dim hlkcounter As Integer = 0
    Dim Division As String = ""
    For Each text1 As String In CobolLines
      hlkcounter += 1
      LoadCobStatementsToArray += 1
      text1 = text1.Replace(vbTab, Space(1))                'replace TAB(S) with single space!
      text1 = text1.Replace(vbNullChar, Space(1))           'replace nulls with space
      text1 = text1.Replace("�", Space(1))
      If Trim(text1).Length = 0 Then                        'drop empty lines
        Continue For
      End If

      Call FillInAreas(text1, SequenceNumberArea, IndicatorArea, AreaA, AreaB, CommentArea)

      If IndicatorArea = "*" Then                           'drop comments
        Continue For
      End If

      AreaAandB = AreaA & AreaB
      If AreaAandB.Trim.Length = 0 Then
        Continue For
      End If
      If Microsoft.VisualBasic.Right(RTrim(AreaAandB), 1) = "." Then
        PeriodIsPresent = True
      Else
        PeriodIsPresent = False
      End If

      CompilerDirective = DropDuplicateSpaces(AreaAandB.ToUpper)
      Dim tDirective As String() = CompilerDirective.Trim.Split(New Char() {" "c})
      cWord.Clear()
      For Each word In tDirective
        If word.Trim.Length > 0 Then
          cWord.Add(UCase(word))
        End If
      Next


      If cWord(0) = "CBL" Then                     'Drop CBL Compiler directive
        Continue For
      End If
      If DivisionFound(cWord, Division) Then    'Division could have been updated
        cStatement = ""
      End If

      If IndicatorArea = "-" Then
        cStatement &= AreaB
        Continue For
      End If

      ' concatenate till end-of-statement, which is a period.

      If PeriodIsPresent Then
        cStatement &= AreaAandB
        cStatement = Mid(cStatement, 1, 4) & DropDuplicateSpaces(Mid(cStatement, 5))
        stmt.Add(cStatement)
        cStatement = ""
      Else
        cStatement &= AreaAandB
      End If

    Next

  End Function
  Function DropDuplicateSpaces(ByVal text As String) As String
    DropDuplicateSpaces = Regex.Replace(text, " +", " ")
  End Function
  Function DivisionFound(ByRef cWord As List(Of String), ByRef Division As String) As Boolean
    ' Caution! This not only returns true/false but ALSO updates the Division value
    '  if it encountered a division statement.
    DivisionFound = False
    If cWord.Count < 2 Then
      Exit Function
    End If
    If cWord(1).IndexOf("DIVISION") > -1 Then
      Select Case cWord(0)
        Case "IDENTIFICATION",
             "ENVIRONMENT",
             "DATA",
             "PROCEDURE"
          Division = cWord(0)
          DivisionFound = True
      End Select
    End If
  End Function
  Sub FillInAreas(ByVal CobolLine As String,
                  ByRef SequenceNumberArea As String,
                  ByRef IndicatorArea As String,
                  ByRef AreaA As String,
                  ByRef AreaB As String,
                  ByRef CommentArea As String)
    ' break the line into COBOL format areas
    ' Ensure line is 80 characters in length
    Dim Line As String = CobolLine.PadRight(80)
    ' extract out the COBOL areas (remember on substring startindex is zero-based!)
    SequenceNumberArea = Line.Substring(0, 6)   'cols 1-6
    IndicatorArea = Line.Substring(6, 1)        'cols 7
    AreaA = Line.Substring(7, 4)                'cols 8-11
    AreaB = Line.Substring(11, 61)              'cols 12-72
    CommentArea = Line.Substring(72, 8)         'cols 73-80
  End Sub
  Sub IncludeCopyMember(ByVal CopyMember As String,
                        ByRef CobolLines() As String,
                        ByRef swTemp As StreamWriter)
    If File.Exists(CopyMember) = False Then
      swTemp.Write(Space(6) & "*Member not found:")
      swTemp.WriteLine(Path.GetFileNameWithoutExtension(CopyMember))
      lblCopybookMessage.Text = "Copy Member not found:" & CopyMember
      Exit Sub
    End If
    Dim IncludeLines As String() = File.ReadAllLines(CopyMember)
    ' append copymember to temp file and drop blank lines
    For Each line As String In IncludeLines
      If Len(Trim(line)) > 0 Then
        swTemp.WriteLine(line)
      End If
    Next
  End Sub


  Function ProcessTheTextLine(ByRef text1 As String, ByRef continuation As Boolean) As Integer
    ' This will drop comments and CBL directives and Blank Lines.
    ' Also 
    ' drop comments
    If Mid(text1, 7, 1) = "*" Then
      ProcessTheTextLine = -1
      Exit Function
    End If
    ' drop CBL compile directives
    If Mid(text1, 8, 4) = "CBL " Then
      ProcessTheTextLine = -1
      Exit Function
    End If
    ' drop blank lines
    If Trim(text1).Length = 0 Then
      ProcessTheTextLine = -1
      Exit Function
    End If

    ' format only the good stuff out of the line (no slashes, no comments)
    text1 = Microsoft.VisualBasic.Left(text1 & Space(80), 80)   'ensure have 80 chars
    text1 = Trim(Mid(text1, 8, 65))                                   'area A and B

    ' determine if there will be a continuation
    If Microsoft.VisualBasic.Right(text1, 1) = "." Then
      continuation = False
      If Microsoft.VisualBasic.Right(text1, 1).Equals(".") Then        'remove the period
        text1 = text1.Remove(text1.Length - 1, 1)
      End If
    Else
      continuation = True
      text1 &= Space(1)
    End If

    ProcessTheTextLine = 0
  End Function

  Sub ReplaceAll(ByRef cStatement As String, ByRef CobolLines As String(), ByRef cIndex As Integer)
    ' for Compiler directive 'Replace'. Do the substitutions here.
    Dim tLine As String = RTrim(cStatement)
    If Microsoft.VisualBasic.Right(tLine, 1).Equals(".") Then        'remove the period
      tLine = tLine.Remove(tLine.Length - 1, 1)
    End If
    Dim tWord = tLine.Split(New Char() {" "c})
    Dim SearchFor As String = tWord(1).Replace("=", " ").Trim
    Dim ReplaceWith As String = tWord(3).Replace("=", " ").Trim
    ' loop through the CobolLines array replacing all <SearchFor> with <ReplaceWith>
    For index As Integer = cIndex + 1 To CobolLines.Count - 1
      If CobolLines(index).IndexOf(SearchFor) > -1 Then
        CobolLines(index) = CobolLines(index).Replace(SearchFor, ReplaceWith)
      End If
    Next
  End Sub
  Function WriteOutput() As Integer
    ' Write the output Pgm, Data, Procedure files.
    ' return of -1 means an error
    ' return of 0 means all is okay

    WriteOutput = 0

    ' Open the output files Pgm, Data, Paragraph, Call and write Header rows
    If WriteOutputPgmHeaders() = -1 Then
      WriteOutput = -1
      Exit Function
    End If
    If WriteOutputDataHeaders() = -1 Then
      WriteOutput = -1
      Exit Function
    End If
    If WriteOutputParagraphHeaders() = -1 Then
      WriteOutput = -1
      Exit Function
    End If
    If WriteOutputCallHeaders() = -1 Then
      WriteOutput = -1
      Exit Function
    End If

    ' Process all statements
    Dim Division As String = ""
    Dim stmtIndex As Integer = -1
    Dim ProcedureDivisionIndex = -1

    For Each statement As String In stmt
      stmtIndex += 1
      If Len(statement) = 0 Then
        MessageBox.Show("statement length 0? WriteOutput:stmtindex=" & Str(stmtIndex))
        Continue For
      End If
      ' break the statement in words dropping empty words and Making upper case
      Call GetCOBOLWords(statement, cWord)

      Dim unused As Boolean = DivisionFound(cWord, Division)

      ' Write the details to the files

      Select Case cWord(0)
        Case "PROGRAM-ID"
          Call ProcessPgm(stmtIndex)
        Case "SELECT"
          Call ProcessData()
        Case "EXEC"
          Call ProcessSQL()
      End Select
      If Division = "PROCEDURE" Then
        If ProcedureDivisionIndex = -1 Then       'first/Main Procedure only
          ProcedureDivisionIndex = stmtIndex
        End If
        If statement.Substring(0, 4) <> Space(4) Then
          Call ProcessParagraph(statement)
        Else
          Call ProcessCalls(statement)
        End If
      End If

    Next
    ' write out the calls file
    Call WriteCallsFile()
    '
    ' close the files
    swPgmFile.Close()
    swDataFile.Close()
    swParaFile.Close()
    swCallFile.Close()

    ' Create a Plantuml file, step by step, based on the Procedure division.
    ListOfParagraphs.Sort()
    If ProcedureDivisionIndex > -1 Then
      Call CreatePuml(ProcedureDivisionIndex)
    End If

  End Function
  Sub ProcessPgm(ByRef stmtIndex As Integer)
    pgmSeq += 1
    pgmName = cWord(1)

    Dim LinesInProgram As Integer = 0
    Call CountTheProgramVerbs(stmtIndex, LinesInProgram)

    swPgmFile.Write(FileNameOnly & Delimiter &
                        pgmName & Delimiter &
                        LTrim(Str(pgmSeq)) & Delimiter &
                        LTrim(Str(LinesInProgram)))
    For Each count As Integer In VerbCount
      swPgmFile.Write(Delimiter & count)
    Next
    swPgmFile.WriteLine("")

  End Sub
  Sub CountTheProgramVerbs(ByRef stmtIndex As Integer,
                           ByRef LinesInProgram As Integer)
    ' starting with the stmtindex until reaching 'end program' or 'program-id'
    ' find each verb and then count it.
    Dim cblWords As New List(Of String)
    Dim statement As String = ""
    Dim WithinProcedureDivision As Boolean = False
    Dim AreaA As String = ""

    For countIndex = 0 To VerbCount.Count - 1   'reset the verb counters for this program-id
      VerbCount(countIndex) = 0
    Next

    For s As Integer = stmtIndex + 1 To stmt.Count - 1 Step 1
      LinesInProgram += 1
      ' break apart stmt into cobol words
      statement = stmt(s)
      ' now I need to see if there are any COBOL verbs in this procedure division in programid
      ' ALTER, CALL, GO, MERGE, PERFORM, SORT 
      AreaA = UCase(LTrim(statement)) & Space(10)        'have at least 10 characters
      If Mid(AreaA, 1, 10) = "PROCEDURE " Then
        WithinProcedureDivision = True
        Continue For
      End If
      If Not WithinProcedureDivision Then
        Continue For
      End If
      If Mid(AreaA, 1, 10) = "PROGRAM-ID" Then
        Exit For
      End If
      Call GetCOBOLWords(statement, cblWords)
      ' for every VERB word count it.
      For v As Integer = 0 To VerbNames.Count - 1 Step 1      'get VERB
        For c As Integer = 0 To cblWords.Count - 1 Step 1     'search for that VERB
          If cblWords(c).Equals(VerbNames(v)) Then
            VerbCount(v) += 1
          End If
        Next c
      Next v

    Next s
  End Sub
  Sub ProcessData()
    ' this is triggered by the SELECT statement

    Dim fdWords As New List(Of String)

    ' Find the file-name-1 value presuming this value is after the SELECT and/or OPTIONAL
    Dim file_name_1 As String = ""
    If cWord(1).Equals("OPTIONAL") Then
      file_name_1 = cWord(2)
    Else
      file_name_1 = cWord(1)
    End If

    Dim assignment_name_1 As String = ""
    Dim index As Integer = GetKeywordIndex("ASSIGN")
    Dim index2 As Integer = 0
    Dim index3 As Integer = 0

    If index > -1 Then
      If cWord(index + 1).Equals("TO") Then
        assignment_name_1 = cWord(index + 2)
      Else
        assignment_name_1 = cWord(index + 1)
      End If
      assignment_name_1 = assignment_name_1.Replace(".", "")
    End If

    ' need ORGANIZATION value SEQUENTIAL, INDEXED, RELATIVE, or LINE SEQUENTIAL
    ' If no 'ORGANIZATION' value the default is SEQUENTIAL
    Dim organization As String = "SEQUENTIAL"
    index = GetKeywordIndex("SEQUENTIAL")
    If index > -1 Then
      Select Case True
        Case "ORGANIZATION".Equals(cWord(index - 1))
        Case "ORGANIZATION".Equals(cWord(index - 2)) Or "IS".Equals(cWord(index - 1))
          organization = "SEQUENTIAL"
      End Select
    End If
    index = GetKeywordIndex("INDEXED")
    If index > -1 Then
      organization = "INDEXED"
    End If
    index = GetKeywordIndex("RELATIVE")
    If index > -1 Then
      organization = "RELATIVE"
    End If
    index = GetKeywordIndex("LINE")
    If index > -1 Then
      index2 = GetKeywordIndex("SEQUENTIAL")
      If index2 > -1 Then
        organization = "LINE SEQUENTIAL"
      End If
    End If

    ' Locate the FD statement for this SELECT statement
    If LocateFDStatement(file_name_1, fdWords) = -1 Then
      MessageBox.Show("FD/SD statement not found:PGM:" & pgmName & ";FILE:" & file_name_1)
    End If

    Dim Level As String = fdWords(0)

    Dim RecordingMode As String = "V"
    index = fdWords.IndexOf("RECORDING")
    If index > -1 Then
      Select Case True
        Case fdWords(index + 1) = "MODE" And fdWords(index + 2) = "IS"
          RecordingMode = fdWords(index + 3)
        Case fdWords(index + 1) = "MODE" Or fdWords(index + 1) = "IS"
          RecordingMode = fdWords(index + 2)
        Case Else
          RecordingMode = fdWords(index + 1)
      End Select
    End If

    Dim RecordSizeMinimum As Integer = 0
    ' search to find the RECORD CLAUSE
    index2 = -1
    index3 = -1
    For index = 0 To fdWords.Count - 1
      If fdWords(index).Equals("RECORD") Then
        Select Case True
          Case IsNumeric(fdWords(index + 1))
            index2 = index + 1
            If fdWords(index2 + 1) = "TO" Then
              index3 = index2 + 2
            End If
            Exit For
          Case fdWords(index + 1).Equals("CONTAIN") Or
               fdWords(index + 1).Equals("CONTAINS")
            If IsNumeric(fdWords(index + 2)) Then
              index2 = index + 2
            End If
            If fdWords(index2 + 1) = "TO" Then
              index3 = index2 + 2
            End If
            Exit For
          Case fdWords(index + 1).Equals("IS") And
               fdWords(index + 2).Equals("VARYING")
            index2 = index + 3
            index2 = GetIndexForRecordSize(index2, fdWords)
            If fdWords(index2 + 1) = "TO" Then
              index3 = index2 + 2
            End If
            Exit For
          Case fdWords(index + 1).Equals("VARYING")
            index2 = index + 2
            index2 = GetIndexForRecordSize(index2, fdWords)
            If fdWords(index2 + 1) = "TO" Then
              index3 = index2 + 2
            End If
            Exit For
        End Select
      End If
    Next
    If index2 > -1 Then
      RecordSizeMinimum = Val(fdWords(index2))
    End If

    Dim RecordSizeMaximum As Integer = RecordSizeMinimum
    If index3 > -1 Then
      RecordSizeMaximum = Val(fdWords(index3))
    End If

    Dim OpenMode As String = GetOpenMode(pgmName, file_name_1)


    swDataFile.WriteLine(FileNameOnly & Delimiter &
                         pgmName & Delimiter &
                         LTrim(Str(pgmSeq)) & Delimiter &
                         file_name_1 & Delimiter &
                         Level & Delimiter &
                         RTrim(OpenMode) & Delimiter &
                         RecordingMode & Delimiter &
                         LTrim(Str(RecordSizeMinimum)) & Delimiter &
                         LTrim(Str(RecordSizeMaximum)) & Delimiter &
                         assignment_name_1 & Delimiter &
                         organization)
  End Sub
  Function GetIndexForRecordSize(ByVal index As Integer, ByRef fdWords As List(Of String)) As Integer
    GetIndexForRecordSize = index
    Select Case True
      Case IsNumeric(fdWords(index)) : Exit Select
      Case IsNumeric(fdWords(index + 1)) : GetIndexForRecordSize += 1
      Case IsNumeric(fdWords(index + 2)) : GetIndexForRecordSize += 2
      Case IsNumeric(fdWords(index + 3)) : GetIndexForRecordSize += 3
      Case Else
        MessageBox.Show("Unknown 'IS VARYING' syntax@" & pgmName & "FD:" & fdWords.ToString)
    End Select
  End Function
  Function LocateFDStatement(ByRef filename As String, ByRef fdWords As List(Of String)) As Integer
    LocateFDStatement = -1          'Not found
    For Each statement As String In stmt
      If Len(statement) = 0 Then
        Continue For
      End If
      ' parse statement into fd words
      Dim tWord = statement.Replace(".", " ").Split(New Char() {" "c})
      fdWords.Clear()
      For Each word As String In tWord
        If word.Trim.Length > 0 Then        'dropping empty words
          fdWords.Add(word.ToUpper)
        End If
      Next
      If fdWords.Count >= 2 Then
        If (fdWords(0) = "FD" Or fdWords(0) = "SD") Then
          If fdWords(1) = filename Then
            LocateFDStatement = 0
            Exit For
          End If
        End If
      End If

    Next

  End Function
  Function GetOpenMode(ByRef pgmName As String, ByRef file_name_1 As String) As String
    ' Determine the OPEN mode of the file
    ' have to scan through the statement file looking for file_name_1 for 
    ' 'PROGRAM-ID=<pgmName> and OPEN ('INPUT' or 'OUTPUT' or 'I-O' or 'EXTEND')
    ' It could have all open modes.
    ' 
    GetOpenMode = ""
    Dim cblWords As New List(Of String)
    Dim currentPgm As String = ""
    Dim ProcedureDivisionFound As Boolean = False
    For Each statement As String In stmt
      ' parse statement into fd words and drop empty words
      Call GetCOBOLWords(statement, cblWords)
      ' find if this file_name_1 is for this program being searched
      If cblWords(0) = "PROGRAM-ID" Then
        currentPgm = cblWords(1)
        Continue For
      End If
      If currentPgm <> pgmName Then
        Continue For
      End If
      If cblWords.Count >= 2 Then
        If cblWords(0) = "PROCEDURE" And cblWords(1) = "DIVISION" Then
          ProcedureDivisionFound = True
          Continue For
        End If
      End If
      If ProcedureDivisionFound = False Then
        Continue For
      End If
      ' search this statement line if it holds any reference to file_name_1
      ' if it does, see what open mode it has.
      Dim fnIndex As Integer = 0
      For fnIndex = 0 To cblWords.Count - 1 Step 1
        fnIndex = cblWords.IndexOf(file_name_1, fnIndex)
        If fnIndex = -1 Then Exit For
        For x As Integer = fnIndex - 1 To 0 Step -1
          Select Case cblWords(x)
            Case "INPUT"
              GetOpenMode &= "I "
              Exit For
            Case "OUTPUT"
              GetOpenMode &= "O "
              Exit For
            Case "I-O"
              GetOpenMode &= "I/O "
              Exit For
            Case "EXTEND"
              GetOpenMode &= "EXTEND "
              Exit For
            ' these cases below indicate this was not an OPEN verb
            Case "READ"
              Exit For
            Case "CLOSE"
              Exit For
            Case "SORT"
              GetOpenMode &= "SORT "
              Exit For
            Case "MERGE"
              GetOpenMode &= "MERGE "
              Exit For
            Case "USING"
              GetOpenMode &= "SORTIN"
              Exit For
            Case "GIVING"
              GetOpenMode &= "SORTOUT"
              Exit For
            Case "OPEN"
              MessageBox.Show("Never found open mode!:" & file_name_1 & ":" & statement)
              Exit For
          End Select
        Next x
      Next fnIndex
    Next

  End Function

  Sub GetCOBOLWords(ByVal statement As String, ByRef cblWords As List(Of String))
    ' takes the stmt and breaks into words and drops blanks
    cblWords.Clear()
    'statement = " DISPLAY '*** CRCALCX REC READ        = ' WS-REC-READ.   "
    statement = statement.Trim
    Dim WithinQuotes As Boolean = False
    Dim word As String = ""
    For x As Integer = 0 To statement.Count - 1
      If statement.Substring(x, 1) = "'" Then
        WithinQuotes = Not WithinQuotes
      End If
      If statement.Substring(x, 1) = " " And Not WithinQuotes Then
        cblWords.Add(word.ToUpper)
        word = ""
      Else
        word &= statement.Substring(x, 1)
      End If
    Next
    If word.EndsWith(".") Then
      word = word.Remove(word.Length - 1)
      cblWords.Add(word.ToUpper)
    End If

    'Dim tWord = statement.Split(New Char() {" "c})
    'Dim tempWord As String = ""
    'cblWords.Clear()
    'For Each word In tWord
    '  If word.Trim.Length > 0 Then
    '    tempWord = word.Trim.ToUpper
    '    If tempWord.EndsWith(".") Then
    '      tempWord = tempWord.Remove(tempWord.Length - 1)
    '    End If
    '    cblWords.Add(tempWord)
    '  End If
    'Next

  End Sub
  Function GetKeywordIndex(keyword As String) As Integer
    ' find the keyword, if any, in the list
    GetKeywordIndex = cWord.IndexOf(keyword)
  End Function

  Sub ProcessSQL()
    If cWord(1) <> "SQL" Then
      Exit Sub
    End If
    If cWord.Count < 3 Then
      Exit Sub
    End If

    ' get file name of sql
    Dim file_name_1 As String = ""
    Dim index As Integer = cWord.IndexOf("FROM")
    If index = -1 Then
      Exit Sub
    End If
    file_name_1 = cWord(index + 1)

    Dim Level As String = "SQL"

    Dim OpenMode As String = cWord(2)

    Dim RecordingMode As String = ""
    Dim RecordSizeMinimum As Integer = 0
    Dim RecordSizeMaximum As Integer = 0
    Dim assignment_name_1 As String = ""
    Dim organization As String = "RDBMS"
    swDataFile.WriteLine(FileNameOnly & Delimiter &
                         pgmName & Delimiter &
                         LTrim(Str(pgmSeq)) & Delimiter &
                         file_name_1 & Delimiter &
                         Level & Delimiter &
                         RTrim(OpenMode) & Delimiter &
                         RecordingMode & Delimiter &
                         LTrim(Str(RecordSizeMinimum)) & Delimiter &
                         LTrim(Str(RecordSizeMaximum)) & Delimiter &
                         assignment_name_1 & Delimiter &
                         organization)
  End Sub
  Sub ProcessParagraph(ByRef statement As String)
    ' extract the paragraph name
    If statement.Substring(0, 4).ToUpper = "END " Then
      Exit Sub
    End If
    Dim tWord = statement.Split(New Char() {"."c})
    Dim ParagraphName As String = tWord(0)
    ListOfParagraphs.Add(ParagraphName)

    'need to count number of times this paragraph name is referenced in this program
    Dim ParagraphNameCount As Integer = 0
    For Each text As String In stmt
      If text.IndexOf(ParagraphName) > 0 Then
        ParagraphNameCount += 1
      End If
    Next

    swParaFile.WriteLine(FileNameOnly & Delimiter &
                        pgmName & Delimiter &
                        LTrim(Str(pgmSeq)) & Delimiter &
                        ParagraphName & Delimiter &
                        LTrim(Str(ParagraphNameCount)))

  End Sub

  Sub ProcessCalls(ByRef statement As String)
    ' find if there is a CALL verb being used on this statement
    If statement.ToUpper.IndexOf(" CALL ") = -1 Then
      Exit Sub
    End If

    ' split the statement into cobol words
    Dim CallNameCount As Integer = 0
    Dim CallName As String = ""
    Dim cblWords As New List(Of String)
    Call GetCOBOLWords(statement, cblWords)


    Dim CalledPrograms As ProgramInfo
    Dim item As ProgramInfo

    ' Process all the cobol words looking for call verbs
    For callIndex As Integer = 0 To cblWords.Count - 1
      If cblWords(callIndex) = "CALL" Then
        CallName = cblWords(callIndex + 1)
        ' validate a good call name (this may be within a DISPLAY)
        '   if last byte is a quote(or double-quote) then the first
        '     byte must be the same.
        Dim rightByte As String = Microsoft.VisualBasic.Right(CallName, 1)
        Dim leftByte As String = Microsoft.VisualBasic.Left(CallName, 1)
        If rightByte = "'" Or rightByte = Chr(34) Then
          If leftByte <> rightByte Then                   'not valid skip this word
            Continue For
          End If
        End If
        ' going to store this name but only once.
        ' count the number of times this occurs
        ' the key is callName and pgmName(called from)
        Try
          item = list_CalledPrograms.First(Function(obj) obj.Name = CallName And obj.CalledFrom = pgmName)
          Dim index = list_CalledPrograms.IndexOf(item)
          item.Count += 1
          list_CalledPrograms(index) = item

        Catch ex As Exception
          If ex.Message = "Sequence contains no matching element" Then
            CalledPrograms.Name = CallName
            CalledPrograms.Count = 1
            CalledPrograms.CalledFrom = pgmName
            list_CalledPrograms.Add(CalledPrograms)
          Else
            MessageBox.Show("find first error:" & ex.Message)
          End If


        End Try

        'If CalledProgramsNames.IndexOf(CallName) = -1 Then
        '  CalledProgramsNames.Add(CallName)
        'End If
      End If
    Next

  End Sub

  Sub WriteCallsFile()
    ' sort the list
    CalledProgramsNames.Sort()
    Dim CallType As String = ""
    ' process through the CalledPrograms array and write out the csv
    For Each CalledPrograms As ProgramInfo In list_CalledPrograms
      CallType = "Dynamic"
      If CalledPrograms.Name.IndexOf("'") > -1 Then          'single quote
        CallType = "Static"
      End If
      If CalledPrograms.Name.IndexOf(Chr(34)) > -1 Then      'double quote
        CallType = "Static"
      End If
      swCallFile.WriteLine(CalledPrograms.Name & Delimiter &
                       CalledPrograms.CalledFrom & Delimiter &
                       CalledPrograms.Count & Delimiter &
                       CallType)
    Next
  End Sub

  Function WriteOutputPgmHeaders() As Integer
    ' Open the output file Pgm and write Header row
    Dim PgmFileName = txtOutFolder.Text & "/" & txtPgmFileName.Text


    Try
      swPgmFile = My.Computer.FileSystem.OpenTextFileWriter(PgmFileName, False)
      swPgmFile.Write("Module" & Delimiter &
                          "PgmName" & Delimiter &
                          "PgmSeq" & Delimiter &
                          "PgmLines")
      For Each verb In VerbNames
        swPgmFile.Write(Delimiter & verb)
      Next
      swPgmFile.WriteLine("")

    Catch ex As Exception
      MessageBox.Show(ex.Message, "Error opening PgmFile")
      WriteOutputPgmHeaders = -1
      Exit Function
    End Try

    WriteOutputPgmHeaders = 0
  End Function

  Function WriteOutputDataHeaders() As Integer
    ' Open the output file Data and write Header row
    Dim DataFileName = txtOutFolder.Text & "/" & txtDataFileName.Text

    Try
      swDataFile = My.Computer.FileSystem.OpenTextFileWriter(DataFileName, False)
      swDataFile.WriteLine("Module" & Delimiter &
                           "PgmName" & Delimiter &
                          "PgmSeq" & Delimiter &
                          "FileName" & Delimiter &
                          "Level" & Delimiter &
                          "OpenMode" & Delimiter &
                          "RecFM" & Delimiter &
                          "MinLRECL" & Delimiter &
                          "MaxLRECL" & Delimiter &
                          "DDName" & Delimiter &
                          "FileType")
    Catch ex As Exception
      MessageBox.Show(ex.Message, "Error opening DataFile")
      WriteOutputDataHeaders = -1
      Exit Function
    End Try

    WriteOutputDataHeaders = 0
  End Function
  Function WriteOutputParagraphHeaders() As Integer
    ' Open the output file Pgm and write Header row
    Dim ParaFileName = txtOutFolder.Text & "/" & txtParaFileName.Text


    Try
      swParaFile = My.Computer.FileSystem.OpenTextFileWriter(ParaFileName, False)
      swParaFile.WriteLine("Module" & Delimiter &
                          "PgmName" & Delimiter &
                          "PgmSeq" & Delimiter &
                          "Paragraph" & Delimiter &
                          "Counts")

    Catch ex As Exception
      MessageBox.Show(ex.Message, "Error opening ParagraphFile")
      WriteOutputParagraphHeaders = -1
      Exit Function
    End Try

    WriteOutputParagraphHeaders = 0
  End Function
  Function WriteOutputCallHeaders() As Integer
    ' Open the output file Pgm and write Header row
    Dim CallFileName = txtOutFolder.Text & "/" & txtCallsFileName.Text


    Try
      swCallFile = My.Computer.FileSystem.OpenTextFileWriter(CallFileName, False)
      swCallFile.WriteLine("Module" & Delimiter &
                          "CalledFrom" & Delimiter &
                          "Counts" & Delimiter &
                          "Static/Dynamic")

    Catch ex As Exception
      MessageBox.Show(ex.Message, "Error opening CallsFile")
      WriteOutputCallHeaders = -1
      Exit Function
    End Try

    WriteOutputCallHeaders = 0
  End Function

  Sub CreatePuml(ByRef ProcedureDivisionIndex As Integer)
    ' Open the output file Pgm and write Header row
    Dim PumlFileName = txtOutFolder.Text & "/" & txtPlantUmlFilename.Text

    ' Open and write at least one time. Not worrying (try/catch) about subsequent writes
    Try
      pumlFile = My.Computer.FileSystem.OpenTextFileWriter(PumlFileName, False)
    Catch ex As Exception
      MessageBox.Show(ex.Message, "Error opening PumlFile")
      Exit Sub
    End Try

    Dim EndCondIndex As Integer = -1
    Dim StartCondIndex As Integer = -1
    Dim ParagraphStarted As Boolean = False
    Dim condStatement As String = ""
    Dim condStatementCR As String = ""
    Dim imperativeStatement As String = ""
    Dim imperativeStatementCR As String = ""
    Dim statement As String = ""
    Dim vwordIndex As Integer = -1

    pumlFile.WriteLine("@startuml " & FileNameOnly)
    pumlFile.WriteLine("header ParseCob(c), by IBM")
    pumlFile.WriteLine("footer Page %page% of %lastpage%")
    pumlFile.WriteLine("title Flowchart of Program " & FileNameOnly)

    For index As Integer = ProcedureDivisionIndex + 1 To stmt.Count - 1
      ' Paragraph names
      If stmt(index).Substring(0, 4) <> Space(4) Then
        Call ProcessPumlParagraph(ParagraphStarted, stmt(index))
        Continue For
      End If

      ' break the statement in words
      Call GetCOBOLWords(stmt(index).Trim, cWord)

      ' Process every VERB word in this statement 
      ' Every verb should be a plum object created.

      IndentLevel = 1
      IFLevelIndex.Clear()
      'If index = 504 Then
      '  MessageBox.Show("pause")
      'End If
      For wordIndex = 0 To cWord.Count - 1
        Select Case cWord(wordIndex)
          Case "IF"
            IFLevelIndex.Add(wordIndex)
            Call ProcessPumlIF(wordIndex)
          Case "ELSE"
            Call ProcessPumlELSE(wordIndex)
          Case "END-IF"
            IndentLevel -= 1
            pumlFile.WriteLine(Indent() & "endif")
          Case "EVALUATE"
            Call ProcessPumlEVALUATE(wordIndex)
          Case "WHEN"
            Call ProcessPumlWHEN(wordIndex)
          Case "END-EVALUATE"
            Call ProcessPumlENDEVALUATE(wordIndex)
          Case "PERFORM"
            Call ProcessPumlPERFORM(wordIndex)
          Case "END-PERFORM"
            Call ProcessPumlENDPERFORM(wordIndex)
          Case "COMPUTE"
            Call ProcessPumlCOMPUTE(wordIndex)
          Case "READ"
            Call ProcessPumlREAD(wordIndex)
          Case "AT", "END", "NOT"
            ProcessPumlReadCondition(wordIndex)
          Case "END-READ"
            ProcessPumlENDREAD(wordIndex)
          Case "GO"
            Call ProcessPumlGOTO(wordIndex)
          Case "EXEC"
            processPumlEXEC(wordIndex)
          Case Else
            Dim EndIndex As Integer = 0
            Dim MiscStatement As String = ""
            Call GetStatement(wordIndex, EndIndex, MiscStatement)
            pumlFile.WriteLine(Indent() & ":" & MiscStatement.Trim & ";")
            wordIndex = EndIndex
        End Select
      Next wordIndex
    Next index

    If ParagraphStarted = True Then
      pumlFile.WriteLine("end")
      ParagraphStarted = False
    End If
    pumlFile.WriteLine("@enduml")

    pumlFile.Close()
  End Sub
  Function Indent() As String
    Return Space(IndentLevel * 2)
  End Function
  Sub ProcessPumlParagraph(ByRef ParagraphStarted As Boolean, ByRef statement As String)
    If ParagraphStarted = True Then
      pumlFile.WriteLine("end")
      pumlFile.WriteLine("")
    End If
    pumlFile.WriteLine("start")
    pumlFile.WriteLine(":**" & Trim(statement.Replace(".", "")) & "**;")
    ParagraphStarted = True
  End Sub
  Sub ProcessPumlIF(ByRef WordIndex As Integer)
    ' find the 'IF' aka Conditional statement
    ' Indentlevel is global
    Dim EndIndex As Integer = 0
    Dim Statement As String = ""
    Call GetStatement(WordIndex, EndIndex, Statement)
    pumlFile.WriteLine(Indent() & "if (" & Statement.Trim & ") then (yes)")
    IndentLevel += 1
    WordIndex = EndIndex
  End Sub
  Sub ProcessPumlELSE(ByRef WordIndex As Integer)
    ' Does current 'ELSE' belong to my 'IF' or another 'IF'???
    ' Look back for an 'IF',
    '   if I find an 'ELSE' this means the current 'ELSE' is not mine,
    '     so write an 'END-IF' and then an 'ELSE'
    '   if I find an 'IF' this means the current 'ELSE' is mine,
    '     so write an 'ELSE'
    '   if I hit top of array there is a syntax error!!! else without an if...
    ' cWord is global
    ' IndentLevel is global
    For backindex = WordIndex - 1 To 0 Step -1
      If cWord(backindex) = "ELSE" Then
        IndentLevel -= 1
        pumlFile.WriteLine(Indent() & "endif")
        IndentLevel -= 1
        pumlFile.WriteLine(Indent() & "else (no)")
        IndentLevel += 1
        Exit For
      End If
      If cWord(backindex) = "IF" Then
        IndentLevel -= 1
        pumlFile.WriteLine(Indent() & "else (no)")
        IndentLevel += 1
        Exit For
      End If
    Next
  End Sub

  Sub ProcessPumlEVALUATE(ByRef wordIndex As Integer)
    'TODO: need to fix embedded Evaluates
    ' find the end of 'EVALUATE' statement which should be at the first 'WHEN' clause
    'cWord is global
    'IndentLevel is global
    Dim Statement As String = ""
    Dim EndIndex As Integer = wordIndex + 1
    For EndIndex = EndIndex To cWord.Count - 1
      If cWord(EndIndex) = "WHEN" Then
        Exit For
      End If
    Next
    EndIndex -= 1
    Call GetStatement(wordIndex, EndIndex, Statement)
    pumlFile.WriteLine(Indent() & ":" & Statement.Trim & ";")
    IndentLevel += 1
    FirstWhenStatement = True
    wordIndex = EndIndex
  End Sub
  Sub ProcessPumlWHEN(ByRef wordindex As Integer)
    'TODO: need to fix embedded WHENs
    ' find the end of 'EVALUATE' statement which should be at the first 'WHEN' clause
    'cWord is global
    'IndentLevel is global
    Dim Statement As String = ""
    Dim EndIndex As Integer = wordindex + 1
    For EndIndex = EndIndex To cWord.Count - 1
      If cWord(EndIndex) = "WHEN" Then
        Exit For
      End If
    Next
    EndIndex -= 1
    Call GetStatement(wordindex, EndIndex, Statement)
    If FirstWhenStatement = True Then
      FirstWhenStatement = False
      pumlFile.WriteLine(Indent() & "if (" & Statement.Trim & ") then (yes)")
      IndentLevel += 1
    Else
      IndentLevel -= 1
      pumlFile.WriteLine(Indent() & "elseif (" & Statement.Trim & ") then (yes)")
      IndentLevel += 1
    End If
    wordindex = EndIndex

  End Sub
  Sub ProcessPumlENDEVALUATE(ByRef wordindex As Integer)
    'TODO: Need to handle embedded end-evaluate
    FirstWhenStatement = False
    IndentLevel -= 1
    pumlFile.WriteLine(Indent() & "endif")
  End Sub
  Sub ProcessPumlCOMPUTE(ByRef WordIndex As Integer)
    ' find the end of 'COMPUTE' statement
    Dim EndIndex As Integer = 0
    Dim Statement As String = ""
    Call GetStatement(WordIndex, EndIndex, Statement)
    pumlFile.WriteLine(Indent() & ":" & Statement.Trim & ";")
    WordIndex = EndIndex
  End Sub
  Sub ProcessPumlGOTO(ByRef WordIndex As Integer)
    ' find the end of 'GO TO' statement
    Dim EndIndex As Integer = 0
    Dim Statement As String = ""
    Call GetStatement(WordIndex, EndIndex, Statement)
    pumlFile.WriteLine(Indent() & ":" & Statement.Trim & ";")
    'pumlFile.WriteLine("detach")
    WordIndex = EndIndex
  End Sub

  Sub ProcessPumlREAD(ByRef WordIndex As Integer)
    'TODO: need to fix embedded READ
    ' find the end of 'READ' statement which should be at either AT, END, NOT or END-READ
    'cWord is global
    'IndentLevel is global

    ' Format 1:Sequential Read
    WithinReadStatement = True
    Dim StartIndex = WordIndex
    Dim EndIndex As Integer = -1

    For EndIndex = WordIndex + 1 To cWord.Count - 1
      Select Case cWord(EndIndex)
        Case "AT", "END", "NOT", "END-READ"
          Exit For
      End Select
    Next
    If EndIndex > cWord.Count - 1 Then
      EndIndex = cWord.Count - 1
    End If
    EndIndex -= 1
    Dim TogetherWords As String = StringTogetherWords(StartIndex, EndIndex)
    Dim ReadStatement As String = AddNewLineAboutEvery30Characters(TogetherWords)
    pumlFile.WriteLine(Indent() & ":" & ReadStatement.Trim & "/")
    WordIndex = EndIndex
    IndentLevel += 1

    ''Format 2:random retrieval
  End Sub
  Sub ProcessPumlReadCondition(ByRef WordIndex As Integer)
    If WithinReadStatement = False Then
      Exit Sub
    End If
    If WithinReadConditionStatement = True Then
      IndentLevel -= 1
      pumlFile.WriteLine(Indent() & "endif")
    End If
    Dim ReadCondition As String = ""
    Dim ReadConditionCount As Integer = 0
    For x As Integer = WordIndex To WordIndex + 3
      Select Case cWord(x)
        Case "AT", "END", "NOT"
          ReadCondition &= cWord(x) & " "
          ReadConditionCount += 1
      End Select
    Next
    WithinReadConditionStatement = True
    pumlFile.WriteLine(Indent() & "if (" & ReadCondition.Trim & "?) then (yes)")
    IndentLevel += 1
    WordIndex += ReadConditionCount - 1
  End Sub

  Sub ProcessPumlENDREAD(ByRef WordIndex As Integer)
    If WithinReadConditionStatement = True Then
      IndentLevel -= 1
      pumlFile.WriteLine(Indent() & "endif")
      IndentLevel -= 1
    End If
    'IndentLevel -= 1
    WithinReadConditionStatement = False
    WithinReadStatement = False
  End Sub
  Sub ProcessPumlPERFORM(ByRef WordIndex As Integer)
    ' find the end of 'PERFORM' statement
    ' Out-of-line Perform is when procedure-name-1 IS specified.
    ' In-line Perform is when procedure-name-1 IS NOT specified.
    ' When in-line there must be delimited with the END-PERFORM clause
    '
    Dim EndIndex As Integer = 0
    Dim Statement As String = ""
    Dim EndPerformFound As Boolean = False
    ' looking for END-PERFORM, if PERFORM is found then there is no END-PERFORM
    For EndIndex = WordIndex + 1 To cWord.Count - 1
      If cWord(EndIndex) = "PERFORM" Then
        EndPerformFound = False
        Exit For
      End If
      If cWord(EndIndex) = "END-PERFORM" Then
        EndPerformFound = True
        Exit For
      End If
    Next
    EndIndex -= 1
    Call GetStatement(WordIndex, EndIndex, Statement)
    If EndPerformFound = False Then
      pumlFile.WriteLine(Indent() & ":" & Statement.Trim & "|")
    Else
      pumlFile.WriteLine(Indent() & "while (" & Statement.Trim & ") is (true)")
      IndentLevel += 1
    End If
    WordIndex = EndIndex
  End Sub

  Sub ProcessPumlENDPERFORM(ByRef wordIndex As Integer)
    IndentLevel -= 1
    pumlFile.WriteLine(Indent() & "endwhile (Complete)")
  End Sub
  Sub ProcessPumlEXEC(ByRef WordIndex As Integer)
    Dim EndIndex As Integer = 0
    Dim EXECStatement As String = ""
    Call GetStatement(WordIndex, EndIndex, EXECStatement)
    pumlFile.WriteLine(Indent() & ":" & EXECStatement.Trim & "}")
    WordIndex = EndIndex
  End Sub
  Sub GetStatement(ByRef WordIndex As Integer, ByRef EndIndex As Integer, ByRef statement As String)
    ' get the whole COBOL statement of this verb by looking for the next verb
    'Dim StartIndex As Integer = WordIndex
    EndIndex = IndexToNextVerb(WordIndex)
    Dim WordsTogether As String = StringTogetherWords(WordIndex, EndIndex)
    statement = AddNewLineAboutEvery30Characters(WordsTogether)
  End Sub
  Function IndexToNextVerb(ByRef StartCondIndex As Integer) As Integer
    ' cWord is a global variable
    ' VerNames is a global variable
    ' find ending index to next COBOL verb in cWord
    Dim EndCondIndex As Integer = -1
    Dim VerbIndex As Integer = -1
    For EndCondIndex = StartCondIndex + 1 To cWord.Count - 1
      If WithinReadStatement = True Then
        Select Case cWord(EndCondIndex)
          Case "AT", "END", "NOT"
            IndexToNextVerb = EndCondIndex - 1
            Exit Function
        End Select
      End If
      VerbIndex = VerbNames.IndexOf(cWord(EndCondIndex))
      If VerbIndex > -1 Then
        IndexToNextVerb = EndCondIndex - 1
        Exit Function
      End If
    Next
    IndexToNextVerb = cWord.Count - 1
  End Function
  Function StringTogetherWords(ByRef StartCondIndex As Integer, ByRef EndCondIndex As Integer) As String
    ' string together from startofconditionindex to endofconditionindex
    ' cWord is a global variable
    Dim wordsStrungTogether As String = ""
    For condIndex As Integer = StartCondIndex To EndCondIndex
      wordsStrungTogether &= cWord(condIndex) & " "
    Next
    StringTogetherWords = wordsStrungTogether.TrimEnd
  End Function
  Function AddNewLineAboutEvery30Characters(ByRef condStatement As String) As String
    ' add "\n" about every 30 characters
    Dim condStatementCR As String = ""
    Dim bytesMoved As Integer = 0
    If condStatement.Length > 30 Then
      'condStatementCR = condStatement.Substring(0, 30) & "\n"
      'StartCondIndex = 31
      For condIndex As Integer = 0 To condStatement.Length - 1
        If condStatement.Substring(condIndex, 1) = Space(1) And bytesMoved > 29 Then
          condStatementCR &= "\n"
          bytesMoved = 0
        End If
        condStatementCR &= condStatement.Substring(condIndex, 1)
        bytesMoved += 1
      Next
    Else
      condStatementCR = condStatement
    End If
    AddNewLineAboutEvery30Characters = condStatementCR
  End Function
  Private Sub btnFindInFile_Click(sender As Object, e As EventArgs) Handles btnFindInFile.Click
    Dim ofd_InFile As New OpenFileDialog

    ' browse for and select file name
    ofd_InFile.Filter = "Cobol Files (*.cbl;*.cob)|*.cbl;*.cob|" &
                        "All files(*.*)|*.*"
    ofd_InFile.Title = "Select the COBOL file"
    If ofd_InFile.ShowDialog() = DialogResult.OK Then
      txtInFile.Text = ofd_InFile.FileName
      DirectoryName = Path.GetDirectoryName(txtInFile.Text)
      FileNameOnly = Path.GetFileNameWithoutExtension(txtInFile.Text)
      txtOutFolder.Text = DirectoryName
      txtIncludeFolderName.Text = DirectoryName
      Call SetOutputFileNames()
    End If

  End Sub
  Private Sub txtInFile_Leave(sender As Object, e As EventArgs) Handles txtInFile.Leave
    Call SetOutputFileNames()
  End Sub
  Sub SetOutputFileNames()
    txtPgmFileName.Text = FileNameOnly & "_program" & ".csv"
    txtDataFileName.Text = FileNameOnly & "_data" & ".csv"
    txtParaFileName.Text = FileNameOnly & "_paragraph" & ".csv"
    txtCallsFileName.Text = FileNameOnly & "_calls" & ".csv"
    tempFileName = DirectoryName & "/" & FileNameOnly & "_hlkhlkhlk.txt"
    txtPlantUmlFilename.Text = FileNameOnly & ".puml"
  End Sub
  Private Sub btnOutFolder_Click(sender As Object, e As EventArgs) Handles btnOutFolder.Click
    ' browse for and select file name
    Dim bfd_OutFolder As New FolderBrowserDialog With {
      .SelectedPath = DirectoryName,
      .Description = "Enter Destination folder name"
    }

    If bfd_OutFolder.ShowDialog() = DialogResult.OK Then
      txtOutFolder.Text = bfd_OutFolder.SelectedPath
    End If

  End Sub

  Private Sub btnFindInclude_Click(sender As Object, e As EventArgs) Handles btnFindInclude.Click
    ' browse for Include folder
    Dim bfd_IncludeFolder As New FolderBrowserDialog With {
      .SelectedPath = DirectoryName,
      .Description = "Enter Include copy members folder name"
    }

    If bfd_IncludeFolder.ShowDialog() = DialogResult.OK Then
      txtIncludeFolderName.Text = bfd_IncludeFolder.SelectedPath
    End If
  End Sub
  Private Sub btnClose_Click(sender As Object, e As EventArgs) Handles btnClose.Click
    Me.Close()
  End Sub

  Private Sub btnOutputFiles_Click(sender As Object, e As EventArgs) Handles btnOutputFiles.Click
    If txtInFile.TextLength = 0 Then
      Exit Sub
    End If
    Try
      txtPgmFileName.Text = "program_" & FileNameOnly & ".csv"
      txtDataFileName.Text = "data_" & FileNameOnly & ".csv"
      txtParaFileName.Text = "paragraph_" & FileNameOnly & ".csv"
      txtCallsFileName.Text = "calls_" & FileNameOnly & ".csv"
      tempFileName = DirectoryName & "/hlkhlkhlk.tmp"
    Catch ex As Exception
      MessageBox.Show("Infile name invalid:" & ex.Message)
    End Try
  End Sub

  Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
    lblCopybookMessage.Text = ""
    ' This area is the COBOL Verb array with counts. 
    ' **BE SURE TO KEEP VerbNames AND VerbCount ARRAYS IN SYNC!!!**
    ' Flow commands
    VerbNames.Add("GO")
    VerbNames.Add("ALTER")
    VerbNames.Add("CALL")
    VerbNames.Add("PERFORM")
    VerbNames.Add("EVALUATE")
    VerbNames.Add("WHEN")
    VerbNames.Add("CONTINUE")
    VerbNames.Add("IF")
    VerbNames.Add("ELSE")
    VerbNames.Add("GOBACK")
    VerbNames.Add("STOP")
    VerbNames.Add("CHAIN")
    ' I/O
    VerbNames.Add("OPEN")
    VerbNames.Add("READ")
    VerbNames.Add("WRITE")
    VerbNames.Add("REWRITE")
    VerbNames.Add("CLOSE")
    VerbNames.Add("EXEC")
    VerbNames.Add("COMMIT")
    VerbNames.Add("CANCEL")
    VerbNames.Add("DELETE")
    VerbNames.Add("MERGE")
    VerbNames.Add("SORT")
    VerbNames.Add("RETURN")
    VerbNames.Add("NEXT")
    ' Maths
    VerbNames.Add("COMPUTE")
    VerbNames.Add("ADD")
    VerbNames.Add("SUBTRACT")
    VerbNames.Add("MULTIPLY")
    VerbNames.Add("DIVIDE")
    ' Misc
    VerbNames.Add("MOVE")
    VerbNames.Add("DISABLE")
    VerbNames.Add("DISPLAY")
    VerbNames.Add("ENABLE")
    VerbNames.Add("END-READ")
    VerbNames.Add("END-EVALUATE")
    VerbNames.Add("END-IF")
    VerbNames.Add("END-INVOKE")
    VerbNames.Add("END-PERFORM")
    VerbNames.Add("END-SET")
    VerbNames.Add("ENTER")
    VerbNames.Add("ENTRY")
    VerbNames.Add("EXAMINE")
    VerbNames.Add("EXECUTE")
    VerbNames.Add("EXHIBIT")
    VerbNames.Add("EXIT")
    VerbNames.Add("GENERATE")
    VerbNames.Add("INITIALIZE")
    VerbNames.Add("INITIATE")
    VerbNames.Add("INSPECT")
    VerbNames.Add("INVOKE")
    VerbNames.Add("NOTE")
    VerbNames.Add("OTHERWISE")
    VerbNames.Add("READY")
    VerbNames.Add("RECEIVE")
    VerbNames.Add("RECOVER")
    VerbNames.Add("RELEASE")
    VerbNames.Add("RESET")
    VerbNames.Add("ROLLBACK")
    VerbNames.Add("SEARCH")
    VerbNames.Add("SEND")
    VerbNames.Add("SERVICE")
    VerbNames.Add("SET")
    VerbNames.Add("START")
    VerbNames.Add("STRING")
    VerbNames.Add("SUPPRESS")
    VerbNames.Add("TERMINATE")
    VerbNames.Add("TRANSFORM")
    VerbNames.Add("UNLOCK")
    VerbNames.Add("UNSTRING")

    ' Flow commands
    VerbCount.Add(0)    'GO
    VerbCount.Add(0)    'ALTER
    VerbCount.Add(0)    'CALL
    VerbCount.Add(0)    'PERFORM
    VerbCount.Add(0)    'EVALUATE
    VerbCount.Add(0)    'WHEN
    VerbCount.Add(0)    'CONTINUE
    VerbCount.Add(0)    'IF
    VerbCount.Add(0)    'ELSE
    VerbCount.Add(0)    'GOBACK
    VerbCount.Add(0)    'STOP
    VerbCount.Add(0)    'CHAIN
    ' I/O
    VerbCount.Add(0)    'OPEN
    VerbCount.Add(0)    'READ
    VerbCount.Add(0)    'WRITE
    VerbCount.Add(0)    'REWRITE
    VerbCount.Add(0)    'CLOSE
    VerbCount.Add(0)    'EXEC
    VerbCount.Add(0)    'COMMIT
    VerbCount.Add(0)    'CANCEL
    VerbCount.Add(0)    'DELETE
    VerbCount.Add(0)    'MERGE
    VerbCount.Add(0)    'SORT
    VerbCount.Add(0)    'RETURN
    VerbCount.Add(0)    'NEXT
    ' Maths
    VerbCount.Add(0)    'COMPUTE
    VerbCount.Add(0)    'ADD
    VerbCount.Add(0)    'SUBTRACT
    VerbCount.Add(0)    'MULTIPLY
    VerbCount.Add(0)    'DIVIDE
    ' Misc
    VerbCount.Add(0)    'MOVE
    VerbCount.Add(0)    'DISABLE
    VerbCount.Add(0)    'DISPLAY
    VerbCount.Add(0)    'ENABLE
    VerbCount.Add(0)    'END-READ
    VerbCount.Add(0)    'END-EVALUATE
    VerbCount.Add(0)    'END-IF
    VerbCount.Add(0)    'END-INVOKE
    VerbCount.Add(0)    'END-PERFORM
    VerbCount.Add(0)    'END-SET
    VerbCount.Add(0)    'ENTER
    VerbCount.Add(0)    'ENTRY
    VerbCount.Add(0)    'EXAMINE
    VerbCount.Add(0)    'EXECUTE
    VerbCount.Add(0)    'EXHIBIT
    VerbCount.Add(0)    'EXIT
    VerbCount.Add(0)    'GENERATE
    VerbCount.Add(0)    'INITIALIZE
    VerbCount.Add(0)    'INITIATE
    VerbCount.Add(0)    'INSPECT
    VerbCount.Add(0)    'INVOKE
    VerbCount.Add(0)    'NOTE
    VerbCount.Add(0)    'OTHERWISE
    VerbCount.Add(0)    'READY
    VerbCount.Add(0)    'RECEIVE
    VerbCount.Add(0)    'RECOVER
    VerbCount.Add(0)    'RELEASE
    VerbCount.Add(0)    'RESET
    VerbCount.Add(0)    'ROLLBACK
    VerbCount.Add(0)    'SEARCH
    VerbCount.Add(0)    'SEND
    VerbCount.Add(0)    'SERVICE
    VerbCount.Add(0)    'SET
    VerbCount.Add(0)    'START
    VerbCount.Add(0)    'STRING
    VerbCount.Add(0)    'SUPPRESS
    VerbCount.Add(0)    'TERMINATE
    VerbCount.Add(0)    'TRANSFORM
    VerbCount.Add(0)    'UNLOCK
    VerbCount.Add(0)    'UNSTRING

  End Sub


End Class
