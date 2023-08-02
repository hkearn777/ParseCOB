Imports System.IO


Public Class Form1

  Dim Delimiter As String = ""
  Dim SourcedFrom As String = ""
  Dim moduleName As String = ""
  Dim pgmName As String = ""
  Dim pgmSeq As Integer = 0
  Dim cWord As New List(Of String)
  Dim stmt As New List(Of String)
  Dim swPgmFile As StreamWriter = Nothing         'File holding all program names
  Dim swDataFile As StreamWriter = Nothing        'File holding all data file names
  Dim swProcFile As StreamWriter = Nothing        'File holding all Procedure names
  Dim logFile As StreamWriter = Nothing


  Dim VerbNames As New List(Of String)
  Dim VerbCount As New List(Of Integer)


  Private Sub btnParseCob_Click(sender As Object, e As EventArgs) Handles btnParseCob.Click
    '
    ' Main processing logic
    '
    txtRecordsRead.Text = ""
    txtStatementCount.Text = ""
    Delimiter = txtDelimiter.Text     'set this constant
    SourcedFrom = txtInFile.Text

    ' Validations of file names
    If Not FileNamesAreValid() Then
      Exit Sub
    End If

    'Load the infile to the stmt List
    Dim cobRecordsCount As Integer = LoadCobStatementsToArray()
    ' log file
    'logFile = My.Computer.FileSystem.OpenTextFileWriter("C:\Users\906074897\Documents\All Projects\State of Illinois\VS Projects\log.txt", False)
    'For Each statement In stmt
    '  logFile.WriteLine(statement)
    'Next
    'logFile.Close()
    '
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

    'write the three output files (Data, Proc)
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
      Case Not IsValidFileNameOrPath(txtOutFolder.Text & "/" & txtProcFileName.Text)
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
    ' Load COB lines to a Cobol statements array. 

    ' Remove the temporary work file
    Try
      If My.Computer.FileSystem.FileExists("HLKHLKHLK.tmp") Then
        My.Computer.FileSystem.DeleteFile("HLKHLKHLK.tmp")
      End If
    Catch ex As Exception
      MessageBox.Show("Removal of Temp hlk error:" & ex.Message)
      LoadCobStatementsToArray = -1
      Exit Function
    End Try

    ' Include all the COPY members to the temporary file
    Dim swTemp As StreamWriter = Nothing
    Dim CobolLines As String() = File.ReadAllLines(txtInFile.Text)
    Dim MemberName As String = ""
    Dim NumberOfCopysFound As Integer = 0
    Dim NumberOfScans As Integer = 0
    'Dim CommandIndex As Integer = -1
    Do
      NumberOfScans += 1
      NumberOfCopysFound = 0
      swTemp = New StreamWriter("HLKHLKHLK.tmp", False)
      For index As Integer = 0 To CobolLines.Count - 1
        Select Case True
          Case Len(Trim(CobolLines(index))) = 0       'drop empty lines
            Continue For
          Case Mid(CobolLines(index), 7, 1) = "*"
            swTemp.WriteLine(CobolLines(index))
          Case Else
            MemberName = CobolLines(index).PadRight(80).Substring(7).ToUpper
            Dim tWord As String() = MemberName.TrimStart.Split(New Char() {" "c})
            If tWord(0) = "COPY" Then
              MemberName = Trim(tWord(1).Replace(".", " "))
              If Len(MemberName) > 8 Then
                MessageBox.Show("somehow Member name > 8:" & MemberName)
                LoadCobStatementsToArray = -1
                Exit Function
              End If
              Mid(CobolLines(index), 7, 1) = "*"
              swTemp.WriteLine(CobolLines(index))
              NumberOfCopysFound += 1
              Call IncludeCopyMember(txtIncludeFileName.Text & "\" & MemberName & ".cbl",
                                 CobolLines, swTemp)
            Else
              swTemp.WriteLine(CobolLines(index))
            End If
        End Select
      Next
      swTemp.Close()
      If NumberOfCopysFound > 0 Then                      'we found at least 1 COPY stmt
        CobolLines = File.ReadAllLines("HLKHLKHLK.tmp")   ' so load what we got
      End If
    Loop Until NumberOfCopysFound = 0                     ' try to resolve COPY stmts


    Dim cStatement As String = ""
    Dim statement As String = ""
    Dim procIndex As Integer = 0
    Dim continuation As Boolean = False
    LoadCobStatementsToArray = 0
    CobolLines = File.ReadAllLines("HLKHLKHLK.tmp")

    For Each text1 As String In CobolLines
      LoadCobStatementsToArray += 1
      text1 = text1.Replace(vbTab, Space(1))
      If ProcessTheTextLine(text1, continuation) = -1 Then          'reject this line
        Continue For
      End If

      ' Build the COB statement
      cStatement &= text1
      ' if NOT continuing building of the COB statement, then add it to the stmt List
      If continuation = False Then
        If cStatement.Length = 0 Then       'this means a statement no commands ie 2 periods
          Continue For
        End If
        Select Case True
          Case Mid(cStatement.ToUpper, 1, 8) = "REPLACE "
            Call ReplaceAll(cStatement, CobolLines)
          Case Else
            stmt.Add(cStatement)
        End Select
        cStatement = ""
      End If
    Next

  End Function

  Sub IncludeCopyMember(ByVal CopyMember As String,
                        ByRef CobolLines() As String,
                        ByRef swTemp As StreamWriter)
    Dim IncludeLines As String() = File.ReadAllLines(CopyMember)
    ' append copymember to temp file
    For Each line As String In IncludeLines
      swTemp.WriteLine(line)
    Next
  End Sub


  Function ProcessTheTextLine(ByRef text1 As String, ByRef continuation As Boolean) As Integer

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

  Sub ReplaceAll(ByRef cStatement As String, ByRef CobolLines As String())
    Dim tWord = cStatement.Split(New Char() {" "c})
    Dim SearchFor As String = tWord(1).Replace("=", " ").Trim
    Dim ReplaceWith As String = tWord(3).Replace("=", " ").Trim
    ' loop through the CobolLines array replacing all <SearchFor> with <ReplaceWith>
    For index As Integer = 0 To CobolLines.Count - 1
      If CobolLines(index).IndexOf(SearchFor) > -1 Then
        CobolLines(index) = CobolLines(index).Replace(SearchFor, ReplaceWith)
      End If
    Next
  End Sub
  Function WriteOutput() As Integer
    ' Write the output Pgm, Data, Proc files.
    ' return of -1 means an error
    ' return of 0 means all is okay

    WriteOutput = 0

    ' Open the output files Pgm, Data, Proc and write Header rows
    If WriteOutputPgmHeaders() = -1 Then
      WriteOutput = -1
      Exit Function
    End If
    If WriteOutputDataHeaders() = -1 Then
      WriteOutput = -1
      Exit Function
    End If

    ' break the statement in words dropping empty words
    Dim stmtIndex As Integer = -1
    For Each statement As String In stmt
      stmtIndex += 1
      If Len(statement) = 0 Then
        MessageBox.Show("statement length 0? WriteOutput:stmtindex=" & Str(stmtIndex))
        Continue For
      End If
      Dim tWord = statement.Split(New Char() {" "c})
      cWord.Clear()
      For Each word In tWord
        If word.Trim.Length > 0 Then
          cWord.Add(UCase(word))
        End If
      Next

      ' Write the details to the files

      Select Case UCase(cWord(0))
        Case "PROGRAM-ID."
          Call ProcessPgm(stmtIndex)
        Case "SELECT"
          Call ProcessData()
      End Select

    Next
    ' close the files
    swPgmFile.Close()
    swDataFile.Close()

  End Function
  Sub ProcessPgm(ByRef stmtIndex As Integer)
    pgmSeq += 1
    pgmName = cWord(1)
    Dim LinesInProgram As Integer = 0
    Call CountTheProgramVerbs(stmtIndex, LinesInProgram)

    swPgmFile.Write(moduleName & Delimiter &
                        pgmName & Delimiter &
                        LTrim(Str(pgmSeq)) & Delimiter &
                        LTrim(Str(LinesInProgram)) & Delimiter)
    For Each count As Integer In VerbCount
      swPgmFile.Write(count & Delimiter)
    Next
    swPgmFile.WriteLine(SourcedFrom)

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
      MessageBox.Show("FD/SD statement not found:PGM:" & pgmName & "FILE:" & file_name_1)
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


    swDataFile.WriteLine(moduleName & Delimiter &
                         pgmName & Delimiter &
                         LTrim(Str(pgmSeq)) & Delimiter &
                         file_name_1 & Delimiter &
                         Level & Delimiter &
                         RTrim(OpenMode) & Delimiter &
                         RecordingMode & Delimiter &
                         LTrim(Str(RecordSizeMinimum)) & Delimiter &
                         LTrim(Str(RecordSizeMaximum)) & Delimiter &
                         assignment_name_1 & Delimiter &
                         organization & Delimiter &
                         SourcedFrom)
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
  Function LocateFDStatement(filename As String, ByRef fdWords As List(Of String)) As Integer
    LocateFDStatement = -1          'Not found
    For Each statement As String In stmt
      If Len(statement) = 0 Then
        Continue For
      End If
      ' parse statement into fd words
      Dim tWord = statement.Split(New Char() {" "c})
      fdWords.Clear()
      For Each word In tWord
        If word.Trim.Length > 0 Then        'dropping empty words
          fdWords.Add(UCase(word))
        End If
      Next
      If fdWords.Count >= 2 Then
        If (fdWords(0) = "FD" Or fdWords(0) = "SD") And fdWords(1) = filename Then
          LocateFDStatement = 0
          Exit For
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
    For Each statement As String In stmt
      ' parse statement into fd words and drop empty words
      Call GetCOBOLWords(statement, cblWords)
      ' find if this file_name_1 is for this program being searched
      If cblWords(0) = "PROGRAM-ID." Then
        currentPgm = cblWords(1)
        Continue For
      End If
      If currentPgm <> pgmName Then
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

  Sub GetCOBOLWords(ByRef statement As String, ByRef cblWords As List(Of String))
    ' takes the stmt and breaks into words and drops blanks
    Dim tWord = statement.Split(New Char() {" "c})
    cblWords.Clear()
    For Each word In tWord
      If word.Trim.Length > 0 Then
        cblWords.Add(UCase(word))
      End If
    Next

  End Sub
  Function GetKeywordIndex(keyword As String) As Integer
    ' find the keyword, if any, in the list
    GetKeywordIndex = cWord.IndexOf(keyword)
  End Function
  Function WriteOutputPgmHeaders() As Integer
    ' Open the output file Pgm and write Header row
    Dim PgmFileName = txtOutFolder.Text & "/" & txtPgmFileName.Text


    Try
      swPgmFile = My.Computer.FileSystem.OpenTextFileWriter(PgmFileName, False)
      swPgmFile.Write("Module" & Delimiter &
                          "PgmName" & Delimiter &
                          "PgmSeq" & Delimiter &
                          "PgmLines" & Delimiter)
      For Each verb In VerbNames
        swPgmFile.Write(verb & Delimiter)
      Next
      swPgmFile.WriteLine("Sourced From")

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
                          "FileType" & Delimiter &
                          "Sourced From")
    Catch ex As Exception
      MessageBox.Show(ex.Message, "Error opening DataFile")
      WriteOutputDataHeaders = -1
      Exit Function
    End Try

    WriteOutputDataHeaders = 0
  End Function
  Private Sub btnFindInFile_Click(sender As Object, e As EventArgs) Handles btnFindInFile.Click
    Dim ofd_InFile As New OpenFileDialog

    ' browse for and select file name
    ofd_InFile.Filter = "Cobol Files (*.cbl;*.cob)|*.cbl;*.cob|" &
                        "All files(*.*)|*.*"
    ofd_InFile.Title = "Select the COBOL file"
    If ofd_InFile.ShowDialog() = DialogResult.OK Then
      txtInFile.Text = ofd_InFile.FileName


    End If

  End Sub

  Private Sub btnOutFolder_Click(sender As Object, e As EventArgs) Handles btnOutFolder.Click
    ' browse for and select file name
    Dim bfd_OutFolder As New FolderBrowserDialog With {
      .Description = "Enter Destination folder name"
    }

    If bfd_OutFolder.ShowDialog() = DialogResult.OK Then
      txtOutFolder.Text = bfd_OutFolder.SelectedPath
    End If

  End Sub

  Private Sub btnFindInclude_Click(sender As Object, e As EventArgs) Handles btnFindInclude.Click
    ' browse for Include folder
    Dim bfd_IncludeFolder As New FolderBrowserDialog With {
      .Description = "Enter Include copy members folder name"
    }

    If bfd_IncludeFolder.ShowDialog() = DialogResult.OK Then
      txtIncludeFileName.Text = bfd_IncludeFolder.SelectedPath
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
      Dim DirectoryName As String = Path.GetDirectoryName(txtInFile.Text)
      Dim FileNameOnly As String = Path.GetFileNameWithoutExtension(txtInFile.Text)
      moduleName = FileNameOnly
      txtPgmFileName.Text = "pgm_" & FileNameOnly & ".csv"
      txtDataFileName.Text = "data_" & FileNameOnly & ".csv"
      txtProcFileName.Text = "proc_" & FileNameOnly & ".csv"
    Catch ex As Exception
      MessageBox.Show("Infile name invalid:" & ex.Message)
    End Try
  End Sub

  Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
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
    VerbNames.Add("END")
    VerbNames.Add("END-EVALUATE")
    VerbNames.Add("END-IF")
    VerbNames.Add("END-INVOKE")
    VerbNames.Add("END-PERFORM")
    VerbNames.Add("END-SET")
    VerbNames.Add("ENTER")
    VerbNames.Add("ENTRY")
    VerbNames.Add("EXAMINE")
    VerbNames.Add("EXEC")
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
    VerbCount.Add(0)    'END
    VerbCount.Add(0)    'END-EVALUATE
    VerbCount.Add(0)    'END-IF
    VerbCount.Add(0)    'END-INVOKE
    VerbCount.Add(0)    'END-PERFORM
    VerbCount.Add(0)    'END-SET
    VerbCount.Add(0)    'ENTER
    VerbCount.Add(0)    'ENTRY
    VerbCount.Add(0)    'EXAMINE
    VerbCount.Add(0)    'EXEC
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
