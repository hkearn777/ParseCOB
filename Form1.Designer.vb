<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Form1
  Inherits System.Windows.Forms.Form

  'Form overrides dispose to clean up the component list.
  <System.Diagnostics.DebuggerNonUserCode()> _
  Protected Overrides Sub Dispose(ByVal disposing As Boolean)
    Try
      If disposing AndAlso components IsNot Nothing Then
        components.Dispose()
      End If
    Finally
      MyBase.Dispose(disposing)
    End Try
  End Sub

  'Required by the Windows Form Designer
  Private components As System.ComponentModel.IContainer

  'NOTE: The following procedure is required by the Windows Form Designer
  'It can be modified using the Windows Form Designer.  
  'Do not modify it using the code editor.
  <System.Diagnostics.DebuggerStepThrough()> _
  Private Sub InitializeComponent()
        Me.btnFindInFile = New System.Windows.Forms.Button()
        Me.txtInFile = New System.Windows.Forms.TextBox()
        Me.btnOutFolder = New System.Windows.Forms.Button()
        Me.txtOutFolder = New System.Windows.Forms.TextBox()
        Me.txtDataFileName = New System.Windows.Forms.TextBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.txtParaFileName = New System.Windows.Forms.TextBox()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.txtDelimiter = New System.Windows.Forms.TextBox()
        Me.btnParseCob = New System.Windows.Forms.Button()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.txtRecordsRead = New System.Windows.Forms.TextBox()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.txtStatementCount = New System.Windows.Forms.TextBox()
        Me.btnClose = New System.Windows.Forms.Button()
        Me.FolderBrowserDialog1 = New System.Windows.Forms.FolderBrowserDialog()
        Me.OpenFileDialog1 = New System.Windows.Forms.OpenFileDialog()
        Me.SaveFileDialog1 = New System.Windows.Forms.SaveFileDialog()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.txtPgmFileName = New System.Windows.Forms.TextBox()
        Me.btnFindInclude = New System.Windows.Forms.Button()
        Me.txtIncludeFolderName = New System.Windows.Forms.TextBox()
        Me.btnOutputFiles = New System.Windows.Forms.Button()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.txtCallsFileName = New System.Windows.Forms.TextBox()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.txtExtension = New System.Windows.Forms.TextBox()
        Me.lblCopybookMessage = New System.Windows.Forms.Label()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.txtPlantUmlFilename = New System.Windows.Forms.TextBox()
        Me.SuspendLayout()
        '
        'btnFindInFile
        '
        Me.btnFindInFile.Location = New System.Drawing.Point(13, 13)
        Me.btnFindInFile.Name = "btnFindInFile"
        Me.btnFindInFile.Size = New System.Drawing.Size(128, 64)
        Me.btnFindInFile.TabIndex = 0
        Me.btnFindInFile.Text = "Main Program File"
        Me.btnFindInFile.UseVisualStyleBackColor = True
        '
        'txtInFile
        '
        Me.txtInFile.Location = New System.Drawing.Point(154, 32)
        Me.txtInFile.Name = "txtInFile"
        Me.txtInFile.Size = New System.Drawing.Size(1027, 26)
        Me.txtInFile.TabIndex = 1
        '
        'btnOutFolder
        '
        Me.btnOutFolder.Location = New System.Drawing.Point(14, 159)
        Me.btnOutFolder.Name = "btnOutFolder"
        Me.btnOutFolder.Size = New System.Drawing.Size(111, 31)
        Me.btnOutFolder.TabIndex = 2
        Me.btnOutFolder.Text = "Out Folder"
        Me.btnOutFolder.UseVisualStyleBackColor = True
        '
        'txtOutFolder
        '
        Me.txtOutFolder.Location = New System.Drawing.Point(154, 161)
        Me.txtOutFolder.Name = "txtOutFolder"
        Me.txtOutFolder.Size = New System.Drawing.Size(1027, 26)
        Me.txtOutFolder.TabIndex = 3
        '
        'txtDataFileName
        '
        Me.txtDataFileName.Location = New System.Drawing.Point(419, 208)
        Me.txtDataFileName.Name = "txtDataFileName"
        Me.txtDataFileName.Size = New System.Drawing.Size(174, 26)
        Me.txtDataFileName.TabIndex = 5
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Location = New System.Drawing.Point(365, 210)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(48, 20)
        Me.Label2.TabIndex = 6
        Me.Label2.Text = "Data:"
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Location = New System.Drawing.Point(613, 211)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(95, 20)
        Me.Label3.TabIndex = 7
        Me.Label3.Text = "Paragraphs:"
        '
        'txtParaFileName
        '
        Me.txtParaFileName.Location = New System.Drawing.Point(705, 208)
        Me.txtParaFileName.Name = "txtParaFileName"
        Me.txtParaFileName.Size = New System.Drawing.Size(168, 26)
        Me.txtParaFileName.TabIndex = 8
        '
        'Label4
        '
        Me.Label4.AutoSize = True
        Me.Label4.Location = New System.Drawing.Point(14, 252)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(75, 20)
        Me.Label4.TabIndex = 9
        Me.Label4.Text = "Delimiter:"
        '
        'txtDelimiter
        '
        Me.txtDelimiter.Location = New System.Drawing.Point(96, 252)
        Me.txtDelimiter.Name = "txtDelimiter"
        Me.txtDelimiter.Size = New System.Drawing.Size(45, 26)
        Me.txtDelimiter.TabIndex = 10
        Me.txtDelimiter.Text = "|"
        Me.txtDelimiter.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'btnParseCob
        '
        Me.btnParseCob.Location = New System.Drawing.Point(18, 295)
        Me.btnParseCob.Name = "btnParseCob"
        Me.btnParseCob.Size = New System.Drawing.Size(123, 48)
        Me.btnParseCob.TabIndex = 11
        Me.btnParseCob.Text = "ParseCob"
        Me.btnParseCob.UseVisualStyleBackColor = True
        '
        'Label5
        '
        Me.Label5.AutoSize = True
        Me.Label5.Location = New System.Drawing.Point(159, 309)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(116, 20)
        Me.Label5.TabIndex = 12
        Me.Label5.Text = "Records Read:"
        '
        'txtRecordsRead
        '
        Me.txtRecordsRead.Location = New System.Drawing.Point(281, 303)
        Me.txtRecordsRead.Name = "txtRecordsRead"
        Me.txtRecordsRead.Size = New System.Drawing.Size(100, 26)
        Me.txtRecordsRead.TabIndex = 13
        '
        'Label6
        '
        Me.Label6.AutoSize = True
        Me.Label6.Location = New System.Drawing.Point(423, 306)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(134, 20)
        Me.Label6.TabIndex = 14
        Me.Label6.Text = "COB Statements:"
        '
        'txtStatementCount
        '
        Me.txtStatementCount.Location = New System.Drawing.Point(563, 303)
        Me.txtStatementCount.Name = "txtStatementCount"
        Me.txtStatementCount.Size = New System.Drawing.Size(100, 26)
        Me.txtStatementCount.TabIndex = 15
        '
        'btnClose
        '
        Me.btnClose.Location = New System.Drawing.Point(1107, 298)
        Me.btnClose.Name = "btnClose"
        Me.btnClose.Size = New System.Drawing.Size(75, 36)
        Me.btnClose.TabIndex = 16
        Me.btnClose.Text = "Close"
        Me.btnClose.UseVisualStyleBackColor = True
        '
        'OpenFileDialog1
        '
        Me.OpenFileDialog1.FileName = "OpenFileDialog1"
        '
        'Label7
        '
        Me.Label7.AutoSize = True
        Me.Label7.Location = New System.Drawing.Point(155, 211)
        Me.Label7.Name = "Label7"
        Me.Label7.Size = New System.Drawing.Size(45, 20)
        Me.Label7.TabIndex = 17
        Me.Label7.Text = "Pgm:"
        '
        'txtPgmFileName
        '
        Me.txtPgmFileName.Location = New System.Drawing.Point(206, 207)
        Me.txtPgmFileName.Name = "txtPgmFileName"
        Me.txtPgmFileName.Size = New System.Drawing.Size(153, 26)
        Me.txtPgmFileName.TabIndex = 18
        '
        'btnFindInclude
        '
        Me.btnFindInclude.Location = New System.Drawing.Point(12, 83)
        Me.btnFindInclude.Name = "btnFindInclude"
        Me.btnFindInclude.Size = New System.Drawing.Size(129, 55)
        Me.btnFindInclude.TabIndex = 19
        Me.btnFindInclude.Text = "Include/Copy Folder"
        Me.btnFindInclude.UseVisualStyleBackColor = True
        '
        'txtIncludeFolderName
        '
        Me.txtIncludeFolderName.Location = New System.Drawing.Point(154, 85)
        Me.txtIncludeFolderName.Name = "txtIncludeFolderName"
        Me.txtIncludeFolderName.Size = New System.Drawing.Size(1027, 26)
        Me.txtIncludeFolderName.TabIndex = 20
        '
        'btnOutputFiles
        '
        Me.btnOutputFiles.Location = New System.Drawing.Point(18, 207)
        Me.btnOutputFiles.Name = "btnOutputFiles"
        Me.btnOutputFiles.Size = New System.Drawing.Size(106, 29)
        Me.btnOutputFiles.TabIndex = 21
        Me.btnOutputFiles.Text = "Output Files"
        Me.btnOutputFiles.UseVisualStyleBackColor = True
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(898, 213)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(47, 20)
        Me.Label1.TabIndex = 22
        Me.Label1.Text = "Calls:"
        '
        'txtCallsFileName
        '
        Me.txtCallsFileName.Location = New System.Drawing.Point(951, 210)
        Me.txtCallsFileName.Name = "txtCallsFileName"
        Me.txtCallsFileName.Size = New System.Drawing.Size(175, 26)
        Me.txtCallsFileName.TabIndex = 23
        '
        'Label8
        '
        Me.Label8.AutoSize = True
        Me.Label8.Location = New System.Drawing.Point(154, 118)
        Me.Label8.Name = "Label8"
        Me.Label8.Size = New System.Drawing.Size(83, 20)
        Me.Label8.TabIndex = 26
        Me.Label8.Text = "Extension:"
        '
        'txtExtension
        '
        Me.txtExtension.Location = New System.Drawing.Point(244, 118)
        Me.txtExtension.Name = "txtExtension"
        Me.txtExtension.Size = New System.Drawing.Size(44, 26)
        Me.txtExtension.TabIndex = 27
        '
        'lblCopybookMessage
        '
        Me.lblCopybookMessage.AutoSize = True
        Me.lblCopybookMessage.BackColor = System.Drawing.SystemColors.Info
        Me.lblCopybookMessage.Location = New System.Drawing.Point(14, 361)
        Me.lblCopybookMessage.Name = "lblCopybookMessage"
        Me.lblCopybookMessage.Size = New System.Drawing.Size(57, 20)
        Me.lblCopybookMessage.TabIndex = 28
        Me.lblCopybookMessage.Text = "Label9"
        '
        'Label9
        '
        Me.Label9.AutoSize = True
        Me.Label9.Location = New System.Drawing.Point(868, 255)
        Me.Label9.Name = "Label9"
        Me.Label9.Size = New System.Drawing.Size(77, 20)
        Me.Label9.TabIndex = 29
        Me.Label9.Text = "PlantUml:"
        '
        'txtPlantUmlFilename
        '
        Me.txtPlantUmlFilename.Location = New System.Drawing.Point(951, 251)
        Me.txtPlantUmlFilename.Name = "txtPlantUmlFilename"
        Me.txtPlantUmlFilename.Size = New System.Drawing.Size(175, 26)
        Me.txtPlantUmlFilename.TabIndex = 30
        '
        'Form1
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(9.0!, 20.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(1203, 407)
        Me.Controls.Add(Me.txtPlantUmlFilename)
        Me.Controls.Add(Me.Label9)
        Me.Controls.Add(Me.lblCopybookMessage)
        Me.Controls.Add(Me.txtExtension)
        Me.Controls.Add(Me.Label8)
        Me.Controls.Add(Me.txtCallsFileName)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.btnOutputFiles)
        Me.Controls.Add(Me.txtIncludeFolderName)
        Me.Controls.Add(Me.btnFindInclude)
        Me.Controls.Add(Me.txtPgmFileName)
        Me.Controls.Add(Me.Label7)
        Me.Controls.Add(Me.btnClose)
        Me.Controls.Add(Me.txtStatementCount)
        Me.Controls.Add(Me.Label6)
        Me.Controls.Add(Me.txtRecordsRead)
        Me.Controls.Add(Me.Label5)
        Me.Controls.Add(Me.btnParseCob)
        Me.Controls.Add(Me.txtDelimiter)
        Me.Controls.Add(Me.Label4)
        Me.Controls.Add(Me.txtParaFileName)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.txtDataFileName)
        Me.Controls.Add(Me.txtOutFolder)
        Me.Controls.Add(Me.btnOutFolder)
        Me.Controls.Add(Me.txtInFile)
        Me.Controls.Add(Me.btnFindInFile)
        Me.Name = "Form1"
        Me.Text = "ParseCOB"
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents btnFindInFile As Button
    Friend WithEvents txtInFile As TextBox
    Friend WithEvents btnOutFolder As Button
    Friend WithEvents txtOutFolder As TextBox
    Friend WithEvents txtDataFileName As TextBox
    Friend WithEvents Label2 As Label
    Friend WithEvents Label3 As Label
    Friend WithEvents txtParaFileName As TextBox
    Friend WithEvents Label4 As Label
    Friend WithEvents txtDelimiter As TextBox
    Friend WithEvents btnParseCob As Button
    Friend WithEvents Label5 As Label
    Friend WithEvents txtRecordsRead As TextBox
    Friend WithEvents Label6 As Label
    Friend WithEvents txtStatementCount As TextBox
    Friend WithEvents btnClose As Button
    Friend WithEvents FolderBrowserDialog1 As FolderBrowserDialog
    Friend WithEvents OpenFileDialog1 As OpenFileDialog
    Friend WithEvents SaveFileDialog1 As SaveFileDialog
    Friend WithEvents Label7 As Label
    Friend WithEvents txtPgmFileName As TextBox
    Friend WithEvents btnFindInclude As Button
    Friend WithEvents txtIncludeFolderName As TextBox
    Friend WithEvents btnOutputFiles As Button
    Friend WithEvents Label1 As Label
    Friend WithEvents txtCallsFileName As TextBox
    Friend WithEvents Label8 As Label
    Friend WithEvents txtExtension As TextBox
    Friend WithEvents lblCopybookMessage As Label
    Friend WithEvents Label9 As Label
    Friend WithEvents txtPlantUmlFilename As TextBox
End Class
