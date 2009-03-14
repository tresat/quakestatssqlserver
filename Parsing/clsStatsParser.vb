Option Explicit On
Option Strict On

Imports System.Data.SqlClient
Imports QuakeStats.Utilities
Imports System.IO

Namespace LogParsing
    Public Class clsStatsParser
#Region "Member Vars"
        Private mcxnStatsDB As SqlConnection
        Private mstrGamesLogPath As String
        Private mrtbConsole As RichTextBox

        Private mobjTimer As clsHighPerformanceTimer
        Private mobjItemManager As clsItemManager
        Private mobjWeaponManager As clsWeaponManager

        Private mdctItems As Dictionary(Of String, Long)
        Private mdctWeapons As Dictionary(Of String, Long)

        Private mlngCurrentLineNo As Long
        Private mstrCurrentLine As String

        Private mlngFileBytes As Long
#End Region

#Region "Properties"
        Public Property Console() As RichTextBox
            Get
                Return mrtbConsole
            End Get
            Set(ByVal value As RichTextBox)
                mrtbConsole = value
            End Set
        End Property

        Public Property StatsDB() As SqlConnection
            Get
                Return mcxnStatsDB
            End Get
            Set(ByVal value As SqlConnection)
                mcxnStatsDB = value
                VerifyDBConnected(mcxnStatsDB)
            End Set
        End Property

        Public Property GamesLogPath() As String
            Get
                Return mstrGamesLogPath
            End Get
            Set(ByVal value As String)
                mstrGamesLogPath = value
            End Set
        End Property
#End Region

#Region "Constructors"
        Public Sub New(ByVal pstrGamesLogFilePath As String, _
                    ByVal pcxnStatsDB As SqlConnection, _
                    ByVal prtbConsole As RichTextBox)
            GamesLogPath = pstrGamesLogFilePath
            StatsDB = pcxnStatsDB
            Console = prtbConsole

            mobjTimer = New clsHighPerformanceTimer
            mobjItemManager = New clsItemManager(mcxnStatsDB)
            mobjWeaponManager = New clsWeaponManager(mcxnStatsDB)
        End Sub
#End Region

#Region "Public Functionality"
        Public Sub Parse()
            'Load the current dictionaries of items and weapons
            mdctItems = mobjItemManager.GetCurrentItemLookupTable()
            mdctWeapons = mobjWeaponManager.GetCurrentWeaponLookupTable()

            'Determine size of log file
            mlngFileBytes = My.Computer.FileSystem.GetFileInfo(mstrGamesLogPath).Length

            Using reader As New StreamReader(New FileStream(mstrGamesLogPath, FileMode.Open))
                'Move to the next line which needs parsing
                SpoolToStartPosition(reader)




            End Using
        End Sub
#End Region

#Region "Private Helpers"
        ''' <summary>
        ''' Moves through the games.log file to the position of the next line
        ''' which needs to be parsed.  This should be a game init line, since the
        ''' parser will always stop after the last complete (in file) game.
        ''' </summary>
        ''' <param name="prdrLogReader">Opened text reader to the games.log file, which hasn't yet read.</param>
        Private Sub SpoolToStartPosition(ByVal prdrLogReader As TextReader)
            Dim lngLastParsedLineNo As Long
            Dim lngReadBytes As Long = 0

            mobjTimer.StartTimer()
            Print("Beginning spooling...")

            'Start on line 1
            mlngCurrentLineNo = 1
            mstrCurrentLine = prdrLogReader.ReadLine

            'Determine the shutdown line no of the last written game,
            'Everything after this hasn't been parsed.
            lngLastParsedLineNo = CLng(clsSystemSettings.GetSystemSetting("LastSavedGameShutdownLineNo", "0"))

            'Spool past last parsed line
            mlngCurrentLineNo = 0
            For lngIdx As Long = 1 To lngLastParsedLineNo
                mstrCurrentLine = prdrLogReader.ReadLine()
                mlngCurrentLineNo += 1

                lngReadBytes += mstrCurrentLine.Length + 1 'add 1 for the EOL char
                If mlngCurrentLineNo Mod 100 = 0 Then Print("Spooling... On " & mlngCurrentLineNo & " read " & lngReadBytes & " of " & mlngFileBytes & " (" & FormatNumber((lngReadBytes / mlngFileBytes) * 100) & "%)")
            Next

            mobjTimer.StopTimer()
            Print("Finished spooling in " & mobjTimer.GetResultAsTimeString & ".")
        End Sub

        ''' <summary>
        ''' Adds the message to the console textbox.
        ''' </summary>
        ''' <param name="pstrMsg">The text to add to the textbox.</param>
        Private Sub Print(ByVal pstrMsg As String, Optional ByVal pblnNewLine As Boolean = True)
            mrtbConsole.Text &= pstrMsg & CStr(IIf(pblnNewLine, vbCrLf, String.Empty))
        End Sub
#End Region
    End Class
End Namespace
