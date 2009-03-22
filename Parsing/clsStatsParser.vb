Option Explicit On
Option Strict On

Imports System.Data.SqlClient
Imports QuakeStats.Utilities
Imports System.IO
Imports QuakeStats.LogParsing.QuakeObjects
Imports System.Text.RegularExpressions
Imports System.Threading

'TODO: Make SystemSetting.SettingKey a unique column

Namespace LogParsing
    Public Class clsStatsParser
#Region "Constants"
        Private Const M_STR_SERVER_STARTUP_ACTION As String = "SERVERSTARTUP"
        Private Const M_STR_GAME_DELIMITER_ACTION As String = "GAMEDELIMITER"
        Private Const M_STR_SERVER_STARTUP_LINE_REGEX As String = "(?<junk>.*)\s\s0\:00\s------------------------------------------------------------"
        Private Const M_STR_GAME_DELIMITER_LINE_REGEX As String = "(?<timestamp>.{7})------------------------------------------------------------"
        Private Const M_STR_MAIN_LINE_REGEX As String = "(?<timestamp>.{7})(?<action>.+?)\:\s*(?<data>.*)"

        Private Const M_STR_CLIENTCONNECT_REGEX As String = "(?<clientNo>\d+)"
        Private Const M_STR_CLIENTDISCONNECT_REGEX As String = "(?<clientNo>\d+)"
        Private Const M_STR_CLIENTUSERINFOCHANGED_REGEX As String = "(?<clientNo>\d+)\s(?<info>.*)"
        Private Const M_STR_CLIENTBEGIN_REGEX As String = "(?<clientNo>\d+)"
        Private Const M_STR_ITEM_REGEX As String = "(?<clientNo>\d+)\s(?<itemName>.*)"
        Private Const M_STR_SAYTEAM_REGEX As String = "(?<clientName>.+)\:\s(?<message>.*)"
        Private Const M_STR_VTELL_REGEX As String = "(?<client1>.+)\sto\s(?<client2>.+)\:(?<message>.*)"
        Private Const M_STR_TELL_REGEX As String = M_STR_VTELL_REGEX
        Private Const M_STR_SAY_REGEX As String = "(?<clientName>.+)\:\s(?<message>.*)"
        Private Const M_STR_KILL_REGEX As String = "(?<killerID>\d+)\s(?<victimID>\d+)\s(?<weaponID>\d+):\s(?<killer>.+)\skilled\s(?<victim>.+)\sby\s(?<weapon>.+)"
        Private Const M_STR_RED_REGEX As String = "(?<red>\d+)\s+blue\:(?<blue>\d+)" 'RED: is action, no need to include
        Private Const M_STR_BLUE_REGEX As String = "(?<blue>\d+)\s+red\:(?<red>\d+)" 'BLUE: is action, no need to include
        Private Const M_STR_SCORE_REGEX As String = "\s*(?<score>\d+)\s+ping\:\s+(?<ping>\d+)\s+client\:\s+(?<clientID>\d+)\s+(?<client>.*)"
#End Region

#Region "Member Vars"
        Private mcxnStatsDB As SqlConnection
        Private mstrGamesLogPath As String

        Private mobjTimer As clsHighPerformanceTimer
        Private mobjItemManager As clsItemManager
        Private mobjWeaponManager As clsWeaponManager

        Private mdctActions As Dictionary(Of String, Long)
        Private mdctItems As Dictionary(Of String, Long)
        Private mdctWeapons As Dictionary(Of String, Long)

        Private mlngCurrentLineNo As Long
        Private mstrCurrentLine As String
        Private mobjCurrentTimestamp As clsTimestamp

        Private mlngFileBytes As Long
        Private mlngReadBytes As Long

        Private mobjServerUppageCurr As clsServerUppage
        Private mobjGameCurr As clsGame

        Private mlngLastParsedLogFileLineNo As Long = 0
#End Region

#Region "Properties"
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
                    ByVal pcxnStatsDB As SqlConnection)
            GamesLogPath = pstrGamesLogFilePath
            StatsDB = pcxnStatsDB

            mobjTimer = New clsHighPerformanceTimer
            mobjItemManager = New clsItemManager(mcxnStatsDB)
            mobjWeaponManager = New clsWeaponManager(mcxnStatsDB)

            mdctActions = New Dictionary(Of String, Long)
        End Sub
#End Region

#Region "Public Functionality"
        Public Sub Parse()
            'Load the current dictionaries of items and weapons
            mdctItems = mobjItemManager.GetCurrentItemLookupTable()
            mdctWeapons = mobjWeaponManager.GetCurrentWeaponLookupTable()

            'Determine size of log file
            mlngFileBytes = My.Computer.FileSystem.GetFileInfo(mstrGamesLogPath).Length

            'We always might be starting with an existing games.log, so make
            'sure we're actually working with the current server uppage
            mobjServerUppageCurr = clsServerUppage.GetCurrentServerUppage(mcxnStatsDB)

            'Get the line of the last log data line we wrote (this could be higher than the last game shutdown line)
            mlngLastParsedLogFileLineNo = CLng(clsSystemSettings.GetSystemSetting("LastSavedLogFileDataLineNo", "0"))
            
            Using reader As New StreamReader(New FileStream(mstrGamesLogPath, FileMode.Open))
                'Move to the next line which needs parsing
                SpoolToStartPosition(reader)

                'Parse the log data
                ParseLogData(reader)
            End Using
        End Sub
#End Region

#Region "Private Helpers"
        ''' <summary>
        ''' Parses the log data.  Runs through the games.log data and writes all LogFileData 
        ''' tables, also writes CalculatedData tables for all games completely present in the
        ''' log.  Does not set previous/next FKs.  Starts at line one past line of last game
        ''' completely present in the log, which was NOT written to the DB.
        ''' </summary>
        ''' <param name="prdrLogReader">The log reader, already opened and set to the initial line to parse at.</param>
        Private Sub ParseLogData(ByVal prdrLogReader As TextReader)
            Dim regexServerStartup As Regex = New Regex(M_STR_SERVER_STARTUP_LINE_REGEX, RegexOptions.ExplicitCapture)
            Dim matchServerStartup As Match
            Dim regexGameDelimiter As Regex = New Regex(M_STR_GAME_DELIMITER_LINE_REGEX, RegexOptions.ExplicitCapture)
            Dim matchGameDeliminter As Match
            Dim regexMain As Regex = New Regex(M_STR_MAIN_LINE_REGEX, RegexOptions.ExplicitCapture)
            Dim matchMain As Match
            Dim strAction As String
            Dim strData As String = String.Empty
            Dim lngLastServerUppageID As Long

            Print("Beginning log parsing...")
            mobjTimer.StartTimer()

            'Enter main read loop
            Do While prdrLogReader.Peek <> -1
                'Read the next line vars
                mlngCurrentLineNo += 1
                mstrCurrentLine = prdrLogReader.ReadLine()

                'Check for startup marker: we can't parse these lines normally cause they can
                'sometimes contain incomplete events: i.e.: 
                '7235:13Item  0:00 ------------------------------------------------------------
                matchServerStartup = regexServerStartup.Match(mstrCurrentLine)
                If matchServerStartup.Success Then
                    mobjCurrentTimestamp = New clsTimestamp("  0:00 ")
                    strAction = M_STR_SERVER_STARTUP_ACTION
                    strData = String.Empty
                Else
                    'Check for game delimiter match
                    matchGameDeliminter = regexGameDelimiter.Match(mstrCurrentLine)
                    If matchGameDeliminter.Success Then
                        mobjCurrentTimestamp = New clsTimestamp(matchGameDeliminter.Groups("timestamp").Value)
                        strAction = M_STR_GAME_DELIMITER_ACTION
                        strData = String.Empty
                    Else
                        'Should be a normal line
                        matchMain = regexMain.Match(mstrCurrentLine)
                        If Not matchMain.Success Then Throw New Exception("Error on primary line regex parse.  Line: " & mlngCurrentLineNo)

                        'Main action line: grab timestamp, action, data
                        mobjCurrentTimestamp = New clsTimestamp(matchMain.Groups("timestamp").Value)
                        strAction = matchMain.Groups("action").Value.ToUpper()
                        strData = matchMain.Groups("data").Value
                    End If
                End If

                'And call the event parsing function
                ParseEvent(strAction, strData)

                'Add action to action count dictionary
                If mdctActions.ContainsKey(strAction) Then
                    mdctActions(strAction) = mdctActions(strAction) + 1
                Else
                    mdctActions.Add(strAction, 1)
                End If

                mlngReadBytes += mstrCurrentLine.Length + 1 'add 1 for the EOL char
                If mlngCurrentLineNo Mod 5000 = 0 Then
                    Print("Parsing... On " & mlngCurrentLineNo & " read " & mlngReadBytes & " of " & mlngFileBytes & " (" & FormatNumber((mlngReadBytes / mlngFileBytes) * 100) & "%)")
                    'Application.DoEvents()
                    'mrtbConsole.ScrollToCaret()
                    'Thread.Sleep(1000)
                End If

                clsSystemSettings.SetSystemSetting("LastParsedLineNo", CStr(mlngCurrentLineNo))
            Loop

            'Write the last server uppage to the  & ")"
            lngLastServerUppageID = mobjServerUppageCurr.WriteDB()
            If lngLastServerUppageID = 0 Then Throw New Exception("Error updating server uppage: " & mobjServerUppageCurr.ServerUppageID & ".  Last line no: " & mlngCurrentLineNo)

            mobjTimer.StopTimer()
            Print("Finished in " & mobjTimer.GetResultAsTimeString & ".")

            'Print action count
            For Each strKey As String In mdctActions.Keys
                Print("'" & strKey & "' -> " & mdctActions(strKey))
            Next
        End Sub

        Private Sub ParseEvent(ByVal pstrAction As String, ByVal pstrData As String)
            Select Case pstrAction.ToUpper
                Case M_STR_SERVER_STARTUP_ACTION
                    ParseEventServerStartup()
                Case M_STR_GAME_DELIMITER_ACTION
                    ParseEventGameDelimiter()
                Case "INITGAME"
                    ParseEventInitGame(pstrData)
                Case "SHUTDOWNGAME"
                    ParseEventShutdownGame()
                Case "WARMUP"
                    ParseEventWarmup(pstrData)
                Case "CLIENTCONNECT"
                    ParseEventClientConnect(pstrData)
                Case "CLIENTDISCONNECT"
                    ParseEventClientDisconnect(pstrData)
                Case "CLIENTUSERINFOCHANGED"
                    ParseEventClientUserInfoChanged(pstrData)
                Case "CLIENTBEGIN"
                    ParseEventClientBegin(pstrData)
                Case "ITEM"
                    ParseEventItem(pstrData)
                Case "SAYTEAM" 'team broadcast
                    ParseEventSayTeam(pstrData)
                Case "VTELL" 'targeted command (to a single member of team)
                    ParseEventVTell(pstrData)
                Case "TELL" 'targeted message (to a single player)
                    ParseEventTell(pstrData)
                Case "SAY" 'broadcast
                    ParseEventSay(pstrData)
                Case "KILL"
                    ParseEventKill(pstrData)
                Case "EXIT"
                    ParseEventExit()
                Case "RED"
                    ParseEventRed(pstrData)
                Case "BLUE"
                    ParseEventBlue(pstrData)
                Case "SCORE"
                    ParseEventScore(pstrData)
                Case Else
                    Throw New Exception("Unknown action type: " & pstrAction)
            End Select
        End Sub

#Region "IndividualEventParsingRoutines"
        Private Sub ParseEventScore(ByVal pstrData As String)
            Dim trnWrite As SqlTransaction = Nothing
            Dim strSQL As String
            Dim sqlcmdWrite As SqlCommand = Nothing
            Dim regexEvent As New Regex(M_STR_SCORE_REGEX, RegexOptions.ExplicitCapture)
            Dim matchEvent As Match
            Dim lngClientLogID As Long
            Dim lngScore As Long
            Dim lngPing As Long
            Dim strClientName As String
            Dim objClient As clsClient

            matchEvent = regexEvent.Match(pstrData)
            If Not matchEvent.Success Then Throw New Exception("Bad data for event: " & pstrData)
            lngClientLogID = CLng(matchEvent.Groups("clientID").Value)
            lngScore = CLng(matchEvent.Groups("score").Value)
            lngPing = CLng(matchEvent.Groups("ping").Value)
            strClientName = matchEvent.Groups("client").Value

            If mlngLastParsedLogFileLineNo < mlngCurrentLineNo Then
                Try
                    trnWrite = mcxnStatsDB.BeginTransaction

                    strSQL = "INSERT INTO LogFileData.ScoreLine (LineNumber, Timestamp, Score, Ping, ClientLogID, ClientName) " & _
                                    "VALUES (@LineNo, @Timestamp, @Score, @Ping, @ClientLog, @ClientName) "

                    sqlcmdWrite = New SqlCommand(strSQL, mcxnStatsDB)
                    sqlcmdWrite.Parameters.AddWithValue("LineNo", mlngCurrentLineNo)
                    sqlcmdWrite.Parameters.AddWithValue("Timestamp", mobjCurrentTimestamp.RawString)
                    sqlcmdWrite.Parameters.AddWithValue("Score", lngScore)
                    sqlcmdWrite.Parameters.AddWithValue("Ping", lngPing)
                    sqlcmdWrite.Parameters.AddWithValue("ClientLog", lngClientLogID)
                    sqlcmdWrite.Parameters.AddWithValue("ClientName", strClientName)
                    sqlcmdWrite.Transaction = trnWrite

                    sqlcmdWrite.ExecuteNonQuery()

                    'Save the last log file update
                    clsSystemSettings.SetSystemSetting("LastSavedLogFileDataLineNo", CStr(mlngCurrentLineNo), trnWrite)

                    trnwrite.Commit()
                Catch ex As Exception
                    If trnWrite IsNot Nothing Then
                        trnWrite.Rollback()
                    End If

                    Throw
                End Try
            End If

            objClient = mobjGameCurr.GetCurrentClient(lngClientLogID)
            objClient.ScoreLineNo = mlngCurrentLineNo
            objClient.Score = lngScore
            objClient.Ping = lngPing
            objClient.ScoreSet = True
        End Sub

        Private Sub ParseEventBlue(ByVal pstrData As String)
            Dim strSQL As String
            Dim sqlcmdWrite As SqlCommand = Nothing
            Dim trnWrite As SqlTransaction = Nothing
            Dim regexEvent As New Regex(M_STR_BLUE_REGEX, RegexOptions.ExplicitCapture)
            Dim matchEvent As Match
            Dim lngRed As Long
            Dim lngBlue As Long

            matchEvent = regexEvent.Match(pstrData)
            If Not matchEvent.Success Then Throw New Exception("Bad data for event: " & pstrData)
            lngRed = CLng(matchEvent.Groups("red").Value)
            lngBlue = CLng(matchEvent.Groups("blue").Value)

            If mlngLastParsedLogFileLineNo < mlngCurrentLineNo Then
                Try
                    trnWrite = mcxnStatsDB.BeginTransaction

                    strSQL = "INSERT INTO LogFileData.BlueLine (LineNumber, Timestamp, Red, Blue) " & _
                                        "VALUES (@LineNo, @Timestamp, @Red, @Blue) "

                    sqlcmdWrite = New SqlCommand(strSQL, mcxnStatsDB)
                    sqlcmdWrite.Parameters.AddWithValue("LineNo", mlngCurrentLineNo)
                    sqlcmdWrite.Parameters.AddWithValue("Timestamp", mobjCurrentTimestamp.RawString)
                    sqlcmdWrite.Parameters.AddWithValue("Red", lngRed)
                    sqlcmdWrite.Parameters.AddWithValue("Blue", lngBlue)
                    sqlcmdWrite.Transaction = trnWrite

                    sqlcmdWrite.ExecuteNonQuery()

                    'Save the last log file update
                    clsSystemSettings.SetSystemSetting("LastSavedLogFileDataLineNo", CStr(mlngCurrentLineNo), trnWrite)

                    trnWrite.Commit()
                Catch ex As Exception
                    If trnWrite IsNot Nothing Then
                        trnWrite.Rollback()
                    End If

                    Throw
                End Try
            End If

            mobjGameCurr.TeamScoresSet = True
            mobjGameCurr.ResultLineNo = mlngCurrentLineNo
            mobjGameCurr.RedScore = lngRed
            mobjGameCurr.BlueScore = lngBlue
        End Sub

        Private Sub ParseEventRed(ByVal pstrData As String)
            Dim trnWrite As SqlTransaction = Nothing
            Dim strSQL As String
            Dim sqlcmdWrite As SqlCommand = Nothing
            Dim regexEvent As New Regex(M_STR_RED_REGEX, RegexOptions.ExplicitCapture)
            Dim matchEvent As Match
            Dim lngRed As Long
            Dim lngBlue As Long

            matchEvent = regexEvent.Match(pstrData)
            If Not matchEvent.Success Then Throw New Exception("Bad data for event: " & pstrData)
            lngRed = CLng(matchEvent.Groups("red").Value)
            lngBlue = CLng(matchEvent.Groups("blue").Value)

            If mlngLastParsedLogFileLineNo < mlngCurrentLineNo Then
                Try
                    trnWrite = mcxnStatsDB.BeginTransaction

                    strSQL = "INSERT INTO LogFileData.RedLine (LineNumber, Timestamp, Red, Blue) " & _
                                    "VALUES (@LineNo, @Timestamp, @Red, @Blue) "

                    sqlcmdWrite = New SqlCommand(strSQL, mcxnStatsDB)
                    sqlcmdWrite.Parameters.AddWithValue("LineNo", mlngCurrentLineNo)
                    sqlcmdWrite.Parameters.AddWithValue("Timestamp", mobjCurrentTimestamp.RawString)
                    sqlcmdWrite.Parameters.AddWithValue("Red", lngRed)
                    sqlcmdWrite.Parameters.AddWithValue("Blue", lngBlue)
                    sqlcmdWrite.Transaction = trnWrite

                    sqlcmdWrite.ExecuteNonQuery()

                    'Save the last log file update
                    clsSystemSettings.SetSystemSetting("LastSavedLogFileDataLineNo", CStr(mlngCurrentLineNo), trnWrite)

                    trnWrite.Commit()
                Catch ex As Exception
                    If trnWrite IsNot Nothing Then
                        trnWrite.Rollback()
                    End If

                    Throw
                End Try
            End If

            mobjGameCurr.TeamScoresSet = True
            mobjGameCurr.ResultLineNo = mlngCurrentLineNo
            mobjGameCurr.RedScore = lngRed
            mobjGameCurr.BlueScore = lngBlue
        End Sub

        Private Sub ParseEventExit()
            Dim trnWrite As SqlTransaction = Nothing
            Dim strSQL As String
            Dim sqlcmdWrite As SqlCommand = Nothing

            If mlngLastParsedLogFileLineNo < mlngCurrentLineNo Then
                Try
                    trnWrite = mcxnStatsDB.BeginTransaction

                    strSQL = "INSERT INTO LogFileData.ExitLine (LineNumber, Timestamp) " & _
                                    "VALUES (@LineNo, @Timestamp) "

                    sqlcmdWrite = New SqlCommand(strSQL, mcxnStatsDB)
                    sqlcmdWrite.Parameters.AddWithValue("LineNo", mlngCurrentLineNo)
                    sqlcmdWrite.Parameters.AddWithValue("Timestamp", mobjCurrentTimestamp.RawString)
                    sqlcmdWrite.Transaction = trnWrite

                    sqlcmdWrite.ExecuteNonQuery()

                    'Save the last log file update
                    clsSystemSettings.SetSystemSetting("LastSavedLogFileDataLineNo", CStr(mlngCurrentLineNo), trnWrite)

                    trnWrite.Commit()
                Catch ex As Exception
                    If trnWrite IsNot Nothing Then
                        trnWrite.Rollback()
                    End If

                    Throw
                End Try
            End If

            'The current game is finished
            mobjGameCurr.IsCompleteGame = True
        End Sub

        Private Sub ParseEventKill(ByVal pstrData As String)
            Const STR_NON_PLAYER As String = "<WORLD>"

            Dim trnWrite As SqlTransaction = Nothing
            Dim strSQL As String
            Dim sqlcmdWrite As SqlCommand = Nothing
            Dim regexEvent As New Regex(M_STR_KILL_REGEX, RegexOptions.ExplicitCapture)
            Dim matchEvent As Match
            Dim lngKillerLogID As Long
            Dim lngVictimLogID As Long
            Dim lngWeaponLogID As Long
            Dim strKillerName As String
            Dim strVictimName As String
            Dim strWeaponName As String
            Dim lngNewWeaponID As Long
            Dim objClient As clsClient
            Dim objClient2 As clsClient
            Dim blnFound As Boolean = False

            For Each matchEvent In regexEvent.Matches(pstrData)
                lngKillerLogID = CLng(matchEvent.Groups("killerID").Value)
                lngVictimLogID = CLng(matchEvent.Groups("victimID").Value)
                lngWeaponLogID = CLng(matchEvent.Groups("weaponID").Value)
                strKillerName = matchEvent.Groups("killer").Value
                strVictimName = matchEvent.Groups("victim").Value
                strWeaponName = matchEvent.Groups("weapon").Value

                If mlngLastParsedLogFileLineNo < mlngCurrentLineNo Then
                    Try
                        trnWrite = mcxnStatsDB.BeginTransaction

                        strSQL = "INSERT INTO LogFileData.KillLine (LineNumber, Timestamp, KillerLogID, VictimLogID, WeaponLogID, KillerName, VictimName, WeaponName) " & _
                                        "VALUES (@LineNo, @Timestamp, @KillerLogID, @VictimLogID, @WeaponLogID, @KillerName, @VictimName, @WeaponName) "

                        sqlcmdWrite = New SqlCommand(strSQL, mcxnStatsDB)
                        sqlcmdWrite.Parameters.AddWithValue("LineNo", mlngCurrentLineNo)
                        sqlcmdWrite.Parameters.AddWithValue("Timestamp", mobjCurrentTimestamp.RawString)
                        sqlcmdWrite.Parameters.AddWithValue("KillerLogID", lngKillerLogID)
                        sqlcmdWrite.Parameters.AddWithValue("VictimLogID", lngVictimLogID)
                        sqlcmdWrite.Parameters.AddWithValue("WeaponLogID", lngWeaponLogID)
                        sqlcmdWrite.Parameters.AddWithValue("KillerName", strKillerName)
                        sqlcmdWrite.Parameters.AddWithValue("VictimName", strVictimName)
                        sqlcmdWrite.Parameters.AddWithValue("WeaponName", strWeaponName)
                        sqlcmdWrite.Transaction = trnWrite

                        sqlcmdWrite.ExecuteNonQuery()

                        'Save the last log file update
                        clsSystemSettings.SetSystemSetting("LastSavedLogFileDataLineNo", CStr(mlngCurrentLineNo), trnWrite)

                        trnWrite.Commit()
                    Catch ex As Exception
                        If trnWrite IsNot Nothing Then
                            trnWrite.Rollback()
                        End If

                        Throw
                    End Try
                End If

                If Not UCase$(strKillerName).Equals(STR_NON_PLAYER) Then
                    objClient = mobjGameCurr.GetCurrentClient(lngKillerLogID)
                Else
                    objClient = Nothing
                End If
                objClient2 = mobjGameCurr.GetCurrentClient(lngVictimLogID)

                If mdctWeapons.ContainsKey(UCase$(strWeaponName)) Then
                    'Weapon already exists, and we have it's DB id
                    mobjGameCurr.AddKill(mobjCurrentTimestamp, mlngCurrentLineNo, objClient2, mdctWeapons(strWeaponName), objClient)
                Else
                    'Need to create the weapon, refresh the weapons collection, and add the new DB id
                    lngNewWeaponID = mobjWeaponManager.CreateNewWeapon(UCase$(strWeaponName), lngWeaponLogID)
                    mdctWeapons = mobjWeaponManager.GetCurrentWeaponLookupTable()

                    mobjGameCurr.AddKill(mobjCurrentTimestamp, mlngCurrentLineNo, objClient2, lngNewWeaponID, objClient)
                End If

                blnFound = True
            Next

            If Not blnFound Then Throw New Exception("Error with KILL on line: " & mlngCurrentLineNo)
        End Sub

        Private Sub ParseEventSay(ByVal pstrData As String)
            Dim trnWrite As SqlTransaction = Nothing
            Dim strSQL As String
            Dim sqlcmdWrite As SqlCommand = Nothing
            Dim regexEvent As New Regex(M_STR_SAY_REGEX, RegexOptions.ExplicitCapture)
            Dim matchEvent As Match
            Dim strClientName As String
            Dim strMessage As String
            Dim objClient As clsClient
            Dim blnFound As Boolean = False

            For Each matchEvent In regexEvent.Matches(pstrData)
                strClientName = CStr(matchEvent.Groups("clientName").Value)
                strMessage = CStr(matchEvent.Groups("message").Value)

                If mlngLastParsedLogFileLineNo < mlngCurrentLineNo Then
                    Try
                        trnWrite = mcxnStatsDB.BeginTransaction

                        strSQL = "INSERT INTO LogFileData.SayLine (LineNumber, Timestamp, ClientName, Message) " & _
                                        "VALUES (@LineNo, @Timestamp, @ClientName, @Message) "

                        sqlcmdWrite = New SqlCommand(strSQL, mcxnStatsDB)
                        sqlcmdWrite.Parameters.AddWithValue("LineNo", mlngCurrentLineNo)
                        sqlcmdWrite.Parameters.AddWithValue("Timestamp", mobjCurrentTimestamp.RawString)
                        sqlcmdWrite.Parameters.AddWithValue("ClientName", strClientName)
                        sqlcmdWrite.Parameters.AddWithValue("Message", strMessage)
                        sqlcmdWrite.Transaction = trnWrite

                        sqlcmdWrite.ExecuteNonQuery()

                        'Save the last log file update
                        clsSystemSettings.SetSystemSetting("LastSavedLogFileDataLineNo", CStr(mlngCurrentLineNo), trnWrite)

                        trnWrite.Commit()
                    Catch ex As Exception
                        If trnWrite IsNot Nothing Then
                            trnWrite.Rollback()
                        End If

                        Throw
                    End Try
                End If

                objClient = mobjGameCurr.GetCurrentClientByName(strClientName)

                If objClient IsNot Nothing Then
                    mobjGameCurr.AddDialog("SAY", mlngCurrentLineNo, mobjCurrentTimestamp, objClient, strMessage)
                    blnFound = True
                    Exit For
                End If
            Next

            If Not blnFound Then Throw New Exception("Couldn't find client for SAY on line: " & mstrCurrentLine)
        End Sub

        Private Sub ParseEventTell(ByVal pstrData As String)
            Dim trnWrite As SqlTransaction = Nothing
            Dim strSQL As String
            Dim sqlcmdWrite As SqlCommand = Nothing
            Dim regexEvent As New Regex(M_STR_TELL_REGEX, RegexOptions.ExplicitCapture)
            Dim matchEvent As Match
            Dim strClient1Name As String
            Dim strClient2Name As String
            Dim strMessage As String
            Dim objClient As clsClient
            Dim objClient2 As clsClient
            Dim blnFound As Boolean = False

            'We'll attempt to match all possible divisions of the string into 2 client
            'names and a message.  This is still possible to misread, if people get
            'really creative with names involving 'to', but shouldn't ever fail.
            'Some TTELL lines are simply ambiguous...
            For Each matchEvent In regexEvent.Matches(pstrData)
                strClient1Name = matchEvent.Groups("client1").Value
                strClient2Name = matchEvent.Groups("client2").Value
                strMessage = matchEvent.Groups("message").Value

                objClient = mobjGameCurr.GetCurrentClientByName(strClient1Name)
                objClient2 = mobjGameCurr.GetCurrentClientByName(strClient2Name)

                'We'll found the match we'll work with, add the dialog to the game
                If objClient IsNot Nothing AndAlso objClient2 IsNot Nothing Then
                    If mlngLastParsedLogFileLineNo < mlngCurrentLineNo Then
                        Try
                            trnWrite = mcxnStatsDB.BeginTransaction

                            strSQL = "INSERT INTO LogFileData.TellLine (LineNumber, Timestamp, ClientNameSpeaker, ClientNameTarget, Message) " & _
                                        "VALUES (@LineNo, @Timestamp, @Speaker, @Target, @Message) "

                            sqlcmdWrite = New SqlCommand(strSQL, mcxnStatsDB)
                            sqlcmdWrite.Parameters.AddWithValue("LineNo", mlngCurrentLineNo)
                            sqlcmdWrite.Parameters.AddWithValue("Timestamp", mobjCurrentTimestamp.RawString)
                            sqlcmdWrite.Parameters.AddWithValue("Speaker", strClient1Name)
                            sqlcmdWrite.Parameters.AddWithValue("Target", strClient2Name)
                            sqlcmdWrite.Parameters.AddWithValue("Message", strMessage)
                            sqlcmdWrite.Transaction = trnWrite

                            sqlcmdWrite.ExecuteNonQuery()

                            'Save the last log file update
                            clsSystemSettings.SetSystemSetting("LastSavedLogFileDataLineNo", CStr(mlngCurrentLineNo), trnWrite)

                            trnWrite.Commit()
                        Catch ex As Exception
                            If trnWrite IsNot Nothing Then
                                trnWrite.Rollback()
                            End If

                            Throw
                        End Try
                    End If

                    mobjGameCurr.AddDialog("TELL", mlngCurrentLineNo, mobjCurrentTimestamp, objClient, strMessage, _
                            False, False, objClient2)

                    blnFound = True
                    Exit For
                End If
            Next

            If Not blnFound Then Throw New Exception("Couldn't match client names in TELL on line: " & mlngCurrentLineNo & " data: " & pstrData)
        End Sub

        Private Sub ParseEventVTell(ByVal pstrData As String)
            Dim trnWrite As SqlTransaction = Nothing
            Dim strSQL As String
            Dim sqlcmdWrite As SqlCommand = Nothing
            Dim regexEvent As New Regex(M_STR_VTELL_REGEX, RegexOptions.ExplicitCapture)
            Dim matchEvent As Match
            Dim strClient1Name As String
            Dim strClient2Name As String
            Dim strMessage As String
            Dim objClient As clsClient
            Dim objClient2 As clsClient
            Dim blnFound As Boolean = False

            'We'll attempt to match all possible divisions of the string into 2 client
            'names and a message.  This is still possible to misread, if people get
            'really creative with names involving 'to', but shouldn't ever fail.
            'Some TTELL lines are simply ambiguous...
            For Each matchEvent In regexEvent.Matches(pstrData)
                strClient1Name = matchEvent.Groups("client1").Value
                strClient2Name = matchEvent.Groups("client2").Value
                strMessage = matchEvent.Groups("message").Value

                objClient = mobjGameCurr.GetCurrentClientByName(strClient1Name)
                objClient2 = mobjGameCurr.GetCurrentClientByName(strClient2Name)

                'We'll found the match we'll work with, add the dialog to the game
                If objClient IsNot Nothing AndAlso objClient2 IsNot Nothing Then
                    If mlngLastParsedLogFileLineNo < mlngCurrentLineNo Then
                        Try
                            trnWrite = mcxnStatsDB.BeginTransaction

                            strSQL = "INSERT INTO LogFileData.VTellLine (LineNumber, Timestamp, ClientNameSpeaker, ClientNameTarget, Message) " & _
                                        "VALUES (@LineNo, @Timestamp, @Speaker, @Target, @Message) "

                            sqlcmdWrite = New SqlCommand(strSQL, mcxnStatsDB)
                            sqlcmdWrite.Parameters.AddWithValue("LineNo", mlngCurrentLineNo)
                            sqlcmdWrite.Parameters.AddWithValue("Timestamp", mobjCurrentTimestamp.RawString)
                            sqlcmdWrite.Parameters.AddWithValue("Speaker", strClient1Name)
                            sqlcmdWrite.Parameters.AddWithValue("Target", strClient2Name)
                            sqlcmdWrite.Parameters.AddWithValue("Message", strMessage)
                            sqlcmdWrite.Transaction = trnWrite

                            sqlcmdWrite.ExecuteNonQuery()

                            'Save the last log file update
                            clsSystemSettings.SetSystemSetting("LastSavedLogFileDataLineNo", CStr(mlngCurrentLineNo), trnWrite)

                            trnWrite.Commit()
                        Catch ex As Exception
                            If trnWrite IsNot Nothing Then
                                trnWrite.Rollback()
                            End If

                            Throw
                        End Try
                    End If

                    mobjGameCurr.AddDialog("VTELL", mlngCurrentLineNo, mobjCurrentTimestamp, objClient, strMessage, _
                            True, True, objClient2)

                    blnFound = True
                    Exit For
                End If
            Next

            If Not blnFound Then Throw New Exception("Couldn't match client names in VTELL on line: " & mlngCurrentLineNo & " data: " & pstrData)
        End Sub

        Private Sub ParseEventSayTeam(ByVal pstrData As String)
            Dim trnWrite As SqlTransaction = Nothing
            Dim strSQL As String
            Dim sqlcmdWrite As SqlCommand = Nothing
            Dim regexEvent As New Regex(M_STR_SAYTEAM_REGEX, RegexOptions.ExplicitCapture)
            Dim matchEvent As Match
            Dim strClientName As String
            Dim strMessage As String
            Dim objClient As clsClient

            matchEvent = regexEvent.Match(pstrData)
            If Not matchEvent.Success Then Throw New Exception("Bad data for event: " & pstrData)
            strClientName = CStr(matchEvent.Groups("clientName").Value)
            strMessage = CStr(matchEvent.Groups("message").Value)

            If mlngLastParsedLogFileLineNo < mlngCurrentLineNo Then
                Try
                    trnWrite = mcxnStatsDB.BeginTransaction

                    strSQL = "INSERT INTO LogFileData.SayTeamLine (LineNumber, Timestamp, ClientName, Message) " & _
                                "VALUES (@LineNo, @Timestamp, @ClientName, @Message) "

                    sqlcmdWrite = New SqlCommand(strSQL, mcxnStatsDB)
                    sqlcmdWrite.Parameters.AddWithValue("LineNo", mlngCurrentLineNo)
                    sqlcmdWrite.Parameters.AddWithValue("Timestamp", mobjCurrentTimestamp.RawString)
                    sqlcmdWrite.Parameters.AddWithValue("ClientName", strClientName)
                    sqlcmdWrite.Parameters.AddWithValue("Message", strMessage)
                    sqlcmdWrite.Transaction = trnWrite

                    sqlcmdWrite.ExecuteNonQuery()

                    'Save the last log file update
                    clsSystemSettings.SetSystemSetting("LastSavedLogFileDataLineNo", CStr(mlngCurrentLineNo), trnWrite)

                    trnWrite.Commit()
                Catch ex As Exception
                    If trnWrite IsNot Nothing Then
                        trnWrite.Rollback()
                    End If

                    Throw
                End Try
            End If

            'Retrieve the player name by looking up the alias
            objClient = mobjGameCurr.GetCurrentClientByName(strClientName)
            mobjGameCurr.AddDialog("SAYTEAM", mlngCurrentLineNo, mobjCurrentTimestamp, objClient, strMessage, True)
        End Sub

        Private Sub ParseEventItem(ByVal pstrData As String)
            Dim trnWrite As SqlTransaction = Nothing
            Dim strSQL As String
            Dim sqlcmdWrite As SqlCommand = Nothing
            Dim regexEvent As New Regex(M_STR_ITEM_REGEX, RegexOptions.ExplicitCapture)
            Dim matchEvent As Match
            Dim lngClientNo As Long
            Dim strItemName As String
            Dim lngNewItemID As Long
            Dim objClient As clsClient

            matchEvent = regexEvent.Match(pstrData)
            If Not matchEvent.Success Then Throw New Exception("Bad data for event: " & pstrData)
            lngClientNo = CLng(matchEvent.Groups("clientNo").Value)
            strItemName = CStr(matchEvent.Groups("itemName").Value)

            If mlngLastParsedLogFileLineNo < mlngCurrentLineNo Then
                Try
                    trnWrite = mcxnStatsDB.BeginTransaction

                    strSQL = "INSERT INTO LogFileData.ItemLine (LineNumber, Timestamp, ClientLogID, ItemName) " & _
                                "VALUES (@LineNo, @Timestamp, @ClientLogNo, @ItemName) "

                    sqlcmdWrite = New SqlCommand(strSQL, mcxnStatsDB)
                    sqlcmdWrite.Parameters.AddWithValue("LineNo", mlngCurrentLineNo)
                    sqlcmdWrite.Parameters.AddWithValue("Timestamp", mobjCurrentTimestamp.RawString)
                    sqlcmdWrite.Parameters.AddWithValue("ClientLogNo", lngClientNo)
                    sqlcmdWrite.Parameters.AddWithValue("ItemName", strItemName)
                    sqlcmdWrite.Transaction = trnWrite

                    sqlcmdWrite.ExecuteNonQuery()

                    'Save the last log file update
                    clsSystemSettings.SetSystemSetting("LastSavedLogFileDataLineNo", CStr(mlngCurrentLineNo), trnWrite)

                    trnWrite.Commit()
                Catch ex As Exception
                    If trnWrite IsNot Nothing Then
                        trnWrite.Rollback()
                    End If

                    Throw
                End Try
            End If

            'Get the log id of the current player from the first part
            'of the data string, data is whatever remains.  Use the log
            'ID to retrieve the correct player object from the game object
            objClient = mobjGameCurr.GetCurrentClient(lngClientNo)
            If mdctItems.ContainsKey(strItemName.ToUpper()) Then
                'Item already exists, and we have it's DB id
                objClient.AddItemPickup(mobjCurrentTimestamp, mlngCurrentLineNo, mdctItems(strItemName.ToUpper))
            Else
                'Need to create the item, refresh the items collection, and add the new DB id
                lngNewItemID = mobjItemManager.CreateNewItem(strItemName.ToUpper(), mlngCurrentLineNo)
                mdctItems = mobjItemManager.GetCurrentItemLookupTable

                objClient.AddItemPickup(mobjCurrentTimestamp, mlngCurrentLineNo, lngNewItemID)
            End If
        End Sub

        Private Sub ParseEventClientDisconnect(ByVal pstrData As String)
            Dim trnWrite As SqlTransaction = Nothing
            Dim strSQL As String
            Dim sqlcmdWrite As SqlCommand = Nothing
            Dim regexEvent As New Regex(M_STR_CLIENTDISCONNECT_REGEX, RegexOptions.ExplicitCapture)
            Dim matchEvent As Match
            Dim lngClientNo As Long
            Dim objClient As clsClient

            matchEvent = regexEvent.Match(pstrData)
            If Not matchEvent.Success Then Throw New Exception("Bad data for event: " & pstrData)
            lngClientNo = CLng(matchEvent.Groups("clientNo").Value)

            If mlngLastParsedLogFileLineNo < mlngCurrentLineNo Then
                Try
                    trnWrite = mcxnStatsDB.BeginTransaction

                    strSQL = "INSERT INTO LogFileData.ClientDisconnectLine (LineNumber, Timestamp, ClientLogID) " & _
                                "VALUES (@LineNo, @Timestamp, @LogID) "

                    sqlcmdWrite = New SqlCommand(strSQL, mcxnStatsDB)
                    sqlcmdWrite.Parameters.AddWithValue("LineNo", mlngCurrentLineNo)
                    sqlcmdWrite.Parameters.AddWithValue("Timestamp", mobjCurrentTimestamp.RawString)
                    sqlcmdWrite.Parameters.AddWithValue("LogID", lngClientNo)
                    sqlcmdWrite.Transaction = trnWrite

                    sqlcmdWrite.ExecuteNonQuery()

                    'Save the last log file update
                    clsSystemSettings.SetSystemSetting("LastSavedLogFileDataLineNo", CStr(mlngCurrentLineNo), trnWrite)

                    trnWrite.Commit()
                Catch ex As Exception
                    If trnWrite IsNot Nothing Then
                        trnWrite.Rollback()
                    End If

                    Throw
                End Try
            End If

            If mobjGameCurr.CurrentClientExists(lngClientNo) Then
                'Get the log id of the current player from the first part
                'of the data string, data is whatever remains.  Use the log
                'ID to retrieve the correct player object from the game object
                objClient = mobjGameCurr.GetCurrentClient(lngClientNo)

                'Set disconnect time and end time for the current player, remove current player
                objClient.DisconnectTime = mobjCurrentTimestamp
                objClient.DisconnectLineNo = mlngCurrentLineNo
                objClient.EndTime = mobjCurrentTimestamp
                objClient.EndLineNo = mlngCurrentLineNo
                mobjGameCurr.RemoveCurrentClient(lngClientNo)
            Else
                'no need to disconnect
            End If
        End Sub

        Private Sub ParseEventShutdownGame()
            Dim trnWrite As SqlTransaction = Nothing
            Dim strSQL As String
            Dim sqlcmdWrite As SqlCommand = Nothing

            If mlngLastParsedLogFileLineNo < mlngCurrentLineNo Then
                Try
                    trnWrite = mcxnStatsDB.BeginTransaction

                    strSQL = "INSERT INTO LogFileData.ShutdownGameLine (LineNumber, Timestamp) " & _
                                "VALUES (@LineNo, @Timestamp) "

                    sqlcmdWrite = New SqlCommand(strSQL, mcxnStatsDB)
                    sqlcmdWrite.Parameters.AddWithValue("LineNo", mlngCurrentLineNo)
                    sqlcmdWrite.Parameters.AddWithValue("Timestamp", mobjCurrentTimestamp.RawString)
                    sqlcmdWrite.Transaction = trnWrite

                    sqlcmdWrite.ExecuteNonQuery()

                    'Save the last log file update
                    clsSystemSettings.SetSystemSetting("LastSavedLogFileDataLineNo", CStr(mlngCurrentLineNo), trnWrite)

                    trnWrite.Commit()
                Catch ex As Exception
                    If trnWrite IsNot Nothing Then
                        trnWrite.Rollback()
                    End If

                    Throw
                End Try
            End If

            'Mark the current game as completely present in the log
            mobjGameCurr.IsCompletelyPresentInLog = True
            'Save the last line no
            mobjGameCurr.ShutdownTime = mobjCurrentTimestamp
            mobjGameCurr.ShutdownGameLineNo = mlngCurrentLineNo
            'And the next line, which should always have been written, is the end delimiter
            mobjGameCurr.EndGameDelimiterLineNo = mlngCurrentLineNo + 1
            'Mark all client end/shutdown info, if not done already
            For Each objClient In mobjGameCurr.GetAllCurrentClients.Values
                If objClient.EndLineNo <> 0 Then
                    objClient.EndLineNo = mlngCurrentLineNo
                End If
                If objClient.EndTime Is Nothing Then
                    objClient.EndTime = mobjCurrentTimestamp
                End If
            Next
        End Sub

        Private Sub ParseEventClientBegin(ByVal pstrData As String)
            Dim trnWrite As SqlTransaction = Nothing
            Dim strSQL As String
            Dim sqlcmdWrite As SqlCommand = Nothing
            Dim regexEvent As New Regex(M_STR_CLIENTBEGIN_REGEX, RegexOptions.ExplicitCapture)
            Dim matchEvent As Match
            Dim lngClientNo As Long
            Dim objClient As clsClient

            matchEvent = regexEvent.Match(pstrData)
            If Not matchEvent.Success Then Throw New Exception("Bad data for event: " & pstrData)
            lngClientNo = CLng(matchEvent.Groups("clientNo").Value)

            If mlngLastParsedLogFileLineNo < mlngCurrentLineNo Then
                Try
                    trnWrite = mcxnStatsDB.BeginTransaction

                    strSQL = "INSERT INTO LogFileData.ClientBeginLine (LineNumber, Timestamp, ClientLogID) " & _
                                "VALUES (@LineNo, @Timestamp, @LogID) "

                    sqlcmdWrite = New SqlCommand(strSQL, mcxnStatsDB)
                    sqlcmdWrite.Parameters.AddWithValue("LineNo", mlngCurrentLineNo)
                    sqlcmdWrite.Parameters.AddWithValue("Timestamp", mobjCurrentTimestamp.RawString)
                    sqlcmdWrite.Parameters.AddWithValue("LogID", lngClientNo)
                    sqlcmdWrite.Transaction = trnWrite

                    sqlcmdWrite.ExecuteNonQuery()

                    'Save the last log file update
                    clsSystemSettings.SetSystemSetting("LastSavedLogFileDataLineNo", CStr(mlngCurrentLineNo), trnWrite)

                    trnWrite.Commit()
                Catch ex As Exception
                    If trnWrite IsNot Nothing Then
                        trnWrite.Rollback()
                    End If

                    Throw
                End Try
            End If

            'Get the log id of the current player from the first part
            'of the data string, data is whatever remains.  Use the log
            'ID to retrieve the correct player object from the game object
            objClient = mobjGameCurr.GetCurrentClient(CLng(pstrData))
            objClient.BeginTime = mobjCurrentTimestamp
            objClient.BeginLineNo = mlngCurrentLineNo
        End Sub

        Private Sub ParseEventClientUserInfoChanged(ByVal pstrData As String)
            Dim trnWrite As SqlTransaction = Nothing
            Dim strSQL As String
            Dim sqlcmdWrite As SqlCommand = Nothing
            Dim regexEvent As New Regex(M_STR_CLIENTUSERINFOCHANGED_REGEX, RegexOptions.ExplicitCapture)
            Dim matchEvent As Match
            Dim lngClientNo As Long
            Dim strInfo As String
            Dim objClient As clsClient
            Dim objClient2 As clsClient

            matchEvent = regexEvent.Match(pstrData)
            If Not matchEvent.Success Then Throw New Exception("Bad data for event: " & pstrData)
            lngClientNo = CLng(matchEvent.Groups("clientNo").Value)
            strInfo = CStr(matchEvent.Groups("info").Value)

            If mlngLastParsedLogFileLineNo < mlngCurrentLineNo Then
                Try
                    trnWrite = mcxnStatsDB.BeginTransaction

                    strSQL = "INSERT INTO LogFileData.ClientUserinfoChangedLine (LineNumber, Timestamp, ClientLogID, InfoString) " & _
                                "VALUES (@LineNo, @Timestamp, @LogID, @Info) "

                    sqlcmdWrite = New SqlCommand(strSQL, mcxnStatsDB)
                    sqlcmdWrite.Parameters.AddWithValue("LineNo", mlngCurrentLineNo)
                    sqlcmdWrite.Parameters.AddWithValue("Timestamp", mobjCurrentTimestamp.RawString)
                    sqlcmdWrite.Parameters.AddWithValue("LogID", lngClientNo)
                    sqlcmdWrite.Parameters.AddWithValue("Info", strInfo)
                    sqlcmdWrite.Transaction = trnWrite

                    sqlcmdWrite.ExecuteNonQuery()

                    'Save the last log file update
                    clsSystemSettings.SetSystemSetting("LastSavedLogFileDataLineNo", CStr(mlngCurrentLineNo), trnWrite)

                    trnWrite.Commit()
                Catch ex As Exception
                    If trnWrite IsNot Nothing Then
                        trnWrite.Rollback()
                    End If

                    Throw
                End Try
            End If

            'Get the log id of the current player from the first part
            'of the data string, data is whatever remains.  Use the log
            'ID to retrieve the correct player object from the game object
            objClient = mobjGameCurr.GetCurrentClient(lngClientNo)
            If Not objClient.UserInfoSet Then
                'Send the data to the player object, to populate it
                objClient.SetUserInfo(matchEvent.Groups("info").Value)
                objClient.UserinfoLineNo = mlngCurrentLineNo
            Else
                'Else, we'll need to create a new client.  First, retrieve and then
                'remove the current client after setting the stop time (used for flag calculations)
                objClient = mobjGameCurr.GetCurrentClient(lngClientNo)
                objClient.EndTime = mobjCurrentTimestamp
                objClient.EndLineNo = mlngCurrentLineNo
                mobjGameCurr.RemoveCurrentClient(lngClientNo)

                'Using the values still in the objClient object, create a new 
                'partially copied client
                objClient2 = New clsClient(mcxnStatsDB, objClient.ConnectTime, lngClientNo)
                objClient2.BeginTime = mobjCurrentTimestamp
                objClient2.UserinfoLineNo = mlngCurrentLineNo
                objClient2.SetUserInfo(strInfo)

                'And pop the new guy in the game
                mobjGameCurr.AddCurrentClient(objClient2)
            End If
        End Sub

        Private Sub ParseEventClientConnect(ByVal pstrData As String)
            Dim trnWrite As SqlTransaction = Nothing
            Dim strSQL As String
            Dim sqlcmdWrite As SqlCommand = Nothing
            Dim regexEvent As New Regex(M_STR_CLIENTCONNECT_REGEX, RegexOptions.ExplicitCapture)
            Dim matchEvent As Match
            Dim lngClientNo As Long
            Dim objClient As clsClient

            matchEvent = regexEvent.Match(pstrData)
            If Not matchEvent.Success Then Throw New Exception("Bad data for event: " & pstrData)
            lngClientNo = CLng(matchEvent.Groups("clientNo").Value)

            If mlngLastParsedLogFileLineNo < mlngCurrentLineNo Then
                Try
                    trnWrite = mcxnStatsDB.BeginTransaction

                    strSQL = "INSERT INTO LogFileData.ClientConnectLine (LineNumber, Timestamp, ClientLogID) " & _
                                "VALUES (@LineNo, @Timestamp, @LogID) "

                    sqlcmdWrite = New SqlCommand(strSQL, mcxnStatsDB)
                    sqlcmdWrite.Parameters.AddWithValue("LineNo", mlngCurrentLineNo)
                    sqlcmdWrite.Parameters.AddWithValue("Timestamp", mobjCurrentTimestamp.RawString)
                    sqlcmdWrite.Parameters.AddWithValue("LogID", lngClientNo)
                    sqlcmdWrite.Transaction = trnWrite

                    sqlcmdWrite.ExecuteNonQuery()

                    'Save the last log file update
                    clsSystemSettings.SetSystemSetting("LastSavedLogFileDataLineNo", CStr(mlngCurrentLineNo), trnWrite)

                    trnWrite.Commit()
                Catch ex As Exception
                    If trnWrite IsNot Nothing Then
                        trnWrite.Rollback()
                    End If

                    Throw
                End Try
            End If

            'Sometimes a client can connect twice in a row, without a disconnect inbetween...
            'in which case, nothing is required of this
            If Not mobjGameCurr.CurrentClientExists(lngClientNo) Then
                'Create a new player object to represent the current player,
                'add the player to the game
                objClient = New clsClient(mcxnStatsDB, mobjCurrentTimestamp, lngClientNo)
                objClient.ConnectLineNo = mlngCurrentLineNo
                mobjGameCurr.AddCurrentClient(objClient)
            Else
                'Set connect lineno on clinet
                objClient = mobjGameCurr.GetCurrentClient(lngClientNo)
                objClient.ConnectLineNo = mlngCurrentLineNo
            End If
        End Sub

        Private Sub ParseEventWarmup(ByVal pstrData As String)
            Dim trnWrite As SqlTransaction = Nothing
            Dim strSQL As String
            Dim sqlcmdWrite As SqlCommand = Nothing

            If mlngLastParsedLogFileLineNo < mlngCurrentLineNo Then
                Try
                    trnWrite = mcxnStatsDB.BeginTransaction

                    strSQL = "INSERT INTO LogFileData.WarmupLine (LineNumber, Timestamp) " & _
                               "VALUES (@LineNo, @Timestamp) "

                    sqlcmdWrite = New SqlCommand(strSQL, mcxnStatsDB)
                    sqlcmdWrite.Parameters.AddWithValue("LineNo", mlngCurrentLineNo)
                    sqlcmdWrite.Parameters.AddWithValue("Timestamp", mobjCurrentTimestamp.RawString)
                    sqlcmdWrite.Transaction = trnWrite

                    sqlcmdWrite.ExecuteNonQuery()

                    'Save the last log file update
                    clsSystemSettings.SetSystemSetting("LastSavedLogFileDataLineNo", CStr(mlngCurrentLineNo), trnWrite)

                    trnWrite.Commit()
                Catch ex As Exception
                    If trnWrite IsNot Nothing Then
                        trnWrite.Rollback()
                    End If

                    Throw
                End Try
            End If
        End Sub

        Private Sub ParseEventInitGame(ByVal pstrData As String)
            Dim trnWrite As SqlTransaction = Nothing
            Dim strSQL As String
            Dim sqlcmdWrite As SqlCommand = Nothing

            If mlngLastParsedLogFileLineNo < mlngCurrentLineNo Then
                Try
                    trnWrite = mcxnStatsDB.BeginTransaction

                    strSQL = "INSERT INTO LogFileData.InitGameLine (LineNumber, Timestamp, InfoString) " & _
                                "VALUES (@LineNo, @Timestamp, @Data) "

                    sqlcmdWrite = New SqlCommand(strSQL, mcxnStatsDB)
                    sqlcmdWrite.Parameters.AddWithValue("LineNo", mlngCurrentLineNo)
                    sqlcmdWrite.Parameters.AddWithValue("Timestamp", mobjCurrentTimestamp.RawString)
                    sqlcmdWrite.Parameters.AddWithValue("Data", pstrData)
                    sqlcmdWrite.Transaction = trnWrite

                    sqlcmdWrite.ExecuteNonQuery()

                    'Save the last log file update
                    clsSystemSettings.SetSystemSetting("LastSavedLogFileDataLineNo", CStr(mlngCurrentLineNo), trnWrite)

                    trnWrite.Commit()
                Catch ex As Exception
                    If trnWrite IsNot Nothing Then
                        trnWrite.Rollback()
                    End If

                    Throw
                End Try
            End If

            'Create a new game object to represent the current game
            mobjGameCurr = New clsGame(mcxnStatsDB, mobjCurrentTimestamp, mlngCurrentLineNo, pstrData)

            'Previous line was begin delimiter
            mobjGameCurr.BeginGameDelimiterLineNo = mlngCurrentLineNo - 1

            'Add it to the current server uppage
            mobjServerUppageCurr.AddGame(mobjGameCurr)
        End Sub

        Private Sub ParseEventGameDelimiter()
            Dim trnWrite As SqlTransaction = Nothing
            Dim strSQL As String
            Dim sqlcmdWrite As SqlCommand = Nothing

            If mlngLastParsedLogFileLineNo < mlngCurrentLineNo Then
                Try
                    trnWrite = mcxnStatsDB.BeginTransaction

                    strSQL = "INSERT INTO LogFileData.GameDelimiterLine (LineNumber, Timestamp, IsFirstLineInUppage) " & _
                                "VALUES (@LineNo, @Timestamp, 0) "

                    sqlcmdWrite = New SqlCommand(strSQL, mcxnStatsDB)
                    sqlcmdWrite.Parameters.AddWithValue("LineNo", mlngCurrentLineNo)
                    sqlcmdWrite.Parameters.AddWithValue("Timestamp", mobjCurrentTimestamp.RawString)
                    sqlcmdWrite.Transaction = trnWrite

                    sqlcmdWrite.ExecuteNonQuery()

                    'Save the last log file update
                    clsSystemSettings.SetSystemSetting("LastSavedLogFileDataLineNo", CStr(mlngCurrentLineNo), trnWrite)

                    trnWrite.Commit()
                Catch ex As Exception
                    If trnWrite IsNot Nothing Then
                        trnWrite.Rollback()
                    End If

                    Throw
                End Try
            End If
        End Sub

        Private Sub ParseEventServerStartup()
            Dim trnWrite As SqlTransaction = Nothing
            Dim strSQL As String
            Dim sqlcmdWrite As SqlCommand = Nothing

            If mlngLastParsedLogFileLineNo < mlngCurrentLineNo Then
                Try
                    trnWrite = mcxnStatsDB.BeginTransaction

                    'We'll write these to the GameDelimiterLine table, and mark the IsFirstGame flag
                    strSQL = "INSERT INTO LogFileData.GameDelimiterLine (LineNumber, Timestamp, IsFirstLineInUppage) " & _
                            "VALUES (@LineNo, @Timestamp, 1) "

                    sqlcmdWrite = New SqlCommand(strSQL, mcxnStatsDB)
                    sqlcmdWrite.Parameters.AddWithValue("LineNo", mlngCurrentLineNo)
                    sqlcmdWrite.Parameters.AddWithValue("Timestamp", mobjCurrentTimestamp.RawString)
                    sqlcmdWrite.Transaction = trnWrite

                    sqlcmdWrite.ExecuteNonQuery()

                    'Save the last log file update
                    clsSystemSettings.SetSystemSetting("LastSavedLogFileDataLineNo", CStr(mlngCurrentLineNo), trnWrite)

                    trnWrite.Commit()
                Catch ex As Exception
                    If trnWrite IsNot Nothing Then
                        trnWrite.Rollback()
                    End If

                    Throw
                End Try
            End If

            'If not on very first line of file (don't want to write empty first server uppage)
            If mlngCurrentLineNo <> 1 Then
                'If there is a current game, it is completely present in the log
                If mobjGameCurr IsNot Nothing Then mobjGameCurr.IsCompletelyPresentInLog = True

                'A new server uppage has occurred, write the current one
                'to the database and begin working with a new one.
                mobjServerUppageCurr.WriteDB()
                mobjServerUppageCurr = New clsServerUppage(mcxnStatsDB)
            End If
        End Sub
#End Region

        ''' <summary>
        ''' Moves through the games.log file to the position of the next line
        ''' which needs to be parsed.  This should be a game init line, since the
        ''' parser will always stop after the last complete (in file) game.
        ''' </summary>
        ''' <param name="prdrLogReader">Opened text reader to the games.log file, which hasn't yet read.</param>
        Private Sub SpoolToStartPosition(ByVal prdrLogReader As TextReader)
            Dim lngLastParsedLineNo As Long

            mobjTimer.StartTimer()
            Print("Beginning spooling...")

            'Determine the shutdown line no of the last written game,
            'Everything after this hasn't been parsed.
            lngLastParsedLineNo = CLng(clsSystemSettings.GetSystemSetting("LastSavedGameShutdownLineNo", "0"))

            'Spool past last parsed line
            mlngCurrentLineNo = 0
            For lngIdx As Long = 1 To lngLastParsedLineNo
                mstrCurrentLine = prdrLogReader.ReadLine()
                mlngCurrentLineNo += 1

                mlngReadBytes += mstrCurrentLine.Length + 1 'add 1 for the EOL char
                If mlngCurrentLineNo Mod 5000 = 0 Then
                    Print("Spooling... On " & mlngCurrentLineNo & " read " & mlngReadBytes & " of " & mlngFileBytes & " (" & FormatNumber((mlngReadBytes / mlngFileBytes) * 100) & "%)")
                    'Application.DoEvents()
                    'mrtbConsole.ScrollToCaret()
                    'Thread.Sleep(1000)
                End If
            Next

            mobjTimer.StopTimer()
            Print("Finished spooling in " & mobjTimer.GetResultAsTimeString & ".")
        End Sub
#End Region
    End Class
End Namespace
