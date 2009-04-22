Option Strict On
Option Explicit On

Imports System.Data.SqlClient

Namespace LogParsing.QuakeObjects
    Public Class clsGame
#Region "Constants"
        Private Const MSTR_DATA_ELEMENT_DELIMITER As String = "\"
#End Region

#Region "Member Vars"
        Private mcxnStatsDB As SqlConnection

        Private mobjInitTime As clsTimestamp
        Private mobjShutdownTime As clsTimestamp
        Private mdctInitData As Dictionary(Of String, String)
        Private mlngGameType As Long

        Private mdctCurrentClients As Dictionary(Of Long, clsClient)
        Private mlstAllClients As List(Of clsClient)

        Private mblnIsCompletelyPresentInLog As Boolean
        Private mblnIsCompleteGame As Boolean

        Private mlngBeginGameDelimiterLineNo As Long
        Private mlngInitGameLineNo As Long
        Private mlngShutdownGameLineNo As Long
        Private mlngEndGameDelimiterLineNo As Long

        Private mlngGameID As Long      'DB Id, If this is non-zero, indicates existing
        Private mlngPreviousGameID As Long
        Private mlngNextGameID As Long

        Private mlstDialog As List(Of clsDialog)
        Private mlstKills As List(Of clsKill)

        Private mblnTeamScoresSet As Boolean
        Private mlngResultLineNo As Long
        Private mlngRedScore As Long
        Private mlngBlueScore As Long
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

        Public Property ResultLineNo() As Long
            Get
                Return mlngResultLineNo
            End Get
            Set(ByVal value As Long)
                mlngResultLineNo = value
            End Set
        End Property

        Public Property BeginGameDelimiterLineNo() As Long
            Get
                Return mlngBeginGameDelimiterLineNo
            End Get
            Set(ByVal value As Long)
                mlngBeginGameDelimiterLineNo = value
            End Set
        End Property

        Public Property EndGameDelimiterLineNo() As Long
            Get
                Return mlngEndGameDelimiterLineNo
            End Get
            Set(ByVal value As Long)
                mlngEndGameDelimiterLineNo = value
            End Set
        End Property

        Public Property TeamScoresSet() As Boolean
            Get
                Return mblnTeamScoresSet
            End Get
            Set(ByVal value As Boolean)
                mblnTeamScoresSet = value
            End Set
        End Property
        Public Property RedScore() As Long
            Get
                Return mlngRedScore
            End Get
            Set(ByVal value As Long)
                mlngRedScore = value
            End Set
        End Property

        Public Property BlueScore() As Long
            Get
                Return mlngBlueScore
            End Get
            Set(ByVal value As Long)
                mlngBlueScore = value
            End Set
        End Property

        Public Property PreviousGameID() As Long
            Get
                Return mlngPreviousGameID
            End Get
            Set(ByVal value As Long)
                mlngPreviousGameID = value
            End Set
        End Property

        Public Property NextGameID() As Long
            Get
                Return mlngNextGameID
            End Get
            Set(ByVal value As Long)
                mlngPreviousGameID = value
            End Set
        End Property

        Public Property GameID() As Long
            Get
                Return mlngGameID
            End Get
            Set(ByVal value As Long)
                mlngGameID = 0
            End Set
        End Property

        Public ReadOnly Property InitTime() As clsTimestamp
            Get
                Return mobjInitTime
            End Get
        End Property

        Public Property ShutdownTime() As clsTimestamp
            Get
                Return mobjShutdownTime
            End Get
            Set(ByVal value As clsTimestamp)
                mobjShutdownTime = value
            End Set
        End Property

        Public Property IsCompletelyPresentInLog() As Boolean
            Get
                Return mblnIsCompletelyPresentInLog
            End Get
            Set(ByVal value As Boolean)
                mblnIsCompletelyPresentInLog = value
            End Set
        End Property

        Public Property IsCompleteGame() As Boolean
            Get
                Return mblnIsCompleteGame
            End Get
            Set(ByVal value As Boolean)
                mblnIsCompleteGame = value
            End Set
        End Property

        Public Property ShutdownGameLineNo() As Long
            Get
                Return mlngShutdownGameLineNo
            End Get
            Set(ByVal value As Long)
                mlngShutdownGameLineNo = value
            End Set
        End Property
#End Region

#Region "Constructors"
        Public Sub New(ByRef pcxnStatsDB As SqlConnection, ByRef pobjTimestamp As clsTimestamp, ByVal plngLineNo As Long, _
                ByVal pstrInitData As String)
            StatsDB = pcxnStatsDB

            mobjInitTime = pobjTimestamp
            mobjShutdownTime = Nothing
            mlngInitGameLineNo = plngLineNo

            mdctInitData = New Dictionary(Of String, String)
            ParseInitData(pstrInitData)

            mdctCurrentClients = New Dictionary(Of Long, clsClient)
            mlstAllClients = New List(Of clsClient)

            mlngGameID = 0
            mlngNextGameID = 0
            mlngPreviousGameID = 0

            mlstDialog = New List(Of clsDialog)
            mlstKills = New List(Of clsKill)

            mblnTeamScoresSet = False
        End Sub
#End Region

#Region "Private Helpers"
        Private Sub ParseInitData(ByVal pstrInitData As String)
            Dim blnIsKey As Boolean = True
            Dim strKey As String = String.Empty

            For Each strElement As String In Split(pstrInitData, MSTR_DATA_ELEMENT_DELIMITER)
                If strElement <> String.Empty Then
                    If blnIsKey Then
                        strKey = strElement
                    Else
                        Select Case UCase$(strKey)
                            Case "G_GAMETYPE"
                                mlngGameType = CLng(strElement)
                            Case Else
                                mdctInitData.Add(strKey, strElement)
                        End Select
                    End If

                    blnIsKey = Not blnIsKey
                End If
            Next
        End Sub
#End Region

#Region "Public Functionality"
        Public Sub AddCurrentClient(ByRef pobjClient As clsClient)
            'First save the current player in the list of all time players
            mlstAllClients.Add(pobjClient)
            'Then add to current players
            Try
                mdctCurrentClients.Add(pobjClient.ClientLogID, pobjClient)
            Catch ex As Exception
                'Sometimes clients DON'T actually disconnect, so we'll have to 
                'disconnect them and create a new client here.
                Dim objOldClient As clsClient = Me.GetCurrentClient(pobjClient.ClientLogID)

                'Set disconnect time for the old player, remove them as current player
                objOldClient.DisconnectTime = pobjClient.ConnectTime
                Me.RemoveCurrentClient(pobjClient.ClientLogID)

                'Add a new current player, and set their previous client
                Me.AddCurrentClient(pobjClient)
            End Try
        End Sub

        Public Sub RemoveCurrentClient(ByVal plngClientLogID As Long)
            mdctCurrentClients.Remove(plngClientLogID)
        End Sub

        Public Function GetAllCurrentClients() As Dictionary(Of Long, clsClient)
            Return mdctCurrentClients
        End Function

        Public Function GetCurrentClient(ByVal plngClientLogID As Long) As clsClient
            Return mdctCurrentClients(plngClientLogID)
        End Function

        Public Function CurrentClientExists(ByVal plngClientLogID As Long) As Boolean
            Return mdctCurrentClients.ContainsKey(plngClientLogID)
        End Function

        Public Function GetCurrentClientByName(ByVal pstrClientName As String) As clsClient
            Dim objResult As clsClient = Nothing

            'Iterate through players in the game until finding one with a
            'matching player alias
            For Each objClient As clsClient In mdctCurrentClients.Values
                If UCase$(objClient.ClientName) = UCase$(pstrClientName) Then
                    objResult = objClient
                End If
            Next

            Return objResult
        End Function

        Public Sub AddDialog(ByVal pstrAction As String, ByVal plngLineNo As Long, ByRef pobjTimestamp As clsTimestamp, ByRef pobjSpeaker As clsClient, ByVal pstrDialog As String, _
                Optional ByVal pblnTeamSpeech As Boolean = False, Optional ByVal pblnCommand As Boolean = False, Optional ByRef pobjTarget As clsClient = Nothing)
            Dim objDialog As clsDialog

            'Create a new dialog and pop it in the list of dialogs
            objDialog = New clsDialog(mcxnStatsDB, pstrAction, plngLineNo, pobjTimestamp, pobjSpeaker, pstrDialog, pblnTeamSpeech, pblnCommand, pobjTarget)
            mlstDialog.Add(objDialog)
        End Sub

        Public Sub AddKill(ByRef pobjTimestamp As clsTimestamp, _
                ByVal plngLineNo As Long, _
                ByRef pobjVictim As clsClient, ByVal plngWeaponID As Long, _
                Optional ByRef pobjKiller As clsClient = Nothing)
            Dim objKill As clsKill

            'Create a new kill and pop it in the list of kills
            objKill = New clsKill(mcxnStatsDB, pobjTimestamp, plngLineNo, pobjKiller, pobjVictim, plngWeaponID)
            mlstKills.Add(objKill)
        End Sub

        Public Function WriteDB(ByVal plngServerUppageID As Long, ByRef ptrnWrite As SqlTransaction) As Long
            Dim strSQL As String
            Dim cmdWrite As SqlCommand = Nothing

            Dim lngClientID As Long
            Dim lngDialogID As Long
            Dim lngKillID As Long

            Dim lngGametypeID As Long

            'Select gametype DB ID from Log ID
            strSQL = "SELECT GametypeID FROM CalculatedData.Gametype WHERE GametypeLogID = @GametypeLogID "

            cmdWrite = New SqlCommand(strSQL, mcxnStatsDB)
            cmdWrite.Parameters.AddWithValue("GametypeLogID", mlngGameType)
            cmdWrite.Transaction = ptrnWrite

            lngGametypeID = CLng(cmdWrite.ExecuteScalar)

            'First, insert a game record
            strSQL = "INSERT INTO CalculatedData.Game (fkGameDelimiterLineNumberInit, fkInitGameLineNumber, fkServerUppageID, ServerInitTime "
            If mobjShutdownTime IsNot Nothing Then strSQL &= ", fkShutdownGameLineNumber, ServerShutdownTime, fkGameDelimiterLineNumberShutdown "
            If mblnTeamScoresSet Then strSQL &= ", RedScore, BlueScore, fkResultLineNumber, IsCompleteInLog "
            If lngGametypeID <> 0 Then strSQL &= ", fkGametypeID "
            strSQL &= ") VALUES (@GameDelimiterLineNumberInit, @InitGameLineNumber, @UppageID, @ServerInitTime "
            If mobjShutdownTime IsNot Nothing Then strSQL &= ", @ShutdownGameLineNumber, @ServerShutdownTime, @GameDelimiterLineNumberShutdown "
            If mblnTeamScoresSet Then strSQL &= ",@Red, @Blue, @ResultLineNumber, 1 "
            If lngGametypeID <> 0 Then strSQL &= ", @GametypeID "
            strSQL &= ") "

            cmdWrite = New SqlCommand(strSQL, mcxnStatsDB)
            cmdWrite.Parameters.AddWithValue("GameDelimiterLineNumberInit", mlngBeginGameDelimiterLineNo)
            cmdWrite.Parameters.AddWithValue("InitGameLineNumber", mlngInitGameLineNo)
            cmdWrite.Parameters.AddWithValue("UppageID", plngServerUppageID)
            cmdWrite.Parameters.AddWithValue("ServerInitTime", mobjInitTime.Seconds)
            cmdWrite.Parameters.AddWithValue("LineNo", mblnIsCompleteGame)
            If mobjShutdownTime IsNot Nothing Then
                cmdWrite.Parameters.AddWithValue("ShutdownGameLineNumber", mlngShutdownGameLineNo)
                cmdWrite.Parameters.AddWithValue("ServerShutdownTime", mobjShutdownTime.Seconds)
                cmdWrite.Parameters.AddWithValue("GameDelimiterLineNumberShutdown", mlngEndGameDelimiterLineNo)
            End If
            If mblnTeamScoresSet Then
                cmdWrite.Parameters.AddWithValue("Red", mlngRedScore)
                cmdWrite.Parameters.AddWithValue("Blue", mlngBlueScore)
                cmdWrite.Parameters.AddWithValue("ResultLineNumber", mlngResultLineNo)
            End If
            If lngGametypeID <> 0 Then cmdWrite.Parameters.AddWithValue("GametypeID", lngGametypeID)
            cmdWrite.Transaction = ptrnWrite

            cmdWrite.ExecuteNonQuery()

            strSQL = "SELECT @@IDENTITY "

            cmdWrite = New SqlCommand(strSQL, mcxnStatsDB)
            cmdWrite.Transaction = ptrnWrite

            mlngGameID = CLng(cmdWrite.ExecuteScalar)

            'Now insert each key/value pair from the init data dictionary
            strSQL = "INSERT INTO CalculatedData.GameData (fkGameID, DataKey, DataValue) " & _
                    "VALUES (@Game, @Key, @Value) "

            For Each strKey As String In mdctInitData.Keys
                cmdWrite = New SqlCommand(strSQL, mcxnStatsDB)
                cmdWrite.Transaction = ptrnWrite

                cmdWrite.Parameters.AddWithValue("Game", mlngGameID)
                cmdWrite.Parameters.AddWithValue("Key", strKey)
                cmdWrite.Parameters.AddWithValue("Value", mdctInitData(strKey))

                cmdWrite.ExecuteNonQuery()
            Next

            'Now let each client update the db
            For Each objClient As clsClient In mlstAllClients
                'Write the current client
                lngClientID = objClient.WriteDB(mlngGameID, ptrnWrite)
                If lngClientID = 0 Then
                    Throw New Exception("Client failed to write for game: " & mlngGameID)
                End If
            Next

            'Then insert all dialog
            For Each objDialog As clsDialog In mlstDialog
                'Write the current dialog
                lngDialogID = objDialog.WriteDB(mlngGameID, ptrnWrite)
                If lngDialogID = 0 Then
                    Throw New Exception("Dialog failed to write for game: " & mlngGameID)
                End If
            Next

            'Now insert the kills
            For Each objKill As clsKill In mlstKills
                'Write the current kill
                lngKillID = objKill.WriteDB(mlngGameID, ptrnWrite)
                If lngKillID = 0 Then
                    Throw New Exception("Kill failed to write for game: " & mlngGameID)
                End If
            Next

            clsSystemSettings.SetSystemSetting("LastSavedGameShutdownLineNo", CStr(mlngShutdownGameLineNo), ptrnWrite)

            Return mlngGameID
        End Function
#End Region
    End Class
End Namespace
