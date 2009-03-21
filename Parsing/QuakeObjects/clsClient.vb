Option Strict On
Option Explicit On

Imports System.Data.SqlClient

Namespace LogParsing.QuakeObjects
    Public Class clsClient
#Region "Constants"
        Private Const MSTR_DATA_ELEMENT_DELIMITER As String = "\"
#End Region

#Region "Member Vars"
        Private mcxnStatsDB As SqlConnection

        Private mlngClientID As Long        'DB ID

        Private mobjConnectTime As clsTimestamp
        Private mobjBeginTime As clsTimestamp
        Private mobjDisconnectTime As clsTimestamp
        Private mobjEndTime As clsTimestamp

        Private mlngBeginLineNo As Long
        Private mlngConnectLineNo As Long
        Private mlngEndLineNo As Long
        Private mlngDisconnectLineNo As Long

        Private mlngClientLogID As Long

        Private mblnHasInfo As Boolean
        Private mlngUserinfoLineNo As Long
        Private mstrClientName As String
        Private mlngTeamLogID As Long
        Private mstrModel As String
        Private mstrHModel As String

        Private mdctOtherData As Dictionary(Of String, String)

        Private mlstItemPickups As List(Of clsItemPickup)

        Private mblnScoreSet As Boolean
        Private mlngScoreLineNo As Long
        Private mlngScore As Long
        Private mlngPing As Long
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

        Public Property UserinfoLineNo() As Long
            Get
                Return mlngUserinfoLineNo
            End Get
            Set(ByVal value As Long)
                mlngUserinfoLineNo = value
            End Set
        End Property

        Public Property DisconnectLineNo() As Long
            Get
                Return mlngDisconnectLineNo
            End Get
            Set(ByVal value As Long)
                mlngDisconnectLineNo = value
            End Set
        End Property

        Public Property ConnectLineNo() As Long
            Get
                Return mlngConnectLineNo
            End Get
            Set(ByVal value As Long)
                mlngConnectLineNo = value
            End Set
        End Property

        Public Property ScoreLineNo() As Long
            Get
                Return mlngScoreLineNo
            End Get
            Set(ByVal value As Long)
                mlngScoreLineNo = value
            End Set
        End Property

        Public Property BeginLineNo() As Long
            Get
                Return mlngBeginLineNo
            End Get
            Set(ByVal value As Long)
                mlngBeginLineNo = value
            End Set
        End Property

        Public Property EndLineNo() As Long
            Get
                Return mlngEndLineNo
            End Get
            Set(ByVal value As Long)
                mlngEndLineNo = value
            End Set
        End Property

        Public Property ScoreSet() As Boolean
            Get
                Return mblnScoreSet
            End Get
            Set(ByVal value As Boolean)
                mblnScoreSet = value
            End Set
        End Property

        Public Property Score() As Long
            Get
                Return mlngScore
            End Get
            Set(ByVal value As Long)
                mlngScore = value
            End Set
        End Property

        Public Property Ping() As Long
            Get
                Return mlngPing
            End Get
            Set(ByVal value As Long)
                mlngPing = value
            End Set
        End Property

        Public ReadOnly Property ClientID() As Long
            Get
                Return mlngClientID
            End Get
        End Property

        Public ReadOnly Property ClientLogID() As Long
            Get
                Return mlngClientLogID
            End Get
        End Property

        Public ReadOnly Property ClientName() As String
            Get
                Return mstrClientName
            End Get
        End Property

        Public ReadOnly Property UserInfoSet() As Boolean
            Get
                Return mblnHasInfo
            End Get
        End Property

        Public ReadOnly Property ConnectTime() As clsTimestamp
            Get
                Return mobjConnectTime
            End Get
        End Property

        Public Property BeginTime() As clsTimestamp
            Get
                Return mobjBeginTime
            End Get
            Set(ByVal value As clsTimestamp)
                mobjBeginTime = value
            End Set
        End Property

        Public Property DisconnectTime() As clsTimestamp
            Get
                Return mobjDisconnectTime
            End Get
            Set(ByVal value As clsTimestamp)
                mobjDisconnectTime = value
            End Set
        End Property

        Public Property EndTime() As clsTimestamp
            Get
                Return mobjEndTime
            End Get
            Set(ByVal value As clsTimestamp)
                mobjEndTime = value
            End Set
        End Property
#End Region

#Region "Constructors"
        Public Sub New(ByRef pcxnStatsDB As SqlConnection, ByRef objTimestamp As clsTimestamp, ByVal plngClientLogID As Long)
            StatsDB = pcxnStatsDB

            mlngClientID = 0

            mobjConnectTime = objTimestamp
            mobjBeginTime = Nothing
            mobjDisconnectTime = Nothing
            mobjEndTime = Nothing

            mlngConnectLineNo = 0
            mlngBeginLineNo = 0
            mlngDisconnectLineNo = 0
            mlngEndLineNo = 0

            mlngClientLogID = plngClientLogID

            mdctOtherData = New Dictionary(Of String, String)
            mlstItemPickups = New List(Of clsItemPickup)

            mblnHasInfo = False
        End Sub
#End Region

#Region "Public Functionality"
        ''' <summary>
        ''' Parses the user info string into the data dictionary and extracts
        ''' known important settings.
        ''' </summary>
        ''' <param name="pstrUserData">The user data string.</param>
        Public Sub SetUserInfo(ByVal pstrUserData As String)
            Dim blnIsKey As Boolean = True
            Dim strKey As String = String.Empty

            If Not mblnHasInfo Then
                'Initial data setting
                For Each strElement As String In Split(pstrUserData, MSTR_DATA_ELEMENT_DELIMITER)
                    If blnIsKey Then
                        strKey = strElement
                    Else
                        Select Case UCase$(strKey)
                            Case "N"
                                mstrClientName = strElement
                            Case "T"
                                mlngTeamLogID = CLng(strElement)
                            Case "MODEL"
                                mstrModel = strElement
                            Case "HMODEL"
                                mstrHModel = strElement
                            Case Else
                                mdctOtherData.Add(strKey, strElement)
                        End Select
                    End If

                    blnIsKey = Not blnIsKey
                Next

                mblnHasInfo = True
            Else
                Throw New Exception("Client user info already set for client: " & mlngClientLogID)
            End If
        End Sub

        ''' <summary>
        ''' Adds an item pickup, to track client getting an item
        ''' </summary>
        ''' <param name="pobjTimestamp">The timestamp of the item.</param>
        ''' <param name="plngLineNo">The line no.</param>
        ''' <param name="plngItemID">The item ID.</param>
        Public Sub AddItemPickup(ByRef pobjTimestamp As clsTimestamp, ByVal plngLineNo As Long, ByVal plngItemID As Long)
            Dim objItemPickup As clsItemPickup

            'Create a new item pickup, and pop it in the list of pickups
            objItemPickup = New clsItemPickup(mcxnStatsDB, pobjTimestamp, plngLineNo, plngItemID)
            mlstItemPickups.Add(objItemPickup)
        End Sub

        ''' <summary>
        ''' Writes the DB with a client record, then all contained item pickup records.
        ''' </summary>
        ''' <param name="plngGameID">The game ID.</param>
        ''' <param name="ptrnWrite">The open transaction to write.</param>
        ''' <returns>New client db ID.</returns>
        Public Function WriteDB(ByVal plngGameID As Long, ByRef ptrnWrite As SqlTransaction) As Long
            Dim strSQL As String
            Dim cmdWrite As SqlCommand
            Dim lngItemPickupID As Long

            'First create a client record
            strSQL = "INSERT INTO CalculatedData.Client (fkGameID, ClientLogID, ServerConnectTime "
            If mlngConnectLineNo <> 0 Then strSQL &= ", fkClientConnectLineNumber "
            If mlngBeginLineNo <> 0 Then strSQL &= ", fkClientBeginLineNumber "
            If mobjBeginTime IsNot Nothing Then strSQL &= ", ServerBeginTime "
            If mobjDisconnectTime IsNot Nothing Then strSQL &= ", ServerDisconnectTime, fkClientDisconnectLineNumber "
            If mblnHasInfo Then strSQL &= ", ClientName, TeamLogID "
            If mblnScoreSet Then strSQL &= ", Score, Ping, IsFinalClient, fkScoreLineNumber "
            If mobjEndTime IsNot Nothing Then strSQL &= ", ServerEndTime "
            strSQL &= ") VALUES (@GameID, @ClientLogID, @ServerConnectTime "
            If mlngConnectLineNo <> 0 Then strSQL &= ", @ConnectLineNo "
            If mlngBeginLineNo <> 0 Then strSQL &= ", @BeginLineNo "
            If mobjBeginTime IsNot Nothing Then strSQL &= ", @ServerBeginTime "
            If mobjDisconnectTime IsNot Nothing Then strSQL &= ", @ServerDisconnectTime, @DisconnectLineNo "
            If mblnHasInfo Then strSQL &= ", @ClientName, @TeamLogID "
            If mblnScoreSet Then strSQL &= ", @Score, @Ping, @IsFinalClient, @ScoreLineNo "
            If mobjEndTime IsNot Nothing Then strSQL &= ", @ServerEndTime "
            strSQL &= ") "

            cmdWrite = New SqlCommand(strSQL, mcxnStatsDB)
            cmdWrite.Parameters.AddWithValue("GameID", plngGameID)
            cmdWrite.Parameters.AddWithValue("ClientLogID", mlngClientLogID)
            cmdWrite.Parameters.AddWithValue("ServerConnectTime", mobjConnectTime.Seconds)
            If mlngConnectLineNo <> 0 Then cmdWrite.Parameters.AddWithValue("ConnectLineNo", mlngConnectLineNo)
            If mlngBeginLineNo <> 0 Then cmdWrite.Parameters.AddWithValue("BeginLineNo", mlngBeginLineNo)
            If mobjBeginTime IsNot Nothing Then cmdWrite.Parameters.AddWithValue("ServerBeginTime", mobjBeginTime.Seconds)
            If mobjDisconnectTime IsNot Nothing Then
                cmdWrite.Parameters.AddWithValue("ServerDisconnectTime", mobjDisconnectTime.Seconds)
                cmdWrite.Parameters.AddWithValue("DisconnectLineNo", mlngDisconnectLineNo)
            End If
            If mblnHasInfo Then
                cmdWrite.Parameters.AddWithValue("ClientName", IIf(mstrClientName Is Nothing, DBNull.Value, mstrClientName))
                cmdWrite.Parameters.AddWithValue("TeamLogID", mlngTeamLogID)
            End If
            If mblnScoreSet Then
                cmdWrite.Parameters.AddWithValue("Score", mlngScore)
                cmdWrite.Parameters.AddWithValue("Ping", mlngPing)
                cmdWrite.Parameters.AddWithValue("IsFinalClient", 1)
                cmdWrite.Parameters.AddWithValue("ScoreLineNo", mlngScoreLineNo)
            End If
            If mobjEndTime IsNot Nothing Then
                cmdWrite.Parameters.AddWithValue("ServerEndTime", mobjEndTime.Seconds)
            End If
            If mlngEndLineNo <> 0 Then
                cmdWrite.Parameters.AddWithValue("EndLineNo", mlngEndLineNo)
            End If
            cmdWrite.Transaction = ptrnWrite

            cmdWrite.ExecuteScalar()

            strSQL = "SELECT @@IDENTITY "

            cmdWrite = New SqlCommand(strSQL, mcxnStatsDB)
            cmdWrite.Transaction = ptrnWrite

            mlngClientID = CLng(cmdWrite.ExecuteScalar)

            'Now insert each key/value pair from the user data dictionary
            strSQL = "INSERT INTO CalculatedData.ClientData (fkClientID, DataKey, DataValue) " & _
                    "VALUES (@Client, @Key, @Value) "

            For Each strKey As String In mdctOtherData.Keys
                cmdWrite = New SqlCommand(strSQL, mcxnStatsDB)
                cmdWrite.Transaction = ptrnWrite

                cmdWrite.Parameters.AddWithValue("Client", mlngClientID)
                cmdWrite.Parameters.AddWithValue("Key", strKey)
                cmdWrite.Parameters.AddWithValue("Value", mdctOtherData(strKey))

                cmdWrite.ExecuteNonQuery()
            Next

            'Next, insert all item pickups
            For Each objItemPickup As clsItemPickup In mlstItemPickups
                'Write the current item pickup
                lngItemPickupID = objItemPickup.WriteDB(mlngClientID, plngGameID, ptrnWrite)
                If lngItemPickupID = 0 Then
                    Throw New Exception("Item pickup failed to write for client: " & mlngClientID)
                End If
            Next

            Return mlngClientID
        End Function
#End Region
    End Class
End Namespace