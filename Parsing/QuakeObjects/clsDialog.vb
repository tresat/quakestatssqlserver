Option Strict On
Option Explicit On

Imports System.Data.SqlClient

Namespace LogParsing.QuakeObjects
    Public Class clsDialog
#Region "Member Vars"
        Private mcxnStatsDB As SqlConnection

        Private mlngDialogID As Long

        Private mstrAction As String
        Private mobjTimestamp As clsTimestamp
        Private mstrDialog As String
        Private mblnTeamSpeech As Boolean
        Private mblnCommand As Boolean
        Private mobjSpeaker As clsClient
        Private mobjTarget As clsClient

        Private mlngLineNo As Long
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
#End Region

#Region "Constructors"
        Public Sub New(ByRef pcxnStatsDB As SqlConnection, ByVal pstrAction As String, ByVal plngLineNo As Long, ByRef pobjTimestamp As clsTimestamp, ByRef pobjSpeaker As clsClient, ByVal pstrDialog As String, _
                Optional ByVal pblnTeamSpeech As Boolean = False, Optional ByVal pblnCommand As Boolean = False, _
                Optional ByRef pobjTargetClient As clsClient = Nothing)
            StatsDB = pcxnStatsDB

            mstrAction = pstrAction
            mobjTimestamp = pobjTimestamp
            mstrDialog = pstrDialog
            mblnTeamSpeech = pblnTeamSpeech
            mblnCommand = pblnCommand
            mobjSpeaker = pobjSpeaker
            mobjTarget = pobjTargetClient
            mlngLineNo = plngLineNo
        End Sub
#End Region

#Region "Public Functionality"
        ''' <summary>
        ''' Writes the DB with a dialog record.
        ''' </summary>
        ''' <param name="plngGameID">The game ID.</param>
        ''' <param name="ptrnWrite">The open transaction to write to.</param>
        ''' <returns>Dialog ID.</returns>
        Public Function WriteDB(ByVal plngGameID As Long, ByRef ptrnWrite As SqlTransaction) As Long
            Dim strSQL As String
            Dim cmdWrite As SqlCommand

            strSQL = "INSERT INTO CalculatedData.Dialog (ServerDialogTime, Action, Message, fkSenderClientID, IsTeamSpeech, IsCommand, fkGameID, LineNumber "
            If mobjTarget IsNot Nothing Then strSQL &= ", fkTargetClientID "
            strSQL &= ") VALUES (@Time, @Action, @Message, @Sender, @IsTeamSpeech, @IsCommand, @Game, @Line "
            If mobjTarget IsNot Nothing Then strSQL &= ", @Target "
            strSQL &= ") "

            cmdWrite = New SqlCommand(strSQL, mcxnStatsDB)
            cmdWrite.Parameters.AddWithValue("Time", mobjTimestamp.Seconds)
            cmdWrite.Parameters.AddWithValue("Action", mstrAction)
            cmdWrite.Parameters.AddWithValue("Message", mstrDialog)
            cmdWrite.Parameters.AddWithValue("Sender", mobjSpeaker.ClientID)
            cmdWrite.Parameters.AddWithValue("IsTeamSpeech", mblnTeamSpeech)
            cmdWrite.Parameters.AddWithValue("IsCommand", mblnCommand)
            cmdWrite.Parameters.AddWithValue("Game", plngGameID)
            cmdWrite.Parameters.AddWithValue("Line", mlngLineNo)
            If mobjTarget IsNot Nothing Then cmdWrite.Parameters.AddWithValue("Target", mobjTarget.ClientID)
            cmdWrite.Transaction = ptrnWrite

            cmdWrite.ExecuteNonQuery()

            strSQL = "SELECT @@IDENTITY "

            cmdWrite = New SqlCommand(strSQL, mcxnStatsDB)
            cmdWrite.Transaction = ptrnWrite

            mlngDialogID = CLng(cmdWrite.ExecuteScalar())

            Return mlngDialogID
        End Function
#End Region
    End Class
End Namespace
