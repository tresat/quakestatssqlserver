Option Explicit On
Option Strict On

Imports System.Data.SqlClient

Namespace LogParsing.QuakeObjects
    Public Class clsKill
#Region "Member Vars"
        Private mcxnStatsDB As SqlConnection

        Private mlngKillID As Long

        Private mobjTimestamp As clsTimestamp
        Private mobjKiller As clsClient
        Private mobjVictim As clsClient
        Private mlngWeaponID As Long
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
        Public Sub New(ByRef pcxnStatsDB As SqlConnection, ByRef pobjTimestamp As clsTimestamp, ByVal plngLineNo As Long, ByRef pobjKiller As clsClient, ByRef pobjVictim As clsClient, ByVal plngWeaponID As Long)
            StatsDB = pcxnStatsDB

            mobjTimestamp = pobjTimestamp
            mobjKiller = pobjKiller
            mobjVictim = pobjVictim
            mlngWeaponID = plngWeaponID
            mlngLineNo = plngLineNo
        End Sub
#End Region

        ''' <summary>
        ''' Writes the DB with a kill record
        ''' </summary>
        ''' <param name="plngGameID">The game ID.</param>
        ''' <param name="ptrnWrite">The open transaction to write.</param>
        ''' <returns>New Kill record ID.</returns>
        Public Function WriteDB(ByVal plngGameID As Long, ByRef ptrnWrite As SqlTransaction) As Long
            Dim strSQL As String
            Dim cmdWrite As SqlCommand

            strSQL = "INSERT INTO CalculatedData.[Kill] (fkKillLineNumber, ServerKillTime, fkGameID, fkVictimClientID, fkWeaponID "
            If mobjKiller IsNot Nothing Then strSQL &= ", fkKillerClientID "
            strSQL &= ") VALUES (@LineNo, @Time, @Game, @Victim, @Weapon "
            If mobjKiller IsNot Nothing Then strSQL &= ", @Killer "
            strSQL &= ") "

            cmdWrite = New SqlCommand(strSQL, mcxnStatsDB)
            cmdWrite.Parameters.AddWithValue("@LineNo", mlngLineNo)
            cmdWrite.Parameters.AddWithValue("@Time", mobjTimestamp.Seconds)
            cmdWrite.Parameters.AddWithValue("@Game", plngGameID)
            cmdWrite.Parameters.AddWithValue("@Victim", mobjVictim.ClientID)
            cmdWrite.Parameters.AddWithValue("@Weapon", mlngWeaponID)
            If mobjKiller IsNot Nothing Then cmdWrite.Parameters.AddWithValue("@Killer", mobjKiller.ClientID)
            cmdWrite.Transaction = ptrnWrite

            cmdWrite.ExecuteNonQuery()

            strSQL = "SELECT @@IDENTITY "

            cmdWrite = New SqlCommand(strSQL, mcxnStatsDB)
            cmdWrite.Transaction = ptrnWrite

            mlngKillID = CLng(cmdWrite.ExecuteScalar())

            Return mlngKillID
        End Function
    End Class
End Namespace
