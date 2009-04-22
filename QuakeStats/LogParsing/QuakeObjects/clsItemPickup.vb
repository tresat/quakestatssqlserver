Option Strict On
Option Explicit On

Imports System.Data.SqlClient

Namespace LogParsing.QuakeObjects
    Public Class clsItemPickup
#Region "Member Vars"
        Private mcxnStatsDB As SqlConnection

        Private mlngItemPickupID As Long

        Private mobjPickupTime As clsTimestamp
        Private mlngItemID As Long
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
        Public Sub New(ByRef pcxnStatsDB As SqlConnection, ByRef pobjPickupTime As clsTimestamp, ByVal plngLineNo As Long, ByVal plngItemID As Long)
            StatsDB = pcxnStatsDB

            mobjPickupTime = pobjPickupTime
            mlngItemID = plngItemID
            mlngLineNo = plngLineNo
        End Sub
#End Region

#Region "Public Functionality"
        ''' <summary>
        ''' Writes the ClientToItem table of the db with the item pickup.
        ''' </summary>
        ''' <param name="plngClientID">The client ID.</param>
        ''' <param name="plngGameID">The game ID.</param>
        ''' <param name="ptrnWrite">The transaction in process to do the write on.</param>
        ''' <returns>ClientToItemID of inserted row.</returns>
        Public Function WriteDB(ByVal plngClientID As Long, ByVal plngGameID As Long, ByRef ptrnWrite As SqlTransaction) As Long
            Dim strSQL As String
            Dim cmdWrite As SqlCommand
            Dim lngClientToItemID As Long

            strSQL = "INSERT INTO CalculatedData.ClientToItem (fkItemID, fkClientID, ServerItemTime, fkGameID, fkItemLineNumber) " & _
                    "VALUES (@Item, @Client, @Time, @Game, @LineNo) "

            cmdWrite = New SqlCommand(strSQL, mcxnStatsDB)
            cmdWrite.Parameters.AddWithValue("Item", mlngItemID)
            cmdWrite.Parameters.AddWithValue("Client", plngClientID)
            cmdWrite.Parameters.AddWithValue("Time", mobjPickupTime.Seconds)
            cmdWrite.Parameters.AddWithValue("Game", plngGameID)
            cmdWrite.Parameters.AddWithValue("LineNo", mlngLineNo)
            cmdWrite.Transaction = ptrnWrite

            cmdWrite.ExecuteNonQuery()

            strSQL = "SELECT @@IDENTITY "

            cmdWrite = New SqlCommand(strSQL, mcxnStatsDB)
            cmdWrite.Transaction = ptrnWrite

            lngClientToItemID = CLng(cmdWrite.ExecuteScalar)

            Return lngClientToItemID
        End Function
#End Region
    End Class
End Namespace
