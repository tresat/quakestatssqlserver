Option Explicit On
Option Strict On

Imports System.Data.SqlClient

Module modGlobalRoutines
    ''' <summary>
    ''' Verifies the connection to the StatsDB is connected.
    ''' </summary>
    ''' <param name="pcxnStatsDB">The connection to test.</param>
    Public Sub VerifyDBConnected(ByVal pcxnStatsDB As SqlConnection)
        If pcxnStatsDB.State <> ConnectionState.Open Then
            pcxnStatsDB.Open()
        End If
    End Sub
End Module
