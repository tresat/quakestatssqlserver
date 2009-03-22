Option Explicit On
Option Strict On

Imports System.Data.SqlClient
Imports QuakeStats.Utilities.clsHighPerformanceTimer

Namespace LogParsing
    Public Class clsMapCalculator
#Region "Member Vars"
        Private mcxnStatsDB As SqlConnection
        Private mobjTimer As Utilities.clsHighPerformanceTimer
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
        Public Sub New(ByRef pcxnDB As SqlConnection)
            StatsDB = pcxnDB

            mobjTimer = New Utilities.clsHighPerformanceTimer
        End Sub
#End Region

#Region "Public Functionality"
        Public Sub CalculateAllGames()
            Dim sqlcmdCalc As SqlCommand

            mobjTimer.StartTimer()
            Print("Beginning map calculations...")

            sqlcmdCalc = New SqlCommand("Calculations.spCreateMaps", mcxnStatsDB)
            sqlcmdCalc.CommandType = CommandType.StoredProcedure

            sqlcmdCalc.ExecuteNonQuery()

            mobjTimer.StopTimer()
            Print("Finished map calculations in " & mobjTimer.GetResultAsTimeString & ".")
        End Sub
#End Region
    End Class
End Namespace