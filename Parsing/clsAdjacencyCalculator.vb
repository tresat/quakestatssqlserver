Option Explicit On
Option Strict On

Imports System.Data.SqlClient
Imports QuakeStats.Utilities.clsHighPerformanceTimer

'TODO: Add server uppage adjacencies calculator, should be run before game adjacency determination.

Namespace LogParsing
    Public Class clsAdjacencyCalculator
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
        Public Sub CalculateAllAdjacencies()
            CalculateGameAdjacencies()

            CalculateInGameClientAdjacencies()
        End Sub
#End Region

#Region "Private Helpers"
        ''' <summary>
        ''' Calculates the in-game client adjacencies.  No dependancies.
        ''' </summary>
        Private Sub CalculateInGameClientAdjacencies()
            Dim sqlcmdCalc As SqlCommand

            mobjTimer.StartTimer()
            Print("Beginning in-game client adjacency calculations...")

            sqlcmdCalc = New SqlCommand("Calculations.spLinkClientsWithinGames", mcxnStatsDB)
            sqlcmdCalc.CommandType = CommandType.StoredProcedure
            sqlcmdCalc.CommandTimeout = 0

            sqlcmdCalc.ExecuteNonQuery()

            mobjTimer.StopTimer()
            Print("Finished in-game client adjacency calculations in " & mobjTimer.GetResultAsTimeString & " (actual " & mobjTimer.GetElapsedAsTimeString & ").")
        End Sub

        ''' <summary>
        ''' Calculates the game adjacencies.  Depends on the server adjacencies being
        ''' already established.
        ''' </summary>
        Private Sub CalculateGameAdjacencies()
            Dim sqlcmdCalc As SqlCommand

            mobjTimer.StartTimer()
            Print("Beginning game adjacency calculations...")

            sqlcmdCalc = New SqlCommand("Calculations.spLinkGames", mcxnStatsDB)
            sqlcmdCalc.CommandType = CommandType.StoredProcedure
            sqlcmdCalc.CommandTimeout = 0

            sqlcmdCalc.ExecuteNonQuery()

            mobjTimer.StopTimer()
            Print("Finished game adjacency calculations in " & mobjTimer.GetResultAsTimeString & " (actual " & mobjTimer.GetElapsedAsTimeString & ").")
        End Sub
#End Region
    End Class
End Namespace