Option Explicit On
Option Strict On

Imports System.Data.SqlClient
Imports QuakeStats.Utilities.clsHighPerformanceTimer

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
            CalculateServerUppageAdjacencies()
            CalculateGameAdjacencies()
            CalculateInGameClientAdjacencies()
        End Sub
#End Region

#Region "Private Helpers"
        ''' <summary>
        ''' Creates an alias for each client which needs one, links clients to appropriate aliases..
        ''' </summary>
        Private Sub CalculateAliases()
            Dim sqlcmdCalc As SqlCommand

            mobjTimer.StartTimer()
            Print("Beginning Alias calculations...")

            sqlcmdCalc = New SqlCommand("Calculations.spLinkClientsToAliases", mcxnStatsDB)
            sqlcmdCalc.CommandType = CommandType.StoredProcedure
            sqlcmdCalc.CommandTimeout = 0

            sqlcmdCalc.ExecuteNonQuery()

            mobjTimer.StopTimer()
            Print("Finished Alias calculations in " & mobjTimer.GetResultAsTimeString & " (actual " & mobjTimer.GetElapsedAsTimeString & ").")
        End Sub

        ''' <summary>
        ''' Calculates the server uppage adjuacencies.
        ''' </summary>
        Private Sub CalculateServerUppageAdjacencies()
            Dim sqlcmdCalc As SqlCommand

            mobjTimer.StartTimer()
            Print("Beginning server uppage adjacency calculations...")

            sqlcmdCalc = New SqlCommand("Calculations.spLinkServerUppages", mcxnStatsDB)
            sqlcmdCalc.CommandType = CommandType.StoredProcedure
            sqlcmdCalc.CommandTimeout = 0

            sqlcmdCalc.ExecuteNonQuery()

            mobjTimer.StopTimer()
            Print("Finished server uppage adjacency calculations in " & mobjTimer.GetResultAsTimeString & " (actual " & mobjTimer.GetElapsedAsTimeString & ").")
        End Sub

        ''' <summary>
        ''' Calculates the in-game client adjacencies.  Should be run
        ''' after server uppage adjacencies are calculated, so that cross-uppage
        ''' game relationships can be established.
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