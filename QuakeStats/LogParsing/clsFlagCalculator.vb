Option Explicit On
Option Strict On

Imports System.Data.SqlClient
Imports System.IO
Imports System.Configuration
Imports QuakeStats.Utilities.clsHighPerformanceTimer
Imports GraphLibrary.DirectedGraph

Namespace LogParsing.FlagCalculator
#Region "Team"
    Public Enum enuTeamType
        None
        Red
        Blue
    End Enum
#End Region

#Region "Significance"
    Public Enum enuSignificanceType
        None
        Steal
        Pickup
        Capture
        CarrierKillWithDrop
        CarrierKillWithReset
        Recovery
        ResetDueToRedTimerExpiration
        ResetDueToBlueTimerExpiration
        CarrierClientEnd
        CarrierClientNumberChange
        CarrierClientTeamChange
    End Enum
#End Region

#Region "Status Node"
    Public Structure stuStatusNode
#Region "Member Variables"
        Public RedFlagInBase As Boolean
        Public BlueFlagInBase As Boolean

        Public RedFlagHolderClientID As Long
        Public BlueFlagHolderClientID As Long

        Public RedFlagResetTime As Long
        Public BlueFlagResetTime As Long
#End Region

#Region "Constructors"
        Public Sub New(ByVal pblnRedFlagInBase As Boolean, ByVal pblnBlueFlagInBase As Boolean, _
                        ByVal plngRedFlagHolderClientID As Long, ByVal plngBlueFlagHolderClientID As Long, _
                        ByVal plngRedFlagResetTime As Long, ByVal plngBlueFlagResetTime As Long)
            RedFlagInBase = pblnRedFlagInBase
            BlueFlagInBase = pblnBlueFlagInBase
            RedFlagHolderClientID = plngRedFlagHolderClientID
            BlueFlagHolderClientID = plngBlueFlagHolderClientID
            RedFlagResetTime = plngRedFlagResetTime
            BlueFlagResetTime = plngBlueFlagResetTime
        End Sub

        ''' <summary>
        ''' Determines whether all members of the 2 status nodes are identically valued.
        ''' </summary>
        ''' <param name="pstuOtherStatusNode">The other status node.</param>
        ''' <returns><c>true/false</c> on equal/not equal.</returns>
        Public Shadows Function Equals(ByRef pstuOtherStatusNode As stuStatusNode) As Boolean
            Return (RedFlagInBase = pstuOtherStatusNode.RedFlagInBase AndAlso _
                BlueFlagInBase = pstuOtherStatusNode.BlueFlagInBase AndAlso _
                RedFlagHolderClientID = pstuOtherStatusNode.RedFlagHolderClientID AndAlso _
                BlueFlagHolderClientID = pstuOtherStatusNode.BlueFlagHolderClientID AndAlso _
                RedFlagResetTime = pstuOtherStatusNode.RedFlagResetTime AndAlso _
                BlueFlagResetTime = pstuOtherStatusNode.BlueFlagResetTime)
        End Function
#End Region
    End Structure
#End Region

#Region "Transition Event"
    Public Structure stuStatusTransition
#Region "Member Vars"
        Public Significance As enuSignificanceType

        Public EventTime As Long
        Public LineNo As Long
        Public EventType As String
        Public Client1ID As Long
        Public Client2ID As Long
        Public Client1Team As enuTeamType
        Public Client2Team As enuTeamType
        Public ItemName As String
        Public WeaponName As String
#End Region

#Region "Constructors"
        Public Sub New(ByVal penuSignificance As enuSignificanceType, _
                        ByVal plngEventTime As Long, ByVal plngLineNo As Long, _
                        ByVal pstrEventType As String, ByVal plngClientID1 As Long, ByVal plngClientID2 As Long, _
                        ByVal penuClientTeam1 As enuTeamType, ByVal penuClientTeam2 As enuTeamType, _
                        ByVal pstrItemName As String, ByVal pstrWeaponName As String)
            Significance = penuSignificance

            EventTime = plngEventTime
            LineNo = plngLineNo
            EventType = pstrEventType
            Client1ID = plngClientID1
            Client2ID = plngClientID2
            Client1Team = penuClientTeam1
            Client2Team = penuClientTeam2
            ItemName = pstrItemName
            WeaponName = pstrWeaponName
        End Sub
#End Region
    End Structure
#End Region

    Public Class clsFlagCalculator
#Region "Member Variables"
        Private mcxnStatsDB As SqlConnection
        Private mobjTimer As Utilities.clsHighPerformanceTimer

        Private mobjGameGraph As clsDirectedGraph(Of stuStatusNode, stuStatusTransition)
        Private mlngRootID As Long
        Private mlstWorkingSet As List(Of Long)

        Private mlngGameID As Long
        Private mintGoalRedScore As Integer
        Private mintGoalBlueScore As Integer
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

#Region "Events"
        Public Event GameParsed(ByVal pblnSuccess As Boolean)
        Public Event GameEventParsed(ByVal pintCurrentEvent As Integer, ByVal pintTotalEvents As Integer, ByVal intWorkingSetCount As Integer)
#End Region

#Region "Constructors"
        Public Sub New(ByRef pcxnDB As SqlConnection)
            StatsDB = pcxnDB
            mobjTimer = New Utilities.clsHighPerformanceTimer
        End Sub
#End Region

#Region "Public Functionality"
        ''' <summary>
        ''' Calculates flag stats for all the games in the DB.
        ''' </summary>
        ''' <param name="pintStopAfter">The number of games to stop calculating after (-1 = infinite).</param>
        ''' <param name="plngOnlyUncalced">Whether or not to recalculate already calculated games.</param>
        Public Sub CalculateAllGames(Optional ByVal pintStopAfter As Integer = -1, Optional ByVal plngOnlyUnCalced As Boolean = True)
            Dim lngMinGameID = GetMinGameID()
            Dim lngMaxGameID = GetMaxGameID()

            Print("Begin calculating flag captures for " & lngMaxGameID - lngMinGameID & " maximum games, from: " & lngMinGameID & " to: " & lngMaxGameID & " ...")
            For lngCurrentGameID As Long = lngMinGameID To lngMaxGameID Step -1
                If IsCompleteInLog(lngCurrentGameID) And IsCTF(lngCurrentGameID) Then
                    If plngOnlyUnCalced AndAlso Not IsFlagCalculationsComplete(lngCurrentGameID) Then
                        CalculateGame(lngCurrentGameID, False)
                    End If
                End If
            Next
            Print("Finished calculating flag captures.")
        End Sub

        ''' <summary>
        ''' Calculates flag stats for a single game.
        ''' </summary>
        ''' <param name="plngGameID">The game ID.</param>
        ''' <param name="pblnVerifyGameGoodToCalc">Whether or not to check if the game is CTF and complete in log (not needed when called from CAGs function)</param>
        Public Sub CalculateGame(ByVal plngGameID As Long, Optional ByVal pblnVerifyGameGoodToCalc As Boolean = True)
            InitCalculateGame(plngGameID)

            If pblnVerifyGameGoodToCalc AndAlso IsCTF(plngGameID) AndAlso IsCompleteInLog(plngGameID) Then
                Try
                    Print("Game: " & plngGameID & " on map: " & GetMapName(plngGameID) & " init: " & GetInitGameLineNo(plngGameID) & " for map: " & GetMapName(plngGameID) & " ")
                    mobjTimer.StartTimer()
                    DoCalculateGame(plngGameID)

                    Print("**************************************SUCCEEDED**************************************")
                    RaiseEvent GameParsed(True)

                    mobjTimer.StopTimer()
                    Print("in: " & mobjTimer.GetResultAsTimeString & " (actual " & mobjTimer.GetElapsedAsTimeString & ").")

                    MarkFlagCalculationsComplete(plngGameID, True)
                Catch ex As Exception
                    Print(ex.Message)

                    Print("**************************************FAILED**************************************")
                    Print("**************************************FAILED**************************************")
                    Print("**************************************FAILED**************************************")
                    RaiseEvent GameParsed(False)

                    mobjTimer.StopTimer()
                    Print("in: " & mobjTimer.GetResultAsTimeString & " (actual " & mobjTimer.GetElapsedAsTimeString & ").")

                    MarkFlagCalculationsComplete(plngGameID, False)
                End Try
            End If
        End Sub
#End Region

#Region "Private Helper Functions"
        ''' <summary>
        ''' Gets the score for a team for a game which was written to the game log.
        ''' </summary>
        ''' <param name="plngGameID">The game ID to get score for.</param>
        ''' <param name="penuTeamType">Team to get score for.</param>
        ''' <returns></returns>
        Private Function GetScore(ByVal plngGameID As Long, ByVal penuTeamType As enuTeamType) As Integer
            Dim strSQL As String
            Dim sqlcmdGet As SqlCommand

            If penuTeamType = enuTeamType.None Then Throw New ArgumentException("Team Type cannot be None!")

            strSQL = "SELECT IsNull(g. " & CStr(IIf(penuTeamType = enuTeamType.Blue, "BlueScore", "RedScore")) & ", 0) " & _
                    "FROM CalculatedData.Game g WHERE g.GameID = @GameID "

            sqlcmdGet = New SqlCommand(strSQL, mcxnStatsDB)
            sqlcmdGet.Parameters.AddWithValue("GameID", plngGameID)

            Return CInt(sqlcmdGet.ExecuteScalar())
        End Function

        ''' <summary>
        ''' Gets the init game line no.
        ''' </summary>
        ''' <param name="plngGameID">The game ID.</param>
        ''' <returns>The init game line number from the DB.</returns>
        Private Function GetInitGameLineNo(ByVal plngGameID As Long) As Long
            Dim strSQL As String
            Dim sqlcmdGet As SqlCommand

            strSQL = "SELECT g.fkInitGameLineNumber " & _
                    "FROM CalculatedData.Game g " & _
                    "WHERE g.GameID = @GameID "

            sqlcmdGet = New SqlCommand(strSQL, mcxnStatsDB)
            sqlcmdGet.Parameters.AddWithValue("GameID", plngGameID)

            Return CLng(sqlcmdGet.ExecuteScalar())
        End Function

        ''' <summary>
        ''' Gets the name of the map.
        ''' </summary>
        ''' <param name="plngGameID">The game ID.</param>
        ''' <returns>Result of calling SQL map name function.</returns>
        Private Function GetMapName(ByVal plngGameID As Long) As String
            Dim strSQL As String
            Dim sqlcmdGet As SqlCommand

            strSQL = "SELECT Calculations.fnGetMapName(m.MapID, 1) AS Result " & _
                    "FROM CalculatedData.Map m " & _
                    "   INNER JOIN CalculatedData.Game g ON g.fkMapID = m.MapID " & _
                    "WHERE g.GameID = @GameID "

            sqlcmdGet = New SqlCommand(strSQL, mcxnStatsDB)
            sqlcmdGet.Parameters.AddWithValue("GameID", plngGameID)

            Return CStr(sqlcmdGet.ExecuteScalar())
        End Function

        ''' <summary>
        ''' Determines whether the flag calculations have already been performed for the specified game ID.
        ''' </summary>
        ''' <param name="plngGameID">The game ID to test.</param>
        ''' <returns>
        ''' <c>true</c> if [is flag calculations complete] <c>true</c> otherwise, <c>false</c>.
        ''' </returns>
        Private Function IsFlagCalculationsComplete(ByVal plngGameID As Long) As Boolean
            Dim strSQL As String
            Dim sqlcmdGet As SqlCommand
            Dim blnComplete As Boolean

            strSQL = "SELECT IsNull(g.IsFlagCalculationsComplete, 0)  " & _
                    "FROM CalculatedData.Game g " & _
                    "WHERE g.GameID = @GameID "

            sqlcmdGet = New SqlCommand(strSQL, mcxnStatsDB)
            sqlcmdGet.Parameters.AddWithValue("GameID", plngGameID)

            blnComplete = CBool(sqlcmdGet.ExecuteScalar())

            Return blnComplete
        End Function

        ''' <summary>
        ''' Determines whether the game is complete in the log by checking the DB.
        ''' </summary>
        ''' <param name="plngGameID">The game ID to verify.</param>
        ''' <returns>
        ''' <c>true</c> if [is complete in log] <c>true</c>; otherwise, <c>false</c>.
        ''' </returns>
        Private Function IsCompleteInLog(ByVal plngGameID As Long) As Boolean
            Dim strSQL As String
            Dim sqlcmdGet As SqlCommand
            Dim blnComplete As Boolean

            strSQL = "SELECT IsNull(g.IsCompleteInLog, 0)  " & _
                    "FROM CalculatedData.Game g " & _
                    "WHERE g.GameID = @GameID "

            sqlcmdGet = New SqlCommand(strSQL, mcxnStatsDB)
            sqlcmdGet.Parameters.AddWithValue("GameID", plngGameID)

            blnComplete = CBool(sqlcmdGet.ExecuteScalar())

            Return blnComplete
        End Function

        ''' <summary>
        ''' Determines whether the game is Capture the Flag by checking the DB.
        ''' </summary>
        ''' <param name="plngGameID">The game ID to verify.</param>
        ''' <returns>
        ''' <c>true</c> if [is a capture the flag game in log] <c>true</c>; otherwise, <c>false</c>.
        ''' </returns>
        Private Function IsCTF(ByVal plngGameID As Long) As Boolean
            Dim strSQL As String
            Dim sqlcmdGet As SqlCommand
            Dim strGameType As String

            strSQL = "SELECT gt.GameTypeName  " & _
                    "FROM CalculatedData.Gametype gt " & _
                    "   INNER JOIN CalculatedData.Game g ON g.fkGameTypeID = gt.GameTypeID " & _
                    "WHERE g.GameID = @GameID "

            sqlcmdGet = New SqlCommand(strSQL, mcxnStatsDB)
            sqlcmdGet.Parameters.AddWithValue("GameID", plngGameID)

            strGameType = CStr(sqlcmdGet.ExecuteScalar())

            If strGameType IsNot Nothing AndAlso strGameType.Equals("Team Capture the Flag") Then
                Return True
            Else
                Return False
            End If
        End Function

        ''' <summary>
        ''' Marks the flag calculations complete in the DB.
        ''' </summary>
        ''' <param name="plngGameID">The game ID.</param>
        ''' <param name="pblnSuccess">if set to <c>true</c>, marks [success], otherwise, marks fail.</param>
        Private Sub MarkFlagCalculationsComplete(ByVal plngGameID As Long, ByVal pblnSuccess As Boolean)
            Dim strSQL As String
            Dim sqlcmdMark As SqlCommand

            strSQL = "UPDATE CalculatedData.Game " & _
                    "SET IsFlagCalculationsComplete = @Success, " & _
                    "   IsFlagCalculationsFailed = 0 " & _
                    "WHERE GameID = @GameID "

            sqlcmdMark = New SqlCommand(strSQL, mcxnStatsDB)
            sqlcmdMark.Parameters.AddWithValue("Success", CInt(IIf(pblnSuccess, 1, 0)))
            sqlcmdMark.Parameters.AddWithValue("GameID", plngGameID)

            sqlcmdMark.ExecuteNonQuery()
        End Sub

        ''' <summary>
        ''' Gets the minimum game ID stored in the DB
        ''' </summary>
        ''' <returns>The first game's ID.</returns>
        Private Function GetMinGameID() As Long
            Dim strSQL As String
            Dim sqlcmdGet As SqlCommand

            strSQL = "SELECT Min(g.GameID) FROM CalculatedData.Game g"

            sqlcmdGet = New SqlCommand(strSQL, mcxnStatsDB)

            Return CLng(sqlcmdGet.ExecuteScalar())
        End Function

        ''' <summary>
        ''' Gets the maximum game ID stored in the DB.
        ''' </summary>
        ''' <returns>The last game's ID.</returns>
        Private Function GetMaxGameID() As Long
            Dim strSQL As String
            Dim sqlcmdGet As SqlCommand

            strSQL = "SELECT Max(g.GameID) FROM CalculatedData.Game g"

            sqlcmdGet = New SqlCommand(strSQL, mcxnStatsDB)

            Return CLng(sqlcmdGet.ExecuteScalar())
        End Function
#End Region

#Region "Private Main Game Parsing Routines"
        ''' <summary>
        ''' Clears all module level vars to prepare for game calculations.
        ''' </summary>
        ''' <param name="plngGameID">The game ID to prep for.</param>
        Private Sub InitCalculateGame(ByVal plngGameID As Long)
            'Create the directed game graph, use constructor to specify
            'a single source node, then save that node ID as both the root
            'and the current working set.
            mobjGameGraph = New clsDirectedGraph(Of stuStatusNode, stuStatusTransition)(1)
            mlngRootID = mobjGameGraph.GetSources(0).VertexID
            mlstWorkingSet = New List(Of Long)
            mlstWorkingSet.Add(mlngRootID)

            mlngGameID = plngGameID
            mintGoalBlueScore = GetScore(mlngGameID, enuTeamType.Blue)
            mintGoalBlueScore = GetScore(mlngGameID, enuTeamType.Blue)
        End Sub

        ''' <summary>
        ''' Gets the ordered flag-related game events for a game.
        ''' </summary>
        ''' <param name="plngGameID">The game ID to fetch game events for.</param>
        ''' <returns>Data table filled with the information.</returns>
        Private Function GetGameEvents(ByVal plngGameID As Long) As DataTable
            Dim dtGameEvents As DataTable = New DataTable
            Dim sqlcmdGet As SqlCommand

            sqlcmdGet = New SqlCommand("Calculations.spGetFlagEvents", mcxnStatsDB)
            sqlcmdGet.CommandType = CommandType.StoredProcedure
            sqlcmdGet.Parameters.AddWithValue("GameID", plngGameID)
            sqlcmdGet.CommandTimeout = 0

            Using reader As SqlDataReader = sqlcmdGet.ExecuteReader
                dtGameEvents.Load(reader)
            End Using

            Return dtGameEvents
        End Function

        ''' <summary>
        ''' Builds the game graph from the log file events.
        ''' Calculates statistics path from the game graph.
        ''' Updates the DB with the statistics.
        ''' ...or fails to.
        ''' </summary>
        ''' <param name="plngGameID">The game ID for calculate flag stats for.</param>
        Private Sub DoCalculateGame(ByVal plngGameID As Long)
            Dim dtGameEvents As DataTable = GetGameEvents(plngGameID)
            Dim lstPotentialStatesToExamine As List(Of Long)
            Dim lstFinalPaths As List(Of List(Of Long))

            'Walk the game events and do the flag calculations to build the game graph
            For intIdx As Integer = 0 To dtGameEvents.Rows.Count - 1
                'Save a copy of the collection of current sink nodes
                lstPotentialStatesToExamine = New List(Of Long)(mlstWorkingSet)

                'We'll want to loop all current sink nodes, and create a new status 
                'vertex representing the current game event for each of them
                For Each lngVertexID As Long In lstPotentialStatesToExamine
                    AddNewGameEventToGraph(lngVertexID, dtGameEvents.Rows(intIdx))
                Next

                'Now, we'll want to consolidate identical sinks, to reduce the size of 
                'the working set, this will create a single vertex for each set of 
                'vertices with identical payloads, and alter all edges pointing to ANY
                'vertex in the set to point to it.
                ConsolidateWorkingSet()

                RaiseEvent GameEventParsed(intIdx, dtGameEvents.Rows.Count, mlstWorkingSet.Count)
            Next

            'The game graph should be 100% complete at this point.  We'll need to find
            'a path from the source to the sink which tallies to the correct score
            lstFinalPaths = mobjGameGraph.GetAllSourceSinkPaths()
        End Sub

        Private Sub AddNewGameEventToGraph(ByVal plngCurrentVertexID As Long, ByRef pdrGameEvent As DataRow)

        End Sub

        ''' <summary>
        ''' Consolidates the working set: makes each set of sink vertices
        ''' with an identical payload into a single vertex.  Swaps all edges
        ''' into the vertices in that set of sinks into the new sink vertex.
        ''' </summary>
        Private Sub ConsolidateWorkingSet()
            Dim lstVertexSets As New List(Of List(Of Long))
            Dim blnFoundSetForVertex As Boolean
            Dim lstNewVertexSet As List(Of Long)
            Dim intLimit As Integer
            Dim lngReplacementVertexID As Long
            Dim lstEdges As List(Of Long)

            For Each lngVertexID As Long In mlstWorkingSet
                Debug.Assert(mobjGameGraph.IsSink(lngVertexID))

                'Consider each of the current set of vertices: does this vertex
                'belong in any of them?  We can compare its payload to the 
                'first vertex's payload in each set (since if the set exists here,
                'there will always be at least one vertex it in).
                blnFoundSetForVertex = False
                For Each lstCompareVertexSet As List(Of Long) In lstVertexSets
                    If mobjGameGraph.IsIdenticalVertexPayload(lngVertexID, lstCompareVertexSet(0)) Then
                        lstCompareVertexSet.Add(lngVertexID)
                        blnFoundSetForVertex = True
                        Exit For
                    End If
                Next

                If Not blnFoundSetForVertex Then
                    'Need to create new vertex set, containing this vertex, since it's unique
                    lstNewVertexSet = New List(Of Long)
                    lstNewVertexSet.Add(lngVertexID)
                    lstVertexSets.Add(lstNewVertexSet)
                End If
            Next

            'Now want to replace all vertices in each vertex set with a single 
            'vertex (we'll use the vertex at index 0), swaping incoming edges to
            'point to that replacement vertex first
            For Each lstVertexSet As List(Of Long) In lstVertexSets
                lngReplacementVertexID = lstVertexSet(0)
                intLimit = lstVertexSet.Count - 1
                For intIdx As Integer = 1 To intLimit
                    lstEdges = mobjGameGraph.GetVertex(lstVertexSet(intIdx)).Edges
                    For Each lngEdgeID As Long In lstEdges
                        If mobjGameGraph.GetEdge(lngEdgeID).EndVertexID <> lngReplacementVertexID Then
                            'An edge exists pointing to a vertex to be discarded,
                            'it needs to repoint to the correct vertex
                            mobjGameGraph.SetEdgeEnd(lngEdgeID, lngReplacementVertexID)
                        Else
                            'An edge exists pointing from a vertex to be discarded
                            'outwards.  Since we're only discarding sinks, this is
                            'an error
                            Throw New Exception("Edge: " & lngEdgeID & " points out from vertex: " & lstVertexSet(intIdx) & " this should never happen, this vertex should be a sink!")
                        End If
                    Next
                Next
            Next

            'Now delete the other (no longer in use) former end vertices from the graph
            For Each lstVertexSet As List(Of Long) In lstVertexSets
                intLimit = lstVertexSet.Count - 1
                For intIdx As Integer = 1 To intLimit
                    mobjGameGraph.RemoveVertex(lstVertexSet(intIdx))
                Next
            Next
        End Sub

#End Region
    End Class
End Namespace