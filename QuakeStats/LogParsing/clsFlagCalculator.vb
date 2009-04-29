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

#Region "Status Transition"
    Public Structure stuStatusTransition
#Region "Member Vars"
        Public Significance As enuSignificanceType

        Public EventID As Long

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
#Region "Inner Types"
#Region "Calculation Steps"
        Public Enum enuCalculationStepType
            ResetFlagCalculationsInDB
            FetchingGameEvents
            FetchedGameEvents
            BuildingGameGraph
            FindingPathsThroughGraph
            FilteringPathsToScore
        End Enum
#End Region
#End Region

#Region "Constants"
        Private Const MINT_FREQUENCY_OF_GAME_EVENT_PROCESSING_STATUS_NOTIFICATIONS As Integer = 10
        Private Const MINT_FREQUENCY_OF_PATH_FILTERING_STATUS_NOTIFICATIONS As Integer = 10

        Private Const MINT_SECONDS_UNTIL_FLAG_RESET As Integer = 30
#End Region

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
        Public Event GameCalculationStatusChanged(ByVal penuCurrentStep As enuCalculationStepType, ByVal plngIdx As Long, ByVal plngLimit As Long)
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
        ''' <param name="pblnResetFlagCalculationsFirst">Whether or not to run Utilities.spResetFlagCalculations first.</param>
        Public Sub CalculateAllGames(Optional ByVal pintStopAfter As Integer = -1, Optional ByVal plngOnlyUnCalced As Boolean = True, Optional ByVal pblnResetFlagCalculationsFirst As Boolean = False)
            Dim lngMinGameID = GetMinGameID()
            Dim lngMaxGameID = GetMaxGameID()
            Dim intGamesCalced As Integer = 0

            If pblnResetFlagCalculationsFirst Then
                RaiseEvent GameCalculationStatusChanged(enuCalculationStepType.ResetFlagCalculationsInDB, Nothing, Nothing)
                ResetFlagCalculationsInDB()
            End If

            Print("Begin calculating flag captures for " & lngMaxGameID - lngMinGameID & " maximum games, from: " & lngMinGameID & " to: " & lngMaxGameID & " ...")
            For lngCurrentGameID As Long = lngMinGameID To lngMaxGameID
                If (pintStopAfter <> -1 AndAlso intGamesCalced < pintStopAfter) Or _
                        (pintStopAfter = -1) Then
                    If IsCompleteInLog(lngCurrentGameID) And IsCTF(lngCurrentGameID) Then
                        If plngOnlyUnCalced AndAlso Not IsFlagCalculationsComplete(lngCurrentGameID) Then
                            CalculateGame(lngCurrentGameID, False)
                            intGamesCalced += 1
                        End If
                    End If
                Else
                    Exit For
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

            If (pblnVerifyGameGoodToCalc AndAlso IsCTF(plngGameID) AndAlso IsCompleteInLog(plngGameID)) Or _
                (Not pblnVerifyGameGoodToCalc) Then
                Try
                    Print("Game: " & plngGameID & " on map: " & GetMapName(plngGameID) & " init: " & GetInitGameLineNo(plngGameID) & " for map: " & GetMapName(plngGameID) & " ")
                    mobjTimer.StartTimer()
                    If Not DoCalculateGame(plngGameID) Then
                        Throw New Exception("DoCalculateGame returned false!")
                    End If

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
        ''' Resets the flag calculations in DB by calling Utilities.spResetFlagCalculations
        ''' </summary>
        Private Sub ResetFlagCalculationsInDB()
            Dim sqlcmdReset As New SqlCommand("Utility.spResetFlagCalculations", mcxnStatsDB)

            sqlcmdReset.CommandType = CommandType.StoredProcedure
            sqlcmdReset.CommandTimeout = 0

            sqlcmdReset.ExecuteNonQuery()
        End Sub

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
        ''' <returns><c>true/false</c> on success/fail.</returns>
        Private Function DoCalculateGame(ByVal plngGameID As Long) As Boolean
            RaiseEvent GameCalculationStatusChanged(enuCalculationStepType.FetchingGameEvents, Nothing, Nothing)
            Dim dtGameEvents As DataTable = GetGameEvents(plngGameID)
            RaiseEvent GameCalculationStatusChanged(enuCalculationStepType.FetchedGameEvents, Nothing, dtGameEvents.Rows.Count)

            Dim lstPotentialStatesToExamine As List(Of Long)
            Dim lstCompletePaths As List(Of List(Of Long))
            Dim lstTallyingPaths As New List(Of List(Of Long))
            Dim intLimit As Integer = dtGameEvents.Rows.Count - 1

            'Walk the game events and do the flag calculations to build the game graph
            For intIdx As Integer = 0 To intLimit
                If intIdx Mod MINT_FREQUENCY_OF_GAME_EVENT_PROCESSING_STATUS_NOTIFICATIONS = 0 Then
                    RaiseEvent GameCalculationStatusChanged(enuCalculationStepType.BuildingGameGraph, intIdx, dtGameEvents.Rows.Count - 1)
                End If

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
            Next

            RaiseEvent GameCalculationStatusChanged(enuCalculationStepType.FindingPathsThroughGraph, Nothing, Nothing)

            'The game graph should be 100% complete at this point.  We'll need to find
            'a path from the source to the sink which tallies to the correct score
            lstCompletePaths = mobjGameGraph.GetAllNonLoopingSourceSinkPaths()

            'Filter the paths based on the score
            Dim lngIdx As Long = 0
            For Each lstPath As List(Of Long) In lstCompletePaths
                lngIdx += 1

                If PathTalliesToScoreFromLog(lstPath) Then
                    lstTallyingPaths.Add(lstPath)
                End If

                If lngIdx Mod MINT_FREQUENCY_OF_PATH_FILTERING_STATUS_NOTIFICATIONS = 0 Then
                    RaiseEvent GameCalculationStatusChanged(enuCalculationStepType.FilteringPathsToScore, lngIdx, lstCompletePaths.Count)
                End If
            Next

            'If there is at least one path remaining, game calculations succeed
            If lstTallyingPaths.Count < 0 Then
                'Finally, update stats in the DB using the first path (for now)
                UpdateDBFlagStatisticsForPath(lstTallyingPaths(0)) 'TODO: Consider a better way of doing this

                Return True
            Else
                Return False
            End If
        End Function

        ''' <summary>
        ''' Processes the current game event in the data table, modifies the game
        ''' graph to include the nessecary transitions.
        ''' </summary>
        ''' <param name="plngCurrentVertexID">The current vertex ID (in the working set to build new vertices off of).</param>
        ''' <param name="pdrGameEvent">The game event data row.</param>
        Private Sub AddNewGameEventToGraph(ByVal plngCurrentVertexID As Long, ByRef pdrGameEvent As DataRow)
            Dim stuCurrentStatus As stuStatusNode = mobjGameGraph.GetVertex(plngCurrentVertexID).Payload
            Dim lngNewStatusNodeID As Long
            Dim lngEventTime As Long = CLng(pdrGameEvent("EventTime"))
            Dim strEventType As String = CStr(pdrGameEvent("EventType"))

            'First, we'll check if either of the timers are expired, if so, 
            'we'll need to create a new transition 
            If stuCurrentStatus.RedFlagResetTime <> 0 AndAlso lngEventTime >= stuCurrentStatus.RedFlagResetTime Then
                'Add a transition to consider what would happen if the timer 
                'expired before this event occurs
                lngNewStatusNodeID = AddNewStatusNode(stuCurrentStatus.RedFlagInBase, True, _
                                                      stuCurrentStatus.RedFlagHolderClientID, 0, _
                                                      stuCurrentStatus.RedFlagResetTime, 0)
                mobjGameGraph.AddNewEdge(plngCurrentVertexID, lngNewStatusNodeID, BuildTransition(pdrGameEvent, enuSignificanceType.ResetDueToRedTimerExpiration))

                stuCurrentStatus = mobjGameGraph.GetVertex(lngNewStatusNodeID).Payload
                'Continue processing this event to consider what would happen if the event
                'fires before the timer expires
            End If
            If stuCurrentStatus.BlueFlagResetTime <> 0 AndAlso lngEventTime >= stuCurrentStatus.BlueFlagResetTime Then
                'Add a transition to consider what would happen if the timer 
                'expired before this event occurs
                lngNewStatusNodeID = AddNewStatusNode(True, stuCurrentStatus.BlueFlagInBase, _
                                                      0, stuCurrentStatus.BlueFlagHolderClientID, _
                                                      0, stuCurrentStatus.BlueFlagResetTime)
                mobjGameGraph.AddNewEdge(plngCurrentVertexID, lngNewStatusNodeID, BuildTransition(pdrGameEvent, enuSignificanceType.ResetDueToBlueTimerExpiration))

                'Continue processing this event to consider what would happen if the event
                'fires before the timer expires
            End If

            Select Case UCase$(strEventType)
                Case "ITEM"
                    ConsiderItemEvent(plngCurrentVertexID, pdrGameEvent)
                Case "KILL"
                    ConsiderKillEvent(plngCurrentVertexID, pdrGameEvent)
                Case "END"
                    ConsiderEndEvent(plngCurrentVertexID, pdrGameEvent)
                Case "NUMBERCHANGE"
                    ConsiderNumberChangeEvent(plngCurrentVertexID, pdrGameEvent)
                Case Else
                    Throw New Exception("UNKNOWN event type: " & strEventType)
            End Select
        End Sub

        ''' <summary>
        ''' Adjusts the game graph, using the current vertex ID as the start point,
        ''' and adds the new state and the appropriate transition to the new state
        ''' to the game graph for a client NUMBERCHANGE (log ID/team ID = client ID changed) event.
        ''' </summary>
        ''' <param name="plngCurrentVertexID">The current vertex ID.</param>
        ''' <param name="pdrGameEvent">The game event row.</param>
        Private Sub ConsiderNumberChangeEvent(ByVal plngCurrentVertexID As Long, ByRef pdrGameEvent As DataRow)
            Dim stuCurrentStatus As stuStatusNode = mobjGameGraph.GetVertex(plngCurrentVertexID).Payload
            Dim stuTransition As stuStatusTransition = BuildTransition(pdrGameEvent)
            Dim lngNewStatusNodeID As Long

            Select Case stuTransition.Client1Team
                Case enuTeamType.Red
                    If stuCurrentStatus.BlueFlagHolderClientID = stuTransition.Client1ID Then
                        lngNewStatusNodeID = AddNewStatusNode(stuCurrentStatus.RedFlagInBase, stuCurrentStatus.BlueFlagInBase, _
                                             stuCurrentStatus.RedFlagHolderClientID, stuTransition.Client2ID, _
                                             stuCurrentStatus.RedFlagResetTime, stuCurrentStatus.BlueFlagResetTime)
                        mobjGameGraph.AddNewEdge(plngCurrentVertexID, lngNewStatusNodeID, BuildTransition(pdrGameEvent, enuSignificanceType.CarrierClientNumberChange))
                    Else
                        'Event doesn't affect flag, so move to next node in current working set
                    End If
                Case enuTeamType.Blue
                    If stuCurrentStatus.RedFlagHolderClientID = stuTransition.Client1ID Then
                        lngNewStatusNodeID = AddNewStatusNode(stuCurrentStatus.RedFlagInBase, stuCurrentStatus.BlueFlagInBase, _
                                             stuTransition.Client2ID, stuCurrentStatus.BlueFlagHolderClientID, _
                                             stuCurrentStatus.RedFlagResetTime, stuCurrentStatus.BlueFlagResetTime)
                        mobjGameGraph.AddNewEdge(plngCurrentVertexID, lngNewStatusNodeID, BuildTransition(pdrGameEvent, enuSignificanceType.CarrierClientNumberChange))
                    Else
                        'Event doesn't affect flag, so move to next node in current working set
                    End If
                Case Else
                    'Spectator number changing
                    'Event doesn't affect flag, so move to next node in current working set 
            End Select
        End Sub

        ''' <summary>
        ''' Adjusts the game graph, using the current vertex ID as the start point,
        ''' and adds the new state and the appropriate transition to the new state
        ''' to the game graph for a client END event.
        ''' </summary>
        ''' <param name="plngCurrentVertexID">The current vertex ID.</param>
        ''' <param name="pdrGameEvent">The game event row.</param>
        Private Sub ConsiderEndEvent(ByVal plngCurrentVertexID As Long, ByRef pdrGameEvent As DataRow)
            Dim stuCurrentStatus As stuStatusNode = mobjGameGraph.GetVertex(plngCurrentVertexID).Payload
            Dim stuTransition As stuStatusTransition = BuildTransition(pdrGameEvent)
            Dim lngNewStatusNodeID As Long

            Select Case stuTransition.Client1Team
                Case enuTeamType.Red
                    If stuCurrentStatus.BlueFlagHolderClientID = stuTransition.Client1ID Then
                        'Red player holding blue flag ending: need to consider whether or not this
                        'causes the flag to be automatically reset and add both possibilities to
                        'the working set
                        lngNewStatusNodeID = AddNewStatusNode(False, stuCurrentStatus.BlueFlagInBase, _
                                             0, stuCurrentStatus.BlueFlagHolderClientID, _
                                             stuTransition.EventTime + MINT_SECONDS_UNTIL_FLAG_RESET, stuCurrentStatus.BlueFlagResetTime)
                        mobjGameGraph.AddNewEdge(plngCurrentVertexID, lngNewStatusNodeID, BuildTransition(pdrGameEvent, enuSignificanceType.CarrierClientEnd))

                        lngNewStatusNodeID = AddNewStatusNode(True, stuCurrentStatus.RedFlagInBase, _
                                             0, stuCurrentStatus.BlueFlagHolderClientID, _
                                             0, stuCurrentStatus.BlueFlagResetTime)
                        mobjGameGraph.AddNewEdge(plngCurrentVertexID, lngNewStatusNodeID, BuildTransition(pdrGameEvent, enuSignificanceType.CarrierClientEnd))
                    Else
                        'Event doesn't affect flag, so move to next node in current working set
                    End If
                Case enuTeamType.Blue
                    If stuCurrentStatus.RedFlagHolderClientID = stuTransition.Client1ID Then
                        'Blue player holding red flag ending: need to consider whether or not this
                        'causes the flag to be automatically reset and add both possibilities to
                        'the working set
                        lngNewStatusNodeID = AddNewStatusNode(stuCurrentStatus.RedFlagInBase, False, _
                                             stuCurrentStatus.RedFlagHolderClientID, 0, _
                                             stuCurrentStatus.RedFlagResetTime, stuTransition.EventTime + MINT_SECONDS_UNTIL_FLAG_RESET)
                        mobjGameGraph.AddNewEdge(plngCurrentVertexID, lngNewStatusNodeID, BuildTransition(pdrGameEvent, enuSignificanceType.CarrierClientEnd))

                        lngNewStatusNodeID = AddNewStatusNode(stuCurrentStatus.RedFlagInBase, True, _
                                             stuCurrentStatus.RedFlagHolderClientID, 0, _
                                             stuCurrentStatus.RedFlagResetTime, 0)
                        mobjGameGraph.AddNewEdge(plngCurrentVertexID, lngNewStatusNodeID, BuildTransition(pdrGameEvent, enuSignificanceType.CarrierClientEnd))
                    Else
                        'Event doesn't affect flag, so move to next node in current working set
                    End If
                Case Else
                    'Spectator leaving
                    'Event doesn't affect flag, so move to next node in current working set
            End Select
        End Sub

        ''' <summary>
        ''' Adjusts the game graph, using the current vertex ID as the start point,
        ''' and adds the new state and the appropriate transition to the new state
        ''' to the game graph for a KILL event.
        ''' </summary>
        ''' <param name="plngCurrentVertexID">The current vertex ID.</param>
        ''' <param name="pdrGameEvent">The game event row.</param>
        Private Sub ConsiderKillEvent(ByVal plngCurrentVertexID As Long, ByRef pdrGameEvent As DataRow)
            Dim stuCurrentStatus As stuStatusNode = mobjGameGraph.GetVertex(plngCurrentVertexID).Payload
            Dim stuTransition As stuStatusTransition = BuildTransition(pdrGameEvent)
            Dim lngNewStatusNodeID As Long

            'Branch on VICTIM team
            Select Case stuTransition.Client2Team
                Case enuTeamType.Red
                    'Red player dying
                    If stuCurrentStatus.BlueFlagHolderClientID = stuTransition.Client2ID Then
                        'Red player holding blue flag dying: need to consider whether or not this
                        'causes the flag to be automatically reset and add both possibilities to
                        'the working set
                        lngNewStatusNodeID = AddNewStatusNode(stuCurrentStatus.RedFlagInBase, False, _
                                             stuCurrentStatus.RedFlagHolderClientID, 0, _
                                             stuCurrentStatus.RedFlagResetTime, stuTransition.EventTime + MINT_SECONDS_UNTIL_FLAG_RESET)
                        mobjGameGraph.AddNewEdge(plngCurrentVertexID, lngNewStatusNodeID, BuildTransition(pdrGameEvent, enuSignificanceType.CarrierKillWithDrop))

                        lngNewStatusNodeID = AddNewStatusNode(stuCurrentStatus.RedFlagInBase, True, _
                                             stuCurrentStatus.RedFlagHolderClientID, 0, _
                                             stuCurrentStatus.RedFlagResetTime, 0)
                        mobjGameGraph.AddNewEdge(plngCurrentVertexID, lngNewStatusNodeID, BuildTransition(pdrGameEvent, enuSignificanceType.CarrierKillWithReset))
                    Else
                        'Event doesn't affect flag, so move to next node in current working set
                    End If
                Case enuTeamType.Blue
                    'Blue player dying
                    If stuCurrentStatus.RedFlagHolderClientID = stuTransition.Client2ID Then
                        'Blue player holding red flag dying: need to consider whether or not this
                        'causes the flag to be automatically reset and add both possibilities to
                        'the working set
                        lngNewStatusNodeID = AddNewStatusNode(False, stuCurrentStatus.BlueFlagInBase, _
                                             0, stuCurrentStatus.BlueFlagHolderClientID, _
                                             stuTransition.EventTime + MINT_SECONDS_UNTIL_FLAG_RESET, stuCurrentStatus.BlueFlagResetTime)
                        mobjGameGraph.AddNewEdge(plngCurrentVertexID, lngNewStatusNodeID, BuildTransition(pdrGameEvent, enuSignificanceType.CarrierKillWithDrop))

                        lngNewStatusNodeID = AddNewStatusNode(True, stuCurrentStatus.RedFlagInBase, _
                                             0, stuCurrentStatus.BlueFlagHolderClientID, _
                                             0, stuCurrentStatus.BlueFlagResetTime)
                        mobjGameGraph.AddNewEdge(plngCurrentVertexID, lngNewStatusNodeID, BuildTransition(pdrGameEvent, enuSignificanceType.CarrierKillWithReset))
                    Else
                        'Event doesn't affect flag, so move to next node in current working set
                    End If
                Case Else
                    Throw New Exception("Team type: " & stuTransition.Client2Team & " is not a valid victim type!")
            End Select
        End Sub

        ''' <summary>
        ''' Adjusts the game graph, using the current vertex ID as the start point,
        ''' and adds the new state and the appropriate transition to the new state
        ''' to the game graph for an ITEM event.
        ''' </summary>
        ''' <param name="plngCurrentVertexID">The current vertex ID.</param>
        ''' <param name="pdrGameEvent">The game event row.</param>
        Private Sub ConsiderItemEvent(ByVal plngCurrentVertexID As Long, ByRef pdrGameEvent As DataRow)
            Dim stuCurrentStatus As stuStatusNode = mobjGameGraph.GetVertex(plngCurrentVertexID).Payload
            Dim stuTransition As stuStatusTransition = BuildTransition(pdrGameEvent)
            Dim lngNewStatusNodeID As Long

            Select Case UCase$(stuTransition.ItemName)
                Case "TEAM_CTF_BLUEFLAG"
                    Select Case stuTransition.Client1Team
                        Case enuTeamType.Red
                            'Red touching blue, could be either steal or pickup
                            If stuCurrentStatus.BlueFlagInBase Then
                                'Red Steals Blue flag
                                lngNewStatusNodeID = AddNewStatusNode(stuCurrentStatus.RedFlagInBase, False, _
                                                     stuCurrentStatus.RedFlagHolderClientID, stuTransition.Client1ID, _
                                                     stuCurrentStatus.RedFlagResetTime, 0)
                                mobjGameGraph.AddNewEdge(plngCurrentVertexID, lngNewStatusNodeID, BuildTransition(pdrGameEvent, enuSignificanceType.Steal))
                            Else
                                'Red Pickup of Blue flag
                                lngNewStatusNodeID = AddNewStatusNode(stuCurrentStatus.RedFlagInBase, False, _
                                                     stuCurrentStatus.RedFlagHolderClientID, stuTransition.Client1ID, _
                                                     stuCurrentStatus.RedFlagResetTime, 0)
                                mobjGameGraph.AddNewEdge(plngCurrentVertexID, lngNewStatusNodeID, BuildTransition(pdrGameEvent, enuSignificanceType.Pickup))
                            End If
                        Case enuTeamType.Blue
                            'Blue touching blue, could be either capture or recovery
                            If stuCurrentStatus.BlueFlagInBase Then
                                If stuTransition.Client1ID = stuCurrentStatus.RedFlagHolderClientID Then
                                    'Blue Team Captures
                                    lngNewStatusNodeID = AddNewStatusNode(True, True, _
                                                     0, 0, _
                                                     0, 0)
                                    mobjGameGraph.AddNewEdge(plngCurrentVertexID, lngNewStatusNodeID, BuildTransition(pdrGameEvent, enuSignificanceType.Capture))
                                Else
                                    'Remove the current state from the game graph: this is impossible.
                                    'Can't have a recovery if the blue flag is in the base, can't have a 
                                    'capture if the red holder isn't the first client.
                                    mobjGameGraph.RemoveVertex(plngCurrentVertexID)
                                End If
                            Else
                                If stuCurrentStatus.BlueFlagHolderClientID = 0 Then
                                    'Blue Recovers Blue Flag
                                    lngNewStatusNodeID = AddNewStatusNode(stuCurrentStatus.RedFlagInBase, True, _
                                                     stuCurrentStatus.RedFlagHolderClientID, 0, _
                                                     stuCurrentStatus.RedFlagResetTime, 0)
                                    mobjGameGraph.AddNewEdge(plngCurrentVertexID, lngNewStatusNodeID, BuildTransition(pdrGameEvent, enuSignificanceType.Recovery))
                                Else
                                    'Can't recover a flag being held
                                    mobjGameGraph.RemoveVertex(plngCurrentVertexID)
                                End If
                            End If
                        Case Else
                            Throw New Exception("Other team type is invalid for flag touch: " & stuTransition.Client1Team)
                    End Select
                Case "TEAM_CTF_REDFLAG"
                    Select Case stuTransition.Client1Team
                        Case enuTeamType.Red
                            'Red touching red, could be either capture or recovery
                            If stuCurrentStatus.RedFlagInBase Then
                                If stuTransition.Client1ID = stuCurrentStatus.BlueFlagHolderClientID Then
                                    'Red Capture
                                    lngNewStatusNodeID = AddNewStatusNode(True, True, _
                                                    0, 0, _
                                                     0, 0)
                                    mobjGameGraph.AddNewEdge(plngCurrentVertexID, lngNewStatusNodeID, BuildTransition(pdrGameEvent, enuSignificanceType.Capture))
                                Else
                                    'Remove the current state from the game graph: this is impossible.
                                    'Can't have a recovery if the red flag is in the base, can't have a 
                                    'capture if the blue holder isn't the first client.
                                    mobjGameGraph.RemoveVertex(plngCurrentVertexID)
                                End If
                            Else
                                If stuCurrentStatus.RedFlagHolderClientID = 0 Then
                                    'Red Recovers Red Flag
                                    lngNewStatusNodeID = AddNewStatusNode(True, stuCurrentStatus.BlueFlagInBase, _
                                                     0, stuCurrentStatus.BlueFlagHolderClientID, _
                                                     0, stuCurrentStatus.BlueFlagResetTime)
                                    mobjGameGraph.AddNewEdge(plngCurrentVertexID, lngNewStatusNodeID, BuildTransition(pdrGameEvent, enuSignificanceType.Recovery))
                                Else
                                    'Can't recover a flag being held
                                    mobjGameGraph.RemoveVertex(plngCurrentVertexID)
                                End If
                            End If
                        Case enuTeamType.Blue
                            'Blue touching red, steal or pickup
                            If stuCurrentStatus.RedFlagInBase Then
                                'Blue Steals Red Flag
                                lngNewStatusNodeID = AddNewStatusNode(stuCurrentStatus.RedFlagInBase, False, _
                                                     stuCurrentStatus.RedFlagHolderClientID, stuTransition.Client1ID, _
                                                     stuCurrentStatus.RedFlagResetTime, 0)
                                mobjGameGraph.AddNewEdge(plngCurrentVertexID, lngNewStatusNodeID, BuildTransition(pdrGameEvent, enuSignificanceType.Steal))
                            Else
                                'Blue Pickup of Red Flag
                                lngNewStatusNodeID = AddNewStatusNode(stuCurrentStatus.RedFlagInBase, False, _
                                                     stuCurrentStatus.RedFlagHolderClientID, stuTransition.Client1ID, _
                                                     stuCurrentStatus.RedFlagResetTime, 0)
                                mobjGameGraph.AddNewEdge(plngCurrentVertexID, lngNewStatusNodeID, BuildTransition(pdrGameEvent, enuSignificanceType.Pickup))
                            End If
                        Case Else
                            Throw New Exception("Other team type is invalid for flag capture: " & stuTransition.Client1Team)
                    End Select
                Case Else
                    Throw New Exception("Unknown item type: " & stuTransition.EventType)
            End Select
        End Sub

        ''' <summary>
        ''' Adds the new, disconnected status node.
        ''' </summary>
        ''' <param name="pblnRedFlagInBase">if set to <c>true</c> [red flag in base].</param>
        ''' <param name="pblnBlueFlagInBase">if set to <c>true</c> [blue flag in base].</param>
        ''' <param name="plngRedFlagHolderClientID">The red flag holder client ID.</param>
        ''' <param name="plngBlueFlagHolderClientID">The blue flag holder client ID.</param>
        ''' <param name="plngRedFlagResetTime">The red flag reset time.</param>
        ''' <param name="plngBlueFlagResetTime">The blue flag reset time.</param>
        ''' <returns>ID of new vertex ID.</returns>
        Private Function AddNewStatusNode(ByVal pblnRedFlagInBase As Boolean, ByVal pblnBlueFlagInBase As Boolean, _
                                          ByVal plngRedFlagHolderClientID As Long, ByVal plngBlueFlagHolderClientID As Long, _
                                          ByVal plngRedFlagResetTime As Long, ByVal plngBlueFlagResetTime As Long) As Long
            Dim stuPayload As New stuStatusNode(pblnRedFlagInBase, pblnBlueFlagInBase, _
                                                plngRedFlagHolderClientID, plngBlueFlagHolderClientID, _
                                                plngRedFlagResetTime, plngBlueFlagResetTime)
            Return mobjGameGraph.AddNewVertex(stuPayload)
        End Function

        ''' <summary>
        ''' Builds the transition from the game event.
        ''' </summary>
        ''' <param name="pdrGameEvent">The game event data row.</param>
        ''' <param name="penuSignificance">The significance of the transition, if known.</param>
        ''' <returns>A new stuTransition struct, with everything populated from the row, and significance assigned from param.</returns>
        Private Function BuildTransition(ByRef pdrGameEvent As DataRow, Optional ByVal penuSignificance As enuSignificanceType = enuSignificanceType.None) As stuStatusTransition
            Dim stuTransition As New stuStatusTransition()

            Dim lngCurrentLineNo As Long
            Dim intCurrentPrimaryID As Integer

            'Vars populated from current event record
            Dim lngEventID As Long
            Dim lngGameID As Long
            Dim lngEventTime As Long
            Dim lngLineNo As Long
            Dim strEventType As String
            Dim lngClientID1 As Long
            Dim lngClientID2 As Long
            Dim enuClientTeam1 As enuTeamType
            Dim enuClientTeam2 As enuTeamType
            Dim strItemName As String
            Dim strWeaponName As String

            'Store current line info
            intCurrentPrimaryID = CInt(pdrGameEvent("PrimaryID"))
            lngCurrentLineNo = CLng(pdrGameEvent("LineNo"))

            'Read status variables from current data row
            lngEventID = CLng(pdrGameEvent("EventID"))
            lngGameID = CLng(pdrGameEvent("GameID"))
            lngEventTime = CLng(pdrGameEvent("EventTime"))
            lngLineNo = CLng(pdrGameEvent("LineNo"))
            strEventType = CStr(pdrGameEvent("EventType"))
            If Not IsDBNull(pdrGameEvent("ClientID1")) Then
                lngClientID1 = CLng(pdrGameEvent("ClientID1"))
            Else
                lngClientID1 = 0
            End If
            If Not IsDBNull(pdrGameEvent("ClientID2")) Then
                lngClientID2 = CLng(pdrGameEvent("ClientID2"))
            Else
                lngClientID2 = 0
            End If
            If Not IsDBNull(pdrGameEvent("ClientTeam1")) Then
                Select Case CStr(pdrGameEvent("ClientTeam1")).ToUpper
                    Case "RED"
                        enuClientTeam1 = enuTeamType.Red
                    Case "BLUE"
                        enuClientTeam1 = enuTeamType.Blue
                    Case Else
                        enuClientTeam1 = enuTeamType.None
                End Select
            Else
                enuClientTeam1 = enuTeamType.None
            End If
            If Not IsDBNull(pdrGameEvent("ClientTeam2")) Then
                Select Case CStr(pdrGameEvent("ClientTeam2")).ToUpper
                    Case "RED"
                        enuClientTeam2 = enuTeamType.Red
                    Case "BLUE"
                        enuClientTeam2 = enuTeamType.Blue
                    Case Else
                        enuClientTeam2 = enuTeamType.None
                End Select
            Else
                enuClientTeam2 = enuTeamType.None
            End If
            If Not IsDBNull(pdrGameEvent("ItemName")) Then
                strItemName = CStr(pdrGameEvent("ItemName"))
            Else
                strItemName = Nothing
            End If
            If Not IsDBNull(pdrGameEvent("WeaponName")) Then
                strWeaponName = CStr(pdrGameEvent("WeaponName"))
            Else
                strWeaponName = Nothing
            End If

            With stuTransition
                .Significance = penuSignificance

                .Client1ID = lngClientID1
                .Client1Team = enuClientTeam1
                .Client2ID = lngClientID2
                .Client2Team = enuClientTeam2
                .EventID = lngEventID
                .EventTime = lngEventTime
                .EventType = strEventType
                .ItemName = strItemName
                .LineNo = lngLineNo
                .WeaponName = strWeaponName
            End With

            Return stuTransition
        End Function

        ''' <summary>
        ''' Determines which of the pathes tallies to the score from log.
        ''' </summary>
        ''' <param name="plstPath">The path to test.</param>
        ''' <returns><c>true</c> if path tallies to the scores from the log, <c>false</c> otherwise.</returns>
        Private Function PathTalliesToScoreFromLog(ByVal plstPath As List(Of Long)) As Boolean
            Dim vCurr As clsDirectedGraph(Of stuStatusNode, stuStatusTransition).clsDirectedGraphVertex(Of stuStatusNode)
            Dim vNext As clsDirectedGraph(Of stuStatusNode, stuStatusTransition).clsDirectedGraphVertex(Of stuStatusNode)
            Dim eConnections As List(Of clsDirectedGraph(Of stuStatusNode, stuStatusTransition).clsDirectedGraphEdge(Of stuStatusTransition))
            Dim intLimit As Integer = plstPath.Count - 1

            Dim intRedScore As Integer = 0
            Dim intBlueScore As Integer = 0

            'Travel the path, starting from the start node to the
            'end of the path
            vCurr = mobjGameGraph.GetVertex(plstPath(0))
            If plstPath.Count > 1 Then
                For intIdx As Integer = 1 To intLimit
                    'Get the transitioning edges
                    vNext = mobjGameGraph.GetVertex(plstPath(intIdx))
                    'There should only be 1 transitioning edge
                    eConnections = mobjGameGraph.GetConnectingEdges(vCurr.VertexID, vNext.VertexID)
                    If eConnections.Count = 1 Then
                        'Check if the connection is a flag capture
                        If eConnections(0).Payload.Significance = enuSignificanceType.Capture Then
                            Select Case eConnections(0).Payload.Client1Team
                                Case enuTeamType.Red
                                    intRedScore += 1
                                Case enuTeamType.Blue
                                    intBlueScore += 1
                                Case Else
                                    Throw New Exception("Team type: " & eConnections(0).Payload.Client1Team & " should not be capturing flag, edge ID: " & eConnections(0).EdgeID)
                            End Select
                        End If
                    Else
                        Throw New Exception("There should be only 1 edge connecting all vertices in path, " & eConnections.Count & " edges exist between vertices with IDs: " & vCurr.VertexID & " and " & vNext.VertexID)
                    End If
                Next
            End If

            'Now check if we've matched the scores from the log
            Return (intRedScore = mintGoalRedScore AndAlso _
                    intBlueScore = mintGoalBlueScore)
        End Function

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

        ''' <summary>
        ''' Updates the flag statistics in DB: captures/steals/pickups/etc.
        ''' </summary>
        ''' <exception cref="Exception">If path isn't valid.</exception>
        ''' <param name="plstStatisticsPath">The statistics path, a list of longs, which are node IDs, ordered from root to sink, on a tallying path.</param>
        ''' <returns><c>true/false</c> on success/failure</returns>
        Private Function UpdateDBFlagStatisticsForPath(ByRef plstStatisticsPath As List(Of Long)) As Boolean
            Dim vCurr As clsDirectedGraph(Of stuStatusNode, stuStatusTransition).clsDirectedGraphVertex(Of stuStatusNode)
            Dim vNext As clsDirectedGraph(Of stuStatusNode, stuStatusTransition).clsDirectedGraphVertex(Of stuStatusNode)
            Dim stuStatusCurr As stuStatusTransition
            Dim trnUpdate As SqlTransaction = Nothing
            Dim intLimit As Integer = plstStatisticsPath.Count - 2

            'Check if statistics path has more than 1 node, else there is no stats to update
            If plstStatisticsPath.Count < 2 Then Return True

            Try
                trnUpdate = mcxnStatsDB.BeginTransaction()

                vCurr = mobjGameGraph.GetVertex(plstStatisticsPath(0))
                vNext = mobjGameGraph.GetVertex(plstStatisticsPath(1))

                'Walk the stats path, from root to sink
                For intIdx As Integer = 0 To intLimit
                    stuStatusCurr = mobjGameGraph.GetConnectingEdges(vCurr.VertexID, vNext.VertexID)(0).Payload

                    'Call the function which will update the event in the DB, if nessecary
                    UpdateDBFlagStatisticsForNode(stuStatusCurr, trnUpdate)

                    vCurr = mobjGameGraph.GetVertex(plstStatisticsPath(intIdx))
                    vNext = mobjGameGraph.GetVertex(plstStatisticsPath(intIdx + 1))
                Next

                trnUpdate.Commit()
                Return True
            Catch ex As Exception
                If trnUpdate IsNot Nothing Then
                    trnUpdate.Rollback()
                End If

                Return False
            End Try
        End Function

        ''' <summary>
        ''' Updates the DB flag statistics for a single transition in the stats path,
        ''' if the transition into the node merits a db update.
        ''' </summary>
        ''' <param name="pstuTransition">The transition to check to update.</param>
        ''' <param name="ptrnUpdate">The path flag stats update transaction.</param>
        Private Sub UpdateDBFlagStatisticsForNode(ByRef pstuTransition As stuStatusTransition, ByRef ptrnUpdate As SqlTransaction)
            Dim strSQL As String
            Dim sqlcmdUpdate As SqlCommand

            Select Case pstuTransition.Significance
                Case enuSignificanceType.Capture
                    strSQL = "UPDATE CalculatedData.ClientToItem " & _
                            "SET IsFlagCapture = 1 " & _
                            "WHERE ClientToItemID = @ID "
                Case enuSignificanceType.CarrierKillWithDrop, enuSignificanceType.CarrierKillWithReset
                    strSQL = "UPDATE CalculatedData.[Kill] " & _
                            "SET IsCarrierKill = 1 " & _
                            "WHERE KillID = @ID "
                Case enuSignificanceType.Pickup
                    strSQL = "UPDATE CalculatedData.ClientToItem " & _
                            "SET IsFlagPickup = 1 " & _
                            "WHERE ClientToItemID = @ID "
                Case enuSignificanceType.Recovery
                    strSQL = "UPDATE CalculatedData.ClientToItem " & _
                            "SET IsFlagRecovery = 1 " & _
                            "WHERE ClientToItemID = @ID "
                Case enuSignificanceType.Steal
                    strSQL = "UPDATE CalculatedData.ClientToItem " & _
                            "SET IsFlagSteal = 1 " & _
                            "WHERE ClientToItemID = @ID "
                Case Else
                    strSQL = Nothing
            End Select

            If strSQL IsNot Nothing Then
                sqlcmdUpdate = New SqlCommand(strSQL, mcxnStatsDB)
                sqlcmdUpdate.Parameters.AddWithValue("ID", pstuTransition.EventID)
                sqlcmdUpdate.Transaction = ptrnUpdate

                sqlcmdUpdate.ExecuteNonQuery()
            End If
        End Sub
#End Region
    End Class
End Namespace