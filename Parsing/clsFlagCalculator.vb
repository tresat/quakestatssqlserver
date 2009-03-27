Option Explicit On
Option Strict On

'TODO: Need to link clients to next/prev next.  Then can add treatment for client end
'caused by clientuserinfochanged where a client is just a continuation.  or maybe, can
'just add new event and xfer flag on info changed, drop on disconnect.  explore more

Imports System.Data.SqlClient
Imports System.IO
Imports System.Configuration
Imports QuakeStats.Utilities.clsHighPerformanceTimer

Public Class clsFlagCalculator
#Region "Inner Types"
    Private Enum enuTeamType
        None
        Red
        Blue
    End Enum

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

#Region "Status Snapshot"
    Private Class clsStatusSnapshot
#Region "Member Variables"
        Private mlngNodeID As Long

        Private mobjParent As clsStatusTransition
        Private mlstChildren As List(Of clsStatusTransition)

        Private mblnRedFlagInBase As Boolean
        Private mblnBlueFlagInBase As Boolean
        Private mlngRedFlagHolderClientID As Long
        Private mlngBlueFlagHolderClientID As Long
        Private mlngRedFlagResetTime As Long
        Private mlngBlueFlagResetTime As Long

        Private Shared mlngCurrentNodeID As Long = 1
#End Region

#Region "Properties"
        Public ReadOnly Property NodeID() As Long
            Get
                Return mlngNodeID
            End Get
        End Property

        Public ReadOnly Property Parent() As clsStatusTransition
            Get
                Return mobjParent
            End Get
        End Property

        Public Property RedFlagInBase() As Boolean
            Get
                Return mblnRedFlagInBase
            End Get
            Set(ByVal value As Boolean)
                mblnRedFlagInBase = value
            End Set
        End Property

        Public Property BlueFlagInBase() As Boolean
            Get
                Return mblnBlueFlagInBase
            End Get
            Set(ByVal value As Boolean)
                mblnBlueFlagInBase = value
            End Set
        End Property

        Public Property RedFlagHolderClientID() As Long
            Get
                Return mlngRedFlagHolderClientID
            End Get
            Set(ByVal value As Long)
                mlngRedFlagHolderClientID = value
            End Set
        End Property

        Public Property BlueFlagHolderClientID() As Long
            Get
                Return mlngBlueFlagHolderClientID
            End Get
            Set(ByVal value As Long)
                mlngBlueFlagHolderClientID = value
            End Set
        End Property

        Public Property RedFlagResetTime() As Long
            Get
                Return mlngRedFlagResetTime
            End Get
            Set(ByVal value As Long)
                mlngRedFlagResetTime = value
            End Set
        End Property

        Public Property BlueFlagResetTime() As Long
            Get
                Return mlngBlueFlagResetTime
            End Get
            Set(ByVal value As Long)
                mlngBlueFlagResetTime = value
            End Set
        End Property
#End Region

#Region "Constructors"
        Public Sub New(ByVal pblnRedFlagInBase As Boolean, ByVal pblnBlueFlagInBase As Boolean, _
                        ByVal plngRedFlagHolderClientID As Long, ByVal plngBlueFlagHolderClientID As Long, _
                        ByVal plngRedFlagResetTime As Long, ByVal plngBlueFlagResetTime As Long)
            'Assign current node id, and increment the node id counter
            mlngNodeID = mlngCurrentNodeID
            mlngCurrentNodeID += 1

            mobjParent = Nothing
            mlstChildren = New List(Of clsStatusTransition)

            mblnRedFlagInBase = pblnRedFlagInBase
            mblnBlueFlagInBase = pblnBlueFlagInBase
            mlngRedFlagHolderClientID = plngRedFlagHolderClientID
            mlngBlueFlagHolderClientID = plngBlueFlagHolderClientID
            mlngRedFlagResetTime = plngRedFlagResetTime
            mlngBlueFlagResetTime = plngBlueFlagResetTime
        End Sub
#End Region

#Region "Public Functionality"
        Public Sub AddParent(ByRef pobjParent As clsStatusTransition)
            If mobjParent IsNot Nothing Then Throw New Exception("Parent already set!")

            mobjParent = pobjParent
        End Sub

        Public Sub AddChild(ByRef pobjChild As clsStatusTransition)
            mlstChildren.Add(pobjChild)
        End Sub

        Public Function Children() As List(Of clsStatusSnapshot)
            Dim lstChildStatusNodes As New List(Of clsStatusSnapshot)

            For Each objTransition In mlstChildren
                lstChildStatusNodes.Add(objTransition.Subsequent)
            Next

            Return lstChildStatusNodes
        End Function

        Public Sub RemoveChild(ByRef pobjChild As clsStatusSnapshot)
            mlstChildren.Remove(pobjChild.Parent)
        End Sub
#End Region
    End Class
#End Region

#Region "Status Transition"
    Private Class clsStatusTransition
#Region "Member Variables"
        Private mobjPrior As clsStatusSnapshot
        Private mobjSubsequent As clsStatusSnapshot

        Private mlngEventID As Long
        Private mlngGameID As Long
        Private mlngEventTime As Long
        Private mlngLineNo As Long
        Private mstrEventType As String
        Private mlngClient1ID As Long
        Private mlngClient2ID As Long
        Private menuClient1Team As enuTeamType
        Private menuClient2Team As enuTeamType
        Private mstrItemName As String
        Private mstrWeaponName As String

        Private menuSignificance As enuSignificanceType

        Private mblnRedTimerReset As Boolean
        Private mblnBlueTimerReset As Boolean
        Private mblnRedFlagReset As Boolean
        Private mblnBlueFlagReset As Boolean
        Private mblnRedTimerCausedReset As Boolean
        Private mblnBlueTimerCausedReset As Boolean
#End Region

#Region "Properties"
        Public ReadOnly Property Prior() As clsStatusSnapshot
            Get
                Return mobjPrior
            End Get
        End Property

        Public ReadOnly Property Subsequent() As clsStatusSnapshot
            Get
                Return mobjSubsequent
            End Get
        End Property

        Public Property EventID() As Long
            Get
                Return mlngEventID
            End Get
            Set(ByVal value As Long)
                mlngEventID = value
            End Set
        End Property

        Public Property GameID() As Long
            Get
                Return mlngGameID
            End Get
            Set(ByVal value As Long)
                mlngGameID = value
            End Set
        End Property

        Public Property EventTime() As Long
            Get
                Return mlngEventTime
            End Get
            Set(ByVal value As Long)
                mlngEventTime = value
            End Set
        End Property

        Public Property LineNo() As Long
            Get
                Return mlngLineNo
            End Get
            Set(ByVal value As Long)
                mlngLineNo = value
            End Set
        End Property

        Public Property EventType() As String
            Get
                Return mstrEventType
            End Get
            Set(ByVal value As String)
                mstrEventType = value
            End Set
        End Property

        Public Property Client1ID() As Long
            Get
                Return mlngClient1ID
            End Get
            Set(ByVal value As Long)
                mlngClient1ID = value
            End Set
        End Property

        Public Property Client2ID() As Long
            Get
                Return mlngClient2ID
            End Get
            Set(ByVal value As Long)
                mlngClient2ID = value
            End Set
        End Property

        Public Property Client1Team() As enuTeamType
            Get
                Return menuClient1Team
            End Get
            Set(ByVal value As enuTeamType)
                menuClient1Team = value
            End Set
        End Property

        Public Property Client2Team() As enuTeamType
            Get
                Return menuClient2Team
            End Get
            Set(ByVal value As enuTeamType)
                menuClient2Team = value
            End Set
        End Property

        Public Property ItemName() As String
            Get
                Return mstrItemName
            End Get
            Set(ByVal value As String)
                mstrItemName = value
            End Set
        End Property

        Public Property WeaponName() As String
            Get
                Return mstrWeaponName
            End Get
            Set(ByVal value As String)
                mstrWeaponName = value
            End Set
        End Property

        Public Property Significance() As enuSignificanceType
            Get
                Return menuSignificance
            End Get
            Set(ByVal value As enuSignificanceType)
                menuSignificance = value
            End Set
        End Property

        Public Property RedTimerReset() As Boolean
            Get
                Return mblnRedTimerReset
            End Get
            Set(ByVal value As Boolean)
                mblnRedFlagReset = value
            End Set
        End Property

        Public Property BlueTimerReset() As Boolean
            Get
                Return mblnBlueTimerReset
            End Get
            Set(ByVal value As Boolean)
                mblnBlueTimerReset = value
            End Set
        End Property

        Public Property RedFlagReset() As Boolean
            Get
                Return mblnRedFlagReset
            End Get
            Set(ByVal value As Boolean)
                mblnRedFlagReset = value
            End Set
        End Property

        Public Property BlueFlagReset() As Boolean
            Get
                Return mblnBlueFlagReset
            End Get
            Set(ByVal value As Boolean)
                mblnBlueFlagReset = value
            End Set
        End Property

        Public Property RedTimerCausedReset() As Boolean
            Get
                Return mblnRedTimerCausedReset
            End Get
            Set(ByVal value As Boolean)
                mblnRedTimerCausedReset = value
            End Set
        End Property

        Public Property BlueTimerCausedReset() As Boolean
            Get
                Return mblnBlueTimerCausedReset
            End Get
            Set(ByVal value As Boolean)
                mblnBlueTimerCausedReset = value
            End Set
        End Property
#End Region

#Region "Constructors"
        Public Sub New(ByRef pobjPrior As clsStatusSnapshot, ByRef pobjSubsequent As clsStatusSnapshot, _
                       ByVal penuSignificance As enuSignificanceType, _
                       ByVal plngEventID As Long, ByVal plngGameID As Long, _
                        ByVal plngEventTime As Long, ByVal plngLineNo As Long, _
                        ByVal pstrEventType As String, ByVal plngClientID1 As Long, ByVal plngClientID2 As Long, _
                        ByVal penuClientTeam1 As enuTeamType, ByVal penuClientTeam2 As enuTeamType, _
                        ByVal pstrItemName As String, ByVal pstrWeaponName As String)
            mobjPrior = pobjPrior
            mobjSubsequent = pobjSubsequent

            menuSignificance = penuSignificance

            mlngEventID = plngEventID
            mlngGameID = plngGameID
            mlngEventTime = plngEventTime
            mlngLineNo = plngLineNo
            mstrEventType = pstrEventType
            mlngClient1ID = plngClientID1
            mlngClient2ID = plngClientID2
            menuClient1Team = penuClientTeam1
            menuClient2Team = penuClientTeam2
            mstrItemName = pstrItemName
            mstrWeaponName = pstrWeaponName
        End Sub
#End Region
    End Class
#End Region
#End Region

#Region "Member Variables"
    Private mcxnStatsDB As SqlConnection

    Private mobjRoot As clsStatusSnapshot
    Private mlstWorkingSet As List(Of clsStatusSnapshot)
    Private mobjTimer As Utilities.clsHighPerformanceTimer

    Private mstrGameTreeOutputPath As String
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
    Public Event GameEventParsed(ByVal pintCurrentEvent As Integer, ByVal pintTotalEvents As Integer)
#End Region

#Region "Constructors"
    Public Sub New(ByRef pcxnDB As SqlConnection)
        StatsDB = pcxnDB

        mobjTimer = New Utilities.clsHighPerformanceTimer

        mstrGameTreeOutputPath = VerifyGameTreeOutputPath()
    End Sub
#End Region

#Region "Public Functionality"
    Public Sub CalculateGame(ByVal plngGameID As Long)
        If IsCTF(plngGameID) Then
            Try
                Print("Game: " & plngGameID & " init: " & GetInitGameLineNo(plngGameID) & " for map: " & GetMapName(plngGameID) & " ", False)
                mobjTimer.StartTimer()
                DoCalculateGame(plngGameID)
                Print("succeeded ", False)
                PrintTreeToFile(plngGameID)
                MarkFlagCalculationsComplete(plngGameID, True)
            Catch ex As Exception
                Print("**************************************FAILED**************************************")
                Print("**************************************FAILED**************************************")
                Print("**************************************FAILED**************************************")
                MarkFlagCalculationsComplete(plngGameID, False)
            End Try
            mobjTimer.StopTimer()
            Print("in: " & mobjTimer.GetResultAsTimeString & " (actual " & mobjTimer.GetElapsedAsTimeString & ").")
        Else
            Print("Game: " & plngGameID & " is NOT CTF, skipping.")
        End If
    End Sub

    Public Sub CalculateAllGames()
        Dim lngMinGameID As Long = GetMinGameID()
        Dim lngMaxGameID As Long = GetMaxGameID()

        Print("Begin calculating flag captures for " & lngMaxGameID - lngMinGameID & " maximum games, from: " & lngMinGameID & " to: " & lngMaxGameID & " ...")
        For lngCurrentGameID As Long = lngMinGameID To lngMaxGameID
            If Not IsFlagCalculationsComplete(lngCurrentGameID) Then
                If IsCompleteInLog(lngCurrentGameID) Then
                    CalculateGame(lngCurrentGameID)
                    Print(lngMaxGameID - lngCurrentGameID & " maximum games remaining...")
                End If
            End If
        Next
        Print("Finished calculating flag captures.")
    End Sub
#End Region

#Region "Private Main Functionality"
    Public Sub PrintFirstBranch()
        Dim strResult As String = String.Empty

        strResult &= "******************************STATUS******************************" & vbCrLf

        PrintFirstBranchRecDown(mobjRoot, 0)
    End Sub

    Public Sub PrintTreeToFile(Optional ByVal plngGameID As Long = 0)
        PrintGameStatusTree(GetGameTreeFileWriter(plngGameID))
    End Sub

    ''' <summary>
    ''' Prints the complete game status tree starting from the root.
    ''' </summary>
    ''' <param name="pobjWriter">A writer to an open stream, if writing to a file. </param>
    Public Sub PrintGameStatusTree(Optional ByVal pobjWriter As StreamWriter = Nothing)
        Dim strResult As String = String.Empty

        strResult &= "******************************STATUS******************************" & vbCrLf

        strResult &= "WORKING SET: "
        For Each objNode As clsStatusSnapshot In mlstWorkingSet
            strResult &= objNode.NodeID & " "
        Next
        strResult &= vbCrLf

        If pobjWriter IsNot Nothing Then
            pobjWriter.Write(strResult)
        Else
            Print(strResult)
        End If

        PrintGameStatusRecDown(mobjRoot, 0, pobjWriter)

        If pobjWriter IsNot Nothing Then pobjWriter.Close()
    End Sub

    ''' <summary>
    ''' Builds the game tree.
    ''' </summary>
    ''' <param name="plngGameID">The PLNG game ID.</param>
    Private Sub DoCalculateGame(ByVal plngGameID As Long)
        Dim dtGameEvents As DataTable = GetGameEvents(plngGameID)
        Dim drCurrentLine As DataRow
        Dim lngCurrentLineNo As Long

        'Current status snapshot variables
        Dim blnRedFlagInBase As Boolean
        Dim blnBlueFlagInBase As Boolean
        Dim lngRedFlagHolderClientID As Long
        Dim lngBlueFlagHolderClientID As Long
        Dim lngRedFlagResetTime As Long
        Dim lngBlueFlagResetTime As Long

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

        Dim lstPotentialStatesToExamine As List(Of clsStatusSnapshot)
        Dim objCurrentStatus As clsStatusSnapshot
        Dim objResultStatus As clsStatusSnapshot
        Dim intStateIdx As Integer

        Dim intPrintIdx As Integer = 0
        'Dim blnPrint As Boolean

        'Create the root node (both flags in base, no timers set, no parent)
        mobjRoot = New clsStatusSnapshot(True, True, 0, 0, 0, 0)

        'Create the working set, add the root to it
        mlstWorkingSet = New List(Of clsStatusSnapshot)
        mlstWorkingSet.Add(mobjRoot)

        Print("with: " & dtGameEvents.Rows.Count & " events... ", False)

        'Walk the game events and do the initial flag calculations
        For intIdx As Integer = 0 To dtGameEvents.Rows.Count - 1
            RaiseEvent GameEventParsed(intIdx, dtGameEvents.Rows.Count)

            'blnPrint = True
            'Print("On event: " & intIdx & " of " & dtGameEvents.Rows.Count - 1)

            'If lngCurrentLineNo = 37992 Then
            'Dim i As Int16
            'i = 48
            'End If

            'Store current line info
            drCurrentLine = dtGameEvents.Rows(intIdx)
            lngCurrentLineNo = CLng(drCurrentLine("LineNo"))

            'Read status variables from current data row
            lngEventID = CLng(drCurrentLine("EventID"))
            lngGameID = CLng(drCurrentLine("GameID"))
            lngEventTime = CLng(drCurrentLine("EventTime"))
            lngLineNo = CLng(drCurrentLine("LineNo"))
            strEventType = CStr(drCurrentLine("EventType"))
            If Not IsDBNull(drCurrentLine("ClientID1")) Then
                lngClientID1 = CLng(drCurrentLine("ClientID1"))
            Else
                lngClientID1 = 0
            End If
            If Not IsDBNull(drCurrentLine("ClientID2")) Then
                lngClientID2 = CLng(drCurrentLine("ClientID2"))
            Else
                lngClientID2 = 0
            End If
            If Not IsDBNull(drCurrentLine("ClientTeam1")) Then
                Select Case CStr(drCurrentLine("ClientTeam1")).ToUpper
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
            If Not IsDBNull(drCurrentLine("ClientTeam2")) Then
                Select Case CStr(drCurrentLine("ClientTeam2")).ToUpper
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
            If Not IsDBNull(drCurrentLine("ItemName")) Then
                strItemName = CStr(drCurrentLine("ItemName"))
            Else
                strItemName = Nothing
            End If
            If Not IsDBNull(drCurrentLine("WeaponName")) Then
                strWeaponName = CStr(drCurrentLine("WeaponName"))
            Else
                strWeaponName = Nothing
            End If

            'Save a copy of the collection of current leaf nodes
            lstPotentialStatesToExamine = CopyWorkingSet()
            'Print("working set size: " & lstPotentialStatesToExamine.Count)

            'Select Case UCase$(strEventType)
            'Case "ITEM"
            'Print("parsing line: " & lngCurrentLineNo & " event: " & strEventType & " client1ID: " & lngClientID1 & " team1: " & CStr(IIf(enuClientTeam1 = enuTeamType.Red, "RED", "BLUE")) & " item: " & strItemName)
            'Case "KILL"
            'Print("parsing line: " & lngCurrentLineNo & " event: " & strEventType & " client1ID: " & lngClientID1 & " team1: " & CStr(IIf(enuClientTeam1 = enuTeamType.Red, "RED", IIf(enuClientTeam1 = enuTeamType.Blue, "BLUE", "NONE"))) & " client2ID: " & lngClientID2 & " team2: " & CStr(IIf(enuClientTeam2 = enuTeamType.Red, "RED", "BLUE")))
            'Case "END"
            'Print("parsing line: " & lngCurrentLineNo & " event: " & strEventType & " client1ID: " & lngClientID1)
            'End Select

            'We'll want to do the calculations for EACH leaf node we had
            intStateIdx = 0

            'Loop over all current leaf nodes
            Do While intStateIdx < lstPotentialStatesToExamine.Count
                'If intStateIdx Mod 500 = 0 Then
                'Print("examining node: " & intStateIdx & " of " & lstPotentialStatesToExamine.Count)
                'End If

                'Grab current status
                objCurrentStatus = lstPotentialStatesToExamine(intStateIdx)

                'Read current status vars from the leaf status we're working with
                blnRedFlagInBase = objCurrentStatus.RedFlagInBase
                blnBlueFlagInBase = objCurrentStatus.BlueFlagInBase
                lngRedFlagHolderClientID = objCurrentStatus.RedFlagHolderClientID
                lngBlueFlagHolderClientID = objCurrentStatus.BlueFlagHolderClientID
                lngRedFlagResetTime = objCurrentStatus.RedFlagResetTime
                lngBlueFlagResetTime = objCurrentStatus.BlueFlagResetTime

                'First, we'll check if either of the timers are expired, if so, 
                'we'll need to create and attach a new transition node
                If lngRedFlagResetTime <> 0 AndAlso lngEventTime >= lngRedFlagResetTime Then
                    'Add a branch to consider what would happen if the timer 
                    'expired before this event (we'll get to it as we get to the end of the 
                    'current potential list)
                    objResultStatus = BranchOnTimerReset(objCurrentStatus, enuTeamType.Red, _
                                                                    blnRedFlagInBase, blnBlueFlagInBase, _
                                                                    lngRedFlagHolderClientID, lngBlueFlagHolderClientID, _
                                                                    lngRedFlagResetTime, lngBlueFlagResetTime, _
                                                                    lngEventID, lngGameID, _
                                                                    lngEventTime, lngLineNo, strEventType, _
                                                                    lngClientID1, lngClientID2, enuClientTeam1, enuClientTeam2, _
                                                                    strItemName, strWeaponName)
                    'Add result to states to examine
                    lstPotentialStatesToExamine.Add(objResultStatus)
                    'By continuing to examine the  current status after this if statement, 
                    'we() 'll consider what would happen if the timer actually expired 
                    'after this event
                End If
                If lngBlueFlagResetTime <> 0 AndAlso lngEventTime >= lngBlueFlagResetTime Then
                    'Add a branch to consider what would happen if the timer 
                    'expired before this event (we'll get to it as we get to the end of the 
                    'current potential list)
                    objResultStatus = BranchOnTimerReset(objCurrentStatus, enuTeamType.Blue, _
                                                                    blnRedFlagInBase, blnBlueFlagInBase, _
                                                                    lngRedFlagHolderClientID, lngBlueFlagHolderClientID, _
                                                                    lngRedFlagResetTime, lngBlueFlagResetTime, _
                                                                    lngEventID, lngGameID, _
                                                                    lngEventTime, lngLineNo, strEventType, _
                                                                    lngClientID1, lngClientID2, enuClientTeam1, enuClientTeam2, _
                                                                    strItemName, strWeaponName)
                    'Add result to states to examine
                    lstPotentialStatesToExamine.Add(objResultStatus)
                    'By continuing to examine the  current status after this if statement, 
                    'we() 'll consider what would happen if the timer actually expired 
                    'after this event
                End If

                Select Case UCase$(strEventType)
                    Case "ITEM"
                        Select Case UCase$(strItemName)
                            Case "TEAM_CTF_BLUEFLAG"
                                Select Case enuClientTeam1
                                    Case enuTeamType.Red
                                        'Red touching blue, could be either steal or pickup
                                        If blnBlueFlagInBase Then
                                            'Red Steals
                                            objResultStatus = ProceedOnSteal(objCurrentStatus, enuTeamType.Red, _
                                                                    blnRedFlagInBase, blnBlueFlagInBase, _
                                                                    lngRedFlagHolderClientID, lngBlueFlagHolderClientID, _
                                                                    lngRedFlagResetTime, lngBlueFlagResetTime, _
                                                                    lngEventID, lngGameID, _
                                                                    lngEventTime, lngLineNo, strEventType, _
                                                                    lngClientID1, lngClientID2, enuClientTeam1, enuClientTeam2, _
                                                                    strItemName, strWeaponName)
                                            mlstWorkingSet.Add(objResultStatus)
                                            mlstWorkingSet.Remove(objCurrentStatus)
                                            intStateIdx += 1
                                        Else
                                            'Red Pickup
                                            objResultStatus = ProceedOnPickup(objCurrentStatus, enuTeamType.Red, _
                                                                    blnRedFlagInBase, blnBlueFlagInBase, _
                                                                    lngRedFlagHolderClientID, lngBlueFlagHolderClientID, _
                                                                    lngRedFlagResetTime, lngBlueFlagResetTime, _
                                                                    lngEventID, lngGameID, _
                                                                    lngEventTime, lngLineNo, strEventType, _
                                                                    lngClientID1, lngClientID2, enuClientTeam1, enuClientTeam2, _
                                                                    strItemName, strWeaponName)
                                            mlstWorkingSet.Add(objResultStatus)
                                            mlstWorkingSet.Remove(objCurrentStatus)
                                            intStateIdx += 1
                                        End If
                                    Case enuTeamType.Blue
                                        'Blue touching blue, could be either capture or recovery
                                        If blnBlueFlagInBase Then
                                            If lngClientID1 = lngRedFlagHolderClientID Then
                                                'Blue Capture
                                                objResultStatus = ProceedOnCapture(objCurrentStatus, enuTeamType.Blue, _
                                                                        blnRedFlagInBase, blnBlueFlagInBase, _
                                                                        lngRedFlagHolderClientID, lngBlueFlagHolderClientID, _
                                                                        lngRedFlagResetTime, lngBlueFlagResetTime, _
                                                                        lngEventID, lngGameID, _
                                                                        lngEventTime, lngLineNo, strEventType, _
                                                                        lngClientID1, lngClientID2, enuClientTeam1, enuClientTeam2, _
                                                                        strItemName, strWeaponName)
                                                mlstWorkingSet.Add(objResultStatus)
                                                mlstWorkingSet.Remove(objCurrentStatus)
                                                intStateIdx += 1
                                            Else
                                                'Remove the current state from the working set: this is impossible.
                                                'Can't have a recovery if the blue flag is in the base, can't have a 
                                                'capture if the red holder isn't the first client.
                                                PruneImpossibleBranch(objCurrentStatus)
                                                intStateIdx += 1
                                            End If
                                        Else
                                            If lngBlueFlagHolderClientID = 0 Then
                                                'Blue Recovers
                                                objResultStatus = ProceedOnRecovery(objCurrentStatus, enuTeamType.Blue, _
                                                                        blnRedFlagInBase, blnBlueFlagInBase, _
                                                                        lngRedFlagHolderClientID, lngBlueFlagHolderClientID, _
                                                                        lngRedFlagResetTime, lngBlueFlagResetTime, _
                                                                        lngEventID, lngGameID, _
                                                                        lngEventTime, lngLineNo, strEventType, _
                                                                        lngClientID1, lngClientID2, enuClientTeam1, enuClientTeam2, _
                                                                        strItemName, strWeaponName)
                                                mlstWorkingSet.Add(objResultStatus)
                                                mlstWorkingSet.Remove(objCurrentStatus)
                                                intStateIdx += 1
                                            Else
                                                'Can't recover a flag being held
                                                PruneImpossibleBranch(objCurrentStatus)
                                                intStateIdx += 1
                                            End If
                                        End If
                                    Case Else
                                        Throw New Exception("Other team type is invalid for flag touch: " & enuClientTeam1)
                                End Select
                            Case "TEAM_CTF_REDFLAG"
                                Select Case enuClientTeam1
                                    Case enuTeamType.Red
                                        'Red touching red, could be either capture or recovery
                                        If blnRedFlagInBase Then
                                            If lngClientID1 = lngBlueFlagHolderClientID Then
                                                'Red Capture
                                                objResultStatus = ProceedOnCapture(objCurrentStatus, enuTeamType.Red, _
                                                                        blnRedFlagInBase, blnBlueFlagInBase, _
                                                                        lngRedFlagHolderClientID, lngBlueFlagHolderClientID, _
                                                                        lngRedFlagResetTime, lngBlueFlagResetTime, _
                                                                        lngEventID, lngGameID, _
                                                                        lngEventTime, lngLineNo, strEventType, _
                                                                        lngClientID1, lngClientID2, enuClientTeam1, enuClientTeam2, _
                                                                        strItemName, strWeaponName)
                                                mlstWorkingSet.Add(objResultStatus)
                                                mlstWorkingSet.Remove(objCurrentStatus)
                                                intStateIdx += 1
                                            Else
                                                'Remove the current state from the working set: this is impossible.
                                                'Can't have a recovery if the red flag is in the base, can't have a 
                                                'capture if the blue holder isn't the first client.
                                                PruneImpossibleBranch(objCurrentStatus)
                                                intStateIdx += 1
                                            End If
                                        Else
                                            If lngRedFlagHolderClientID = 0 Then
                                                'Red Recovers
                                                objResultStatus = ProceedOnRecovery(objCurrentStatus, enuTeamType.Red, _
                                                                        blnRedFlagInBase, blnBlueFlagInBase, _
                                                                        lngRedFlagHolderClientID, lngBlueFlagHolderClientID, _
                                                                        lngRedFlagResetTime, lngBlueFlagResetTime, _
                                                                        lngEventID, lngGameID, _
                                                                        lngEventTime, lngLineNo, strEventType, _
                                                                        lngClientID1, lngClientID2, enuClientTeam1, enuClientTeam2, _
                                                                        strItemName, strWeaponName)
                                                mlstWorkingSet.Add(objResultStatus)
                                                mlstWorkingSet.Remove(objCurrentStatus)
                                                intStateIdx += 1
                                            Else
                                                'Can't recover a flag being held
                                                PruneImpossibleBranch(objCurrentStatus)
                                                intStateIdx += 1
                                            End If
                                        End If
                                    Case enuTeamType.Blue
                                        'Blue touching red, steal or pickup
                                        If blnRedFlagInBase Then
                                            'Blue Steals
                                            objResultStatus = ProceedOnSteal(objCurrentStatus, enuTeamType.Blue, _
                                                                    blnRedFlagInBase, blnBlueFlagInBase, _
                                                                    lngRedFlagHolderClientID, lngBlueFlagHolderClientID, _
                                                                    lngRedFlagResetTime, lngBlueFlagResetTime, _
                                                                    lngEventID, lngGameID, _
                                                                    lngEventTime, lngLineNo, strEventType, _
                                                                    lngClientID1, lngClientID2, enuClientTeam1, enuClientTeam2, _
                                                                    strItemName, strWeaponName)
                                            mlstWorkingSet.Add(objResultStatus)
                                            mlstWorkingSet.Remove(objCurrentStatus)
                                            intStateIdx += 1
                                        Else
                                            'Blue Pickup
                                            objResultStatus = ProceedOnPickup(objCurrentStatus, enuTeamType.Blue, _
                                                                    blnRedFlagInBase, blnBlueFlagInBase, _
                                                                    lngRedFlagHolderClientID, lngBlueFlagHolderClientID, _
                                                                    lngRedFlagResetTime, lngBlueFlagResetTime, _
                                                                    lngEventID, lngGameID, _
                                                                    lngEventTime, lngLineNo, strEventType, _
                                                                    lngClientID1, lngClientID2, enuClientTeam1, enuClientTeam2, _
                                                                    strItemName, strWeaponName)
                                            mlstWorkingSet.Add(objResultStatus)
                                            mlstWorkingSet.Remove(objCurrentStatus)
                                            intStateIdx += 1
                                        End If
                                    Case Else
                                        Throw New Exception("Other team type is invalid for flag capture: " & enuClientTeam1)
                                End Select
                            Case Else
                                Throw New Exception("Unknown item type: " & strEventType)
                        End Select
                    Case "KILL"
                        Select Case enuClientTeam2
                            Case enuTeamType.Red
                                'Red player dying
                                If lngBlueFlagHolderClientID = lngClientID2 Then
                                    'Red player holding blue flag dying: need to consider whether or not this
                                    'causes the flag to be automatically reset and add both possibilities to
                                    'the working set
                                    objResultStatus = BranchOnEndOfFlagHoldingWithReset(objCurrentStatus, enuTeamType.Red, _
                                                                    blnRedFlagInBase, blnBlueFlagInBase, _
                                                                    lngRedFlagHolderClientID, lngBlueFlagHolderClientID, _
                                                                    lngRedFlagResetTime, lngBlueFlagResetTime, _
                                                                    lngEventID, lngGameID, _
                                                                    lngEventTime, lngLineNo, strEventType, _
                                                                    lngClientID1, lngClientID2, enuClientTeam1, enuClientTeam2, _
                                                                    strItemName, strWeaponName)
                                    mlstWorkingSet.Add(objResultStatus)

                                    objResultStatus = BranchOnEndOfFlagHoldingWithoutReset(objCurrentStatus, enuTeamType.Red, _
                                                                    blnRedFlagInBase, blnBlueFlagInBase, _
                                                                    lngRedFlagHolderClientID, lngBlueFlagHolderClientID, _
                                                                    lngRedFlagResetTime, lngBlueFlagResetTime, _
                                                                    lngEventID, lngGameID, _
                                                                    lngEventTime, lngLineNo, strEventType, _
                                                                    lngClientID1, lngClientID2, enuClientTeam1, enuClientTeam2, _
                                                                    strItemName, strWeaponName)
                                    mlstWorkingSet.Add(objResultStatus)

                                    mlstWorkingSet.Remove(objCurrentStatus)

                                    intStateIdx += 1
                                Else
                                    'Event doesn't affect flag, so move to next node in current working set
                                    intStateIdx += 1
                                    'blnPrint = False
                                End If
                            Case enuTeamType.Blue
                                'Blue player dying
                                If lngRedFlagHolderClientID = lngClientID2 Then
                                    'Blue player holding red flag dying: need to consider whether or not this
                                    'causes the flag to be automatically reset and add both possibilities to
                                    'the working set
                                    objResultStatus = BranchOnEndOfFlagHoldingWithReset(objCurrentStatus, enuTeamType.Blue, _
                                                                    blnRedFlagInBase, blnBlueFlagInBase, _
                                                                    lngRedFlagHolderClientID, lngBlueFlagHolderClientID, _
                                                                    lngRedFlagResetTime, lngBlueFlagResetTime, _
                                                                    lngEventID, lngGameID, _
                                                                    lngEventTime, lngLineNo, strEventType, _
                                                                    lngClientID1, lngClientID2, enuClientTeam1, enuClientTeam2, _
                                                                    strItemName, strWeaponName)
                                    mlstWorkingSet.Add(objResultStatus)

                                    objResultStatus = BranchOnEndOfFlagHoldingWithoutReset(objCurrentStatus, enuTeamType.Blue, _
                                                                    blnRedFlagInBase, blnBlueFlagInBase, _
                                                                    lngRedFlagHolderClientID, lngBlueFlagHolderClientID, _
                                                                    lngRedFlagResetTime, lngBlueFlagResetTime, _
                                                                    lngEventID, lngGameID, _
                                                                    lngEventTime, lngLineNo, strEventType, _
                                                                    lngClientID1, lngClientID2, enuClientTeam1, enuClientTeam2, _
                                                                    strItemName, strWeaponName)
                                    mlstWorkingSet.Add(objResultStatus)

                                    mlstWorkingSet.Remove(objCurrentStatus)

                                    intStateIdx += 1
                                Else
                                    'Event doesn't affect flag, so move to next node in current working set
                                    intStateIdx += 1
                                    'blnPrint = False
                                End If
                        End Select
                    Case "END"
                        Select Case enuClientTeam1
                            Case enuTeamType.Red
                                If lngBlueFlagHolderClientID = lngClientID1 Then
                                    'Red player holding blue flag ending: need to consider whether or not this
                                    'causes the flag to be automatically reset and add both possibilities to
                                    'the working set
                                    objResultStatus = BranchOnEndOfFlagHoldingWithReset(objCurrentStatus, enuTeamType.Red, _
                                                                    blnRedFlagInBase, blnBlueFlagInBase, _
                                                                    lngRedFlagHolderClientID, lngBlueFlagHolderClientID, _
                                                                    lngRedFlagResetTime, lngBlueFlagResetTime, _
                                                                    lngEventID, lngGameID, _
                                                                    lngEventTime, lngLineNo, strEventType, _
                                                                    lngClientID1, lngClientID2, enuClientTeam1, enuClientTeam2, _
                                                                    strItemName, strWeaponName)
                                    mlstWorkingSet.Add(objResultStatus)

                                    objResultStatus = BranchOnEndOfFlagHoldingWithoutReset(objCurrentStatus, enuTeamType.Red, _
                                                                    blnRedFlagInBase, blnBlueFlagInBase, _
                                                                    lngRedFlagHolderClientID, lngBlueFlagHolderClientID, _
                                                                    lngRedFlagResetTime, lngBlueFlagResetTime, _
                                                                    lngEventID, lngGameID, _
                                                                    lngEventTime, lngLineNo, strEventType, _
                                                                    lngClientID1, lngClientID2, enuClientTeam1, enuClientTeam2, _
                                                                    strItemName, strWeaponName)
                                    mlstWorkingSet.Add(objResultStatus)

                                    mlstWorkingSet.Remove(objCurrentStatus)

                                    intStateIdx += 1
                                Else
                                    'Event doesn't affect flag, so move to next node in current working set
                                    intStateIdx += 1
                                    'blnPrint = False
                                End If
                            Case enuTeamType.Blue
                                If lngRedFlagHolderClientID = lngClientID1 Then
                                    'Blue player holding red flag ending: need to consider whether or not this
                                    'causes the flag to be automatically reset and add both possibilities to
                                    'the working set
                                    objResultStatus = BranchOnEndOfFlagHoldingWithReset(objCurrentStatus, enuTeamType.Blue, _
                                                                    blnRedFlagInBase, blnBlueFlagInBase, _
                                                                    lngRedFlagHolderClientID, lngBlueFlagHolderClientID, _
                                                                    lngRedFlagResetTime, lngBlueFlagResetTime, _
                                                                    lngEventID, lngGameID, _
                                                                    lngEventTime, lngLineNo, strEventType, _
                                                                    lngClientID1, lngClientID2, enuClientTeam1, enuClientTeam2, _
                                                                    strItemName, strWeaponName)
                                    mlstWorkingSet.Add(objResultStatus)

                                    objResultStatus = BranchOnEndOfFlagHoldingWithoutReset(objCurrentStatus, enuTeamType.Blue, _
                                                                    blnRedFlagInBase, blnBlueFlagInBase, _
                                                                    lngRedFlagHolderClientID, lngBlueFlagHolderClientID, _
                                                                    lngRedFlagResetTime, lngBlueFlagResetTime, _
                                                                    lngEventID, lngGameID, _
                                                                    lngEventTime, lngLineNo, strEventType, _
                                                                    lngClientID1, lngClientID2, enuClientTeam1, enuClientTeam2, _
                                                                    strItemName, strWeaponName)
                                    mlstWorkingSet.Add(objResultStatus)

                                    mlstWorkingSet.Remove(objCurrentStatus)

                                    intStateIdx += 1
                                Else
                                    'Event doesn't affect flag, so move to next node in current working set
                                    intStateIdx += 1
                                    'blnPrint = False
                                End If
                            Case Else
                                'Spectator leaving
                                'Event doesn't affect flag, so move to next node in current working set
                                intStateIdx += 1
                                'blnPrint = False
                        End Select
                    Case "NUMBERCHANGE"
                        Select Case enuClientTeam1
                            Case enuTeamType.Red
                                If lngBlueFlagHolderClientID = lngClientID1 Then
                                    objResultStatus = ProceedOnHolderClientIDChange(objCurrentStatus, enuTeamType.Red, _
                                                                        blnRedFlagInBase, blnBlueFlagInBase, _
                                                                        lngRedFlagHolderClientID, lngBlueFlagHolderClientID, _
                                                                        lngRedFlagResetTime, lngBlueFlagResetTime, _
                                                                        lngEventID, lngGameID, _
                                                                        lngEventTime, lngLineNo, strEventType, _
                                                                        lngClientID1, lngClientID2, enuClientTeam1, enuClientTeam2, _
                                                                        strItemName, strWeaponName)
                                    mlstWorkingSet.Add(objResultStatus)
                                    mlstWorkingSet.Remove(objCurrentStatus)

                                    intStateIdx += 1
                                Else
                                    'Event doesn't affect flag, so move to next node in current working set
                                    intStateIdx += 1
                                    'blnPrint = False
                                End If
                            Case enuTeamType.Blue
                                If lngRedFlagHolderClientID = lngClientID1 Then
                                    objResultStatus = ProceedOnHolderClientIDChange(objCurrentStatus, enuTeamType.Blue, _
                                                                        blnRedFlagInBase, blnBlueFlagInBase, _
                                                                        lngRedFlagHolderClientID, lngBlueFlagHolderClientID, _
                                                                        lngRedFlagResetTime, lngBlueFlagResetTime, _
                                                                        lngEventID, lngGameID, _
                                                                        lngEventTime, lngLineNo, strEventType, _
                                                                        lngClientID1, lngClientID2, enuClientTeam1, enuClientTeam2, _
                                                                        strItemName, strWeaponName)
                                    mlstWorkingSet.Add(objResultStatus)
                                    mlstWorkingSet.Remove(objCurrentStatus)

                                    intStateIdx += 1
                                Else
                                    'Event doesn't affect flag, so move to next node in current working set
                                    intStateIdx += 1
                                    'blnPrint = False
                                End If
                            Case Else
                                'Spectator number changing
                                'Event doesn't affect flag, so move to next node in current working set
                                intStateIdx += 1
                                'blnPrint = False
                        End Select
                    Case Else
                        Throw New Exception("Unknown event type: " & strEventType)
                End Select
            Loop

            'If blnPrint Then
            'PruneDuplicatesFromWorkingSet()
            'PrintGameStatusTree()
            'PrintFirstBranch()
            'PrintTreeToFile(intPrintIdx)
            'intPrintIdx += 1
            'End If

            'blnPrint = False
        Next
        'Print("BEFORE:")
        'PrintGameStatusTree()

        'PrintTreeToFile()

        PruneBranchesNotTallying(plngGameID)

        'Print game tree status
        'Using fileOutput As New FileStream(ConfigurationManager.AppSettings("OutputFilesDir") & "GameTree_" & Now.ToString("yyyyMMddHHmmss") & ".log", FileMode.CreateNew)
        'Using writer As New StreamWriter(fileOutput)
        'PrintGameStatusTree(writer)
        'End Using
        'End Using

        'Print("AFTER:")
        'PrintGameStatusTree()

        If mobjRoot Is Nothing Then
            Throw New Exception("ROOT IS GONE!!!")
        End If

        'If mobjRoot.Children.Count = 0 Then
        'Throw New Exception("Tree got destroyed!")
        'End If
    End Sub
#End Region

#Region "Private Helper Functions"
    ''' <summary>
    ''' Returns a string of X concatenated tabs.
    ''' </summary>
    ''' <param name="pintLevel">X</param>
    ''' <returns>The tab string.</returns>
    Private Function TabOut(ByVal pintLevel As Integer) As String
        Static intCurrentLevel As Integer = 0
        Static strCurrentResult As String = String.Empty

        If pintLevel = 0 Then
            intCurrentLevel = 0
            strCurrentResult = String.Empty
        End If

        If pintLevel <> intCurrentLevel Then
            strCurrentResult = String.Empty

            For intIdx As Integer = 1 To pintLevel
                strCurrentResult &= "  "
            Next

            intCurrentLevel = pintLevel
        End If

        Return strCurrentResult
    End Function

    ''' <summary>
    ''' Prints the game status for a specified node, and all its children, only first child recursively.
    ''' </summary>
    ''' <param name="pobjNode">The node to print.</param>
    ''' <param name="pintLevel">The level in the tree of the current node.</param>
    ''' <param name="pobjWriter">Optional the writer object to write a file with, or Nothing to output to the console.</param>
    Private Sub PrintFirstBranchRecDown(ByVal pobjNode As clsStatusSnapshot, ByVal pintLevel As Integer, _
                                   Optional ByVal pobjWriter As StreamWriter = Nothing)
        Dim strResult As String = PrintCurrentNode(pobjNode, pintLevel)
        Dim lstChildStatusNodes As List(Of clsStatusSnapshot) = pobjNode.Children

        If pobjWriter IsNot Nothing Then
            pobjWriter.Write(strResult)
        Else
            Print(strResult)
        End If

        If lstChildStatusNodes.Count > 0 Then
            PrintFirstBranchRecDown(lstChildStatusNodes(0), pintLevel + 1, pobjWriter)
        End If
    End Sub

    ''' <summary>
    ''' Prints the game status for a specified node, and all its children, breadth-first recursively.
    ''' </summary>
    ''' <param name="pobjNode">The node to print.</param>
    ''' <param name="pintLevel">The level in the tree of the current node.</param>
    ''' <param name="pobjWriter">Optional the writer object to write a file with, or Nothing to output to the console.</param>
    Private Sub PrintGameStatusRecDown(ByVal pobjNode As clsStatusSnapshot, ByVal pintLevel As Integer, _
                                   Optional ByVal pobjWriter As StreamWriter = Nothing)
        Dim strResult As String = PrintCurrentNode(pobjNode, pintLevel)
        Dim lstChildStatusNodes As List(Of clsStatusSnapshot) = pobjNode.Children

        If pobjWriter IsNot Nothing Then
            pobjWriter.Write(strResult)
        Else
            Print(strResult)
        End If

        For Each objChild As clsStatusSnapshot In lstChildStatusNodes
            PrintGameStatusRecDown(objChild, pintLevel + 1, pobjWriter)
        Next
    End Sub

    Private Sub PrintGameStatusRecUp(ByVal pobjNode As clsStatusSnapshot, ByVal pintLevel As Integer, _
                                   Optional ByVal pobjWriter As StreamWriter = Nothing)
        Dim strResult As String = PrintCurrentNode(pobjNode, pintLevel)
        Dim lstChildStatusNodes As List(Of clsStatusSnapshot) = pobjNode.Children

        If pobjWriter IsNot Nothing Then
            pobjWriter.Write(strResult)
        Else
            Print(strResult)
        End If

        If pobjNode.Parent IsNot Nothing Then
            PrintGameStatusRecUp(pobjNode.Parent.Prior, pintLevel + 1)
        End If
    End Sub

    Private Function PrintCurrentNode(ByVal pobjNode As clsStatusSnapshot, ByVal pintLevel As Integer) As String
        Dim strResult As String = String.Empty

        strResult &= TabOut(pintLevel)
        If pobjNode.Parent IsNot Nothing Then
            strResult &= pobjNode.Parent.LineNo & ": "
            Select Case pobjNode.Parent.Significance
                Case enuSignificanceType.Capture
                    Select Case pobjNode.Parent.Client1Team
                        Case enuTeamType.Red
                            strResult &= "RED "
                        Case enuTeamType.Blue
                            strResult &= "BLUE "
                    End Select
                    strResult &= pobjNode.Parent.Client1ID & " (" & GetClientLogID(pobjNode.Parent.Client1ID) & ") "
                    strResult &= "CAPTURES "
                Case enuSignificanceType.CarrierClientEnd
                    Select Case pobjNode.Parent.Client1Team
                        Case enuTeamType.Red
                            strResult &= "RED "
                        Case enuTeamType.Blue
                            strResult &= "BLUE "
                    End Select
                    strResult &= pobjNode.Parent.Client1ID & " (" & GetClientLogID(pobjNode.Parent.Client1ID) & ") "
                    strResult &= "CLIENT CARRIER OF "
                    Select Case pobjNode.Parent.Client1Team
                        Case enuTeamType.Red
                            strResult &= "BLUE FLAG "
                        Case enuTeamType.Blue
                            strResult &= "RED FLAG "
                    End Select
                    strResult &= "ENDS "
                Case enuSignificanceType.CarrierKillWithDrop
                    Select Case pobjNode.Parent.Client1Team
                        Case enuTeamType.Red
                            strResult &= "RED "
                        Case enuTeamType.Blue
                            strResult &= "BLUE "
                    End Select
                    strResult &= pobjNode.Parent.Client1ID & " (" & GetClientLogID(pobjNode.Parent.Client1ID) & ") "
                    strResult &= "KILLS "
                    Select Case pobjNode.Parent.Client2Team
                        Case enuTeamType.Red
                            strResult &= "RED "
                        Case enuTeamType.Blue
                            strResult &= "BLUE "
                    End Select
                    strResult &= pobjNode.Parent.Client2ID & " (" & GetClientLogID(pobjNode.Parent.Client2ID) & ") "
                    strResult &= "CARRIER OF "
                    Select Case pobjNode.Parent.Client2Team
                        Case enuTeamType.Red
                            strResult &= "BLUE FLAG "
                        Case enuTeamType.Blue
                            strResult &= "RED FLAG "
                    End Select
                    strResult &= "FLAG DROPS "
                Case enuSignificanceType.CarrierKillWithReset
                    Select Case pobjNode.Parent.Client1Team
                        Case enuTeamType.Red
                            strResult &= "RED "
                        Case enuTeamType.Blue
                            strResult &= "BLUE "
                    End Select
                    strResult &= pobjNode.Parent.Client1ID & " (" & GetClientLogID(pobjNode.Parent.Client1ID) & ") "
                    strResult &= "KILLS "
                    Select Case pobjNode.Parent.Client2Team
                        Case enuTeamType.Red
                            strResult &= "RED "
                        Case enuTeamType.Blue
                            strResult &= "BLUE "
                    End Select
                    strResult &= pobjNode.Parent.Client2ID & " (" & GetClientLogID(pobjNode.Parent.Client2ID) & ") "
                    strResult &= "CARRIER OF "
                    Select Case pobjNode.Parent.Client2Team
                        Case enuTeamType.Red
                            strResult &= "BLUE FLAG "
                        Case enuTeamType.Blue
                            strResult &= "RED FLAG "
                    End Select
                    strResult &= "FLAG RESETS "
                Case enuSignificanceType.Pickup
                    Select Case pobjNode.Parent.Client1Team
                        Case enuTeamType.Red
                            strResult &= "RED "
                        Case enuTeamType.Blue
                            strResult &= "BLUE "
                    End Select
                    strResult &= pobjNode.Parent.Client1ID & " (" & GetClientLogID(pobjNode.Parent.Client1ID) & ") "
                    strResult &= "PICKS UP "
                    Select Case pobjNode.Parent.Client1Team
                        Case enuTeamType.Red
                            strResult &= "BLUE FLAG "
                        Case enuTeamType.Blue
                            strResult &= "RED FLAG "
                    End Select
                Case enuSignificanceType.Recovery
                    Select Case pobjNode.Parent.Client1Team
                        Case enuTeamType.Red
                            strResult &= "RED "
                        Case enuTeamType.Blue
                            strResult &= "BLUE "
                    End Select
                    strResult &= pobjNode.Parent.Client1ID & " (" & GetClientLogID(pobjNode.Parent.Client1ID) & ") "
                    strResult &= "RECOVERS "
                    Select Case pobjNode.Parent.Client1Team
                        Case enuTeamType.Red
                            strResult &= "RED FLAG "
                        Case enuTeamType.Blue
                            strResult &= "BLUE FLAG "
                    End Select
                Case enuSignificanceType.ResetDueToRedTimerExpiration
                    strResult &= "RED TIMER EXPIRES RED FLAG RESET "
                Case enuSignificanceType.ResetDueToBlueTimerExpiration
                    strResult &= "BLUE TIMER EXPIRES BLUE FLAG RESET "
                Case enuSignificanceType.Steal
                    Select Case pobjNode.Parent.Client1Team
                        Case enuTeamType.Red
                            strResult &= "RED "
                        Case enuTeamType.Blue
                            strResult &= "BLUE "
                    End Select
                    strResult &= pobjNode.Parent.Client1ID & " (" & GetClientLogID(pobjNode.Parent.Client1ID) & ") "
                    strResult &= "STEALS "
                    Select Case pobjNode.Parent.Client1Team
                        Case enuTeamType.Red
                            strResult &= "BLUE FLAG "
                        Case enuTeamType.Blue
                            strResult &= "RED FLAG "
                    End Select
                Case enuSignificanceType.CarrierClientNumberChange
                    Select Case pobjNode.Parent.Client1Team
                        Case enuTeamType.Red
                            strResult &= "RED "
                        Case enuTeamType.Blue
                            strResult &= "BLUE "
                    End Select
                    strResult &= pobjNode.Parent.Client1ID & " (" & GetClientLogID(pobjNode.Parent.Client1ID) & ") "
                    strResult &= "ID CHANGES TO " & pobjNode.Parent.Client2ID & " (" & GetClientLogID(pobjNode.Parent.Client2ID) & ") "
                Case enuSignificanceType.CarrierClientTeamChange
                    Select Case pobjNode.Parent.Client1Team
                        Case enuTeamType.Red
                            strResult &= "RED "
                        Case enuTeamType.Blue
                            strResult &= "BLUE "
                    End Select
                    strResult &= pobjNode.Parent.Client1ID & " (" & GetClientLogID(pobjNode.Parent.Client1ID) & ") "
                    strResult &= "TEAM CHANGES TO "
                    Select Case pobjNode.Parent.Client2Team
                        Case enuTeamType.Red
                            strResult &= "RED "
                        Case enuTeamType.Blue
                            strResult &= "BLUE "
                    End Select
                    strResult &= pobjNode.Parent.Client2ID & " (" & GetClientLogID(pobjNode.Parent.Client2ID) & ") "
                Case Else
                    Throw New Exception("Unknown event cause!")
            End Select
        Else
            strResult &= "START OF GAME "
        End If
        strResult &= " ->"
        strResult &= vbCrLf

        strResult &= TabOut(pintLevel) & "NODE: " & pobjNode.NodeID & " Rbase: " & CStr(IIf(pobjNode.RedFlagInBase, "Y", "N")) & " Rhold: " & pobjNode.RedFlagHolderClientID & " (" & GetClientLogID(pobjNode.RedFlagHolderClientID) & ") " & _
                "Bbase: " & CStr(IIf(pobjNode.BlueFlagInBase, "Y", "N")) & " Bhold: " & pobjNode.BlueFlagHolderClientID & " (" & GetClientLogID(pobjNode.BlueFlagHolderClientID) & ") Rtime: " & pobjNode.RedFlagResetTime & " Btime: " & pobjNode.BlueFlagResetTime & vbCrLf

        'Recurse on each child, increasing depth of recusion
        strResult &= TabOut(pintLevel) & "has " & pobjNode.Children.Count & " children: " & vbCrLf

        Return strResult
    End Function

    Private Sub PruneBranchesNotTallying(ByVal plngGameID As Long)
        Dim objWSNode As clsStatusSnapshot
        Dim objCurrentNode As clsStatusSnapshot
        Dim intRedScoreDB As Integer = GetRedScore(plngGameID)
        Dim intBlueScoreDB As Integer = GetBlueScore(plngGameID)
        Dim intRedScoreCalc As Integer = 0
        Dim intBlueScoreCalc As Integer = 0

        'Consider each leaf node
        Dim intIdx As Integer = 0
        Do While intIdx < mlstWorkingSet.Count - 1
            objWSNode = mlstWorkingSet(intIdx)
            objCurrentNode = objWSNode

            'Ascend tree, counting captures
            Do Until objCurrentNode.Parent Is Nothing
                If objCurrentNode.Parent.Significance = enuSignificanceType.Capture Then
                    Select Case objCurrentNode.Parent.Client1Team
                        Case enuTeamType.Red
                            intRedScoreCalc += 1
                        Case enuTeamType.Blue
                            intBlueScoreCalc += 1
                        Case Else
                            Throw New Exception("Red or blue must capture flag!")
                    End Select
                End If

                objCurrentNode = objCurrentNode.Parent.Prior
            Loop

            'Check if captures tally
            If intRedScoreCalc <> intRedScoreDB OrElse intBlueScoreCalc <> intBlueScoreDB Then
                PruneImpossibleBranch(objWSNode)
            Else
                intIdx += 1
            End If
        Loop
    End Sub

    ''' <summary>
    ''' Prunes the impossible branch from the game tree
    ''' </summary>
    ''' <param name="pobjBeginNode">A leaf node present on the branch to trim.</param>
    Private Sub PruneImpossibleBranch(ByRef pobjBeginNode As clsStatusSnapshot)
        Dim objCurrentNode As clsStatusSnapshot = pobjBeginNode
        Dim objClipNode As clsStatusSnapshot = Nothing

        If pobjBeginNode.NodeID = 12 Then
            Dim i As Int16 = 0
        End If

        'Print("Pruning from node with id: " & pobjBeginNode.NodeID)
        'Print("BEFORE PRINTING TO FILE WITH -1:")
        'PrintTreeToFile(-1)

        'Walk the tree to find the clip node
        Do Until objClipNode IsNot Nothing
            'Step up the tree, if possible
            If objCurrentNode.Parent IsNot Nothing Then
                'Check if the parent node has multiple children, if so, it is the clip point
                If objCurrentNode.Parent.Prior.Children.Count > 1 Then
                    objClipNode = objCurrentNode.Parent.Prior
                Else 'continue walking up
                    objCurrentNode = objCurrentNode.Parent.Prior()
                End If
            Else
                'No parent node, we've reached the start of the tree.  Should never happen
                Throw New Exception("Can't prune root of tree!")
            End If
        Loop

        'Do the clipping 
        objClipNode.RemoveChild(objCurrentNode)

        'And remove the current node from the working set
        mlstWorkingSet.Remove(pobjBeginNode)

        'Print("AFTER:")
        'PrintGameStatusTree()
    End Sub

    ''' <summary>
    ''' Calls CreateNewPotentialStatus() to branches a new status caused by player ending when holding the flag.
    ''' </summary>
    ''' <param name="pobjCurrentStatus">The current status.</param>
    ''' <param name="penuTeam">The team which is dropping the flag.</param>
    ''' <param name="pblnRedFlagInBase">if set to <c>true</c> [red flag in base].</param>
    ''' <param name="pblnBlueFlagInBase">if set to <c>true</c> [blue flag in base].</param>
    ''' <param name="plngRedFlagHolderClientID">The red flag holder client ID.</param>
    ''' <param name="plngBlueFlagHolderClientID">The blue flag holder client ID.</param>
    ''' <param name="plngRedFlagResetTime">The red flag reset time.</param>
    ''' <param name="plngBlueFlagResetTime">The blue flag reset time.</param>
    ''' <param name="plngEventID">The event ID.</param>
    ''' <param name="plngGameID">The game ID.</param>
    ''' <param name="plngEventTime">The event time.</param>
    ''' <param name="plngLineNo">The line no.</param>
    ''' <param name="pstrEventType">Type of the event.</param>
    ''' <param name="plngClientID1">The client I d1.</param>
    ''' <param name="plngClientID2">The client I d2.</param>
    ''' <param name="penuClientTeam1">The client team1.</param>
    ''' <param name="penuClientTeam2">The client team2.</param>
    ''' <param name="pstrItemName">Name of the item.</param>
    ''' <param name="pstrWeaponName">Name of the weapon.</param>
    ''' <returns>New result status snapshot object created at the end of the branch.</returns>
    Private Function ProceedOnClientEnd(ByRef pobjCurrentStatus As clsStatusSnapshot, ByVal penuTeam As enuTeamType, _
                                        ByVal pblnRedFlagInBase As Boolean, ByVal pblnBlueFlagInBase As Boolean, _
                                        ByVal plngRedFlagHolderClientID As Long, ByVal plngBlueFlagHolderClientID As Long, _
                                        ByVal plngRedFlagResetTime As Long, ByVal plngBlueFlagResetTime As Long, _
                                        ByVal plngEventID As Long, ByVal plngGameID As Long, _
                                        ByVal plngEventTime As Long, ByVal plngLineNo As Long, _
                                        ByVal pstrEventType As String, ByVal plngClientID1 As Long, ByVal plngClientID2 As Long, _
                                        ByVal penuClientTeam1 As enuTeamType, ByVal penuClientTeam2 As enuTeamType, _
                                        ByVal pstrItemName As String, ByVal pstrWeaponName As String) As clsStatusSnapshot
        Select Case penuTeam
            Case enuTeamType.Red
                If pblnBlueFlagInBase Then Throw New Exception("Can't end with flag already in base!")
                If plngBlueFlagHolderClientID = 0 Then Throw New Exception("Can't recover flag not being held!")

                Return CreateNewPotentialStatus(pobjCurrentStatus, enuSignificanceType.CarrierClientEnd, _
                                                pblnRedFlagInBase, True, plngRedFlagHolderClientID, 0, _
                                                plngRedFlagResetTime, 0, plngEventID, _
                                                plngGameID, plngEventTime, plngLineNo, pstrEventType, _
                                                plngClientID1, plngClientID2, penuClientTeam1, penuClientTeam2, _
                                                pstrItemName, pstrWeaponName)

            Case enuTeamType.Blue
                If pblnRedFlagInBase Then Throw New Exception("Can't end with flag already in base!")
                If plngRedFlagHolderClientID = 0 Then Throw New Exception("Can't recover flag not being held!")

                Return CreateNewPotentialStatus(pobjCurrentStatus, enuSignificanceType.CarrierClientEnd, _
                                                True, pblnBlueFlagInBase, 0, plngBlueFlagHolderClientID, _
                                                0, plngBlueFlagResetTime, plngEventID, _
                                                plngGameID, plngEventTime, plngLineNo, pstrEventType, _
                                                plngClientID1, plngClientID2, penuClientTeam1, penuClientTeam2, _
                                                pstrItemName, pstrWeaponName)
            Case Else
                Throw New Exception("Invalid team type for client end: " & penuTeam)
        End Select
    End Function

    ''' <summary>
    ''' Calls CreateNewPotentialStatus() to branches a new status caused by a flag recovery.
    ''' </summary>
    ''' <param name="pobjCurrentStatus">The current status.</param>
    ''' <param name="penuTeam">The team which is recovering the flag.</param>
    ''' <param name="pblnRedFlagInBase">if set to <c>true</c> [red flag in base].</param>
    ''' <param name="pblnBlueFlagInBase">if set to <c>true</c> [blue flag in base].</param>
    ''' <param name="plngRedFlagHolderClientID">The red flag holder client ID.</param>
    ''' <param name="plngBlueFlagHolderClientID">The blue flag holder client ID.</param>
    ''' <param name="plngRedFlagResetTime">The red flag reset time.</param>
    ''' <param name="plngBlueFlagResetTime">The blue flag reset time.</param>
    ''' <param name="plngEventID">The event ID.</param>
    ''' <param name="plngGameID">The game ID.</param>
    ''' <param name="plngEventTime">The event time.</param>
    ''' <param name="plngLineNo">The line no.</param>
    ''' <param name="pstrEventType">Type of the event.</param>
    ''' <param name="plngClientID1">The client I d1.</param>
    ''' <param name="plngClientID2">The client I d2.</param>
    ''' <param name="penuClientTeam1">The client team1.</param>
    ''' <param name="penuClientTeam2">The client team2.</param>
    ''' <param name="pstrItemName">Name of the item.</param>
    ''' <param name="pstrWeaponName">Name of the weapon.</param>
    ''' <returns>New result status snapshot object created at the end of the branch.</returns>
    Private Function ProceedOnRecovery(ByRef pobjCurrentStatus As clsStatusSnapshot, ByVal penuTeam As enuTeamType, _
                                        ByVal pblnRedFlagInBase As Boolean, ByVal pblnBlueFlagInBase As Boolean, _
                                        ByVal plngRedFlagHolderClientID As Long, ByVal plngBlueFlagHolderClientID As Long, _
                                        ByVal plngRedFlagResetTime As Long, ByVal plngBlueFlagResetTime As Long, _
                                        ByVal plngEventID As Long, ByVal plngGameID As Long, _
                                        ByVal plngEventTime As Long, ByVal plngLineNo As Long, _
                                        ByVal pstrEventType As String, ByVal plngClientID1 As Long, ByVal plngClientID2 As Long, _
                                        ByVal penuClientTeam1 As enuTeamType, ByVal penuClientTeam2 As enuTeamType, _
                                        ByVal pstrItemName As String, ByVal pstrWeaponName As String) As clsStatusSnapshot
        Select Case penuTeam
            Case enuTeamType.Red
                If pblnRedFlagInBase Then Throw New Exception("Can't recover flag already in base!")
                If plngRedFlagHolderClientID <> 0 Then Throw New Exception("Can't recover flag being held!")

                Return CreateNewPotentialStatus(pobjCurrentStatus, enuSignificanceType.Recovery, _
                                                True, pblnBlueFlagInBase, 0, plngBlueFlagHolderClientID, _
                                                0, plngBlueFlagResetTime, plngEventID, _
                                                plngGameID, plngEventTime, plngLineNo, pstrEventType, _
                                                plngClientID1, plngClientID2, penuClientTeam1, penuClientTeam2, _
                                                pstrItemName, pstrWeaponName)
            Case enuTeamType.Blue
                If pblnBlueFlagInBase Then Throw New Exception("Can't recover flag already in base!")
                If plngBlueFlagHolderClientID <> 0 Then Throw New Exception("Can't recover flag being held!")

                Return CreateNewPotentialStatus(pobjCurrentStatus, enuSignificanceType.Recovery, _
                                                pblnRedFlagInBase, True, plngRedFlagHolderClientID, 0, _
                                                plngRedFlagResetTime, 0, plngEventID, _
                                                plngGameID, plngEventTime, plngLineNo, pstrEventType, _
                                                plngClientID1, plngClientID2, penuClientTeam1, penuClientTeam2, _
                                                pstrItemName, pstrWeaponName)
            Case Else
                Throw New Exception("Invalid team type for capture: " & penuTeam)
        End Select
    End Function

    ''' <summary>
    ''' Calls CreateNewPotentialStatus() to branches a new status caused by a flag holder's id changing.
    ''' </summary>
    ''' <param name="pobjCurrentStatus">The current status.</param>
    ''' <param name="penuTeam">The team which is has a player with id changing.</param>
    ''' <param name="pblnRedFlagInBase">if set to <c>true</c> [red flag in base].</param>
    ''' <param name="pblnBlueFlagInBase">if set to <c>true</c> [blue flag in base].</param>
    ''' <param name="plngRedFlagHolderClientID">The red flag holder client ID.</param>
    ''' <param name="plngBlueFlagHolderClientID">The blue flag holder client ID.</param>
    ''' <param name="plngRedFlagResetTime">The red flag reset time.</param>
    ''' <param name="plngBlueFlagResetTime">The blue flag reset time.</param>
    ''' <param name="plngEventID">The event ID.</param>
    ''' <param name="plngGameID">The game ID.</param>
    ''' <param name="plngEventTime">The event time.</param>
    ''' <param name="plngLineNo">The line no.</param>
    ''' <param name="pstrEventType">Type of the event.</param>
    ''' <param name="plngClientID1">The client I d1.</param>
    ''' <param name="plngClientID2">The client I d2.</param>
    ''' <param name="penuClientTeam1">The client team1.</param>
    ''' <param name="penuClientTeam2">The client team2.</param>
    ''' <param name="pstrItemName">Name of the item.</param>
    ''' <param name="pstrWeaponName">Name of the weapon.</param>
    ''' <returns>New result status snapshot object created at the end of the branch.</returns>
    Private Function ProceedOnHolderClientIDChange(ByRef pobjCurrentStatus As clsStatusSnapshot, ByVal penuTeam As enuTeamType, _
                                        ByVal pblnRedFlagInBase As Boolean, ByVal pblnBlueFlagInBase As Boolean, _
                                        ByVal plngRedFlagHolderClientID As Long, ByVal plngBlueFlagHolderClientID As Long, _
                                        ByVal plngRedFlagResetTime As Long, ByVal plngBlueFlagResetTime As Long, _
                                        ByVal plngEventID As Long, ByVal plngGameID As Long, _
                                        ByVal plngEventTime As Long, ByVal plngLineNo As Long, _
                                        ByVal pstrEventType As String, ByVal plngClientID1 As Long, ByVal plngClientID2 As Long, _
                                        ByVal penuClientTeam1 As enuTeamType, ByVal penuClientTeam2 As enuTeamType, _
                                        ByVal pstrItemName As String, ByVal pstrWeaponName As String) As clsStatusSnapshot
        If Not plngClientID1 < plngClientID2 Then Throw New Exception("New client id not greater than previous client id!")

        Select Case penuTeam
            Case enuTeamType.Red
                If pblnBlueFlagInBase Then Throw New Exception("Blue flag currently in base!")
                If plngBlueFlagHolderClientID <> plngClientID1 Then Throw New Exception("Only blue flag holder can cause holder id change!")

                Return CreateNewPotentialStatus(pobjCurrentStatus, enuSignificanceType.CarrierClientNumberChange, _
                                                pblnRedFlagInBase, False, plngRedFlagHolderClientID, plngClientID2, _
                                                plngRedFlagResetTime, 0, plngEventID, _
                                                plngGameID, plngEventTime, plngLineNo, pstrEventType, _
                                                plngClientID1, plngClientID2, penuClientTeam1, penuClientTeam2, _
                                                pstrItemName, pstrWeaponName)
            Case enuTeamType.Blue
                If pblnRedFlagInBase Then Throw New Exception("Red flag currently in base!")
                If plngRedFlagHolderClientID <> plngClientID1 Then Throw New Exception("Only red flag holder can cause holder id change!")

                Return CreateNewPotentialStatus(pobjCurrentStatus, enuSignificanceType.CarrierClientNumberChange, _
                                                False, pblnBlueFlagInBase, plngClientID2, plngBlueFlagHolderClientID, _
                                                0, plngBlueFlagResetTime, plngEventID, _
                                                plngGameID, plngEventTime, plngLineNo, pstrEventType, _
                                                plngClientID1, plngClientID2, penuClientTeam1, penuClientTeam2, _
                                                pstrItemName, pstrWeaponName)
            Case Else
                Throw New Exception("Invalid team type for client number change: " & penuTeam)
        End Select

    End Function

    ''' <summary>
    ''' Calls CreateNewPotentialStatus() to branches a new status caused by a flag holder's team changing.
    ''' </summary>
    ''' <param name="pobjCurrentStatus">The current status.</param>
    ''' <param name="penuTeam">The inital team of the player who is team changing.</param>
    ''' <param name="pblnRedFlagInBase">if set to <c>true</c> [red flag in base].</param>
    ''' <param name="pblnBlueFlagInBase">if set to <c>true</c> [blue flag in base].</param>
    ''' <param name="plngRedFlagHolderClientID">The red flag holder client ID.</param>
    ''' <param name="plngBlueFlagHolderClientID">The blue flag holder client ID.</param>
    ''' <param name="plngRedFlagResetTime">The red flag reset time.</param>
    ''' <param name="plngBlueFlagResetTime">The blue flag reset time.</param>
    ''' <param name="plngEventID">The event ID.</param>
    ''' <param name="plngGameID">The game ID.</param>
    ''' <param name="plngEventTime">The event time.</param>
    ''' <param name="plngLineNo">The line no.</param>
    ''' <param name="pstrEventType">Type of the event.</param>
    ''' <param name="plngClientID1">The client I d1.</param>
    ''' <param name="plngClientID2">The client I d2.</param>
    ''' <param name="penuClientTeam1">The client team1.</param>
    ''' <param name="penuClientTeam2">The client team2.</param>
    ''' <param name="pstrItemName">Name of the item.</param>
    ''' <param name="pstrWeaponName">Name of the weapon.</param>
    ''' <returns>New result status snapshot object created at the end of the branch.</returns>
    Private Function ProceedOnHolderTeamChange(ByRef pobjCurrentStatus As clsStatusSnapshot, ByVal penuTeam As enuTeamType, _
                                        ByVal pblnRedFlagInBase As Boolean, ByVal pblnBlueFlagInBase As Boolean, _
                                        ByVal plngRedFlagHolderClientID As Long, ByVal plngBlueFlagHolderClientID As Long, _
                                        ByVal plngRedFlagResetTime As Long, ByVal plngBlueFlagResetTime As Long, _
                                        ByVal plngEventID As Long, ByVal plngGameID As Long, _
                                        ByVal plngEventTime As Long, ByVal plngLineNo As Long, _
                                        ByVal pstrEventType As String, ByVal plngClientID1 As Long, ByVal plngClientID2 As Long, _
                                        ByVal penuClientTeam1 As enuTeamType, ByVal penuClientTeam2 As enuTeamType, _
                                        ByVal pstrItemName As String, ByVal pstrWeaponName As String) As clsStatusSnapshot
        If Not plngClientID1 < plngClientID2 Then Throw New Exception("New client id not greater than previous client id!")

        Select Case penuTeam
            Case enuTeamType.Red
                If pblnBlueFlagInBase Then Throw New Exception("Blue flag currently in base!")
                If plngBlueFlagHolderClientID <> plngClientID1 Then Throw New Exception("Only blue flag holder can cause holder team change!")

                Return CreateNewPotentialStatus(pobjCurrentStatus, enuSignificanceType.CarrierClientTeamChange, _
                                                pblnRedFlagInBase, True, plngRedFlagHolderClientID, 0, _
                                                plngRedFlagResetTime, 0, plngEventID, _
                                                plngGameID, plngEventTime, plngLineNo, pstrEventType, _
                                                plngClientID1, plngClientID2, penuClientTeam1, penuClientTeam2, _
                                                pstrItemName, pstrWeaponName)
            Case enuTeamType.Blue
                If pblnRedFlagInBase Then Throw New Exception("Red flag currently in base!")
                If plngRedFlagHolderClientID <> plngClientID1 Then Throw New Exception("Only red flag holder can cause holder team change!")

                Return CreateNewPotentialStatus(pobjCurrentStatus, enuSignificanceType.CarrierClientTeamChange, _
                                                True, pblnBlueFlagInBase, 0, plngBlueFlagHolderClientID, _
                                                0, plngBlueFlagResetTime, plngEventID, _
                                                plngGameID, plngEventTime, plngLineNo, pstrEventType, _
                                                plngClientID1, plngClientID2, penuClientTeam1, penuClientTeam2, _
                                                pstrItemName, pstrWeaponName)
            Case Else
                Throw New Exception("Invalid team type for client team change: " & penuTeam)
        End Select

    End Function

    ''' <summary>
    ''' Calls CreateNewPotentialStatus() to branches a new status caused by a flag capture.
    ''' </summary>
    ''' <param name="pobjCurrentStatus">The current status.</param>
    ''' <param name="penuTeam">The team which is capturing the flag.</param>
    ''' <param name="pblnRedFlagInBase">if set to <c>true</c> [red flag in base].</param>
    ''' <param name="pblnBlueFlagInBase">if set to <c>true</c> [blue flag in base].</param>
    ''' <param name="plngRedFlagHolderClientID">The red flag holder client ID.</param>
    ''' <param name="plngBlueFlagHolderClientID">The blue flag holder client ID.</param>
    ''' <param name="plngRedFlagResetTime">The red flag reset time.</param>
    ''' <param name="plngBlueFlagResetTime">The blue flag reset time.</param>
    ''' <param name="plngEventID">The event ID.</param>
    ''' <param name="plngGameID">The game ID.</param>
    ''' <param name="plngEventTime">The event time.</param>
    ''' <param name="plngLineNo">The line no.</param>
    ''' <param name="pstrEventType">Type of the event.</param>
    ''' <param name="plngClientID1">The client I d1.</param>
    ''' <param name="plngClientID2">The client I d2.</param>
    ''' <param name="penuClientTeam1">The client team1.</param>
    ''' <param name="penuClientTeam2">The client team2.</param>
    ''' <param name="pstrItemName">Name of the item.</param>
    ''' <param name="pstrWeaponName">Name of the weapon.</param>
    ''' <returns>New result status snapshot object created at the end of the branch.</returns>
    Private Function ProceedOnCapture(ByRef pobjCurrentStatus As clsStatusSnapshot, ByVal penuTeam As enuTeamType, _
                                        ByVal pblnRedFlagInBase As Boolean, ByVal pblnBlueFlagInBase As Boolean, _
                                        ByVal plngRedFlagHolderClientID As Long, ByVal plngBlueFlagHolderClientID As Long, _
                                        ByVal plngRedFlagResetTime As Long, ByVal plngBlueFlagResetTime As Long, _
                                        ByVal plngEventID As Long, ByVal plngGameID As Long, _
                                        ByVal plngEventTime As Long, ByVal plngLineNo As Long, _
                                        ByVal pstrEventType As String, ByVal plngClientID1 As Long, ByVal plngClientID2 As Long, _
                                        ByVal penuClientTeam1 As enuTeamType, ByVal penuClientTeam2 As enuTeamType, _
                                        ByVal pstrItemName As String, ByVal pstrWeaponName As String) As clsStatusSnapshot
        Select Case penuTeam
            Case enuTeamType.Red
                If Not pblnRedFlagInBase Then Throw New Exception("Red can't capture without flag in base!")
                If plngBlueFlagHolderClientID <> plngClientID1 Then Throw New Exception("Only blue flag holder can score a red capture!")

                Return CreateNewPotentialStatus(pobjCurrentStatus, enuSignificanceType.Capture, _
                                                True, True, 0, 0, _
                                                0, 0, plngEventID, _
                                                plngGameID, plngEventTime, plngLineNo, pstrEventType, _
                                                plngClientID1, plngClientID2, penuClientTeam1, penuClientTeam2, _
                                                pstrItemName, pstrWeaponName)
            Case enuTeamType.Blue
                If Not pblnBlueFlagInBase Then Throw New Exception("Blue can't capture without flag in base!")
                If plngRedFlagHolderClientID <> plngClientID1 Then Throw New Exception("Only red flag holder can score a blue capture!")

                Return CreateNewPotentialStatus(pobjCurrentStatus, enuSignificanceType.Capture, _
                                                True, True, 0, 0, _
                                                0, 0, plngEventID, _
                                                plngGameID, plngEventTime, plngLineNo, pstrEventType, _
                                                plngClientID1, plngClientID2, penuClientTeam1, penuClientTeam2, _
                                                pstrItemName, pstrWeaponName)
            Case Else
                Throw New Exception("Invalid team type for capture: " & penuTeam)
        End Select
    End Function


    ''' <summary>
    ''' Calls CreateNewPotentialStatus() to branches a new status caused by a flag pickup.
    ''' </summary>
    ''' <param name="pobjCurrentStatus">The current status.</param>
    ''' <param name="penuTeam">The team which is picking up the flag.</param>
    ''' <param name="pblnRedFlagInBase">if set to <c>true</c> [red flag in base].</param>
    ''' <param name="pblnBlueFlagInBase">if set to <c>true</c> [blue flag in base].</param>
    ''' <param name="plngRedFlagHolderClientID">The red flag holder client ID.</param>
    ''' <param name="plngBlueFlagHolderClientID">The blue flag holder client ID.</param>
    ''' <param name="plngRedFlagResetTime">The red flag reset time.</param>
    ''' <param name="plngBlueFlagResetTime">The blue flag reset time.</param>
    ''' <param name="plngEventID">The event ID.</param>
    ''' <param name="plngGameID">The game ID.</param>
    ''' <param name="plngEventTime">The event time.</param>
    ''' <param name="plngLineNo">The line no.</param>
    ''' <param name="pstrEventType">Type of the event.</param>
    ''' <param name="plngClientID1">The client I d1.</param>
    ''' <param name="plngClientID2">The client I d2.</param>
    ''' <param name="penuClientTeam1">The client team1.</param>
    ''' <param name="penuClientTeam2">The client team2.</param>
    ''' <param name="pstrItemName">Name of the item.</param>
    ''' <param name="pstrWeaponName">Name of the weapon.</param>
    ''' <returns>New result status snapshot object created at the end of the branch.</returns>
    Private Function ProceedOnPickup(ByRef pobjCurrentStatus As clsStatusSnapshot, ByVal penuTeam As enuTeamType, _
                                        ByVal pblnRedFlagInBase As Boolean, ByVal pblnBlueFlagInBase As Boolean, _
                                        ByVal plngRedFlagHolderClientID As Long, ByVal plngBlueFlagHolderClientID As Long, _
                                        ByVal plngRedFlagResetTime As Long, ByVal plngBlueFlagResetTime As Long, _
                                        ByVal plngEventID As Long, ByVal plngGameID As Long, _
                                        ByVal plngEventTime As Long, ByVal plngLineNo As Long, _
                                        ByVal pstrEventType As String, ByVal plngClientID1 As Long, ByVal plngClientID2 As Long, _
                                        ByVal penuClientTeam1 As enuTeamType, ByVal penuClientTeam2 As enuTeamType, _
                                        ByVal pstrItemName As String, ByVal pstrWeaponName As String) As clsStatusSnapshot
        Select Case penuTeam
            Case enuTeamType.Red
                If pblnBlueFlagInBase Then Throw New Exception("Blue flag can't be in base for pickup!")
                'bad
                'If plngBlueFlagHolderClientID <> 0 Then Throw New Exception("Red team already holding blue flag!")

                Return CreateNewPotentialStatus(pobjCurrentStatus, enuSignificanceType.Pickup, _
                                                pblnRedFlagInBase, False, plngRedFlagHolderClientID, plngClientID1, _
                                                plngRedFlagResetTime, 0, plngEventID, _
                                                plngGameID, plngEventTime, plngLineNo, pstrEventType, _
                                                plngClientID1, plngClientID2, penuClientTeam1, penuClientTeam2, _
                                                pstrItemName, pstrWeaponName)
            Case enuTeamType.Blue
                If pblnRedFlagInBase Then Throw New Exception("Red flag can't be in base for pickup!")
                'breaks
                'If plngRedFlagHolderClientID <> 0 Then Throw New Exception("Blue team already holding red flag!")

                Return CreateNewPotentialStatus(pobjCurrentStatus, enuSignificanceType.Pickup, _
                                                False, pblnBlueFlagInBase, plngClientID1, plngBlueFlagHolderClientID, _
                                                0, plngBlueFlagResetTime, plngEventID, _
                                                plngGameID, plngEventTime, plngLineNo, pstrEventType, _
                                                plngClientID1, plngClientID2, penuClientTeam1, penuClientTeam2, _
                                                pstrItemName, pstrWeaponName)
            Case Else
                Throw New Exception("Invalid team type for pickup branching: " & penuTeam)
        End Select
    End Function

    ''' <summary>
    ''' Calls CreateNewPotentialStatus() to branches a new status caused by a flag steal.
    ''' </summary>
    ''' <param name="pobjCurrentStatus">The current status.</param>
    ''' <param name="penuTeam">The team which is capturing the flag.</param>
    ''' <param name="pblnRedFlagInBase">if set to <c>true</c> [red flag in base].</param>
    ''' <param name="pblnBlueFlagInBase">if set to <c>true</c> [blue flag in base].</param>
    ''' <param name="plngRedFlagHolderClientID">The red flag holder client ID.</param>
    ''' <param name="plngBlueFlagHolderClientID">The blue flag holder client ID.</param>
    ''' <param name="plngRedFlagResetTime">The red flag reset time.</param>
    ''' <param name="plngBlueFlagResetTime">The blue flag reset time.</param>
    ''' <param name="plngEventID">The event ID.</param>
    ''' <param name="plngGameID">The game ID.</param>
    ''' <param name="plngEventTime">The event time.</param>
    ''' <param name="plngLineNo">The line no.</param>
    ''' <param name="pstrEventType">Type of the event.</param>
    ''' <param name="plngClientID1">The client I d1.</param>
    ''' <param name="plngClientID2">The client I d2.</param>
    ''' <param name="penuClientTeam1">The client team1.</param>
    ''' <param name="penuClientTeam2">The client team2.</param>
    ''' <param name="pstrItemName">Name of the item.</param>
    ''' <param name="pstrWeaponName">Name of the weapon.</param>
    ''' <returns>New result status snapshot object created at the end of the branch.</returns>
    Private Function ProceedOnSteal(ByRef pobjCurrentStatus As clsStatusSnapshot, ByVal penuTeam As enuTeamType, _
                                        ByVal pblnRedFlagInBase As Boolean, ByVal pblnBlueFlagInBase As Boolean, _
                                        ByVal plngRedFlagHolderClientID As Long, ByVal plngBlueFlagHolderClientID As Long, _
                                        ByVal plngRedFlagResetTime As Long, ByVal plngBlueFlagResetTime As Long, _
                                        ByVal plngEventID As Long, ByVal plngGameID As Long, _
                                        ByVal plngEventTime As Long, ByVal plngLineNo As Long, _
                                        ByVal pstrEventType As String, ByVal plngClientID1 As Long, ByVal plngClientID2 As Long, _
                                        ByVal penuClientTeam1 As enuTeamType, ByVal penuClientTeam2 As enuTeamType, _
                                        ByVal pstrItemName As String, ByVal pstrWeaponName As String) As clsStatusSnapshot
        Select Case penuTeam
            Case enuTeamType.Red
                If Not pblnBlueFlagInBase Then Throw New Exception("Blue flag not in base for steal!")
                If plngBlueFlagHolderClientID <> 0 Then Throw New Exception("Red team already holding blue flag!")

                Return CreateNewPotentialStatus(pobjCurrentStatus, enuSignificanceType.Steal, _
                                                pblnRedFlagInBase, False, plngRedFlagHolderClientID, plngClientID1, _
                                                plngRedFlagResetTime, 0, plngEventID, _
                                                plngGameID, plngEventTime, plngLineNo, pstrEventType, _
                                                plngClientID1, plngClientID2, penuClientTeam1, penuClientTeam2, _
                                                pstrItemName, pstrWeaponName)
            Case enuTeamType.Blue
                If Not pblnRedFlagInBase Then Throw New Exception("Red flag not in base for steal!")
                If plngRedFlagHolderClientID <> 0 Then Throw New Exception("Blue team already holding red flag!")

                Return CreateNewPotentialStatus(pobjCurrentStatus, enuSignificanceType.Steal, _
                                                False, pblnBlueFlagInBase, plngClientID1, plngBlueFlagHolderClientID, _
                                                0, plngBlueFlagResetTime, plngEventID, _
                                                plngGameID, plngEventTime, plngLineNo, pstrEventType, _
                                                plngClientID1, plngClientID2, penuClientTeam1, penuClientTeam2, _
                                                pstrItemName, pstrWeaponName)
            Case Else
                Throw New Exception("Invalid team type for steal branching: " & penuTeam)
        End Select
    End Function

    ''' <summary>
    ''' Calls CreateNewPotentialStatus() to branches a new status caused by a flag holder dying/ending.
    ''' </summary>
    ''' <param name="pobjCurrentStatus">The current status.</param>
    ''' <param name="penuTeam">The team of the player who is dying.</param>
    ''' <param name="pblnRedFlagInBase">if set to <c>true</c> [red flag in base].</param>
    ''' <param name="pblnBlueFlagInBase">if set to <c>true</c> [blue flag in base].</param>
    ''' <param name="plngRedFlagHolderClientID">The red flag holder client ID.</param>
    ''' <param name="plngBlueFlagHolderClientID">The blue flag holder client ID.</param>
    ''' <param name="plngRedFlagResetTime">The red flag reset time.</param>
    ''' <param name="plngBlueFlagResetTime">The blue flag reset time.</param>
    ''' <param name="plngEventID">The event ID.</param>
    ''' <param name="plngGameID">The game ID.</param>
    ''' <param name="plngEventTime">The event time.</param>
    ''' <param name="plngLineNo">The line no.</param>
    ''' <param name="pstrEventType">Type of the event.</param>
    ''' <param name="plngClientID1">The client I d1.</param>
    ''' <param name="plngClientID2">The client I d2.</param>
    ''' <param name="penuClientTeam1">The client team1.</param>
    ''' <param name="penuClientTeam2">The client team2.</param>
    ''' <param name="pstrItemName">Name of the item.</param>
    ''' <param name="pstrWeaponName">Name of the weapon.</param>
    ''' <returns>New result status snapshot object created at the end of the branch.</returns>
    Private Function BranchOnEndOfFlagHoldingWithReset(ByRef pobjCurrentStatus As clsStatusSnapshot, ByVal penuTeam As enuTeamType, _
                                        ByVal pblnRedFlagInBase As Boolean, ByVal pblnBlueFlagInBase As Boolean, _
                                        ByVal plngRedFlagHolderClientID As Long, ByVal plngBlueFlagHolderClientID As Long, _
                                        ByVal plngRedFlagResetTime As Long, ByVal plngBlueFlagResetTime As Long, _
                                        ByVal plngEventID As Long, ByVal plngGameID As Long, _
                                        ByVal plngEventTime As Long, ByVal plngLineNo As Long, _
                                        ByVal pstrEventType As String, ByVal plngClientID1 As Long, ByVal plngClientID2 As Long, _
                                        ByVal penuClientTeam1 As enuTeamType, ByVal penuClientTeam2 As enuTeamType, _
                                        ByVal pstrItemName As String, ByVal pstrWeaponName As String) As clsStatusSnapshot
        Select Case penuTeam
            Case enuTeamType.Red
                If pblnBlueFlagInBase Then Throw New Exception("Blue flag is in the base!")
                If pstrEventType.Equals("KILL") And plngBlueFlagHolderClientID <> plngClientID2 Then Throw New Exception("Blue flag holder not the victim!")
                If pstrEventType.Equals("END") And plngBlueFlagHolderClientID <> plngClientID1 Then Throw New Exception("Blue flag holder not ending!")

                Return CreateNewPotentialStatus(pobjCurrentStatus, CType(IIf(pstrEventType.Equals("KILL"), enuSignificanceType.CarrierKillWithReset, enuSignificanceType.CarrierClientEnd), enuSignificanceType), _
                                pblnRedFlagInBase, True, plngRedFlagHolderClientID, 0, _
                                plngRedFlagResetTime, 0, plngEventID, _
                                plngGameID, plngEventTime, plngLineNo, pstrEventType, _
                                plngClientID1, plngClientID2, penuClientTeam1, penuClientTeam2, _
                                pstrItemName, pstrWeaponName)
            Case enuTeamType.Blue
                If pblnRedFlagInBase Then Throw New Exception("Red flag is in the base!")
                If pstrEventType.Equals("KILL") And plngRedFlagHolderClientID <> plngClientID2 Then Throw New Exception("Red flag holder not the victim!")
                If pstrEventType.Equals("END") And plngRedFlagHolderClientID <> plngClientID1 Then Throw New Exception("Red flag holder not ending!")

                Return CreateNewPotentialStatus(pobjCurrentStatus, CType(IIf(pstrEventType.Equals("KILL"), enuSignificanceType.CarrierKillWithReset, enuSignificanceType.CarrierClientEnd), enuSignificanceType), _
                                                True, pblnBlueFlagInBase, 0, plngBlueFlagHolderClientID, _
                                                0, plngBlueFlagResetTime, plngEventID, _
                                                plngGameID, plngEventTime, plngLineNo, pstrEventType, _
                                                plngClientID1, plngClientID2, penuClientTeam1, penuClientTeam2, _
                                                pstrItemName, pstrWeaponName)
            Case Else
                Throw New Exception("Player dying not on red or blue team: " & penuClientTeam2)
        End Select
    End Function

    ''' <summary>
    ''' Calls CreateNewPotentialStatus() to branches a new status caused by a flag holder dying/ending.
    ''' </summary>
    ''' <param name="pobjCurrentStatus">The current status.</param>
    ''' <param name="penuTeam">The team of the player who is dying.</param>
    ''' <param name="pblnRedFlagInBase">if set to <c>true</c> [red flag in base].</param>
    ''' <param name="pblnBlueFlagInBase">if set to <c>true</c> [blue flag in base].</param>
    ''' <param name="plngRedFlagHolderClientID">The red flag holder client ID.</param>
    ''' <param name="plngBlueFlagHolderClientID">The blue flag holder client ID.</param>
    ''' <param name="plngRedFlagResetTime">The red flag reset time.</param>
    ''' <param name="plngBlueFlagResetTime">The blue flag reset time.</param>
    ''' <param name="plngEventID">The event ID.</param>
    ''' <param name="plngGameID">The game ID.</param>
    ''' <param name="plngEventTime">The event time.</param>
    ''' <param name="plngLineNo">The line no.</param>
    ''' <param name="pstrEventType">Type of the event.</param>
    ''' <param name="plngClientID1">The client I d1.</param>
    ''' <param name="plngClientID2">The client I d2.</param>
    ''' <param name="penuClientTeam1">The client team1.</param>
    ''' <param name="penuClientTeam2">The client team2.</param>
    ''' <param name="pstrItemName">Name of the item.</param>
    ''' <param name="pstrWeaponName">Name of the weapon.</param>
    ''' <returns>New result status snapshot object created at the end of the branch.</returns>
    Private Function BranchOnEndOfFlagHoldingWithoutReset(ByRef pobjCurrentStatus As clsStatusSnapshot, ByVal penuTeam As enuTeamType, _
                                        ByVal pblnRedFlagInBase As Boolean, ByVal pblnBlueFlagInBase As Boolean, _
                                        ByVal plngRedFlagHolderClientID As Long, ByVal plngBlueFlagHolderClientID As Long, _
                                        ByVal plngRedFlagResetTime As Long, ByVal plngBlueFlagResetTime As Long, _
                                        ByVal plngEventID As Long, ByVal plngGameID As Long, _
                                        ByVal plngEventTime As Long, ByVal plngLineNo As Long, _
                                        ByVal pstrEventType As String, ByVal plngClientID1 As Long, ByVal plngClientID2 As Long, _
                                        ByVal penuClientTeam1 As enuTeamType, ByVal penuClientTeam2 As enuTeamType, _
                                        ByVal pstrItemName As String, ByVal pstrWeaponName As String) As clsStatusSnapshot
        Select Case penuTeam
            Case enuTeamType.Red
                If pblnBlueFlagInBase Then Throw New Exception("Blue flag is in the base!")
                If pstrEventType.Equals("KILL") And plngBlueFlagHolderClientID <> plngClientID2 Then Throw New Exception("Blue flag holder not the victim!")
                If pstrEventType.Equals("END") And plngBlueFlagHolderClientID <> plngClientID1 Then Throw New Exception("Blue flag holder not ending!")

                Return CreateNewPotentialStatus(pobjCurrentStatus, CType(IIf(pstrEventType.Equals("KILL"), enuSignificanceType.CarrierKillWithDrop, enuSignificanceType.CarrierClientEnd), enuSignificanceType), _
                                                pblnRedFlagInBase, pblnBlueFlagInBase, plngRedFlagHolderClientID, 0, _
                                                plngRedFlagResetTime, plngEventTime + 30, plngEventID, _
                                                plngGameID, plngEventTime, plngLineNo, pstrEventType, _
                                                plngClientID1, plngClientID2, penuClientTeam1, penuClientTeam2, _
                                                pstrItemName, pstrWeaponName)
            Case enuTeamType.Blue
                If pblnRedFlagInBase Then Throw New Exception("Red flag is in the base!")
                If pstrEventType.Equals("KILL") And plngRedFlagHolderClientID <> plngClientID2 Then Throw New Exception("Red flag holder not the victim!")
                If pstrEventType.Equals("END") And plngRedFlagHolderClientID <> plngClientID1 Then Throw New Exception("Red flag holder not ending!")

                Return CreateNewPotentialStatus(pobjCurrentStatus, CType(IIf(pstrEventType.Equals("KILL"), enuSignificanceType.CarrierKillWithDrop, enuSignificanceType.CarrierClientEnd), enuSignificanceType), _
                                                pblnRedFlagInBase, pblnBlueFlagInBase, 0, plngBlueFlagHolderClientID, _
                                                plngEventTime + 30, plngBlueFlagResetTime, plngEventID, _
                                                plngGameID, plngEventTime, plngLineNo, pstrEventType, _
                                                plngClientID1, plngClientID2, penuClientTeam1, penuClientTeam2, _
                                                pstrItemName, pstrWeaponName)
            Case Else
                Throw New Exception("Player dying not on red or blue team: " & penuClientTeam2)
        End Select
    End Function

    ''' <summary>
    ''' Calls CreateNewPotentialStatus() to branches a new status caused by a timer resetting.
    ''' </summary>
    ''' <param name="pobjCurrentStatus">The current status.</param>
    ''' <param name="penuTeam">The team of the timer which is resetting.</param>
    ''' <param name="pblnRedFlagInBase">if set to <c>true</c> [red flag in base].</param>
    ''' <param name="pblnBlueFlagInBase">if set to <c>true</c> [blue flag in base].</param>
    ''' <param name="plngRedFlagHolderClientID">The red flag holder client ID.</param>
    ''' <param name="plngBlueFlagHolderClientID">The blue flag holder client ID.</param>
    ''' <param name="plngRedFlagResetTime">The red flag reset time.</param>
    ''' <param name="plngBlueFlagResetTime">The blue flag reset time.</param>
    ''' <param name="plngEventID">The event ID.</param>
    ''' <param name="plngGameID">The game ID.</param>
    ''' <param name="plngEventTime">The event time.</param>
    ''' <param name="plngLineNo">The line no.</param>
    ''' <param name="pstrEventType">Type of the event.</param>
    ''' <param name="plngClientID1">The client I d1.</param>
    ''' <param name="plngClientID2">The client I d2.</param>
    ''' <param name="penuClientTeam1">The client team1.</param>
    ''' <param name="penuClientTeam2">The client team2.</param>
    ''' <param name="pstrItemName">Name of the item.</param>
    ''' <param name="pstrWeaponName">Name of the weapon.</param>
    ''' <returns>New result status snapshot object created at the end of the branch.</returns>
    Private Function BranchOnTimerReset(ByRef pobjCurrentStatus As clsStatusSnapshot, ByVal penuTeam As enuTeamType, _
                                        ByVal pblnRedFlagInBase As Boolean, ByVal pblnBlueFlagInBase As Boolean, _
                                        ByVal plngRedFlagHolderClientID As Long, ByVal plngBlueFlagHolderClientID As Long, _
                                        ByVal plngRedFlagResetTime As Long, ByVal plngBlueFlagResetTime As Long, _
                                        ByVal plngEventID As Long, ByVal plngGameID As Long, _
                                        ByVal plngEventTime As Long, ByVal plngLineNo As Long, _
                                        ByVal pstrEventType As String, ByVal plngClientID1 As Long, ByVal plngClientID2 As Long, _
                                        ByVal penuClientTeam1 As enuTeamType, ByVal penuClientTeam2 As enuTeamType, _
                                        ByVal pstrItemName As String, ByVal pstrWeaponName As String) As clsStatusSnapshot
        Select Case penuTeam
            Case enuTeamType.Red
                If plngRedFlagResetTime = 0 Then Throw New Exception("Red timer already 0!")

                Return CreateNewPotentialStatus(pobjCurrentStatus, enuSignificanceType.ResetDueToRedTimerExpiration, _
                                                True, pblnBlueFlagInBase, plngRedFlagHolderClientID, plngBlueFlagHolderClientID, _
                                                0, plngBlueFlagResetTime, plngEventID, _
                                                plngGameID, plngEventTime, plngLineNo, pstrEventType, _
                                                plngClientID1, plngClientID2, penuClientTeam1, penuClientTeam2, _
                                                pstrItemName, pstrWeaponName)
            Case enuTeamType.Blue
                If plngBlueFlagResetTime = 0 Then Throw New Exception("Blue timer already 0!")

                Return CreateNewPotentialStatus(pobjCurrentStatus, enuSignificanceType.ResetDueToBlueTimerExpiration, _
                                                pblnRedFlagInBase, True, plngRedFlagHolderClientID, plngBlueFlagHolderClientID, _
                                                plngRedFlagResetTime, 0, plngEventID, _
                                                plngGameID, plngEventTime, plngLineNo, pstrEventType, _
                                                plngClientID1, plngClientID2, penuClientTeam1, penuClientTeam2, _
                                                pstrItemName, pstrWeaponName)
            Case Else
                Throw New Exception("Invalid team type for timer reset branching: " & penuTeam)
        End Select
    End Function

    ''' <summary>
    ''' Creates a new potential status, given a current status and the 
    ''' status variable states of the resulting status.
    ''' Attaches the result status as a child of the current status via a new
    ''' transistion.
    ''' </summary>
    ''' <param name="pobjCurrentStatus">The current status, to be used as the parent of the new status we'll create.</param>
    ''' <param name="pblnRedFlagInBase">if set to <c>true</c> [red flag in base].</param>
    ''' <param name="pblnBlueFlagInBase">if set to <c>true</c> [blue flag in base].</param>
    ''' <param name="plngRedFlagHolderClientID">The red flag holder client ID.</param>
    ''' <param name="plngBlueFlagHolderClientID">The blue flag holder client ID.</param>
    ''' <param name="plngRedFlagResetTime">The red flag reset time.</param>
    ''' <param name="plngBlueFlagResetTime">The blue flag reset time.</param>
    ''' <param name="plngEventID">The event ID.</param>
    ''' <param name="plngGameID">The game ID.</param>
    ''' <param name="plngEventTime">The event time.</param>
    ''' <param name="plngLineNo">The line no.</param>
    ''' <param name="pstrEventType">Type of the event.</param>
    ''' <param name="plngClientID1">The client I d1.</param>
    ''' <param name="plngClientID2">The client I d2.</param>
    ''' <param name="penuClientTeam1">The client team1.</param>
    ''' <param name="penuClientTeam2">The client team2.</param>
    ''' <param name="pstrItemName">Name of the item.</param>
    ''' <param name="pstrWeaponName">Name of the weapon.</param>
    ''' <returns>New result status snapshot object created.</returns>
    Private Function CreateNewPotentialStatus(ByRef pobjCurrentStatus As clsStatusSnapshot, ByVal penuSignificance As enuSignificanceType, _
                                        ByVal pblnRedFlagInBase As Boolean, ByVal pblnBlueFlagInBase As Boolean, _
                                        ByVal plngRedFlagHolderClientID As Long, ByVal plngBlueFlagHolderClientID As Long, _
                                        ByVal plngRedFlagResetTime As Long, ByVal plngBlueFlagResetTime As Long, _
                                        ByVal plngEventID As Long, ByVal plngGameID As Long, _
                                        ByVal plngEventTime As Long, ByVal plngLineNo As Long, _
                                        ByVal pstrEventType As String, ByVal plngClientID1 As Long, ByVal plngClientID2 As Long, _
                                        ByVal penuClientTeam1 As enuTeamType, ByVal penuClientTeam2 As enuTeamType, _
                                        ByVal pstrItemName As String, ByVal pstrWeaponName As String) As clsStatusSnapshot
        Dim objTransition As clsStatusTransition
        Dim objResultStatus As clsStatusSnapshot

        If pblnBlueFlagInBase And plngBlueFlagHolderClientID <> 0 Then Throw New Exception("Blue flag can't be held AND in base!")
        If pblnRedFlagInBase And plngRedFlagHolderClientID <> 0 Then Throw New Exception("Red flag can't be held AND in base!")

        'Create the new status result
        objResultStatus = New clsStatusSnapshot(pblnRedFlagInBase, pblnBlueFlagInBase, _
                                                plngRedFlagHolderClientID, plngBlueFlagHolderClientID, _
                                                plngRedFlagResetTime, plngBlueFlagResetTime)

        'Create the new status transition
        objTransition = New clsStatusTransition(pobjCurrentStatus, objResultStatus, penuSignificance, _
                                                plngEventID, plngGameID, plngEventTime, plngLineNo, pstrEventType, _
                                                plngClientID1, plngClientID2, penuClientTeam1, penuClientTeam2, _
                                                pstrItemName, pstrWeaponName)

        'Assign child transistion to current status
        pobjCurrentStatus.AddChild(objTransition)
        'Assign the parent transition to the result status
        objResultStatus.AddParent(objTransition)

        'Print("Adding node: " & objResultStatus.NodeID & " as child of: " & pobjCurrentStatus.NodeID)

        Return objResultStatus
    End Function

    ''' <summary>
    ''' Copies the current leaves to a new list, so that calculations
    ''' can be run on each of them.
    ''' </summary>
    ''' <returns>A new list filled with references to the leaves at call time.</returns>
    Private Function CopyWorkingSet() As List(Of clsStatusSnapshot)
        Dim lstResult As New List(Of clsStatusSnapshot)

        For Each objLeaf As clsStatusSnapshot In mlstWorkingSet
            lstResult.Add(objLeaf)
        Next

        Return lstResult
    End Function

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

        Using reader As SqlDataReader = sqlcmdGet.ExecuteReader
            dtGameEvents.Load(reader)
        End Using

        Return dtGameEvents
    End Function

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

    Private Function GetMinGameID() As Long
        Dim strSQL As String
        Dim sqlcmdGet As SqlCommand

        strSQL = "SELECT Min(g.GameID) FROM CalculatedData.Game g"

        sqlcmdGet = New SqlCommand(strSQL, mcxnStatsDB)

        Return CLng(sqlcmdGet.ExecuteScalar())
    End Function

    Private Function GetMaxGameID() As Long
        Dim strSQL As String
        Dim sqlcmdGet As SqlCommand

        strSQL = "SELECT Max(g.GameID) FROM CalculatedData.Game g"

        sqlcmdGet = New SqlCommand(strSQL, mcxnStatsDB)

        Return CLng(sqlcmdGet.ExecuteScalar())
    End Function

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

    Private Sub MarkFlagCalculationsComplete(ByVal plngGameID As Long, ByVal pblnSuccess As Boolean)
        Dim strSQL As String
        Dim sqlcmdMark As SqlCommand

        If pblnSuccess Then
            strSQL = "UPDATE CalculatedData.Game " & _
                    "SET IsFlagCalculationsComplete = 1 " & _
                    "WHERE GameID = @GameID "
        Else
            strSQL = "UPDATE CalculatedData.Game " & _
                    "SET IsFlagCalculationsFailed = 1 " & _
                    "WHERE GameID = @GameID "
        End If

        sqlcmdMark = New SqlCommand(strSQL, mcxnStatsDB)
        sqlcmdMark.Parameters.AddWithValue("GameID", plngGameID)

        sqlcmdMark.ExecuteNonQuery()
    End Sub

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

    Private Function GetRedScore(ByVal plngGameID As Long) As Integer
        Dim strSQL As String
        Dim sqlcmdGet As SqlCommand

        strSQL = "SELECT IsNull(g.RedScore, 0) FROM CalculatedData.Game g WHERE g.GameID = @GameID "

        sqlcmdGet = New SqlCommand(strSQL, mcxnStatsDB)
        sqlcmdGet.Parameters.AddWithValue("GameID", plngGameID)

        Return CInt(sqlcmdGet.ExecuteScalar())
    End Function

    Private Function GetBlueScore(ByVal plngGameID As Long) As Integer
        Dim strSQL As String
        Dim sqlcmdGet As SqlCommand

        strSQL = "SELECT IsNull(g.BlueScore, 0) FROM CalculatedData.Game g WHERE g.GameID = @GameID "

        sqlcmdGet = New SqlCommand(strSQL, mcxnStatsDB)
        sqlcmdGet.Parameters.AddWithValue("GameID", plngGameID)

        Return CInt(sqlcmdGet.ExecuteScalar())
    End Function

    Private Function GetClientLogID(ByVal plngClientID As Long) As Integer
        Dim strSQL As String
        Dim sqlcmdGet As SqlCommand

        strSQL = "SELECT c.ClientLogID FROM CalculatedData.Client c WHERE c.ClientID = @ClientID "

        sqlcmdGet = New SqlCommand(strSQL, mcxnStatsDB)
        sqlcmdGet.Parameters.AddWithValue("ClientID", plngClientID)

        Return CInt(sqlcmdGet.ExecuteScalar())
    End Function

    ''' <summary>
    ''' Verifies the game tree output path exists, creates it if it doesn't.
    ''' </summary>
    Private Function VerifyGameTreeOutputPath() As String
        Dim strBaseOutputPath As String = ConfigurationManager.AppSettings("BaseOutputFilesPath")
        Dim strGamesTreeOutputFilesRelPath = ConfigurationManager.AppSettings("GamesTreeOutputFilesRelPath")
        Dim strGameTreeOutputFilesFullPath As String = My.Computer.FileSystem.CombinePath(strBaseOutputPath, strGamesTreeOutputFilesRelPath)

        If Not My.Computer.FileSystem.DirectoryExists(strBaseOutputPath) Then
            My.Computer.FileSystem.CreateDirectory(strBaseOutputPath)
        End If
        If Not My.Computer.FileSystem.DirectoryExists(strGameTreeOutputFilesFullPath) Then
            My.Computer.FileSystem.CreateDirectory(strGameTreeOutputFilesFullPath)
        End If

        Return strGameTreeOutputFilesFullPath
    End Function

    ''' <summary>
    ''' Creates a file for writing named game-GAMEID.txt in the game tree output dir.
    ''' Overwrites existing files.
    ''' </summary>
    ''' <param name="plngGameID">The game ID.</param>
    ''' <returns>StreamWriter to the game file.</returns>
    Private Function GetGameTreeFileWriter(ByVal plngGameID As Long) As StreamWriter
        Dim strGameTreeFilePath As String = My.Computer.FileSystem.CombinePath(mstrGameTreeOutputPath, "game-" & plngGameID & ".txt")
        Dim writer As New StreamWriter(New FileStream(strGameTreeFilePath, FileMode.Create))

        Return writer
    End Function
#End Region
End Class
