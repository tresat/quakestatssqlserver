﻿Option Explicit On
Option Strict On

'TODO: Need to link clients to next/prev next.  Then can add treatment for client end
'caused by clientuserinfochanged where a client is just a continuation.  or maybe, can
'just add new event and xfer flag on info changed, drop on disconnect.  explore more

Imports System.Data.SqlClient
Imports System.IO
Imports System.Configuration
Imports QuakeStats.Utilities.clsHighPerformanceTimer
Imports System.Math

Namespace LogParsing
    Public Class clsFlagCalculator
#Region "Inner Types"
        Public Enum enuTeamType
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
        Public Class clsStatusSnapshot
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

            Private mblnPrinted As Boolean

            Private mlngEventID As Long

            Private Shared mlngCurrentNodeID As Long = 1
#End Region

#Region "Properties"
            Public Property EventID() As Long
                Get
                    Return mlngEventID
                End Get
                Set(ByVal value As Long)
                    mlngEventID = value
                End Set
            End Property

            Public Property Printed() As Boolean
                Get
                    Return mblnPrinted
                End Get
                Set(ByVal value As Boolean)
                    mblnPrinted = value
                End Set
            End Property

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
                            ByVal plngRedFlagResetTime As Long, ByVal plngBlueFlagResetTime As Long, _
                            ByVal plngEventID As Long)
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

                mlngEventID = plngEventID

                mblnPrinted = False
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
        Public Class clsStatusTransition
#Region "Member Variables"
            Private mobjPriors As List(Of clsStatusSnapshot)
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
            Public ReadOnly Property Priors() As List(Of clsStatusSnapshot)
                Get
                    Return mobjPriors
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
                mobjPriors = New List(Of clsStatusSnapshot)
                mobjPriors.Add(pobjPrior)

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

#Region "Public Functionality"
            ''' <summary>
            ''' Adds the prior node to the list of prior nodes.
            ''' </summary>
            ''' <param name="pobjPrior">The prior status object..</param>
            Public Sub AddPrior(ByRef pobjPrior As clsStatusSnapshot)
                mobjPriors.Add(pobjPrior)
            End Sub
#End Region
        End Class
#End Region


#Region "Flag Statistics"
        Private Class clsFlagStatistics
#Region "Member Variables"
            Private mlstRedCaptures As List(Of Long)    'list of ClientToItem record IDs which are captures
            Private mlstRedRecoveries As List(Of Long)
            Private mlstRedSteals As List(Of Long)
            Private mlstRedPickups As List(Of Long)
            Private mlstBlueCaptures As List(Of Long)    'list of ClientToItem record IDs which are captures
            Private mlstBlueRecoveries As List(Of Long)
            Private mlstBlueSteals As List(Of Long)
            Private mlstBluePickups As List(Of Long)

            Private mlstRedCarrierKills As List(Of Long) 'list of Kill record IDs
            Private mlstBlueCarrierKills As List(Of Long) 'list of Kill record IDs
#End Region

#Region "Constructors"
            Public Sub New()
                mlstRedCaptures = New List(Of Long)
                mlstRedRecoveries = New List(Of Long)
                mlstRedSteals = New List(Of Long)
                mlstRedPickups = New List(Of Long)
                mlstBlueCaptures = New List(Of Long)
                mlstBlueRecoveries = New List(Of Long)
                mlstBlueSteals = New List(Of Long)
                mlstBluePickups = New List(Of Long)
                mlstRedCarrierKills = New List(Of Long)
                mlstBlueCarrierKills = New List(Of Long)
            End Sub
#End Region

#Region "Public Functionality"
            Public Sub AddCapture(ByVal plngClientToItemID As Long, ByVal penuTeam As enuTeamType)
                If penuTeam = enuTeamType.Red Then
                    mlstRedCaptures.Add(plngClientToItemID)
                Else
                    mlstBlueCaptures.Add(plngClientToItemID)
                End If
            End Sub

            Public Sub AddRecovery(ByVal plngClientToItemID As Long, ByVal penuTeam As enuTeamType)
                If penuTeam = enuTeamType.Red Then
                    mlstRedRecoveries.Add(plngClientToItemID)
                Else
                    mlstBlueRecoveries.Add(plngClientToItemID)
                End If
            End Sub

            Public Sub AddSteal(ByVal plngClientToItemID As Long, ByVal penuTeam As enuTeamType)
                If penuTeam = enuTeamType.Red Then
                    mlstRedSteals.Add(plngClientToItemID)
                Else
                    mlstBlueSteals.Add(plngClientToItemID)
                End If
            End Sub

            Public Sub AddPickup(ByVal plngClientToItemID As Long, ByVal penuTeam As enuTeamType)
                If penuTeam = enuTeamType.Red Then
                    mlstRedPickups.Add(plngClientToItemID)
                Else
                    mlstBluePickups.Add(plngClientToItemID)
                End If
            End Sub

            Public Sub AddCarrierKill(ByVal plngKillID As Long, ByVal penuTeam As enuTeamType)
                If penuTeam = enuTeamType.Red Then
                    mlstRedCarrierKills.Add(plngKillID)
                Else
                    mlstBlueCarrierKills.Add(plngKillID)
                End If
            End Sub

            Public Function RedRecoveryCount() As Integer
                Return mlstRedRecoveries.Count
            End Function

            Public Function BlueRecoveryCount() As Integer
                Return mlstBlueRecoveries.Count
            End Function

            Public Function RedStealCount() As Integer
                Return mlstRedSteals.Count
            End Function

            Public Function BlueStealCount() As Integer
                Return mlstBlueSteals.Count
            End Function

            Public Function RedPickupCount() As Integer
                Return mlstRedPickups.Count
            End Function

            Public Function BluePickupCount() As Integer
                Return mlstBluePickups.Count
            End Function

            Public Function RedCaptureCount() As Integer
                Return mlstRedCaptures.Count
            End Function

            Public Function BlueCaptureCount() As Integer
                Return mlstBlueCaptures.Count
            End Function

            Public Function RedCarrierKillCount() As Integer
                Return mlstRedCarrierKills.Count
            End Function

            Public Function BlueCarrierKillCount() As Integer
                Return mlstBlueCarrierKills.Count
            End Function
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

        Private mintGoalRedScore As Integer
        Private mintGoalBlueScore As Integer

        Private mlngPointOfFailureID As Integer
        Private mlstSwapPointIDs As List(Of Integer)
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
        Public Event GameEventParsed(ByVal pintCurrentEvent As Integer, ByVal pintTotalEvents As Integer, ByVal pintWorkingSetSize As Integer)
        Public Event GameParsed(ByVal pblnSuccess As Boolean)
#End Region

#Region "Constructors"
        Public Sub New(ByRef pcxnDB As SqlConnection)
            StatsDB = pcxnDB

            mobjTimer = New Utilities.clsHighPerformanceTimer

            mstrGameTreeOutputPath = VerifyGameTreeOutputPath()

            mlstSwapPointIDs = New List(Of Integer)
        End Sub
#End Region

#Region "Public Functionality"
        Public Sub CalculateGame(ByVal plngGameID As Long)
            mlngPointOfFailureID = 0

            If IsCTF(plngGameID) Then
                Try
                    Print("Game: " & plngGameID & " on map: " & GetMapName(plngGameID) & " init: " & GetInitGameLineNo(plngGameID) & " for map: " & GetMapName(plngGameID) & " ")
                    mobjTimer.StartTimer()
                    DoCalculateGame(plngGameID)
                    Print("**************************************SUCCEEDED**************************************")
                    RaiseEvent GameParsed(True)

                    mobjTimer.StopTimer()
                    Print("in: " & mobjTimer.GetResultAsTimeString & " (actual " & mobjTimer.GetElapsedAsTimeString & ").")

                    MarkFlagCalculationsComplete(plngGameID, True)
                    mlstSwapPointIDs.Clear()
                Catch ex As Exception
                    Print(ex.Message)

                    mobjTimer.StopTimer()
                    Print("in: " & mobjTimer.GetResultAsTimeString & " (actual " & mobjTimer.GetElapsedAsTimeString & ").")

                    If mlngPointOfFailureID <> 0 Then
                        Print("**************************************RETRYING**************************************")

                        'Re-attempt parse, swaping P.o.F. line with subsequent line
                        mlstSwapPointIDs.Add(mlngPointOfFailureID)
                        CalculateGame(plngGameID)
                    Else
                        Print("**************************************FAILED**************************************")
                        Print("**************************************FAILED**************************************")
                        Print("**************************************FAILED**************************************")
                        RaiseEvent GameParsed(False)
                        MarkFlagCalculationsComplete(plngGameID, False)
                        mlstSwapPointIDs.Clear()
                    End If
                End Try
            Else
                Print("Game: " & plngGameID & " is NOT CTF, skipping.")
            End If
        End Sub

        Public Sub CalculateAllGames()
            Dim lngMinGameID As Long = GetMinGameID()
            Dim lngMaxGameID As Long = GetMaxGameID()

            Print("Begin calculating flag captures for " & lngMaxGameID - lngMinGameID & " maximum games, from: " & lngMinGameID & " to: " & lngMaxGameID & " ...")
            For lngCurrentGameID As Long = lngMaxGameID To lngMaxGameID - 100 Step -1
                If Not IsFlagCalculationsComplete(lngCurrentGameID) Then
                    If IsCompleteInLog(lngCurrentGameID) Then
                        CalculateGame(lngCurrentGameID)
                        Print(lngMaxGameID - lngCurrentGameID & " maximum games remaining...")
                        'Return
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

        Public Sub PrintTreeToFile(Optional ByVal plngGameID As Long = 0, Optional ByVal plngIdx As Long = 0)
            PrintGameStatusTree(GetGameTreeFileWriter(plngGameID, plngIdx))
        End Sub

        ''' <summary>
        ''' Prints the complete game status tree starting from the root.
        ''' </summary>
        ''' <param name="pobjWriter">A writer to an open stream, if writing to a file. </param>
        Public Sub PrintGameStatusTree(Optional ByVal pobjWriter As StreamWriter = Nothing)
            Dim strResult As String = String.Empty

            ResetPrinted(mobjRoot)

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

        Private Sub ResetPrinted(ByVal pobjNode As clsStatusSnapshot)
            pobjNode.Printed = False
            For Each objChild As clsStatusSnapshot In pobjNode.Children
                ResetPrinted(objChild)
            Next
        End Sub

        ''' <summary>
        ''' Builds the game tree.
        ''' </summary>
        ''' <param name="plngGameID">The PLNG game ID.</param>
        Private Sub DoCalculateGame(ByVal plngGameID As Long)
            Dim dtGameEvents As DataTable = GetGameEvents(plngGameID)
            Dim drCurrentLine As DataRow
            Dim lngCurrentLineNo As Long
            Dim intCurrentPrimaryID As Integer

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

            Dim intPrintIdx As Integer = 1
            Dim blnPrint As Boolean

            'Dim lstTallyingPaths As List(Of List(Of Long))
            Dim lstStatisticsPath As List(Of Long)

            'Create the root node (both flags in base, no timers set, no parent)
            mobjRoot = New clsStatusSnapshot(True, True, 0, 0, 0, 0, 0)

            'Create the working set, add the root to it
            mlstWorkingSet = New List(Of clsStatusSnapshot)
            mlstWorkingSet.Add(mobjRoot)

            Print("with: " & dtGameEvents.Rows.Count & " events... ")

            'Walk the game events and do the initial flag calculations
            For intIdx As Integer = 0 To dtGameEvents.Rows.Count - 1
                RaiseEvent GameEventParsed(intIdx, dtGameEvents.Rows.Count, mlstWorkingSet.Count)

                blnPrint = True
                'Print("On event: " & intIdx & " of " & dtGameEvents.Rows.Count - 1)

                'If lngCurrentLineNo = 37992 Then
                'Dim i As Int16
                'i = 48
                'End If

                'Store current line info
                drCurrentLine = dtGameEvents.Rows(intIdx)
                intCurrentPrimaryID = CInt(drCurrentLine("PrimaryID"))
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

                    If lngLineNo = 1679808 Then
                        Dim i As Int16 = 0
                    End If

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
                                                    PruneImpossibleBranch(objCurrentStatus, intCurrentPrimaryID)
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
                                                    PruneImpossibleBranch(objCurrentStatus, intCurrentPrimaryID)
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
                                                    PruneImpossibleBranch(objCurrentStatus, intCurrentPrimaryID)
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
                                                    PruneImpossibleBranch(objCurrentStatus, intCurrentPrimaryID)
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
                                        blnPrint = False
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
                                        blnPrint = False
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
                                        blnPrint = False
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
                                        blnPrint = False
                                    End If
                                Case Else
                                    'Spectator leaving
                                    'Event doesn't affect flag, so move to next node in current working set
                                    intStateIdx += 1
                                    blnPrint = False
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
                                        blnPrint = False
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
                                        blnPrint = False
                                    End If
                                Case Else
                                    'Spectator number changing
                                    'Event doesn't affect flag, so move to next node in current working set
                                    intStateIdx += 1
                                    blnPrint = False
                            End Select
                        Case Else
                            Throw New Exception("Unknown event type: " & strEventType)
                    End Select
                Loop

                MergeDuplicateLeaves()

                If blnPrint Then
                    'PrintTreeToFile(lngGameID, intPrintIdx)
                    intPrintIdx += 1
                End If

                blnPrint = False
            Next

            lstStatisticsPath = FindFirstTallyingPath(plngGameID)

            ''There should be at least one route through the game graph where the calculated
            ''score matches the reported score in the games.log file
            'lstTallyingPaths = FindAllTallyingPaths(plngGameID)
            'If lstTallyingPaths.Count = 0 Then
            '    Throw New Exception("Did not find path which tallied to final score in current game graph!")
            'End If

            ''Determine the path to be used for statistics
            'lstStatisticsPath = DetermineStatisticsPath(lstTallyingPaths)
            'If lstStatisticsPath Is Nothing Then
            '    Throw New Exception("Error determining the statistics path!")
            'End If

            'Finally, update stats in the DB
            UpdateDBFlagStatisticsForPath(lstStatisticsPath)
        End Sub
#End Region

#Region "Private Helper Functions"
        ''' <summary>
        ''' Determines the tallying path, which of all the paths through the
        ''' game graph, will be used as the statistics path, for updating the DB.
        ''' 
        ''' Final game graph should exist before calling.
        ''' </summary>
        ''' <param name="plstTallyingPaths">List of all the tallying paths, which are lists of node IDs, ordered from root to end point.</param>
        ''' <returns>The statistics path, a list of longs, which are node IDs, ordered from root to end point.</returns>
        Private Function DetermineStatisticsPath(ByRef plstTallyingPaths As List(Of List(Of Long))) As List(Of Long)
            Dim dctPathToStats As New Dictionary(Of List(Of Long), clsFlagStatistics)
            Dim objCurrStats As clsFlagStatistics
            Dim lstStatisticsPath As List(Of Long) = Nothing

            Dim intTotalRedRecoveries As Integer = 0
            Dim intTotalBlueRecoveries As Integer = 0
            Dim intTotalRedSteals As Integer = 0
            Dim intTotalBlueSteals As Integer = 0
            Dim intTotalRedPickups As Integer = 0
            Dim intTotalBluePickups As Integer = 0
            Dim intTotalRedCarrierKills As Integer = 0
            Dim intTotalBlueCarrierKills As Integer = 0

            Dim dblAvgRedRecoveries As Double = 0
            Dim dblAvgBlueRecoveries As Double = 0
            Dim dblAvgRedSteals As Double = 0
            Dim dblAvgBlueSteals As Double = 0
            Dim dblAvgRedPickups As Double = 0
            Dim dblAvgBluePickups As Double = 0
            Dim dblAvgRedCarrierKills As Double = 0
            Dim dblAvgBlueCarrierKills As Double = 0

            Dim dblTotalDistanceFromAvgs As Double = 0
            Dim dblMinDistanceFromAvgs As Double = Nothing

            'Get statistics for each tallying path
            For Each lstCurrTallyingPath As List(Of Long) In plstTallyingPaths
                dctPathToStats.Add(lstCurrTallyingPath, GatherStatsForPath(lstCurrTallyingPath))
            Next

            'Sum all flag stats in each path
            For Each lstCurrTallyingPath As List(Of Long) In dctPathToStats.Keys
                objCurrStats = dctPathToStats(lstCurrTallyingPath)

                intTotalRedRecoveries += objCurrStats.RedRecoveryCount
                intTotalBlueRecoveries += objCurrStats.BlueRecoveryCount
                intTotalRedSteals += objCurrStats.RedStealCount
                intTotalBlueSteals += objCurrStats.BlueStealCount
                intTotalRedPickups += objCurrStats.RedPickupCount
                intTotalBluePickups += objCurrStats.BluePickupCount
                intTotalRedCarrierKills += objCurrStats.RedCarrierKillCount
                intTotalBlueCarrierKills += objCurrStats.BlueCarrierKillCount
            Next

            'Determine averages
            dblAvgRedRecoveries = intTotalRedRecoveries / dctPathToStats.Count
            dblAvgBlueRecoveries = intTotalBlueRecoveries / dctPathToStats.Count
            dblAvgRedSteals = intTotalRedSteals / dctPathToStats.Count
            dblAvgBlueSteals = intTotalBlueSteals / dctPathToStats.Count
            dblAvgRedPickups = intTotalRedPickups / dctPathToStats.Count
            dblAvgBluePickups = intTotalBluePickups / dctPathToStats.Count
            dblAvgRedCarrierKills = intTotalRedCarrierKills / dctPathToStats.Count
            dblAvgBlueCarrierKills = intTotalBlueCarrierKills / dctPathToStats.Count

            'Compare each path to average, find first path where the average of differences
            'for each stat is the least, we'll use that path for updating
            For Each lstCurrTallyingPath As List(Of Long) In dctPathToStats.Keys
                objCurrStats = dctPathToStats(lstCurrTallyingPath)

                dblTotalDistanceFromAvgs = 0

                dblTotalDistanceFromAvgs += Math.Abs(objCurrStats.BlueCarrierKillCount - dblAvgBlueCarrierKills)
                dblTotalDistanceFromAvgs += Math.Abs(objCurrStats.RedCarrierKillCount - dblAvgRedCarrierKills)
                dblTotalDistanceFromAvgs += Math.Abs(objCurrStats.BluePickupCount - dblAvgBluePickups)
                dblTotalDistanceFromAvgs += Math.Abs(objCurrStats.RedPickupCount - dblAvgRedPickups)
                dblTotalDistanceFromAvgs += Math.Abs(objCurrStats.BlueRecoveryCount - dblAvgBlueRecoveries)
                dblTotalDistanceFromAvgs += Math.Abs(objCurrStats.RedRecoveryCount - dblAvgRedRecoveries)
                dblTotalDistanceFromAvgs += Math.Abs(objCurrStats.BlueStealCount - dblAvgBlueSteals)
                dblTotalDistanceFromAvgs += Math.Abs(objCurrStats.RedStealCount - dblAvgRedSteals)

                If dblMinDistanceFromAvgs = Nothing Then
                    lstStatisticsPath = lstCurrTallyingPath
                    dblMinDistanceFromAvgs = dblTotalDistanceFromAvgs
                Else
                    If dblTotalDistanceFromAvgs < dblMinDistanceFromAvgs Then
                        lstStatisticsPath = lstCurrTallyingPath
                        dblMinDistanceFromAvgs = dblTotalDistanceFromAvgs
                    End If
                End If
            Next

            Return lstStatisticsPath
        End Function

        ''' <summary>
        ''' Walks the path to populate a flag statistics object.
        ''' 
        ''' Final game graph should exist before calling.
        ''' </summary>
        ''' <param name="plstCurrTallyingPath">The tallying path through the game graph to walk.</param>
        ''' <returns>The flag statistics object.</returns>
        Private Function GatherStatsForPath(ByRef plstCurrTallyingPath As List(Of Long)) As clsFlagStatistics
            Dim objFlagStatistics As New clsFlagStatistics
            Dim objCurrentNode As clsStatusSnapshot

            'Make sure to get to second node (not on root), before start checking
            'significance of parent, as root lacks a parent transition and will fail
            objCurrentNode = mobjRoot
            For Each lngNodeID As Long In plstCurrTallyingPath
                For Each objChild As clsStatusSnapshot In objCurrentNode.Children
                    If objChild.NodeID = lngNodeID Then
                        objCurrentNode = objChild
                        Exit For
                    End If
                Next

                If objCurrentNode.NodeID <> mobjRoot.NodeID Then
                    'Have reached next node in path, check transition which led us here
                    Select Case objCurrentNode.Parent.Significance
                        Case enuSignificanceType.Capture
                            objFlagStatistics.AddCapture(objCurrentNode.Parent.EventID, objCurrentNode.Parent.Client1Team)
                        Case enuSignificanceType.Recovery
                            objFlagStatistics.AddRecovery(objCurrentNode.Parent.EventID, objCurrentNode.Parent.Client1Team)
                        Case enuSignificanceType.Steal
                            objFlagStatistics.AddSteal(objCurrentNode.Parent.EventID, objCurrentNode.Parent.Client1Team)
                        Case enuSignificanceType.Pickup
                            objFlagStatistics.AddPickup(objCurrentNode.Parent.EventID, objCurrentNode.Parent.Client1Team)
                        Case enuSignificanceType.CarrierKillWithDrop, enuSignificanceType.CarrierKillWithReset
                            objFlagStatistics.AddCarrierKill(objCurrentNode.Parent.EventID, objCurrentNode.Parent.Client1Team) 'killer team
                    End Select
                End If
            Next

            Return objFlagStatistics
        End Function

        ''' <summary>
        ''' Updates the flag statistics in DB: captures/steals/pickups/etc.
        ''' </summary>
        ''' <param name="plstStatisticsPath">The statistics path,  a list of longs, which are node IDs, ordered from root to end point.</param>
        Private Sub UpdateDBFlagStatisticsForPath(ByRef plstStatisticsPath As List(Of Long))
            Dim objCurrentNode As clsStatusSnapshot = mobjRoot
            Dim intCurrentPathIdx As Integer = 0
            Dim lngCurrentNodeID As Long = plstStatisticsPath.First
            Dim blnFoundNextChild As Boolean = False
            Dim trnUpdate As SqlTransaction = Nothing

            Try
                trnUpdate = mcxnStatsDB.BeginTransaction()

                'Walk the stats path, from root to end point
                Do Until objCurrentNode Is Nothing
                    'Call the function which will update the event in the DB, if nessecary
                    UpdateDBFlagStatisticsForNode(objCurrentNode, trnUpdate)

                    'Advance to next node, if possible
                    intCurrentPathIdx += 1
                    If intCurrentPathIdx >= plstStatisticsPath.Count Then
                        objCurrentNode = Nothing
                    Else
                        lngCurrentNodeID = plstStatisticsPath(intCurrentPathIdx)

                        blnFoundNextChild = False
                        For Each objChildNode As clsStatusSnapshot In objCurrentNode.Children
                            If objChildNode.NodeID = lngCurrentNodeID Then
                                objCurrentNode = objChildNode
                                blnFoundNextChild = True
                                Exit For
                            End If
                        Next

                        Debug.Assert(blnFoundNextChild)
                    End If
                Loop

                trnUpdate.Commit()
            Catch ex As Exception
                If trnUpdate IsNot Nothing Then
                    trnUpdate.Rollback()
                End If

                Throw
            End Try
        End Sub

        ''' <summary>
        ''' Updates the DB flag statistics for a single node in the stats path,
        ''' if the transition into the node merits a db update.
        ''' </summary>
        ''' <param name="pobjCurrentNode">The node to check to update.</param>
        ''' <param name="ptrnUpdate">The path flag stats update transaction.</param>
        Private Sub UpdateDBFlagStatisticsForNode(ByRef pobjCurrentNode As clsStatusSnapshot, ByRef ptrnUpdate As SqlTransaction)
            Dim strSQL As String
            Dim sqlcmdUpdate As SqlCommand

            If pobjCurrentNode.Parent IsNot Nothing Then
                Select Case pobjCurrentNode.Parent.Significance
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
                    sqlcmdUpdate.Parameters.AddWithValue("ID", pobjCurrentNode.Parent.EventID)
                    sqlcmdUpdate.Transaction = ptrnUpdate

                    sqlcmdUpdate.ExecuteNonQuery()
                End If
            End If
        End Sub

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

        'Private Sub PrintGameStatusRecUp(ByVal pobjNode As clsStatusSnapshot, ByVal pintLevel As Integer, _
        '                               Optional ByVal pobjWriter As StreamWriter = Nothing)
        '    Dim strResult As String = PrintCurrentNode(pobjNode, pintLevel)
        '    Dim lstChildStatusNodes As List(Of clsStatusSnapshot) = pobjNode.Children

        '    If pobjWriter IsNot Nothing Then
        '        pobjWriter.Write(strResult)
        '    Else
        '        Print(strResult)
        '    End If

        '    If pobjNode.Parent IsNot Nothing Then
        '        PrintGameStatusRecUp(pobjNode.Parent.Prior, pintLevel + 1)
        '    End If
        'End Sub

        Private Function PrintCurrentNode(ByVal pobjNode As clsStatusSnapshot, ByVal pintLevel As Integer) As String
            Dim strResult As String = String.Empty

            If Not pobjNode.Printed Then
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

                    pobjNode.Printed = True
                Else
                    strResult &= "START OF GAME "
                End If
                strResult &= " ->"
                strResult &= vbCrLf

                strResult &= TabOut(pintLevel) & "NODE: " & pobjNode.NodeID & " Rbase: " & CStr(IIf(pobjNode.RedFlagInBase, "Y", "N")) & " Rhold: " & pobjNode.RedFlagHolderClientID & " (" & GetClientLogID(pobjNode.RedFlagHolderClientID) & ") " & _
                        "Bbase: " & CStr(IIf(pobjNode.BlueFlagInBase, "Y", "N")) & " Bhold: " & pobjNode.BlueFlagHolderClientID & " (" & GetClientLogID(pobjNode.BlueFlagHolderClientID) & ") Rtime: " & pobjNode.RedFlagResetTime & " Btime: " & pobjNode.BlueFlagResetTime & vbCrLf

                strResult &= TabOut(pintLevel) & "has " & pobjNode.Children.Count & " children: " & vbCrLf
            Else
                strResult &= TabOut(pintLevel) & "NODE: " & pobjNode.NodeID & " (already printed)" & vbCrLf
                strResult &= TabOut(pintLevel) & "has " & pobjNode.Children.Count & " children: " & vbCrLf
            End If

            Return strResult
        End Function

        ''' <summary>
        ''' Finds the first tallying path.  Always tracking backwards to the first parent.
        ''' This should be deterministic, as the manner in which the game graph is built is deterministic.
        ''' </summary>
        ''' <param name="plngGameID">The game ID.</param>
        ''' <returns>A list of longs, representing a root->leaf path through the graph for which the score tallys.</returns>
        Private Function FindFirstTallyingPath(ByVal plngGameID As Long) As List(Of Long)
            Dim lstResult As New List(Of Long)

            'Track descisions: choices we made last time we moved through the graph.
            'Since the first time we go through might not tally, we'll have to keep
            'changing decisions in a structured way until we reach a path that will work.
            'We'll do this by incrementing the index of the parent furthest from the
            'leaf, until we try all possible combinations.
            Dim lstDecisions As New List(Of KeyValuePair(Of Long, Integer))
            'Current decision point, what node should we increment the parent index on, 
            'if current path fails.  Should begin as farthest node from the leaf
            'with multiple parents, then proceed closer like a gas pump gauge.
            Dim lngDecisionPoint As Long
            'Idx of parent chosen last time at decision point
            Dim intDecision As Integer

            Dim objCurrentNode As clsStatusSnapshot
            Dim blnDoneCheckedCurrentWSNode As Boolean = False
            Dim blnPathFound As Boolean = False

            mintGoalRedScore = GetRedScore(plngGameID)
            mintGoalBlueScore = GetBlueScore(plngGameID)

            'We'll have to try this for each node in the current working set,
            'until we find a path that works
            For Each objWSNode As clsStatusSnapshot In mlstWorkingSet
                'Add leaf node to the beginning of the list and reset decision vars
                lstResult.Insert(0, objWSNode.NodeID)
                lstDecisions.Clear()
                lngDecisionPoint = -1

                'Build the initial path by walking to the root, taking first parent
                objCurrentNode = objWSNode
                Do While objCurrentNode.Parent IsNot Nothing
                    lstResult.Insert(0, objWSNode.NodeID)
                    objCurrentNode = objCurrentNode.Parent.Priors(0)
                Loop

                Do While True
                    'Now enter the check/modify decision path loop, we'll keep doing this
                    'until we either find a winner, or the decision logic tells us we're 
                    'done checking this WS node
                    If IsTallyingPath(lstResult) Then
                        'Skip out and return as soon as we find a winner
                        Exit For
                    Else
                        'We'll have to modify the path based on the current decision loop
                        blnDoneCheckedCurrentWSNode = MoveToNextPathToCheck(lstResult, lngDecisionPoint, lstDecisions)
                    End If
                Loop
            Next

            If blnPathFound Then
                Return lstResult
            Else
                Throw New Exception("No path which tallys found in graph!")
            End If
        End Function

        ''' <summary>
        ''' Moves to next path to check, using the reverse "gas-pump-gauge" logic, closest to
        ''' the root first.  All vars are references, and will change.
        ''' </summary>
        ''' <param name="plstPath">The path through the graph.</param>
        ''' <param name="plngDecisionPoint">The decision point.</param>
        ''' <param name="lstDecisions">All current decisions, as a list of NodeID->parent index pairs.</param>
        ''' <returns><c>true</c> if we've exhausted all combinations, <c>false</c> otherwise.</returns>
        Private Function MoveToNextPathToCheck(ByRef plstPath As List(Of Long), ByRef plngDecisionPoint As Long, ByRef lstDecisions As List(Of KeyValuePair(Of Long, Integer))) As Boolean
            Dim objDecisionPoint As clsStatusSnapshot = mobjRoot
            Dim intIdx As Integer = 0
            Dim lngTargetChildID As Long

            'Walk the path to the decision point
            Do Until objDecisionPoint.NodeID = plngDecisionPoint
                'Advance to next child
                lngTargetChildID = plstPath(intIdx)
                For Each objChild As clsStatusSnapshot In objDecisionPoint.Children
                    If objChild.NodeID = lngTargetChildID Then
                        Exit For
                    End If
                Next

                'We're at the decision point, check if there is a next parent




                'Increment current index in path
                intIdx += 1
            Loop

            'Check if there is a next parent at the decision point
            'If plstPath Then

        End Function

        ''' <summary>
        ''' Walks the path, checks if the totals match.
        ''' Module level score totals should be set before calling.
        ''' </summary>
        ''' <param name="plstResult">The path to walk.</param>
        ''' <returns>
        ''' <c>true</c> if [is tallying path]; otherwise, <c>false</c>.
        ''' </returns>
        Private Function IsTallyingPath(ByRef plstResult As List(Of Long)) As Boolean
            Dim objCurrentNode As clsStatusSnapshot = mobjRoot
            Dim intIdx As Integer = 0
            Dim intLimit As Integer = plstResult.Count - 1
            Dim lngTargetChildID As Long

            Dim intRedScoreOnPath As Integer = 0
            Dim intBlueScoreOnPath As Integer = 0

            'Root cannot be capture, so no need to check it, advance to second node first,
            'if possible, and check that transition for a capture (which should still be technically impossible, actually)
            For intIdx = 1 To intLimit
                'Advance to the appropriate child node, following the path
                lngTargetChildID = plstResult(intIdx)
                For Each objChildNode As clsStatusSnapshot In objCurrentNode.Children
                    If objChildNode.NodeID = lngTargetChildID Then
                        objCurrentNode = objChildNode
                        Exit For
                    End If
                Next

                'Check parent transition, add to score totals if nessecary
                If objCurrentNode.Parent.Significance = enuSignificanceType.Capture Then
                    If objCurrentNode.Parent.Client1Team = enuTeamType.Red Then
                        intRedScoreOnPath += 1
                    Else
                        intBlueScoreOnPath += 1
                    End If
                End If
            Next

            'Done tallying path, if totals match expected totals in module level vars, we're good
            Return ((intRedScoreOnPath = mintGoalRedScore) AndAlso (intBlueScoreOnPath = mintGoalBlueScore))
        End Function

        ''' <summary>
        ''' Finds all tallying paths from root to end in graph.
        ''' </summary>
        ''' <param name="plngGameID">The game ID.</param>
        ''' <returns>A list of lists of long, representing path through the graph from root to leaf for which score tallys.</returns>
        Private Function FindAllTallyingPaths(ByVal plngGameID As Long) As List(Of List(Of Long))
            Dim lstFinalResult As New List(Of List(Of Long))

            mintGoalRedScore = GetRedScore(plngGameID)
            mintGoalBlueScore = GetBlueScore(plngGameID)

            'Print(vbCrLf & "Searching for goal score of Red: " & mintGoalRedScore & " Blue: " & mintGoalBlueScore & " ...")

            For Each objWSNode As clsStatusSnapshot In mlstWorkingSet
                For Each lstPath As List(Of Long) In FindAllTallyingPathsRec(objWSNode, 0, 0, New List(Of Long))
                    lstFinalResult.Add(lstPath)
                Next
            Next

            Return lstFinalResult
        End Function

        Private Function FindAllTallyingPathsRec(ByRef pobjCurrentNode As clsStatusSnapshot, _
                ByVal pintCurrentRedScore As Integer, ByVal pintCurrentBlueScore As Integer, _
                ByRef plstCurrentPath As List(Of Long)) As List(Of List(Of Long))
            Dim lstResult As New List(Of List(Of Long))
            Dim intRedScoreIncludingCurrentNode As Integer = pintCurrentRedScore
            Dim intBlueScoreIncludingCurrentNode As Integer = pintCurrentBlueScore
            Dim lstPathIncludingCurrentNode As List(Of Long) = New List(Of Long)(plstCurrentPath) 'use copy constructor here, to avoid list changing during recursion

            lstPathIncludingCurrentNode.Insert(0, pobjCurrentNode.NodeID)

            If pobjCurrentNode.Parent Is Nothing Then
                'Print("Hit root, this path results in Red: " & intRedScoreIncludingCurrentNode & " Blue: " & intBlueScoreIncludingCurrentNode)

                'We've hit the root, make a determination and return
                If pintCurrentBlueScore = mintGoalBlueScore AndAlso _
                        pintCurrentRedScore = mintGoalRedScore Then
                    lstResult.Add(lstPathIncludingCurrentNode)
                End If

                Return lstResult
            Else
                'Add parent transition to score totals
                If pobjCurrentNode.Parent.Significance = enuSignificanceType.Capture Then
                    If pobjCurrentNode.Parent.Client1Team = enuTeamType.Red Then
                        intRedScoreIncludingCurrentNode += 1
                    Else
                        intBlueScoreIncludingCurrentNode += 1
                    End If
                End If

                'Loop each parent node
                For Each objParent As clsStatusSnapshot In pobjCurrentNode.Parent.Priors
                    For Each lstPath As List(Of Long) In FindAllTallyingPathsRec(objParent, intRedScoreIncludingCurrentNode, intBlueScoreIncludingCurrentNode, lstPathIncludingCurrentNode)
                        lstResult.Add(lstPath)
                    Next
                Next
            End If

            Return lstResult
        End Function

        'Private Sub PruneBranchesNotTallying(ByVal plngGameID As Long)
        '    Dim objWSNode As clsStatusSnapshot
        '    Dim objCurrentNode As clsStatusSnapshot
        '    Dim intRedScoreDB As Integer = GetRedScore(plngGameID)
        '    Dim intBlueScoreDB As Integer = GetBlueScore(plngGameID)
        '    Dim intIdx As Integer = 0

        '    'Consider each leaf node, can't use for each, because nodes
        '    'might be evaporating from the current working set as we
        '    'delete them during the recursive pruning process
        '    Do While intIdx < mlstWorkingSet.Count - 1
        '        PruneBranchesNotTallyingRec(mlstWorkingSet(intIdx), 0, 0)

        '        intIdx += 1
        '    Loop



        '    'Consider each leaf node
        '    Dim intIdx As Integer = 0
        '    Do While intIdx < mlstWorkingSet.Count - 1
        '        objWSNode = mlstWorkingSet(intIdx)
        '        objCurrentNode = objWSNode

        '        'Ascend tree, counting captures
        '        Do Until objCurrentNode.Parent Is Nothing
        '            If objCurrentNode.Parent.Significance = enuSignificanceType.Capture Then
        '                Select Case objCurrentNode.Parent.Client1Team
        '                    Case enuTeamType.Red
        '                        intRedScoreCalc += 1
        '                    Case enuTeamType.Blue
        '                        intBlueScoreCalc += 1
        '                    Case Else
        '                        Throw New Exception("Red or blue must capture flag!")
        '                End Select
        '            End If

        '            objCurrentNode = objCurrentNode.Parent.Prior
        '        Loop

        '        'Check if captures tally
        '        If intRedScoreCalc <> intRedScoreDB OrElse intBlueScoreCalc <> intBlueScoreDB Then
        '            PruneImpossibleBranch(objWSNode)
        '        Else
        '            intIdx += 1
        '        End If
        '    Loop
        'End Sub

        'Private Sub PruneBranchesNotTallyingRec(ByRef pobjBeginNode As clsStatusSnapshot, _
        '        ByVal pintCurrentRedScoreCount As Integer, ByVal pintCurrentBlueScoreCount As Integer)


        'End Sub

        ''' <summary>
        ''' Prunes the impossible branch from the game graph
        ''' </summary>
        ''' <param name="pobjBeginNode">A leaf node present on the branch to trim.</param>
        Private Sub PruneImpossibleBranch(ByRef pobjBeginNode As clsStatusSnapshot, ByVal pintCurrentPrimaryID As Integer)
            'Remove current node from the working set first
            mlstWorkingSet.Remove(pobjBeginNode)

            PruneImpossibleBranchRec(pobjBeginNode)

            'If this is the end of parsing, record point of failure
            If mlstWorkingSet.Count = 0 Then
                mlngPointOfFailureID = pintCurrentPrimaryID
                Throw New Exception("Working set size: 0.  Point of failure, PrimaryID: " & mlngPointOfFailureID)
            End If
        End Sub

        Private Sub PruneImpossibleBranchRec(ByRef pobjBeginNode As clsStatusSnapshot)
            If pobjBeginNode.Parent IsNot Nothing Then
                'Loop all parent nodes
                For Each objParent As clsStatusSnapshot In pobjBeginNode.Parent.Priors
                    If objParent.Children.Count = 1 Then
                        'Continue recursing upwards
                        PruneImpossibleBranchRec(objParent)
                    Else
                        'Remove child from begin node
                        objParent.Children.Remove(pobjBeginNode)
                        'pobjBeginNode.Parent.Priors.Remove(objParent) CAUSES ERROR...
                    End If
                Next
            End If
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
                                                    plngRedFlagResetTime, plngBlueFlagResetTime, _
                                                    plngEventID)

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

            Dim drSwapRow As DataRow
            Dim drNextRow As DataRow

            sqlcmdGet = New SqlCommand("Calculations.spGetFlagEvents", mcxnStatsDB)
            sqlcmdGet.CommandType = CommandType.StoredProcedure
            sqlcmdGet.Parameters.AddWithValue("GameID", plngGameID)
            sqlcmdGet.CommandTimeout = 0

            Using reader As SqlDataReader = sqlcmdGet.ExecuteReader
                dtGameEvents.Load(reader)
            End Using

            For Each intPrimaryID In mlstSwapPointIDs
                drSwapRow = dtGameEvents.Rows(intPrimaryID)
                drNextRow = dtGameEvents.Rows(intPrimaryID + 1)

                Do Until IsValidSwap(drSwapRow, drNextRow)
                    intPrimaryID -= 1

                    drSwapRow = dtGameEvents.Rows(intPrimaryID)
                    drNextRow = dtGameEvents.Rows(intPrimaryID + 1)
                Loop

                If IsValidSwap(drSwapRow, drNextRow) Then
                    Print("Swaping rows with PrimaryIDs: " & intPrimaryID & " and " & intPrimaryID + 1)
                    SwapDataBetweenRows(drSwapRow, drNextRow)
                Else
                    Throw New Exception("Bad swap suggested, PrimaryIDs: " & intPrimaryID & " and " & intPrimaryID + 1)
                End If
            Next

            Return dtGameEvents
        End Function

        Private Function IsValidSwap(ByRef pdrSwapRow As DataRow, ByRef pdrNextRow As DataRow) As Boolean
            If pdrSwapRow("EventType").ToString.ToUpper.Equals("ITEM") AndAlso _
                pdrNextRow("EventType").ToString.ToUpper.Equals("KILL") AndAlso _
                CLng(pdrSwapRow("EventTime").ToString) = CLng(pdrNextRow("EventTime").ToString) AndAlso _
                CLng(pdrSwapRow("LineNo").ToString) + 1 = CLng(pdrNextRow("LineNo").ToString) Then
                Return True
            Else
                Return False
            End If
        End Function

        Private Sub SwapDataBetweenRows(ByRef pdrSwapRow As DataRow, ByRef pdrNextRow As DataRow)
            Dim objTemp As Object

            'Skip first row, since it is the primary key row
            For intIdx As Integer = 1 To pdrSwapRow.ItemArray.Count - 1
                objTemp = pdrNextRow.ItemArray.ElementAt(intIdx)

                pdrNextRow.SetField(intIdx, pdrSwapRow.ItemArray.ElementAt(intIdx))
                pdrSwapRow.SetField(intIdx, objTemp)
            Next
        End Sub

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
                        "SET IsFlagCalculationsComplete = 1, " & _
                        "   IsFlagCalculationsFailed = 0 " & _
                        "WHERE GameID = @GameID "
            Else
                strSQL = "UPDATE CalculatedData.Game " & _
                        "SET IsFlagCalculationsFailed = 1, " & _
                        "   IsFlagCalculationsComplete = 0 " & _
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
        Private Function GetGameTreeFileWriter(ByVal plngGameID As Long, Optional ByVal plngIdx As Long = 0) As StreamWriter
            Dim strGamePrintDir As String = My.Computer.FileSystem.CombinePath(mstrGameTreeOutputPath, CStr(plngGameID) & "\")
            If Not My.Computer.FileSystem.DirectoryExists(strGamePrintDir) Then
                My.Computer.FileSystem.CreateDirectory(strGamePrintDir)
            End If

            Dim strGamePrintFileName As String = "game-" & plngGameID & "-idx-" & plngIdx & ".txt"
            Dim strGameTreeFilePath As String = My.Computer.FileSystem.CombinePath(strGamePrintDir, strGamePrintFileName)
            Dim writer As New StreamWriter(New FileStream(strGameTreeFilePath, FileMode.Create))

            Return writer
        End Function

        ''' <summary>
        ''' Merges the duplicate leaves present in the current working set.
        ''' Meant to be run at the end of every event parse.  Should only
        ''' run at the end of an event parse, assumes all nodes in the WS will
        ''' have a parent.
        ''' </summary>
        Private Sub MergeDuplicateLeaves()
            Dim lstUniqueLeaves As New List(Of clsStatusSnapshot)
            Dim lstLeavesToRemoveFromWorkingSet As New List(Of clsStatusSnapshot)
            Dim blnFoundUniqueMatch As Boolean
            Dim objUniqueNode As clsStatusSnapshot = Nothing

            'Loop each working set node, trying to find identical game state
            'in the unique leaves list by looping it and comparing state vars.
            For Each objWorkingSetNode As clsStatusSnapshot In mlstWorkingSet
                'Look for matching game state already in unique nodes list
                blnFoundUniqueMatch = False
                For Each objUniqueNode In lstUniqueLeaves
                    If IsIdenticalGameState(objWorkingSetNode, objUniqueNode) Then
                        blnFoundUniqueMatch = True
                        Exit For 'objUniqueNode remains as current match
                    End If
                Next

                If blnFoundUniqueMatch Then
                    'Add this leaf to the list of nodes to remove from the working set, 
                    '(don't delete it now, that would screw up iteration, and 
                    'make ALL its parent have the unique node it matched as a child
                    lstLeavesToRemoveFromWorkingSet.Add(objWorkingSetNode)

                    For Each objParentNode As clsStatusSnapshot In objWorkingSetNode.Parent.Priors
                        'Cut link to current working set node
                        objParentNode.Children.Remove(objWorkingSetNode)

                        'Make this parent link to the unique node
                        objParentNode.AddChild(objUniqueNode.Parent)
                        objUniqueNode.Parent.AddPrior(objParentNode)
                    Next
                Else
                    'Add current leaf to the unique leafs list
                    lstUniqueLeaves.Add(objWorkingSetNode)
                End If
            Next

            'Clean nodes no longer in use from the WS
            For Each objNodeToDelete As clsStatusSnapshot In lstLeavesToRemoveFromWorkingSet
                mlstWorkingSet.Remove(objNodeToDelete)
            Next
        End Sub

        ''' <summary>
        ''' Determines whether the parameters represent the same current flag status state of the game.
        ''' </summary>
        ''' <param name="pobjNode1">The first node to compare.</param>
        ''' <param name="pobjNode2">The second node to compate.</param>
        ''' <returns>
        ''' <c>true</c> if [is identical game state]; otherwise, <c>false</c>.
        ''' </returns>
        Private Function IsIdenticalGameState(ByVal pobjNode1 As clsStatusSnapshot, ByVal pobjNode2 As clsStatusSnapshot) As Boolean
            Return pobjNode1.BlueFlagHolderClientID = pobjNode2.BlueFlagHolderClientID AndAlso _
                pobjNode1.BlueFlagInBase = pobjNode2.BlueFlagInBase AndAlso _
                pobjNode1.BlueFlagResetTime = pobjNode2.BlueFlagResetTime AndAlso _
                pobjNode1.RedFlagHolderClientID = pobjNode2.RedFlagHolderClientID AndAlso _
                pobjNode1.RedFlagInBase = pobjNode2.RedFlagInBase AndAlso _
                pobjNode1.RedFlagResetTime = pobjNode2.RedFlagResetTime
        End Function
#End Region
    End Class
End Namespace