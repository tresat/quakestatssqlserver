Option Explicit On
Option Strict On

Namespace Utilities
    Public Class clsHighPerformanceTimer

#Region "Member Vars"
        Private mblnTimerRunning As Boolean
        Private mint64Start As Int64
        Private mint64Stop As Int64
#End Region

#Region "Constructors"
        Public Sub New()
            mint64Start = 0
            mint64Stop = 0
            mblnTimerRunning = False
        End Sub
#End Region

#Region "Win API Calls"
        ''' <summary>
        ''' Win API function call to query the performance counter.  QueryPerformanceCounter 
        ''' returns the current performance counter 
        ''' value in a 64-bit integer. You can use this function in the same way as you use the other
        ''' timers: get the starting value, perform an operation, and then subtract the starting 
        ''' value from the ending value.  That gives you the difference in performance counter values, 
        ''' but it doesn't provide a time reference.
        ''' </summary>
        ''' <param name="perfcount">Reference param which holds return value.</param>
        ''' <returns>Success/Failure.</returns>
        <System.Runtime.InteropServices.DllImport("Kernel32.dll")> _
        Private Shared Function QueryPerformanceCounter(ByRef perfcount As Int64) As Boolean
            ' Leave function empty - DLLImport attribute 
            ' forces calls to function to
            ' be forwarded to function in KERNEL32.DLL
        End Function

        ''' <summary>
        ''' Win API function call to query the performance frequency.  The time reference 
        ''' is supplied by QueryPerformanceFrequency. This function returns the performance 
        ''' frequency—the number of performance counter values per second. 
        ''' So the number of elapsed seconds is:
        '''   <c>(ending_value – starting_value) / frequency</c>
        ''' </summary>
        ''' <param name="freq">Reference param which holds return value.</param>
        ''' <returns>Success/Failure.</returns>
        <System.Runtime.InteropServices.DllImport("Kernel32.dll")> _
        Private Shared Function QueryPerformanceFrequency(ByRef freq As Int64) As Boolean
            ' Leave function empty - DLLImport attribute 
            ' forces calls to function to
            ' be forwarded to function in KERNEL32.DLL
        End Function
#End Region

#Region "Public Functionality"
        ''' <summary>
        ''' Starts the timer.
        ''' </summary>
        Public Sub StartTimer()
            If mblnTimerRunning Then
                Throw New Exception("Timer already running!")
            End If

            If Not QueryPerformanceCounter(mint64Start) Then
                Throw New Exception("Error: call to QueryPerformanceCounter() failed!")
            End If

            mblnTimerRunning = True
        End Sub

        ''' <summary>
        ''' Stops the timer.
        ''' </summary>
        Public Sub StopTimer()
            If Not mblnTimerRunning Then
                Throw New Exception("Timer not started!")
            End If

            If Not QueryPerformanceCounter(mint64Stop) Then
                Throw New Exception("Error: call to QueryPerformanceCounter() failed!")
            End If

            mblnTimerRunning = False
        End Sub

        ''' <summary>
        ''' Gets the result in seconds.
        ''' </summary>
        ''' <returns>Time is seconds, with milliseconds after decimal</returns>
        Public Function GetResultInSeconds() As Double
            Dim int64ElapsedCount As Int64
            Dim int64Frequency As Int64
            Dim dblElapsedSeconds As Double

            If mblnTimerRunning Then Throw New Exception("Timer still running!")

            'Calculate elapsed time
            int64ElapsedCount = mint64Stop - mint64Start

            'Get the performance frequency
            QueryPerformanceFrequency(int64Frequency)

            'Calculate performance in seconds
            dblElapsedSeconds = int64ElapsedCount / int64Frequency

            Return dblElapsedSeconds
        End Function

        ''' <summary>
        ''' Gets the result in a more readable form.
        ''' </summary>
        ''' <returns>Time in form "X hrs, Y min, Z.ZZZZ sec"</returns>
        Public Function GetResultAsTimeString() As String
            Const NUM_MILLISECONDS_IN_SECOND As Integer = 1000
            Const NUM_SECONDS_IN_MINUTE As Integer = 60
            Const NUM_MINUTES_IN_HOUR As Integer = 60

            Dim int64ElapsedCount As Int64
            Dim int64Frequency As Int64
            Dim lngElapsedMilliSeconds As Long
            Dim lngHours As Long, lngMinutes As Long, lngSeconds As Long

            If mblnTimerRunning Then Throw New Exception("Timer still running!")

            'Calculate elapsed time
            int64ElapsedCount = mint64Stop - mint64Start

            'Get the performance frequency
            QueryPerformanceFrequency(int64Frequency)

            'Round performance to closest millisecs
            lngElapsedMilliSeconds = CLng((int64ElapsedCount / int64Frequency) * NUM_MILLISECONDS_IN_SECOND)

            'Calculate time values
            lngHours = lngElapsedMilliSeconds \ (NUM_MINUTES_IN_HOUR * NUM_SECONDS_IN_MINUTE * NUM_MILLISECONDS_IN_SECOND)
            lngElapsedMilliSeconds -= lngHours * (NUM_MINUTES_IN_HOUR * NUM_SECONDS_IN_MINUTE * NUM_MILLISECONDS_IN_SECOND)
            lngMinutes = lngElapsedMilliSeconds \ (NUM_SECONDS_IN_MINUTE * NUM_MILLISECONDS_IN_SECOND)
            lngElapsedMilliSeconds -= lngMinutes * (NUM_SECONDS_IN_MINUTE * NUM_MILLISECONDS_IN_SECOND)
            lngSeconds = lngElapsedMilliSeconds \ (NUM_MILLISECONDS_IN_SECOND)
            lngElapsedMilliSeconds -= lngSeconds * NUM_MILLISECONDS_IN_SECOND

            Return lngHours & " hrs, " & MakeXDigits(lngMinutes, 2) & " min, " & MakeXDigits(lngSeconds, 2) & "." & MakeXDigits(lngElapsedMilliSeconds, 4) & " sec"
        End Function
#End Region

#Region "Private Helpers"
        ''' <summary>
        ''' Given a value less than or equal to X digits long, returns a string of length X, prepadded with 0s.
        ''' </summary>
        ''' <param name="plngValue">The value.</param>
        ''' <param name="pintNumDigits">The length to pad .</param>
        ''' <returns>String representing value prepadded with 0s to proper length.</returns>
        Private Function MakeXDigits(ByVal plngValue As Long, ByVal pintNumDigits As Integer) As String
            Dim strResult As String = CStr(plngValue)

            If plngValue >= 0 Then
                If strResult.Length > pintNumDigits Then
                    Throw New Exception("Value: " & plngValue & " is too large to fit in a string of length: " & pintNumDigits & "!")
                End If

                While Len(strResult) < pintNumDigits
                    strResult = "0" & strResult
                End While
            Else
                Throw New Exception("Value: " & plngValue & " is less than 0.")
            End If

            Return strResult
        End Function
#End Region
    End Class
End Namespace
