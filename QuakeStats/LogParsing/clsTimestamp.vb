Option Explicit On
Option Strict On

Namespace LogParsing
    Public Class clsTimestamp
        Public Enum enuTimestampPrecisionType
            Unknown = 0
            Seconds = 1
            TensOfSeconds = 2
            Minutes = 3
        End Enum

        Private Const MINT_COLON_POSITION_ADJUSTMENT As Integer = 3
        Private Const MINT_SECONDS_IN_MINUTE As Integer = 60
        Private Const MINT_SECONDS_IN_HOUR As Integer = MINT_SECONDS_IN_MINUTE * 60
        Private Const MINT_SECONDS_IN_DAY As Integer = MINT_SECONDS_IN_HOUR * 24
        Private Const MINT_TICKS_IN_SECOND As Integer = 1000

        Private mstrRawString As String
        Private mlngSeconds As Long
        Private menuPrecision As enuTimestampPrecisionType

        Public ReadOnly Property RawString() As String
            Get
                Return mstrRawString
            End Get
        End Property

        Public ReadOnly Property Seconds() As Long
            Get
                Return mlngSeconds
            End Get
        End Property

        Public ReadOnly Property Precision() As enuTimestampPrecisionType
            Get
                Return menuPrecision
            End Get
        End Property

        Public Sub New(ByVal pstrRawTimeString As String)
            'Constructor takes the first seven chars from a line
            Dim strMinutes As String
            Dim strSeconds As String

            mstrRawString = pstrRawTimeString

            'Detemine precision by position of colon: subtract 
            'magic number of colon postion adjuster from actual position of the
            'colon in the raw data string to determine precision
            menuPrecision = CType(InStr(pstrRawTimeString, ":") - MINT_COLON_POSITION_ADJUSTMENT, enuTimestampPrecisionType)

            'Read minutes portion of the timestamp string
            strMinutes = pstrRawTimeString.Substring(0, InStr(pstrRawTimeString, ":") - 1)
            'Read seconds portion
            strSeconds = pstrRawTimeString.Substring(InStr(pstrRawTimeString, ":"))

            'Convert to seconds and store value
            mlngSeconds = (CLng(strMinutes) * MINT_SECONDS_IN_MINUTE) + CLng(strSeconds)
        End Sub

        Public Shared Function PrecisionToString(ByVal penuTimestampPrecision As enuTimestampPrecisionType) As String
            Dim strResult As String

            Select Case penuTimestampPrecision
                Case enuTimestampPrecisionType.Unknown
                    strResult = "UNKNOWN"
                Case enuTimestampPrecisionType.Seconds
                    strResult = "SECONDS"
                Case enuTimestampPrecisionType.TensOfSeconds
                    strResult = "TENS OF SECONDS"
                Case enuTimestampPrecisionType.Minutes
                    strResult = "MINUTES"
                Case Else
                    strResult = "INVALID"
            End Select

            Return strResult
        End Function

        Public Function Print(Optional ByVal pblnUseFixedSpacing As Boolean = False) As String
            Dim lngRemainingSeconds As Long
            Dim lngDays As Long
            Dim lngHours As Long
            Dim lngMinutes As Long
            Dim strResult As String = String.Empty

            lngRemainingSeconds = mlngSeconds

            lngDays = lngRemainingSeconds \ MINT_SECONDS_IN_DAY
            lngRemainingSeconds = lngRemainingSeconds Mod MINT_SECONDS_IN_DAY

            lngHours = lngRemainingSeconds \ MINT_SECONDS_IN_HOUR
            lngRemainingSeconds = lngRemainingSeconds Mod MINT_SECONDS_IN_HOUR

            lngMinutes = lngRemainingSeconds \ MINT_SECONDS_IN_MINUTE
            lngRemainingSeconds = lngRemainingSeconds Mod MINT_SECONDS_IN_MINUTE

            If pblnUseFixedSpacing Then
                strResult = lngDays & " days, " & lngHours & " hours, " & lngMinutes & " min, " & lngRemainingSeconds & " sec"
            Else
                If lngDays > 0 Then
                    strResult &= lngDays & " days"
                End If
                If lngHours > 0 Or lngDays > 0 Then
                    If strResult = String.Empty Then
                        strResult = lngHours & " hrs"
                    Else
                        strResult &= ", " & lngHours & " hours"
                    End If
                End If
                If lngMinutes > 0 Or lngHours > 0 Or lngDays > 0 Then
                    If strResult = String.Empty Then
                        strResult = lngMinutes & " min"
                    Else
                        strResult &= ", " & lngMinutes & " min"
                    End If
                End If
                If strResult = String.Empty Then
                    strResult = lngRemainingSeconds & " sec"
                Else
                    strResult &= ", " & lngRemainingSeconds & " sec"
                End If
            End If

            Return strResult
        End Function

        Public Function ActualTime(ByVal pdtmServerStartupTime As DateTime) As DateTime
            Return New Date(pdtmServerStartupTime.Ticks + (mlngSeconds * MINT_TICKS_IN_SECOND))
        End Function
    End Class
End Namespace