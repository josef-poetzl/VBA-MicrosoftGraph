VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "Form_CreateEvent"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Compare Database
Option Explicit
Dim sGrantType As String

Private Sub cmdCreateEvent_Click()
    If IsNull(dStart) Or IsNull(tStart) Or IsNull(dEnd) Or IsNull(tEnd) Then
        MsgBox "Start and End Date and Times are mandatory"
        Exit Sub
    End If
    If Nz(Me.fromMailbox, "") = "" And sGrantType = "client_credentials" Then
        MsgBox "From whose mailbox is mandatory and must be a valid mailbox in this client."
        Me.fromMailbox.SetFocus
    Else
        CreateCalendarEvent Graph.CreateEvent(Nz(Me.fromMailbox, ""), Nz(Me.sSubject, ""), "HTML", Nz(Me.sBody, ""), dStart, tStart, dEnd, tEnd, Nz(sLocation, ""), Nz(Me.sAttendees, ""), Nz(Me.sOptional, ""))
    End If
End Sub

Private Sub CreateCalendarEvent(Response As WebResponse)
    If Response.StatusCode = WebStatusCode.Created Then
        MsgBox "Event created."
    Else
        MsgBox "Error " & Response.StatusCode & ": " & Response.Content
    End If
End Sub

Private Sub Form_Open(Cancel As Integer)
    sGrantType = DLookup("GrantType", "AdminTable") 'Authorization grant type
    If sGrantType = "client_credentials" Then
        Me.fromMailbox.Visible = True
    Else
        Me.fromMailbox.Visible = False
    End If
End Sub

