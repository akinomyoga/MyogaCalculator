Module forms
#Region "structure button"
    Public Structure button
        Public p1 As Point
        Public p2 As Point
        Public shape As String
        Public id As String
        Public down As Boolean
        Public theta As Integer
        Public text As String
        Public Sub setButton(ByVal p1 As Point, ByVal p2 As Point, ByVal id As String, Optional ByVal shape As String = "square")
            If p1.X > p2.X Then p1.X += p2.X : p2.X = p1.X - p2.X : p1.X -= p2.X
            If p1.Y > p2.Y Then p1.Y += p2.Y : p2.Y = p1.Y - p2.Y : p1.Y -= p2.Y
            Me.p1 = p1
            Me.p2 = p2
            Me.id = id
            Me.shape = shape
            If shape = "circle" Then Me.theta = 90 - Microsoft.VisualBasic.Int(System.Math.Atan(Me.height ^ 2 / Me.width ^ 2) * 57.295779513082323)
        End Sub

        Public Sub showText(ByVal grf As Graphics, ByVal text As String)
            grf.DrawString(text, Form1.DefaultFont, Brushes.Black, p1.X, (p1.Y + p2.Y - Form1.DefaultFont.Size) / 2)
        End Sub
        Public Sub showButton(ByVal grf As Graphics)
            Select Case Me.shape
                Case "square"
                    grf.DrawLine(pen1, p1.X, p1.Y, p2.X, p1.Y)
                    grf.DrawLine(pen1, p1.X, p1.Y, p1.X, p2.Y)
                    grf.DrawLine(pen2, p1.X, p2.Y, p2.X, p2.Y)
                    grf.DrawLine(pen2, p2.X, p1.Y, p2.X, p2.Y)
                Case "square2"
                    Dim n, nn
                    For n = p1.Y To p2.Y
                        nn = ((p2.Y - n) / (p2.Y - p1.Y) + 1) / 2
                        pen3.Color = Color.FromArgb(255 * nn, 255 * nn, 255 * nn)
                        grf.DrawLine(pen3, p1.X, n, p2.X, n)
                    Next n
                    grf.DrawLine(pen1, p1.X, p1.Y, p1.X, p2.Y)
                    grf.DrawLine(pen2, p2.X, p1.Y, p2.X, p2.Y)
                Case "circle"
                    grf.DrawArc(pen1, p1.X, p1.Y, p2.X - p1.X, p2.Y - p1.Y, 90 + theta, 180)
                    grf.DrawArc(pen2, p1.X, p1.Y, p2.X - p1.X, p2.Y - p1.Y, 270 + theta, 180)
            End Select
            showText(grf, Me.text)
        End Sub
        Public Sub showMouseDown(ByVal grf As Graphics)
            Select Case Me.shape
                Case "square"
                    grf.DrawLine(pen2, p1.X, p1.Y, p2.X, p1.Y)
                    grf.DrawLine(pen2, p1.X, p1.Y, p1.X, p2.Y)
                    grf.DrawLine(pen1, p1.X, p2.Y, p2.X, p2.Y)
                    grf.DrawLine(pen1, p2.X, p1.Y, p2.X, p2.Y)
                Case "square2"
                    Dim n, nn
                    For n = p1.Y To p2.Y
                        nn = 128 * (1 - (p2.Y - n) / (p2.Y - p1.Y)) + 127
                        pen3.Color = Color.FromArgb(nn, nn, nn)
                        grf.DrawLine(pen3, p1.X, n, p2.X, n)
                    Next n
                    grf.DrawLine(pen1, p1.X, p1.Y, p1.X, p2.Y)
                    grf.DrawLine(pen2, p2.X, p1.Y, p2.X, p2.Y)
                Case "circle"
                    grf.DrawArc(pen2, p1.X, p1.Y, p2.X - p1.X, p2.Y - p1.Y, theta + 90, 180)
                    grf.DrawArc(pen1, p1.X, p1.Y, p2.X - p1.X, p2.Y - p1.Y, theta + 270, 180)
            End Select
            showText(grf, Me.text)
        End Sub

        Public Sub whenMouseDown(ByVal grf As Graphics, ByVal p As Point, ByVal mb As MouseButtons)
            If OnOrOutButton(p) And MouseButton(mb, "左") Then
                Me.showMouseDown(grf)
                down = True
            End If
        End Sub
        Public Sub whenMouseUp(ByVal grf As Graphics, ByVal p As Point, ByVal mb As MouseButtons)
            If MouseButton(oldmousebuttons - mb, "左") Then
                If OnOrOutButton(p) Then
                    clicked(id)
                End If
                If Me.down = True Then showButton(grf) : Me.down = False
            End If
        End Sub
        Public Function OnOrOutButton(ByVal position As Point) As Boolean
            Select Case Me.shape
                Case "square", "square2"
                    OnOrOutButton = Me.p1.X <= position.X And position.X <= Me.p2.X And Me.p1.Y <= position.Y And position.Y <= Me.p2.Y
                Case "circle"
                    Dim xa = System.Math.Abs(2 * position.X - (Me.p1.X + Me.p2.X)) / (Me.p2.X - Me.p1.X)
                    Dim ya = System.Math.Abs(2 * position.Y - (Me.p1.Y + Me.p2.Y)) / (Me.p2.Y - Me.p1.Y)
                    OnOrOutButton = (xa * xa + ya * ya <= 1)
            End Select
        End Function
#Region "buttons.Pubic Properties"
        Public Property width() As Integer
            Get
                width = Me.p2.X - Me.p1.X
            End Get
            Set(ByVal value As Integer)
                Me.p2.X = Me.p1.X + System.Math.Abs(value)
            End Set
        End Property
        Public Property height() As Integer
            Get
                height = Me.p2.Y - Me.p1.Y
            End Get
            Set(ByVal Value As Integer)
                Me.p2.Y = Me.p1.Y + System.Math.Abs(Value)
            End Set
        End Property
        Public Property left() As Integer
            Get
                left = Me.p1.X
            End Get
            Set(ByVal Value As Integer)
                Me.p2.X += left - Me.p1.X
                Me.p1.X = left
            End Set
        End Property
        Public Property top() As Integer
            Get
                top = Me.p1.Y
            End Get
            Set(ByVal Value As Integer)
                Me.p2.Y += top - Me.p1.Y
                Me.p1.Y = top
            End Set
        End Property
#End Region
    End Structure
#End Region

    Public forma As Form1

    Public buttons(99) As forms.button
    Public button_cnt = 0

    Public oldmousebuttons As MouseButtons
    Public keyF As String = "ffffffffffffffffffffffff" 'F1 to F24
    Public keyS As String = "ffff" 'shift ctrl alt fn"
    Public keyL As String = "ffffffffffffffffffffffffff"
    Public keyN As String = "ffffffffff"
    Public Function FromID(ByVal id As String) As forms.button
        For Each but As forms.button In buttons
            If id = but.id Then Return but
        Next
        Return buttons(0)
    End Function
    Public Function IndexFromID(ByVal id As String) As Integer
        Dim m As Integer = UBound(buttons)
        Dim n As Integer
        For n = 0 To m
            If id = buttons(n).id Then Return n
        Next
    End Function

    Public Function MouseButton(ByVal mousebutton1 As MouseButtons, ByVal whichbutton As String) As Boolean
        MouseButton = False
        mousebutton1 /= 1048576
        Select Case whichbutton
            Case 1, "左"
                If mousebutton1 Mod 2 = 1 Then Return True
            Case 2, "右"
                If mousebutton1 Mod 4 > 1 Then Return True
            Case 3, "中"
                If mousebutton1 Mod 8 > 3 Then Return True
            Case 4, "上"
                If mousebutton1 Mod 16 > 7 Then Return True
            Case 5, "下"
                If mousebutton1 > 15 Then Return True
        End Select
    End Function
    Public Sub draw_button(ByRef frm As Graphics, ByVal p1 As Point, ByVal p2 As Point, Optional ByVal shape As String = "square")
        If button_cnt = 100 Then Exit Sub
        buttons(button_cnt).setButton(p1, p2, shape)
        buttons(button_cnt).showButton(frm)
        button_cnt = button_cnt + 1
    End Sub
    Public Sub CreateButton(ByVal p1 As System.Drawing.Point, ByVal p2 As System.Drawing.Point, ByVal id As String, Optional ByVal shape As String = "square")
        If button_cnt = 100 Then Exit Sub
        buttons(button_cnt).setButton(p1, p2, id, shape)
        button_cnt += 1
    End Sub
#Region "event"
    Public Sub MouseDown(ByVal grf As Graphics, ByVal p As Point)
        Dim n
        For n = 1 To button_cnt
            buttons(n - 1).whenMouseDown(grf, p, Form1.MouseButtons)
        Next
        oldmousebuttons = forma.MouseButtons
    End Sub
    Public Sub MouseUp(ByVal grf As Graphics, ByVal p As Point)
        Dim n As Integer
        For n = 1 To button_cnt
            buttons(n - 1).whenMouseUp(grf, p, Form1.MouseButtons)
        Next
        oldmousebuttons = forma.MouseButtons
    End Sub
    Public Sub KeyDown(ByVal keycode As keyS)
        Select Case keycode
            Case 112 To 135
                keyF = StrReIns(keyF, keycode - 111, "t")
            Case 65 To 90
                keyL = StrReIns(keyL, keycode - 64, "t")
            Case 48 To 57
                keyN = StrReIns(keyN, keycode - 47, "t")
            Case 16
                keyS = StrReIns(keyS, 1, "t")
            Case 17
                keyS = StrReIns(keyS, 2, "t")
            Case 18
                keyS = StrReIns(keyS, 3, "t")
            Case Else
        End Select
    End Sub
    Public Sub KeyUp(ByVal keycode As keyS)
        Select Case keycode
            Case 112 To 135
                keyF = StrReIns(keyF, keycode - 111, "f")
            Case 65 To 90
                keyL = StrReIns(keyL, keycode - 64, "f")
            Case 48 To 57
                keyN = StrReIns(keyN, keycode - 47, "f")
            Case 16
                keyS = StrReIns(keyS, 1, "f")
            Case 17
                keyS = StrReIns(keyS, 2, "f")
            Case 18
                keyS = StrReIns(keyS, 3, "f")
            Case Else
                MsgBox("keyup keycode = " & keycode)
        End Select
    End Sub
    Public Sub LFocus()
        keyF = "ffffffffffffffffffffffff"
        keyS = "ffff"
        keyL = "ffffffffffffffffffffffffffff"
        keyN = "ffffffffff"
    End Sub
#End Region

    Public pen1 As New System.Drawing.Pen(Color.FromArgb(255, 255, 255), 1)
    Public pen2 As New System.Drawing.Pen(Color.FromArgb(0, 0, 0), 1)
    Public pen3 As New System.Drawing.Pen(Color.FromArgb(0, 0, 0), 1)

    Public Sub setParentForm(ByVal form)
        forma = form
    End Sub
    '#####################################
    Public Sub clicked(ByVal id As String)
        Select Case id
            Case 0
                Dim x, msg1
                For x = 0 To 10
                    msg1 &= "Tn_" & x & " = " & 計算1.Tn(x, 0) & Chr(13)
                Next
                MsgBox(msg1 & "Bn_10 = " & Bn(10))
            Case "f"
                If forma.monitorshowed = False Then
                    forma.monitor1 = New monitor
                    forma.monitorshowed = True
                    forma.monitor1.parent1 = forma
                End If

                forma.monitor1.Show()
            Case "1"
                Dim x, n
                MsgBox("関数 Γ2() でΓ関数の計算を10000回行います。")
                Dim msg = Format(Date.Now, "m:s")
                Do While n <= 10000
                    x = 計算1.Γ(n + 0.5)
                    n += 1
                Loop
                MsgBox(msg & Chr(13) & Format(Date.Now, "m:s"))
            Case 4
                lifegame.life1()
            Case 5
                lifegame.set1()
            Case 3
                MsgBox(計算1.Γa(5, 5))
            Case 6
            Case "graph1"
                graph.graph1()
            Case "左"
                graph.seep.x -= 5
                graph.seed.x += 5
                graph.grf1.Clear(RGB(255, 255, 255))
                graph.graph1()
            Case "右"
                graph.seep.x += 5
                graph.seed.x -= 5
                graph.grf1.Clear(RGB(255, 255, 255))
                graph.graph1()
            Case "奥"
                graph.seep.y += 5
                graph.seed.y -= 5
                graph.grf1.Clear(RGB(255, 255, 255))
                graph.graph1()
            Case "手前"
                graph.seep.y -= 5
                graph.seed.y += 5
                graph.grf1.Clear(RGB(255, 255, 255))
                graph.graph1()
        End Select
    End Sub

    Public keylabels(35) As System.Windows.Forms.Label

    '#####################################
End Module
