Module graph

    Public grf1 As Graphics
    Sub setgraphics(ByVal grf As Graphics)
        grf1 = grf
    End Sub

    Public penA As New Pen(Color.FromArgb(0, 0, 0))
    Public Sub SetPenAColor(ByVal col As Color)
        penA.Color = col
    End Sub

    Public Sub line(ByVal p1 As Point, ByVal p2 As Point, Optional ByVal baseX As Integer = 0, Optional ByVal baseY As Integer = 0)
        p1.X += baseX
        p2.X += baseX
        p1.Y += baseY
        p2.Y += baseY
        grf1.DrawLine(penA, p1, p2)
    End Sub
    Public Sub pset(ByVal p1 As Point, Optional ByVal clr1 As Object = 0)
        Dim pen0 As Pen = penA.Clone
        If TypeOf clr1 Is Color Then
            pen0.Color = clr1
        End If
        grf1.DrawLine(pen0, p1.X, p1.Y, p1.X + 1, p1.Y + 1)
    End Sub

#Region "3d"
    Public Structure point3d
        Public x As Double
        Public y As Double
        Public z As Double
        Public Sub New(ByVal x As Double, ByVal y As Double, ByVal z As Double)
            Me.x = x
            Me.y = y
            Me.z = z
        End Sub
        Public Function to2d(ByVal seePoint As point3d, ByVal seeDirection As point3d) As System.Drawing.PointF
            Dim p1 As New point3d(Me.x - seePoint.x, Me.y - seePoint.y, Me.z - seePoint.z)
            Dim r1 As Double = RadMinus(XYRad(p1.y, p1.z), XYRad(seeDirection.y, seeDirection.z))
            Dim r2 As Double = RadMinus(XYRad(p1.x, p1.y), XYRad(seeDirection.x, seeDirection.y))
            If r1 > pi Then r1 -= C2MUpi
            If r2 > pi Then r2 -= C2MUpi
            to2d.X = -r2 * 500
            to2d.Y = -r1 * 500
        End Function
        Public Function minus(ByVal p As point3d) As point3d
            minus.x = Me.x - p.x
            minus.y = Me.y - p.y
            minus.z = Me.z - p.z
        End Function
    End Structure
#End Region
#Region "bitmap"
    Dim bmp As System.drawing.Bitmap
    Public Function BmpEmb(ByVal theta As Single, Optional ByVal distance As Integer = 1, Optional ByVal emphasis As Double = 0.5) As System.Drawing.Bitmap
        BmpEmb = bmp
        If theta > 6.2831853071795862 Then theta = DegRad(theta)
        If emphasis = 0 Then emphasis = 0.5
        Dim sin As Double = System.Math.Sin(theta)
        Dim cos As Double = System.Math.Cos(theta)
        Dim r As Integer, g As Integer, b As Integer
        Dim x As Integer, y As Integer, n2 As Integer, x2 As Double, y2 As Double
        Dim c As Integer
        For x = 1 To bmp.Width
            For y = 1 To bmp.Height
                x2 = x + cos * distance
                y2 = y - sin * distance
                For n2 = 1 To distance 'ü‚è‚Ì“_‚ÌF‚ÌûW
                    If 1 <= x2 And x2 <= bmp.Width And 1 <= y2 And y2 <= bmp.Height Then
                        r += bmp.GetPixel(x2, y2).R : g += bmp.GetPixel(x2, y2).G : b += bmp.GetPixel(x2, y2).B
                        c += 1
                    End If
                    x2 -= cos
                    y2 += sin
                Next n2
                If Not c = 0 Then
                    r /= c : g /= c : b /= c
                End If
                r = 127 + (bmp.GetPixel(x, y).R - r) / emphasis
                g = 127 + (bmp.GetPixel(x, y).G - g) / emphasis
                b = 127 + (bmp.GetPixel(x, y).B - b) / emphasis
                BmpEmb.SetPixel(x, y, Color.FromArgb(r, g, b))
            Next y
        Next x
    End Function
    Public Function BmpMono(Optional ByVal unit As Integer = 1) As Bitmap
        BmpMono = bmp
        Dim x As Integer, y As Integer
        For x = 1 To bmp.Width
            For y = 1 To bmp.Height
                BmpMono.SetPixel(x, y, colormono(bmp.GetPixel(x, y), unit))
            Next
        Next
    End Function
#End Region
#Region "color"
    Public Function RGB(ByVal r As Integer, ByVal g As Integer, ByVal b As Integer) As Color
        RGB = Color.FromArgb(r, g, b)
    End Function
    Public Function colorplus(ByVal col1 As Color, ByVal col2 As Color) As Color
        Return Color.FromArgb((col1.R + col2.R) / 2, (col1.G + col2.G) / 2, (col1.B + col2.B) / 2)
    End Function
    Public Function colormulti(ByVal col1 As Color, ByVal col2 As Color) As Color
        Return Color.FromArgb(col1.R * col2.R / 255, col1.G * col2.G / 255, col1.B * col2.B / 255)
    End Function
    Public Function colormono(ByVal col As Color, Optional ByVal unit As Integer = 1) As Color
        Dim a As Integer = (col.R + col.G + col.B) / 3
        a -= a Mod unit
        colormono.FromArgb(a, a, a)
    End Function
#End Region
#Region "point"
    Public Function IntPoint(ByVal x As Double, ByVal y As Double) As Point
        IntPoint.X = Microsoft.VisualBasic.Int(x)
        IntPoint.Y = Microsoft.VisualBasic.Int(y)
    End Function
    Public Function IntPointF(ByVal x As Double, ByVal y As Double) As PointF
        IntPointF.X = CSng(x)
        IntPointF.Y = CSng(y)
    End Function
    Public Function IntP3(ByVal x As Decimal, ByVal y As Decimal, ByVal z As Decimal) As point3d
        IntP3.x = x
        IntP3.y = y
        IntP3.z = z
    End Function
    Public Function PntFPnt(ByVal p As PointF) As Point
        PntFPnt.X = Microsoft.VisualBasic.Int(p.X)
        PntFPnt.Y = Microsoft.VisualBasic.Int(p.Y)
    End Function
#End Region
#Region "angle"
    Public Function XYRad(ByVal x As Double, ByVal y As Double) As Double
        If x = 0 Then If y < 0 Then Return CpiRE2 + pi Else Return CpiRE2
        XYRad = System.Math.Atan(y / x)
        If XYRad < 0 Then XYRad += pi
        If y < 0 Then Return XYRad + pi
        If x < 0 And XYRad = 0 Then Return pi
    End Function
    Public Function RadMinus(ByVal x As Double, ByVal y As Double) As Double
        If x < 0 Then x += C2MUpi * Microsoft.VisualBasic.Int((-x) / C2MUpi + 1) : GoTo skip
        Do While x < 0
            x += C2MUpi
        Loop
skip:
        Do While y > 0
            y -= C2MUpi
        Loop
        RadMinus = (x - y) Mod C2MUpi
    End Function
#End Region
    Public seep As point3d = IntP3(10, -50, 30)
    Public seed As point3d = IntP3(-10, 50, -30)
    Public Sub graph1()
        Dim x As Double, y As Double, z As Double, r As Double
        Dim op As PointF, p As PointF, op2 As PointF, p2 As PointF
        For x = -20 To 20
            For y = -20 To 20 Step 0.5
                r = System.Math.Sqrt(x * x + y * y)
                'z = System.Math.Cos(r)
                z = System.Math.Exp(-r * r / 32) * 10 '³‹K•ª•z
                p = IntP3(y, x, z).to2d(seep, seed)
                p2 = IntP3(x, y, z).to2d(seep, seed)
                If Not y = -20 Then
                    graph.line(PntFPnt(op), PntFPnt(p), 400, 200)
                    graph.line(PntFPnt(op2), PntFPnt(p2), 400, 200)
                End If
                op = p
                op2 = p2
            Next
        Next
    End Sub
End Module
Public Module lifegame
    Public a(101, 101) As Boolean, b(101, 101) As Boolean
    Public times = 10 'ŒJ‚è•Ô‚µ”

    Public red As Color = RGB(255, 255, 255)
    Public green As Color = RGB(0, 0, 0)
    Public Sub set1()
        Select Case 3
            Case 1
                a(21, 22) = True : b(21, 22) = True
                a(22, 22) = True : b(22, 22) = True
                a(23, 22) = True : b(23, 22) = True
                a(23, 21) = True : b(23, 21) = True
                a(22, 23) = True : b(22, 23) = True
            Case 2
                Dim x As Integer
                For x = 0 To 101
                    a(50, x) = True
                    b(50, x) = True
                    a(x, 50) = True
                    b(x, 50) = True
                Next
            Case 3
                Dim x As Integer
                For x = 0 To 101
                    a(x, x) = True
                    b(x, x) = True
                    a(x, 101 - x) = True
                    b(x, 101 - x) = True
                Next
        End Select
    End Sub
    Public Sub life1()

        Dim c As Integer, d As Integer
        Dim x As Integer
        Dim y As Integer
        For c = 1 To times
            For d = 1 To 100
                Dim z As Integer
                For x = 1 To 100
                    For y = 1 To 100
                        z = 0
                        If a(x - 1, y) Then z += 1
                        If a(x - 1, y - 1) Then z += 1
                        If a(x - 1, y + 1) Then z += 1
                        If a(x + 1, y) Then z += 1
                        If a(x + 1, y - 1) Then z += 1
                        If a(x + 1, y + 1) Then z += 1
                        If a(x, y - 1) Then z += 1
                        If a(x, y + 1) Then z += 1
                        If z = 3 Then
                            b(x, y) = True
                            If Not a(x, y) Then pset(IntPoint(2 * (x - 1), 2 * (y - 1)), red)
                        ElseIf z > 3 Or z < 2 Then
                            b(x, y) = False
                            If a(x, y) Then pset(IntPoint(2 * (x - 1), 2 * (y - 1)), green)
                        End If
                    Next y
                Next x
                For x = 1 To 100
                    For y = 1 To 100
                        a(x, y) = b(x, y)
                    Next y
                    a(x, 0) = b(x, 100)
                    a(x, 101) = b(x, 1)
                    a(0, x) = b(100, x)
                    a(101, x) = b(1, x)
                Next x
                a(0, 0) = b(100, 100)
                a(0, 101) = b(100, 1)
                a(101, 0) = b(1, 100)
                a(101, 101) = b(1, 1)
            Next d
            Application.DoEvents()
        Next c
        '  V = V + times
        ' Form1.Caption = "Life Game-" & V & "‰ñŒvZ"
    End Sub
End Module